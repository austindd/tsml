module [
    Substitution,
    UnifyError,
    unify_constraints,
    apply_substitution,
    solve_constraint,
]

import TypeCore exposing [TypeId, TypeTable, SimpleType]
import SimpleConstraint exposing [Constraint, ConstraintSet]
import Dict

Substitution : Dict.Dict TypeId TypeId

UnifyError : [
    TypeMismatch TypeId TypeId,
    OccursCheck TypeId TypeId,
    FieldMissing TypeId Str,
    NotCallable TypeId,
]

unify_constraints : ConstraintSet -> Result (Substitution, TypeTable) UnifyError
unify_constraints = \constraint_set ->
    initial_subst = Dict.empty {}
    solve_all constraint_set.constraints constraint_set.type_table initial_subst

solve_all : List Constraint, TypeTable, Substitution -> Result (Substitution, TypeTable) UnifyError
solve_all = \constraints, table, subst ->
    when constraints is
        [] -> Ok (subst, table)
        [first, .. as rest] ->
            when solve_constraint first table subst is
                Ok (new_subst, new_table) ->
                    # Apply substitution to remaining constraints
                    updated_constraints = List.map rest \c ->
                        apply_constraint_subst c new_subst
                    solve_all updated_constraints new_table new_subst
                Err e -> Err e

solve_constraint : Constraint, TypeTable, Substitution -> Result (Substitution, TypeTable) UnifyError
solve_constraint = \constraint, table, subst ->
    when constraint is
        CEq t1 t2 ->
            unify_types t1 t2 table subst

        CSub sub super ->
            # For now, treat subtyping as equality (simplified)
            unify_types sub super table subst

        CField obj_id field_name field_tid ->
            # Get the actual type of the object
            obj_tid = apply_subst obj_id subst
            when TypeCore.get_type table obj_tid is
                Ok (TObject fields) ->
                    # Check if field exists
                    when List.find_first fields \f -> f.key == field_name is
                        Ok field ->
                            unify_types field.tid field_tid table subst
                        Err _ ->
                            Err (FieldMissing obj_id field_name)
                Ok (TVar _) ->
                    # Object is a variable - create object type with this field
                    (new_obj_tid, table1) = TypeCore.mk_obj table [{ key: field_name, tid: field_tid, optional: Bool.false }]
                    new_subst = Dict.insert subst obj_tid new_obj_tid
                    Ok (new_subst, table1)
                _ ->
                    Err (FieldMissing obj_id field_name)

        CCall fn_tid arg_tids result_tid ->
            # Get function type
            fn_actual = apply_subst fn_tid subst
            when TypeCore.get_type table fn_actual is
                Ok (TFunction params_tid ret_tid) ->
                    # For simplicity, assume params is a tuple
                    # In real implementation, we'd handle multiple params properly
                    when arg_tids is
                        [] ->
                            unify_types ret_tid result_tid table subst
                        [single_arg] ->
                            when unify_types params_tid single_arg table subst is
                                Ok (subst1, table1) ->
                                    unify_types ret_tid result_tid table1 subst1
                                Err e -> Err e
                        _ ->
                            # Multiple args - would need tuple handling
                            unify_types ret_tid result_tid table subst

                Ok (TVar _) ->
                    # Function is a variable - create function type
                    (params_tid, table1) = when arg_tids is
                        [] -> TypeCore.mk_lit table LUndefined
                        [single] -> (single, table)
                        _ -> TypeCore.mk_var table  # Simplified

                    (new_fn_tid, table2) = TypeCore.mk_fun table1 params_tid result_tid
                    new_subst = Dict.insert subst fn_actual new_fn_tid
                    Ok (new_subst, table2)

                _ ->
                    Err (NotCallable fn_tid)

unify_types : TypeId, TypeId, TypeTable, Substitution -> Result (Substitution, TypeTable) UnifyError
unify_types = \tid1, tid2, table, subst ->
    # Apply current substitution
    actual1 = apply_subst tid1 subst
    actual2 = apply_subst tid2 subst

    if actual1 == actual2 then
        Ok (subst, table)
    else
        when (TypeCore.get_type table actual1, TypeCore.get_type table actual2) is
            (Ok (TVar _), _) ->
                # First is variable - bind it
                if occurs_in actual1 actual2 table then
                    Err (OccursCheck actual1 actual2)
                else
                    Ok (Dict.insert subst actual1 actual2, table)

            (_, Ok (TVar _)) ->
                # Second is variable - bind it
                if occurs_in actual2 actual1 table then
                    Err (OccursCheck actual2 actual1)
                else
                    Ok (Dict.insert subst actual2 actual1, table)

            (Ok (TPrim p1), Ok (TPrim p2)) ->
                if p1 == p2 then
                    Ok (subst, table)
                else
                    Err (TypeMismatch actual1 actual2)

            (Ok (TLit l1), Ok (TLit l2)) ->
                if literals_equal l1 l2 then
                    Ok (subst, table)
                else
                    Err (TypeMismatch actual1 actual2)

            (Ok (TArray e1), Ok (TArray e2)) ->
                unify_types e1 e2 table subst

            (Ok (TFunction p1 r1), Ok (TFunction p2 r2)) ->
                when unify_types p1 p2 table subst is
                    Ok (subst1, table1) ->
                        unify_types r1 r2 table1 subst1
                    Err e -> Err e

            _ ->
                Err (TypeMismatch actual1 actual2)

apply_subst : TypeId, Substitution -> TypeId
apply_subst = \tid, subst ->
    when Dict.get subst tid is
        Ok new_tid -> apply_subst new_tid subst  # Follow chains
        Err _ -> tid

apply_constraint_subst : Constraint, Substitution -> Constraint
apply_constraint_subst = \constraint, subst ->
    when constraint is
        CEq t1 t2 -> CEq (apply_subst t1 subst) (apply_subst t2 subst)
        CSub t1 t2 -> CSub (apply_subst t1 subst) (apply_subst t2 subst)
        CField obj field result -> CField (apply_subst obj subst) field (apply_subst result subst)
        CCall fn args result -> CCall (apply_subst fn subst) (List.map args \a -> apply_subst a subst) (apply_subst result subst)

apply_substitution : TypeTable, Substitution -> TypeTable
apply_substitution = \table, subst ->
    # In a real implementation, we'd update the type table
    # For now, just return as-is since substitution is separate
    table

occurs_in : TypeId, TypeId, TypeTable -> Bool
occurs_in = \var_id, type_id, table ->
    if var_id == type_id then
        Bool.true
    else
        when TypeCore.get_type table type_id is
            Ok stype ->
                when stype is
                    TVar _ -> Bool.false
                    TArray elem -> occurs_in var_id elem table
                    TFunction params ret ->
                        occurs_in var_id params table || occurs_in var_id ret table
                    TObject fields ->
                        List.any fields \f -> occurs_in var_id f.tid table
                    TUnion tids ->
                        List.any tids \tid -> occurs_in var_id tid table
                    _ -> Bool.false
            Err _ -> Bool.false

literals_equal : TypeCore.Literal, TypeCore.Literal -> Bool
literals_equal = \l1, l2 ->
    when (l1, l2) is
        (LStr s1, LStr s2) -> s1 == s2
        (LNum n1, LNum n2) -> Num.is_approx_eq n1 n2 {}
        (LBool b1, LBool b2) -> b1 == b2
        (LNull, LNull) -> Bool.true
        (LUndefined, LUndefined) -> Bool.true
        _ -> Bool.false
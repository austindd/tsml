module [
    Substitution,
    UnificationError,
    unify,
    unify_many,
    solve_constraints,
    apply_substitution,
    apply_substitution_to_type,
    compose_substitutions,
]

import Type exposing [Type, TypeVar]
import TypeAlgebra
import TypeConstraint exposing [Constraint]
import Dict
import Set
import Result exposing [Result]

Substitution : Dict.Dict TypeVar Type

UnificationError : [
    InfiniteType TypeVar Type,
    TypeMismatch Type Type,
    FieldMissing Type Str,
    NotCallable Type,
    ArityMismatch U64 U64,
]

unify : Type, Type -> Result Substitution UnificationError
unify = \t1, t2 ->
    unify_helper (Type.normalize_type t1) (Type.normalize_type t2) (Dict.empty {})

unify_helper : Type, Type, Substitution -> Result Substitution UnificationError
unify_helper = \t1, t2, subst ->
    s1 = apply_substitution_to_type t1 subst
    s2 = apply_substitution_to_type t2 subst

    when (s1, s2) is
        (ty1, ty2) if types_structurally_equal ty1 ty2 -> Ok subst

        (Var v1, Var v2) if v1 == v2 -> Ok subst
        (Var v, t) | (t, Var v) ->
            if occurs_check v t then
                Err (InfiniteType v t)
            else
                Ok (Dict.insert subst v t)

        (Top, Top) | (Bottom, Bottom) -> Ok subst

        (Literal l1, Literal l2) ->
            if literals_equal l1 l2 then
                Ok subst
            else
                Err (TypeMismatch s1 s2)

        (Primitive p1, Primitive p2) ->
            if p1 == p2 then
                Ok subst
            else
                Err (TypeMismatch s1 s2)

        (Array e1, Array e2) ->
            unify_helper e1 e2 subst

        (Tuple es1, Tuple es2) ->
            if List.len es1 == List.len es2 then
                unify_lists es1 es2 subst
            else
                Err (ArityMismatch (List.len es1) (List.len es2))

        (Function fn1, Function fn2) ->
            if List.len fn1.params == List.len fn2.params then
                when unify_lists fn1.params fn2.params subst is
                    Ok param_subst ->
                        unify_helper fn1.ret fn2.ret param_subst
                    Err e -> Err e
            else
                Err (ArityMismatch (List.len fn1.params) (List.len fn2.params))

        (Record fields1, Record fields2) ->
            unify_records fields1 fields2 subst

        (Union types1, Union types2) ->
            if TypeAlgebra.equivalent s1 s2 then
                Ok subst
            else
                Err (TypeMismatch s1 s2)

        (Intersection types1, Intersection types2) ->
            if TypeAlgebra.equivalent s1 s2 then
                Ok subst
            else
                Err (TypeMismatch s1 s2)

        _ ->
            if TypeAlgebra.subtype s1 s2 || TypeAlgebra.subtype s2 s1 then
                Ok subst
            else
                Err (TypeMismatch s1 s2)

unify_lists : List Type, List Type, Substitution -> Result Substitution UnificationError
unify_lists = \list1, list2, subst ->
    when (list1, list2) is
        ([], []) -> Ok subst
        ([h1, .. as t1], [h2, .. as t2]) ->
            when unify_helper h1 h2 subst is
                Ok new_subst -> unify_lists t1 t2 new_subst
                Err e -> Err e
        _ -> Err (TypeMismatch (Type.mk_tuple list1) (Type.mk_tuple list2))

unify_records : List { key : Str, value : Type, optional : Bool }, List { key : Str, value : Type, optional : Bool }, Substitution -> Result Substitution UnificationError
unify_records = \fields1, fields2, subst ->
    List.walk fields2 (Ok subst) \result, field2 ->
        when result is
            Err e -> Err e
            Ok current_subst ->
                when List.find_first fields1 (\f -> f.key == field2.key) is
                    Ok field1 ->
                        if !field1.optional && field2.optional then
                            Err (TypeMismatch (Type.mk_record fields1) (Type.mk_record fields2))
                        else
                            unify_helper field1.value field2.value current_subst
                    Err _ ->
                        if field2.optional then
                            Ok current_subst
                        else
                            Err (FieldMissing (Type.mk_record fields1) field2.key)

unify_many : List (Type, Type) -> Result Substitution UnificationError
unify_many = \pairs ->
    List.walk pairs (Ok (Dict.empty {})) \result, (t1, t2) ->
        when result is
            Ok subst ->
                s1 = apply_substitution_to_type t1 subst
                s2 = apply_substitution_to_type t2 subst
                when unify s1 s2 is
                    Ok new_subst -> Ok (compose_substitutions subst new_subst)
                    Err e -> Err e
            Err e -> Err e

solve_constraints : List Constraint -> Result Substitution UnificationError
solve_constraints = \constraints ->
    solve_constraints_helper constraints (Dict.empty {})

solve_constraints_helper : List Constraint, Substitution -> Result Substitution UnificationError
solve_constraints_helper = \constraints, subst ->
    when constraints is
        [] -> Ok subst
        [first, .. as rest] ->
            when solve_single_constraint first subst is
                Ok new_subst ->
                    updated_rest = List.map rest \c -> apply_substitution_to_constraint c new_subst
                    solve_constraints_helper updated_rest new_subst
                Err e -> Err e

solve_single_constraint : Constraint, Substitution -> Result Substitution UnificationError
solve_single_constraint = \constraint, subst ->
    when constraint is
        Equal t1 t2 ->
            s1 = apply_substitution_to_type t1 subst
            s2 = apply_substitution_to_type t2 subst
            when unify s1 s2 is
                Ok new_subst -> Ok (compose_substitutions subst new_subst)
                Err e -> Err e

        Subtype sub super ->
            s_sub = apply_substitution_to_type sub subst
            s_super = apply_substitution_to_type super subst
            if TypeAlgebra.subtype s_sub s_super then
                Ok subst
            else
                when (s_sub, s_super) is
                    (Var v, t) | (t, Var v) ->
                        if occurs_check v t then
                            Err (InfiniteType v t)
                        else
                            Ok (Dict.insert subst v t)
                    _ -> Err (TypeMismatch s_sub s_super)

        HasField obj_type field_name field_type ->
            s_obj = apply_substitution_to_type obj_type subst
            s_field = apply_substitution_to_type field_type subst
            when s_obj is
                Record fields ->
                    when List.find_first fields (\f -> f.key == field_name) is
                        Ok field ->
                            when unify field.value s_field is
                                Ok new_subst -> Ok (compose_substitutions subst new_subst)
                                Err e -> Err e
                        Err _ ->
                            Err (FieldMissing s_obj field_name)
                Var v ->
                    new_record = Type.mk_record [{ key: field_name, value: s_field, optional: Bool.false }]
                    Ok (Dict.insert subst v new_record)
                _ ->
                    Err (FieldMissing s_obj field_name)

        IsCallable fn_type arg_types result_type ->
            s_fn = apply_substitution_to_type fn_type subst
            s_args = List.map arg_types \a -> apply_substitution_to_type a subst
            s_result = apply_substitution_to_type result_type subst
            when s_fn is
                Function { params, ret } ->
                    if List.len params == List.len s_args then
                        when unify_lists params s_args subst is
                            Ok param_subst ->
                                s_ret = apply_substitution_to_type ret param_subst
                                when unify s_ret s_result is
                                    Ok result_subst ->
                                        Ok (compose_substitutions param_subst result_subst)
                                    Err e -> Err e
                            Err e -> Err e
                    else
                        Err (ArityMismatch (List.len params) (List.len s_args))
                Var v ->
                    new_fn_type = Type.mk_function s_args s_result
                    Ok (Dict.insert subst v new_fn_type)
                _ ->
                    Err (NotCallable s_fn)

apply_substitution : Substitution -> Substitution
apply_substitution = \subst ->
    Dict.map subst \_, t -> apply_substitution_to_type t subst

apply_substitution_to_type : Type, Substitution -> Type
apply_substitution_to_type = \type, subst ->
    when type is
        Var v ->
            when Dict.get subst v is
                Ok t -> apply_substitution_to_type t subst
                Err _ -> type
        Array elem ->
            Type.mk_array (apply_substitution_to_type elem subst)
        Tuple elems ->
            Type.mk_tuple (List.map elems \e -> apply_substitution_to_type e subst)
        Function { params, ret } ->
            Type.mk_function
                (List.map params \p -> apply_substitution_to_type p subst)
                (apply_substitution_to_type ret subst)
        Record fields ->
            Type.mk_record (List.map fields \f ->
                { f & value: apply_substitution_to_type f.value subst })
        Union types ->
            Type.mk_union (List.map types \t -> apply_substitution_to_type t subst)
        Intersection types ->
            Type.mk_intersection (List.map types \t -> apply_substitution_to_type t subst)
        Negation inner ->
            Type.mk_negation (apply_substitution_to_type inner subst)
        _ -> type

apply_substitution_to_constraint : Constraint, Substitution -> Constraint
apply_substitution_to_constraint = \constraint, subst ->
    when constraint is
        Equal t1 t2 ->
            Equal (apply_substitution_to_type t1 subst) (apply_substitution_to_type t2 subst)
        Subtype t1 t2 ->
            Subtype (apply_substitution_to_type t1 subst) (apply_substitution_to_type t2 subst)
        HasField obj field result ->
            HasField (apply_substitution_to_type obj subst) field (apply_substitution_to_type result subst)
        IsCallable fn args result ->
            IsCallable
                (apply_substitution_to_type fn subst)
                (List.map args \a -> apply_substitution_to_type a subst)
                (apply_substitution_to_type result subst)

compose_substitutions : Substitution, Substitution -> Substitution
compose_substitutions = \s1, s2 ->
    applied_s1 = Dict.map s1 \_, t -> apply_substitution_to_type t s2
    Dict.walk s2 applied_s1 \acc, k, v ->
        if Dict.contains acc k then
            acc
        else
            Dict.insert acc k v

occurs_check : TypeVar, Type -> Bool
occurs_check = \var, type ->
    when type is
        Var v -> v == var
        Array elem -> occurs_check var elem
        Tuple elems -> List.any elems \e -> occurs_check var e
        Function { params, ret } ->
            List.any params \p -> occurs_check var p || occurs_check var ret
        Record fields ->
            List.any fields \f -> occurs_check var f.value
        Union types | Intersection types ->
            List.any types \t -> occurs_check var t
        Negation inner -> occurs_check var inner
        _ -> Bool.false

types_structurally_equal : Type, Type -> Bool
types_structurally_equal = \t1, t2 ->
    TypeAlgebra.equivalent t1 t2

literals_equal = \l1, l2 ->
    when (l1, l2) is
        (StrLit s1, StrLit s2) -> s1 == s2
        (NumLit n1, NumLit n2) -> Num.is_approx_eq n1 n2 {}
        (BoolLit b1, BoolLit b2) -> b1 == b2
        (NullLit, NullLit) -> Bool.true
        (UndefinedLit, UndefinedLit) -> Bool.true
        _ -> Bool.false
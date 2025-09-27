module [
    Constraint,
    ConstraintKind,
    Solution,
    Substitution,
    SolverState,
    SolverError,
    # Main solver API
    empty_solver_state,
    add_constraint,
    solve_constraints,
    apply_substitution,
    # Type relationship checks
    is_subtype_of,
    unify_types,
    # Utilities
    constraint_to_str,
    solution_to_str,
]

import ComprehensiveTypeIndexed exposing [
    TypeStore,
    TypeId,
    RowId,
    TypeDef,
    RowDef,
    LiteralValue,
    ClassId,
    InterfaceId,
    ClassDef,
    InterfaceDef,
    TypeParamId,
    TypeParamDef,
    EnumId,
    EnumDef,
    EnumMemberValue,
    get_type,
    get_row,
    get_class,
    get_interface,
    get_type_param,
    get_enum,
]

# Constraint kinds
ConstraintKind : [
    # Basic equality constraint
    Equality TypeId TypeId,

    # Subtyping constraint (left <: right means left is subtype of right)
    Subtype TypeId TypeId,

    # Row constraints for objects
    RowEquality RowId RowId,
    RowSubtype RowId RowId,  # For width subtyping

    # Instance constraint (type is instance of generic)
    Instance TypeId TypeId (List TypeId),

    # Type parameter constraint
    TypeParamConstraint TypeParamId TypeId,
]

# A constraint with optional source location for error reporting
Constraint : {
    kind: ConstraintKind,
    source: Result Str [NoSource],  # Optional description for debugging
}

# Substitution mapping type variables to concrete types
Substitution : {
    type_var_map: List { var: U32, replacement: TypeId },
    row_var_map: List { var: U32, replacement: RowId },
}

# Solution to constraint solving
Solution : Result Substitution SolverError

# Possible solver errors
SolverError : [
    UnificationError Str,
    SubtypeError { expected: TypeId, actual: TypeId, reason: Str },
    CyclicType TypeId,
    RowError Str,
    UnsolvableConstraint Constraint,
    ConflictingConstraints (List Constraint),
]

# Solver state during constraint solving
SolverState : {
    store: TypeStore,
    constraints: List Constraint,
    substitution: Substitution,
    work_list: List Constraint,  # Constraints being processed
    deferred: List Constraint,    # Constraints to retry later
}

# Create empty solver state
empty_solver_state : TypeStore -> SolverState
empty_solver_state = \store -> {
    store,
    constraints: [],
    substitution: { type_var_map: [], row_var_map: [] },
    work_list: [],
    deferred: [],
}

# Add a constraint to the solver
add_constraint : SolverState, ConstraintKind, Result Str [NoSource] -> SolverState
add_constraint = \state, kind, source ->
    constraint = { kind, source }
    { state &
        constraints: List.append state.constraints constraint,
        work_list: List.append state.work_list constraint,
    }

# Main constraint solving algorithm
solve_constraints : SolverState -> Solution
solve_constraints = \initial_state ->
    solve_loop initial_state 0

solve_loop : SolverState, U32 -> Solution
solve_loop = \state, iteration ->
    # Prevent infinite loops
    if iteration > 1000 then
        Err (UnificationError "Constraint solving did not converge after 1000 iterations")
    else
        when state.work_list is
            [] ->
                # No more work, check if we have deferred constraints
                if List.is_empty state.deferred then
                    Ok state.substitution
                else
                    # Try deferred constraints once more
                    new_state = { state & work_list: state.deferred, deferred: [] }
                    solve_loop new_state (iteration + 1)

            [constraint, .. as rest] ->
                # Process one constraint
                when process_constraint state.store constraint state.substitution is
                    Ok new_subst ->
                        # Successfully processed, continue with rest
                        new_state = { state &
                            work_list: rest,
                            substitution: new_subst,
                        }
                        solve_loop new_state (iteration + 1)

                    Err (Defer) ->
                        # Defer this constraint for later
                        new_state = { state &
                            work_list: rest,
                            deferred: List.append state.deferred constraint,
                        }
                        solve_loop new_state (iteration + 1)

                    Err (SolverError error) ->
                        # Unrecoverable error
                        Err error

# Process a single constraint
process_constraint : TypeStore, Constraint, Substitution -> Result Substitution [Defer, SolverError SolverError]
process_constraint = \store, constraint, subst ->
    when constraint.kind is
        Equality type1 type2 ->
            unify_types_internal store type1 type2 subst

        Subtype sub_type super_type ->
            check_subtype store sub_type super_type subst

        RowEquality row1 row2 ->
            unify_rows store row1 row2 subst

        RowSubtype sub_row super_row ->
            check_row_subtype store sub_row super_row subst

        Instance type_id base_type type_args ->
            instantiate_generic store type_id base_type type_args subst

        TypeParamConstraint param_id type_id ->
            constrain_type_param store param_id type_id subst

# Unify two types
unify_types : TypeStore, TypeId, TypeId -> Result Substitution SolverError
unify_types = \store, type1, type2 ->
    when unify_types_internal store type1 type2 { type_var_map: [], row_var_map: [] } is
        Ok subst -> Ok subst
        Err (SolverError error) -> Err error
        Err Defer -> Err (UnificationError "Cannot unify types - deferred constraint")

unify_types_internal : TypeStore, TypeId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_types_internal = \store, type1_id, type2_id, subst ->
    # Apply existing substitutions first
    actual_type1 = apply_type_substitution store type1_id subst
    actual_type2 = apply_type_substitution store type2_id subst

    if actual_type1 == actual_type2 then
        Ok subst  # Already equal
    else
        when (get_type store actual_type1, get_type store actual_type2) is
            (Ok type1_def, Ok type2_def) ->
                unify_type_defs store type1_def type2_def actual_type1 actual_type2 subst

            _ -> Err (SolverError (UnificationError "Invalid type IDs"))

# Unify type definitions
unify_type_defs : TypeStore, TypeDef, TypeDef, TypeId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_type_defs = \store, type1_def, type2_def, type1_id, type2_id, subst ->
    when (type1_def, type2_def) is
        # Primitives and special types must match exactly
        (TNumber, TNumber) -> Ok subst
        (TString, TString) -> Ok subst
        (TBoolean, TBoolean) -> Ok subst
        (TNull, TNull) -> Ok subst
        (TUndefined, TUndefined) -> Ok subst
        (TBigInt, TBigInt) -> Ok subst
        (TSymbol, TSymbol) -> Ok subst
        (TAny, TAny) -> Ok subst
        (TNever, TNever) -> Ok subst
        (TUnknown, TUnknown) -> Ok subst
        (TVoid, TVoid) -> Ok subst

        # Any type unifies with anything
        (TAny, _) -> Ok subst
        (_, TAny) -> Ok subst

        # Never type is bottom type
        (TNever, _) -> Ok subst
        (_, TNever) -> Ok (add_type_substitution subst type2_id type1_id)

        # Unknown requires special handling
        (TUnknown, _) -> Ok (add_type_substitution subst type1_id type2_id)
        (_, TUnknown) -> Ok (add_type_substitution subst type2_id type1_id)

        # Literals
        (TLiteral lit1, TLiteral lit2) ->
            if literals_equal lit1 lit2 then
                Ok subst
            else
                Err (SolverError (UnificationError "Literal values don't match"))

        # Arrays
        (TArray elem1, TArray elem2) ->
            unify_types_internal store elem1 elem2 subst

        # Tuples
        (TTuple types1, TTuple types2) ->
            if List.len types1 == List.len types2 then
                unify_type_lists store types1 types2 subst
            else
                Err (SolverError (UnificationError "Tuple lengths don't match"))

        # Objects
        (TObject row1, TObject row2) ->
            unify_rows store row1 row2 subst

        # Functions
        (TFunction func1, TFunction func2) ->
            unify_functions store func1 func2 subst

        # Unions
        (TUnion types1, TUnion types2) ->
            unify_unions store types1 types2 subst

        # Intersections
        (TIntersection types1, TIntersection types2) ->
            unify_intersections store types1 types2 subst

        # Type variables
        (TypeVariable var, _) ->
            Ok (add_type_substitution subst var type2_id)
        (_, TypeVariable var) ->
            Ok (add_type_substitution subst var type1_id)

        # Classes
        (TClass class1, TClass class2) ->
            if class1 == class2 then
                Ok subst
            else
                Err (SolverError (UnificationError "Classes don't match"))

        # Interfaces
        (TInterface iface1, TInterface iface2) ->
            if iface1 == iface2 then
                Ok subst
            else
                Err (SolverError (UnificationError "Interfaces don't match"))

        # Enums
        (TEnum enum1, TEnum enum2) ->
            if enum1 == enum2 then
                Ok subst
            else
                Err (SolverError (UnificationError "Enums don't match"))

        # Enum members
        (TEnumMember mem1, TEnumMember mem2) ->
            if mem1.enum_id == mem2.enum_id && mem1.member_name == mem2.member_name then
                Ok subst
            else
                Err (SolverError (UnificationError "Enum members don't match"))

        # Generics
        (TGeneric gen1, TGeneric gen2) ->
            if gen1.base == gen2.base then
                unify_type_lists store gen1.args gen2.args subst
            else
                Err (SolverError (UnificationError "Generic base types don't match"))

        # Conditional types
        (TConditional cond1, TConditional cond2) ->
            unify_conditionals store cond1 cond2 subst

        # Mapped types
        (TMappedType mapped1, TMappedType mapped2) ->
            unify_mapped_types store mapped1 mapped2 subst

        # Template literals
        (TTemplateLiteral tmpl1, TTemplateLiteral tmpl2) ->
            unify_template_literals store tmpl1 tmpl2 subst

        # KeyOf types
        (TKeyOf obj1, TKeyOf obj2) ->
            unify_types_internal store obj1 obj2 subst

        # Indexed access types
        (TIndexedAccess acc1, TIndexedAccess acc2) ->
            unify_indexed_access store acc1 acc2 subst

        # Default: types don't unify
        _ -> Err (SolverError (UnificationError "Types are not compatible"))

# Helper to unify lists of types
unify_type_lists : TypeStore, List TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_type_lists = \store, types1, types2, initial_subst ->
    when (types1, types2) is
        ([], []) -> Ok initial_subst
        ([t1, .. as rest1], [t2, .. as rest2]) ->
            when unify_types_internal store t1 t2 initial_subst is
                Ok new_subst ->
                    unify_type_lists store rest1 rest2 new_subst
                Err e -> Err e
        _ -> Err (SolverError (UnificationError "Type list lengths don't match"))

# Unify function types
unify_functions : TypeStore, { params: List { name: Str, param_type: TypeId, optional: Bool }, ret: TypeId, type_params: List TypeParamId, is_async: Bool, is_generator: Bool }, { params: List { name: Str, param_type: TypeId, optional: Bool }, ret: TypeId, type_params: List TypeParamId, is_async: Bool, is_generator: Bool }, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_functions = \store, func1, func2, subst ->
    if func1.is_async != func2.is_async then
        Err (SolverError (UnificationError "Async mismatch in functions"))
    else if func1.is_generator != func2.is_generator then
        Err (SolverError (UnificationError "Generator mismatch in functions"))
    else if List.len func1.params != List.len func2.params then
        Err (SolverError (UnificationError "Function parameter count mismatch"))
    else
        # Unify parameters (contravariant in theory, but we unify for now)
        param_types1 = List.map func1.params \p -> p.param_type
        param_types2 = List.map func2.params \p -> p.param_type
        when unify_type_lists store param_types1 param_types2 subst is
            Ok subst_with_params ->
                # Unify return types (covariant)
                unify_types_internal store func1.ret func2.ret subst_with_params
            Err e -> Err e

# Unify union types
unify_unions : TypeStore, List TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_unions = \store, types1, types2, subst ->
    # For now, unions must have exact same members
    # TODO: Implement proper union unification with normalization
    if List.len types1 == List.len types2 then
        unify_type_lists store types1 types2 subst
    else
        Err (SolverError (UnificationError "Union types have different number of members"))

# Unify intersection types
unify_intersections : TypeStore, List TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_intersections = \store, types1, types2, subst ->
    # For now, intersections must have exact same members
    # TODO: Implement proper intersection unification with normalization
    if List.len types1 == List.len types2 then
        unify_type_lists store types1 types2 subst
    else
        Err (SolverError (UnificationError "Intersection types have different number of members"))

# Unify conditional types
unify_conditionals : TypeStore, { check: TypeId, extends: TypeId, true_type: TypeId, false_type: TypeId }, { check: TypeId, extends: TypeId, true_type: TypeId, false_type: TypeId }, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_conditionals = \store, cond1, cond2, subst ->
    # Unify all components of the conditional
    unify_types_internal store cond1.check cond2.check subst
        |> Result.try \s1 -> unify_types_internal store cond1.extends cond2.extends s1
        |> Result.try \s2 -> unify_types_internal store cond1.true_type cond2.true_type s2
        |> Result.try \s3 -> unify_types_internal store cond1.false_type cond2.false_type s3

# Unify mapped types
unify_mapped_types : TypeStore, { key_type: TypeId, value_type: TypeId, readonly: Bool, optional: Bool }, { key_type: TypeId, value_type: TypeId, readonly: Bool, optional: Bool }, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_mapped_types = \store, mapped1, mapped2, subst ->
    if mapped1.readonly != mapped2.readonly || mapped1.optional != mapped2.optional then
        Err (SolverError (UnificationError "Mapped type modifiers don't match"))
    else
        unify_types_internal store mapped1.key_type mapped2.key_type subst
            |> Result.try \s -> unify_types_internal store mapped1.value_type mapped2.value_type s

# Unify template literals
unify_template_literals : TypeStore, { parts: List Str, types: List TypeId }, { parts: List Str, types: List TypeId }, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_template_literals = \store, tmpl1, tmpl2, subst ->
    if tmpl1.parts != tmpl2.parts then
        Err (SolverError (UnificationError "Template literal parts don't match"))
    else
        unify_type_lists store tmpl1.types tmpl2.types subst

# Unify indexed access types
unify_indexed_access : TypeStore, { object: TypeId, index: TypeId }, { object: TypeId, index: TypeId }, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_indexed_access = \store, acc1, acc2, subst ->
    unify_types_internal store acc1.object acc2.object subst
        |> Result.try \s -> unify_types_internal store acc1.index acc2.index s

# Unify rows (object types)
unify_rows : TypeStore, RowId, RowId, Substitution -> Result Substitution [Defer, SolverError SolverError]
unify_rows = \store, row1_id, row2_id, subst ->
    # Apply existing row substitutions
    actual_row1 = apply_row_substitution store row1_id subst
    actual_row2 = apply_row_substitution store row2_id subst

    if actual_row1 == actual_row2 then
        Ok subst
    else
        when (get_row store actual_row1, get_row store actual_row2) is
            (Ok REmpty, Ok REmpty) -> Ok subst

            (Ok (RVar var), Ok row_def) ->
                Ok (add_row_substitution subst var actual_row2)
            (Ok row_def, Ok (RVar var)) ->
                Ok (add_row_substitution subst var actual_row1)

            (Ok (RExtend ext1), Ok (RExtend ext2)) ->
                if ext1.label == ext2.label then
                    # Same field, unify types and continue with rest
                    when unify_types_internal store ext1.field_type ext2.field_type subst is
                        Ok subst_with_field ->
                            if ext1.optional == ext2.optional && ext1.readonly == ext2.readonly then
                                unify_rows store ext1.rest ext2.rest subst_with_field
                            else
                                Err (SolverError (RowError "Field modifiers don't match"))
                        Err e -> Err e
                else
                    # Different fields - need to reorder or fail
                    # For now, we'll fail. TODO: Implement row reordering
                    Err (SolverError (RowError "Row fields don't match"))

            _ -> Err (SolverError (RowError "Incompatible row types"))

# Check subtype relationship
check_subtype : TypeStore, TypeId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_subtype = \store, sub_id, super_id, subst ->
    # Apply substitutions
    actual_sub = apply_type_substitution store sub_id subst
    actual_super = apply_type_substitution store super_id subst

    if actual_sub == actual_super then
        Ok subst  # Same type is reflexive
    else
        when (get_type store actual_sub, get_type store actual_super) is
            (Ok sub_def, Ok super_def) ->
                check_subtype_defs store sub_def super_def actual_sub actual_super subst
            _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Invalid type IDs" }))

# Check subtyping for type definitions
check_subtype_defs : TypeStore, TypeDef, TypeDef, TypeId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_subtype_defs = \store, sub_def, super_def, sub_id, super_id, subst ->
    when (sub_def, super_def) is
        # Any is top type - everything is subtype of Any
        (_, TAny) -> Ok subst

        # Never is bottom type - subtype of everything
        (TNever, _) -> Ok subst

        # Unknown - everything is subtype of unknown
        (_, TUnknown) -> Ok subst

        # Void subtypes
        (TVoid, TVoid) -> Ok subst
        (TVoid, TUndefined) -> Ok subst  # void can be used as undefined

        # Literal to primitive subtyping
        (TLiteral (NumLit _), TNumber) -> Ok subst
        (TLiteral (StrLit _), TString) -> Ok subst
        (TLiteral (BoolLit _), TBoolean) -> Ok subst
        (TLiteral (BigIntLit _), TBigInt) -> Ok subst

        # Same literals
        (TLiteral lit1, TLiteral lit2) ->
            if literals_equal lit1 lit2 then
                Ok subst
            else
                Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Literals don't match" }))

        # Arrays are covariant
        (TArray sub_elem, TArray super_elem) ->
            check_subtype store sub_elem super_elem subst

        # Tuples
        (TTuple sub_types, TTuple super_types) ->
            if List.len sub_types == List.len super_types then
                check_subtype_lists store sub_types super_types subst
            else
                Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Tuple lengths don't match" }))

        # Objects - structural subtyping
        (TObject sub_row, TObject super_row) ->
            check_row_subtype store sub_row super_row subst

        # Functions - contravariant in params, covariant in return
        (TFunction sub_func, TFunction super_func) ->
            check_function_subtype store sub_func super_func subst

        # Union subtyping - all members of sub must be subtypes of super
        (TUnion sub_types, _) ->
            check_all_subtypes store sub_types super_id subst

        # Subtype of union - must be subtype of at least one member
        (_, TUnion super_types) ->
            check_any_supertype store sub_id super_types subst

        # Intersection subtyping
        (TIntersection sub_types, _) ->
            # Intersection is subtype if any member is subtype
            check_any_subtype store sub_types super_id subst

        (_, TIntersection super_types) ->
            # Must be subtype of all members
            check_all_supertypes store sub_id super_types subst

        # Type variables defer
        (TypeVariable _, _) -> Err Defer
        (_, TypeVariable _) -> Err Defer

        # Classes and interfaces
        (TClass sub_class, TClass super_class) ->
            check_class_subtype store sub_class super_class subst

        (TClass class_id, TInterface iface_id) ->
            check_class_implements store class_id iface_id subst

        (TInterface sub_iface, TInterface super_iface) ->
            check_interface_subtype store sub_iface super_iface subst

        # Enum subtyping
        (TEnum enum1, TEnum enum2) ->
            # Enums are nominally typed - must be same enum
            if enum1 == enum2 then
                Ok subst
            else
                Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Different enums are not subtypes" }))

        # Enum member is subtype of its enum
        (TEnumMember mem, TEnum enum_id) ->
            if mem.enum_id == enum_id then
                Ok subst
            else
                Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum member not from this enum" }))

        # Enum members with same value
        (TEnumMember mem1, TEnumMember mem2) ->
            if mem1.enum_id == mem2.enum_id && mem1.member_name == mem2.member_name then
                Ok subst
            else
                Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Different enum members" }))

        # Enum member to literal (for const enums)
        (TEnumMember mem, _) ->
            # Check if this is a const enum and get its value
            when get_enum store mem.enum_id is
                Ok enum_def ->
                    if enum_def.is_const then
                        # Find the member's value
                        when List.find_first enum_def.members \m -> m.name == mem.member_name is
                            Ok member ->
                                # Check if the member's value type matches the target
                                when (member.value, super_def) is
                                    (EnumNumValue n, TNumber) -> Ok subst
                                    (EnumStrValue _, TString) -> Ok subst
                                    (EnumNumValue n, TLiteral (NumLit expected)) ->
                                        if Num.to_str n == Num.to_str expected then
                                            Ok subst
                                        else
                                            Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum value doesn't match literal" }))
                                    (EnumStrValue s, TLiteral (StrLit expected)) ->
                                        if s == expected then
                                            Ok subst
                                        else
                                            Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum value doesn't match literal" }))
                                    _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum member type mismatch" }))
                            Err _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum member not found" }))
                    else
                        Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Non-const enum member cannot be subtype of literal" }))
                Err _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum not found" }))

        # Literal to enum member (reverse const enum check)
        (TLiteral lit, TEnumMember mem) ->
            when get_enum store mem.enum_id is
                Ok enum_def ->
                    if enum_def.is_const then
                        when List.find_first enum_def.members \m -> m.name == mem.member_name is
                            Ok member ->
                                when (lit, member.value) is
                                    (NumLit n, EnumNumValue expected) ->
                                        if Num.to_str n == Num.to_str expected then
                                            Ok subst
                                        else
                                            Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Literal doesn't match enum value" }))
                                    (StrLit s, EnumStrValue expected) ->
                                        if s == expected then
                                            Ok subst
                                        else
                                            Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Literal doesn't match enum value" }))
                                    _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Type mismatch with enum value" }))
                            Err _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum member not found" }))
                    else
                        Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Cannot assign to non-const enum member" }))
                Err _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum not found" }))

        # Enum to union containing its members
        (TEnum enum_id, TUnion union_types) ->
            # Check if union contains all possible enum members
            when get_enum store enum_id is
                Ok enum_def ->
                    # For simplicity, just check if enum is in the union
                    # A full implementation would expand enum to its members
                    if List.any union_types \t ->
                        when get_type store t is
                            Ok (TEnum e) -> e == enum_id
                            _ -> Bool.false
                    then
                        Ok subst
                    else
                        Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum not in union" }))
                Err _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Enum not found" }))

        # Conditional types
        (TConditional cond, _) ->
            # Evaluate conditional and check result
            evaluate_conditional_subtype store cond super_id subst

        (_, TConditional cond) ->
            # Check if sub is subtype of both branches
            check_subtype store sub_id cond.true_type subst
                |> Result.try \s -> check_subtype store sub_id cond.false_type s

        # Mapped types
        (TMappedType mapped1, TMappedType mapped2) ->
            check_mapped_subtype store mapped1 mapped2 subst

        # Template literals
        (TTemplateLiteral tmpl, TString) ->
            # Template literal is subtype of string
            Ok subst

        (TTemplateLiteral tmpl1, TTemplateLiteral tmpl2) ->
            check_template_literal_subtype store tmpl1 tmpl2 subst

        # KeyOf types
        (TKeyOf obj_id, _) ->
            # Extract keys and check if they're subtypes
            check_keyof_subtype store obj_id super_id subst

        # Indexed access
        (TIndexedAccess acc, _) ->
            # Resolve indexed access and check
            resolve_indexed_access_subtype store acc super_id subst

        # Generics
        (TGeneric gen1, TGeneric gen2) ->
            if gen1.base == gen2.base then
                # Check type arguments (depends on variance)
                check_generic_args_subtype store gen1.args gen2.args subst
            else
                Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Generic base types don't match" }))

        # Same primitive types
        (TNumber, TNumber) -> Ok subst
        (TString, TString) -> Ok subst
        (TBoolean, TBoolean) -> Ok subst
        (TNull, TNull) -> Ok subst
        (TUndefined, TUndefined) -> Ok subst
        (TBigInt, TBigInt) -> Ok subst
        (TSymbol, TSymbol) -> Ok subst

        # Otherwise not subtypes
        _ -> Err (SolverError (SubtypeError { expected: super_id, actual: sub_id, reason: "Incompatible types" }))

# Check if all types in list are subtypes of target
check_all_subtypes : TypeStore, List TypeId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_all_subtypes = \store, sub_types, super_id, initial_subst ->
    List.walk_until sub_types (Ok initial_subst) \result, sub_id ->
        when result is
            Ok subst ->
                when check_subtype store sub_id super_id subst is
                    Ok new_subst -> Continue (Ok new_subst)
                    Err e -> Break (Err e)
            Err e -> Break (Err e)

# Check if type is subtype of at least one in list
check_any_supertype : TypeStore, TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_any_supertype = \store, sub_id, super_types, subst ->
    # Try each supertype until one succeeds
    List.walk super_types (Err (SolverError (SubtypeError { expected: 0, actual: sub_id, reason: "Not subtype of any union member" }))) \best_result, super_id ->
        when best_result is
            Ok _ -> best_result  # Already found a match
            Err _ ->
                when check_subtype store sub_id super_id subst is
                    Ok new_subst -> Ok new_subst
                    Err _ -> best_result  # Keep looking

# Check if any type in list is subtype of target
check_any_subtype : TypeStore, List TypeId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_any_subtype = \store, sub_types, super_id, subst ->
    # At least one must be subtype
    List.walk sub_types (Err (SolverError (SubtypeError { expected: super_id, actual: 0, reason: "No intersection member is subtype" }))) \best_result, sub_id ->
        when best_result is
            Ok _ -> best_result  # Already found a match
            Err _ ->
                when check_subtype store sub_id super_id subst is
                    Ok new_subst -> Ok new_subst
                    Err _ -> best_result  # Keep looking

# Check if type is subtype of all in list
check_all_supertypes : TypeStore, TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_all_supertypes = \store, sub_id, super_types, initial_subst ->
    List.walk_until super_types (Ok initial_subst) \result, super_id ->
        when result is
            Ok subst ->
                when check_subtype store sub_id super_id subst is
                    Ok new_subst -> Continue (Ok new_subst)
                    Err e -> Break (Err e)
            Err e -> Break (Err e)

# Check subtype relationship for lists
check_subtype_lists : TypeStore, List TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_subtype_lists = \store, sub_types, super_types, initial_subst ->
    when (sub_types, super_types) is
        ([], []) -> Ok initial_subst
        ([sub, .. as sub_rest], [super, .. as super_rest]) ->
            when check_subtype store sub super initial_subst is
                Ok new_subst ->
                    check_subtype_lists store sub_rest super_rest new_subst
                Err e -> Err e
        _ -> Err (SolverError (UnificationError "List lengths don't match"))

# Check function subtyping (contravariant params, covariant return)
check_function_subtype : TypeStore, { params: List { name: Str, param_type: TypeId, optional: Bool }, ret: TypeId, type_params: List TypeParamId, is_async: Bool, is_generator: Bool }, { params: List { name: Str, param_type: TypeId, optional: Bool }, ret: TypeId, type_params: List TypeParamId, is_async: Bool, is_generator: Bool }, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_function_subtype = \store, sub_func, super_func, subst ->
    if sub_func.is_async != super_func.is_async then
        Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Async mismatch" }))
    else if sub_func.is_generator != super_func.is_generator then
        Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Generator mismatch" }))
    else if List.len sub_func.params > List.len super_func.params then
        # Subtype can have fewer params (rest are optional)
        Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Too many required parameters" }))
    else
        # Check parameters (contravariant)
        check_function_params store sub_func.params super_func.params subst
            |> Result.try \subst_with_params ->
                # Check return type (covariant)
                check_subtype store sub_func.ret super_func.ret subst_with_params

# Check function parameters (contravariant)
check_function_params : TypeStore, List { name: Str, param_type: TypeId, optional: Bool }, List { name: Str, param_type: TypeId, optional: Bool }, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_function_params = \store, sub_params, super_params, initial_subst ->
    when (sub_params, super_params) is
        ([], []) -> Ok initial_subst
        ([], [super_param, .. as rest]) ->
            # Missing param ok if optional
            if super_param.optional then
                check_function_params store [] rest initial_subst
            else
                Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Missing required parameter" }))
        ([sub_param, .. as sub_rest], [super_param, .. as super_rest]) ->
            # Parameters are contravariant
            when check_subtype store super_param.param_type sub_param.param_type initial_subst is
                Ok new_subst ->
                    check_function_params store sub_rest super_rest new_subst
                Err e -> Err e
        _ -> Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Parameter mismatch" }))

# Check row (object) subtyping - width and depth subtyping
check_row_subtype : TypeStore, RowId, RowId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_row_subtype = \store, sub_row_id, super_row_id, subst ->
    # Apply row substitutions
    actual_sub = apply_row_substitution store sub_row_id subst
    actual_super = apply_row_substitution store super_row_id subst

    when get_row store actual_super is
        Ok REmpty -> Ok subst  # Any row is subtype of empty row
        Ok (RVar var) -> Ok (add_row_substitution subst var actual_sub)
        Ok (RExtend super_ext) ->
            # Super has a required field - sub must have it too
            find_and_check_field store actual_sub super_ext.label super_ext subst
        Ok (RIndex super_index) ->
            # Index signature - check all fields match
            check_index_subtype store actual_sub super_index subst
        _ -> Err (SolverError (RowError "Invalid super row"))

# Find field in sub row and check it matches super's requirements
find_and_check_field : TypeStore, RowId, Str, { label: Str, field_type: TypeId, optional: Bool, readonly: Bool, rest: RowId }, Substitution -> Result Substitution [Defer, SolverError SolverError]
find_and_check_field = \store, sub_row_id, field_name, super_field, subst ->
    when get_row store sub_row_id is
        Ok REmpty ->
            if super_field.optional then
                Ok subst  # Optional field can be missing
            else
                Err (SolverError (RowError "Required field missing in subtype"))

        Ok (RExtend sub_ext) ->
            if sub_ext.label == field_name then
                # Found the field - check it
                if super_field.readonly && !sub_ext.readonly then
                    Err (SolverError (RowError "Readonly mismatch"))
                else
                    # Check field type (depth subtyping)
                    when check_subtype store sub_ext.field_type super_field.field_type subst is
                        Ok new_subst ->
                            # Continue checking the rest of super row
                            check_row_subtype store sub_ext.rest super_field.rest new_subst
                        Err e -> Err e
            else
                # Keep looking in rest of sub row
                find_and_check_field store sub_ext.rest field_name super_field subst

        Ok (RVar var) ->
            Err Defer  # Can't determine yet

        _ -> Err (SolverError (RowError "Invalid sub row"))

# Check index signature subtyping
check_index_subtype : TypeStore, RowId, { key_type: TypeId, value_type: TypeId, rest: RowId }, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_index_subtype = \store, sub_row_id, super_index, subst ->
    # All fields in sub must be assignable to index signature
    # TODO: Implement index signature checking
    Ok subst

# Check class subtyping (nominal + structural)
check_class_subtype : TypeStore, ClassId, ClassId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_class_subtype = \store, sub_class, super_class, subst ->
    if sub_class == super_class then
        Ok subst
    else
        # Check if sub extends super
        when get_class store sub_class is
            Ok sub_def ->
                when sub_def.extends is
                    Ok extends_type ->
                        # Check if extends type is the super class
                        when get_type store extends_type is
                            Ok (TClass parent_class) ->
                                if parent_class == super_class then
                                    Ok subst
                                else
                                    # Check parent's parent (transitive)
                                    check_class_subtype store parent_class super_class subst
                            _ -> Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Class doesn't extend super" }))
                    Err _ -> Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Class has no parent" }))
            Err _ -> Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Invalid sub class" }))

# Check if class implements interface
check_class_implements : TypeStore, ClassId, InterfaceId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_class_implements = \store, class_id, iface_id, subst ->
    when (get_class store class_id, get_interface store iface_id) is
        (Ok class_def, Ok iface_def) ->
            # Check if class explicitly implements interface
            has_explicit = List.any class_def.implements \impl_type ->
                when get_type store impl_type is
                    Ok (TInterface impl_iface) -> impl_iface == iface_id
                    _ -> Bool.false

            if has_explicit then
                Ok subst
            else
                # Check structural compatibility
                check_class_interface_structure store class_def iface_def subst
        _ -> Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Invalid class or interface" }))

# Check structural compatibility between class and interface
check_class_interface_structure : TypeStore, ClassDef, InterfaceDef, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_class_interface_structure = \store, class_def, iface_def, initial_subst ->
    # Check all interface properties exist in class
    List.walk_until iface_def.properties (Ok initial_subst) \result, iface_prop ->
        when result is
            Ok subst ->
                when find_class_property class_def.properties iface_prop.name is
                    Ok class_prop ->
                        # Check property type
                        when check_subtype store class_prop.prop_type iface_prop.prop_type subst is
                            Ok new_subst -> Continue (Ok new_subst)
                            Err e -> Break (Err e)
                    Err _ ->
                        if iface_prop.optional then
                            Continue (Ok subst)
                        else
                            Break (Err (SolverError (RowError "Required interface property missing in class")))
            Err e -> Break (Err e)

# Find property in class
find_class_property : List { name: Str, prop_type: TypeId, is_static: Bool, is_private: Bool }, Str -> Result { name: Str, prop_type: TypeId, is_static: Bool, is_private: Bool } [NotFound]
find_class_property = \properties, prop_name ->
    List.find_first properties \p -> p.name == prop_name

# Check interface subtyping
check_interface_subtype : TypeStore, InterfaceId, InterfaceId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_interface_subtype = \store, sub_iface, super_iface, subst ->
    if sub_iface == super_iface then
        Ok subst
    else
        when (get_interface store sub_iface, get_interface store super_iface) is
            (Ok sub_def, Ok super_def) ->
                # Check if sub extends super
                has_extends = List.any sub_def.extends \extend_type ->
                    when get_type store extend_type is
                        Ok (TInterface extend_iface) -> extend_iface == super_iface
                        _ -> Bool.false

                if has_extends then
                    Ok subst
                else
                    # Check structural compatibility
                    check_interface_structure store sub_def super_def subst
            _ -> Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Invalid interfaces" }))

# Check structural interface compatibility
check_interface_structure : TypeStore, InterfaceDef, InterfaceDef, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_interface_structure = \store, sub_def, super_def, initial_subst ->
    # Check all super properties exist in sub
    List.walk_until super_def.properties (Ok initial_subst) \result, super_prop ->
        when result is
            Ok subst ->
                when find_interface_property sub_def.properties super_prop.name is
                    Ok sub_prop ->
                        # Check property type
                        when check_subtype store sub_prop.prop_type super_prop.prop_type subst is
                            Ok new_subst -> Continue (Ok new_subst)
                            Err e -> Break (Err e)
                    Err _ ->
                        if super_prop.optional then
                            Continue (Ok subst)
                        else
                            Break (Err (SolverError (RowError "Required property missing in sub-interface")))
            Err e -> Break (Err e)

# Find property in interface
find_interface_property : List { name: Str, prop_type: TypeId, optional: Bool, readonly: Bool }, Str -> Result { name: Str, prop_type: TypeId, optional: Bool, readonly: Bool } [NotFound]
find_interface_property = \properties, prop_name ->
    List.find_first properties \p -> p.name == prop_name

# Evaluate conditional type for subtyping
evaluate_conditional_subtype : TypeStore, { check: TypeId, extends: TypeId, true_type: TypeId, false_type: TypeId }, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
evaluate_conditional_subtype = \store, cond, super_id, subst ->
    # Check if check extends extends
    when check_subtype store cond.check cond.extends subst is
        Ok _ ->
            # Condition is true, use true branch
            check_subtype store cond.true_type super_id subst
        Err _ ->
            # Condition is false, use false branch
            check_subtype store cond.false_type super_id subst

# Check mapped type subtyping
check_mapped_subtype : TypeStore, { key_type: TypeId, value_type: TypeId, readonly: Bool, optional: Bool }, { key_type: TypeId, value_type: TypeId, readonly: Bool, optional: Bool }, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_mapped_subtype = \store, sub_mapped, super_mapped, subst ->
    # Readonly and optional must be compatible
    readonly_ok = !super_mapped.readonly || sub_mapped.readonly
    optional_ok = !sub_mapped.optional || super_mapped.optional

    if !readonly_ok || !optional_ok then
        Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Mapped type modifiers incompatible" }))
    else
        # Check key and value types
        check_subtype store sub_mapped.key_type super_mapped.key_type subst
            |> Result.try \s -> check_subtype store sub_mapped.value_type super_mapped.value_type s

# Check template literal subtyping
check_template_literal_subtype : TypeStore, { parts: List Str, types: List TypeId }, { parts: List Str, types: List TypeId }, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_template_literal_subtype = \store, sub_tmpl, super_tmpl, subst ->
    # For now, require exact match
    # TODO: Implement proper template literal subtyping
    if sub_tmpl.parts == super_tmpl.parts then
        check_subtype_lists store sub_tmpl.types super_tmpl.types subst
    else
        Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Template literals don't match" }))

# Check keyof subtyping
check_keyof_subtype : TypeStore, TypeId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_keyof_subtype = \store, obj_id, super_id, subst ->
    # Extract object keys and check if they're subtypes of super
    when get_type store obj_id is
        Ok (TObject row_id) ->
            # Get keys from object
            keys = extract_object_keys store row_id
            # Check if key union is subtype of super
            when keys is
                [] -> check_subtype store 0 super_id subst  # Never type
                _ ->
                    # TODO: Create union of key literal types
                    Ok subst
        _ -> Err Defer

# Extract keys from object row
extract_object_keys : TypeStore, RowId -> List Str
extract_object_keys = \store, row_id ->
    extract_keys_helper store row_id []

extract_keys_helper : TypeStore, RowId, List Str -> List Str
extract_keys_helper = \store, row_id, acc ->
    when get_row store row_id is
        Ok REmpty -> acc
        Ok (RExtend ext) ->
            new_acc = List.append acc ext.label
            extract_keys_helper store ext.rest new_acc
        Ok (RIndex _) -> acc  # Index signatures don't contribute specific keys
        _ -> acc

# Resolve indexed access for subtyping
resolve_indexed_access_subtype : TypeStore, { object: TypeId, index: TypeId }, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
resolve_indexed_access_subtype = \store, acc, super_id, subst ->
    # Resolve the indexed access to a concrete type
    when (get_type store acc.object, get_type store acc.index) is
        (Ok (TObject row_id), Ok (TLiteral (StrLit key))) ->
            # Look up the key in the object
            when find_object_field store row_id key is
                Ok field_type -> check_subtype store field_type super_id subst
                Err _ -> Err (SolverError (SubtypeError { expected: super_id, actual: 0, reason: "Key not found in object" }))
        _ -> Err Defer

# Find field in object row
find_object_field : TypeStore, RowId, Str -> Result TypeId [NotFound]
find_object_field = \store, row_id, field_name ->
    when get_row store row_id is
        Ok REmpty -> Err NotFound
        Ok (RExtend ext) ->
            if ext.label == field_name then
                Ok ext.field_type
            else
                find_object_field store ext.rest field_name
        Ok (RIndex idx) ->
            # Any field matches index signature
            Ok idx.value_type
        _ -> Err NotFound

# Check generic type arguments subtyping
check_generic_args_subtype : TypeStore, List TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_generic_args_subtype = \store, sub_args, super_args, subst ->
    # For now, assume invariance (exact match required)
    # TODO: Handle variance annotations properly
    if List.len sub_args == List.len super_args then
        check_type_args_equal store sub_args super_args subst
    else
        Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Generic argument count mismatch" }))

# Check type arguments are equal (for invariance)
check_type_args_equal : TypeStore, List TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
check_type_args_equal = \store, args1, args2, initial_subst ->
    when (args1, args2) is
        ([], []) -> Ok initial_subst
        ([a1, .. as rest1], [a2, .. as rest2]) ->
            # Check both directions for equality
            check_subtype store a1 a2 initial_subst
                |> Result.try \s1 -> check_subtype store a2 a1 s1
                |> Result.try \s2 -> check_type_args_equal store rest1 rest2 s2
        _ -> Err (SolverError (SubtypeError { expected: 0, actual: 0, reason: "Type argument lists don't match" }))

# Generic instantiation
instantiate_generic : TypeStore, TypeId, TypeId, List TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
instantiate_generic = \store, type_id, base_type, type_args, subst ->
    # TODO: Implement generic instantiation
    Ok subst

# Type parameter constraint
constrain_type_param : TypeStore, TypeParamId, TypeId, Substitution -> Result Substitution [Defer, SolverError SolverError]
constrain_type_param = \store, param_id, type_id, subst ->
    when get_type_param store param_id is
        Ok param_def ->
            when param_def.constraint is
                Ok constraint_type ->
                    # Check type satisfies constraint
                    check_subtype store type_id constraint_type subst
                Err _ ->
                    # No constraint, always ok
                    Ok subst
        Err _ -> Err (SolverError (UnificationError "Invalid type parameter"))

# Apply substitution to a type
apply_substitution : TypeStore, TypeId, Substitution -> TypeId
apply_substitution = \store, type_id, subst ->
    apply_type_substitution store type_id subst

apply_type_substitution : TypeStore, TypeId, Substitution -> TypeId
apply_type_substitution = \store, type_id, subst ->
    when get_type store type_id is
        Ok (TypeVariable var) ->
            # Look up in substitution
            when List.find_first subst.type_var_map \mapping -> mapping.var == var is
                Ok mapping -> mapping.replacement
                Err _ -> type_id  # No substitution
        _ -> type_id  # Not a variable

apply_row_substitution : TypeStore, RowId, Substitution -> RowId
apply_row_substitution = \store, row_id, subst ->
    when get_row store row_id is
        Ok (RVar var) ->
            # Look up in substitution
            when List.find_first subst.row_var_map \mapping -> mapping.var == var is
                Ok mapping -> mapping.replacement
                Err _ -> row_id  # No substitution
        _ -> row_id  # Not a variable

# Add type substitution
add_type_substitution : Substitution, U32, TypeId -> Substitution
add_type_substitution = \subst, var, replacement ->
    { subst & type_var_map: List.append subst.type_var_map { var, replacement } }

# Add row substitution
add_row_substitution : Substitution, U32, RowId -> Substitution
add_row_substitution = \subst, var, replacement ->
    { subst & row_var_map: List.append subst.row_var_map { var, replacement } }

# Check if two literals are equal
literals_equal : LiteralValue, LiteralValue -> Bool
literals_equal = \lit1, lit2 ->
    when (lit1, lit2) is
        (NumLit n1, NumLit n2) ->
            # Compare floating point values by converting to string
            Num.to_str n1 == Num.to_str n2
        (StrLit s1, StrLit s2) -> s1 == s2
        (BoolLit b1, BoolLit b2) -> b1 == b2
        (BigIntLit s1, BigIntLit s2) -> s1 == s2
        _ -> Bool.false

# Public subtype checking
is_subtype_of : TypeStore, TypeId, TypeId -> Bool
is_subtype_of = \store, sub_type, super_type ->
    empty_subst = { type_var_map: [], row_var_map: [] }
    when check_subtype store sub_type super_type empty_subst is
        Ok _ -> Bool.true
        Err _ -> Bool.false

# Convert constraint to string for debugging
constraint_to_str : TypeStore, Constraint -> Str
constraint_to_str = \store, constraint ->
    source_str = when constraint.source is
        Ok s -> " ($(s))"
        Err _ -> ""

    kind_str = when constraint.kind is
        Equality t1 t2 ->
            t1_str = Num.to_str t1
            t2_str = Num.to_str t2
            "$(t1_str) = $(t2_str)"

        Subtype sub super ->
            sub_str = Num.to_str sub
            super_str = Num.to_str super
            "$(sub_str) <: $(super_str)"

        RowEquality r1 r2 ->
            r1_str = Num.to_str r1
            r2_str = Num.to_str r2
            "row $(r1_str) = row $(r2_str)"

        RowSubtype sub super ->
            sub_str = Num.to_str sub
            super_str = Num.to_str super
            "row $(sub_str) <: row $(super_str)"

        Instance t base args ->
            t_str = Num.to_str t
            base_str = Num.to_str base
            "$(t_str) ~ instance of $(base_str)"

        TypeParamConstraint param t ->
            param_str = Num.to_str param
            t_str = Num.to_str t
            "param $(param_str) : $(t_str)"

    "$(kind_str)$(source_str)"

# Convert solution to string
solution_to_str : Solution -> Str
solution_to_str = \solution ->
    when solution is
        Ok subst ->
            type_mappings = List.map subst.type_var_map \m ->
                "T$(Num.to_str m.var) -> $(Num.to_str m.replacement)"
            row_mappings = List.map subst.row_var_map \m ->
                "R$(Num.to_str m.var) -> $(Num.to_str m.replacement)"
            all_mappings = List.concat type_mappings row_mappings
            if List.is_empty all_mappings then
                "Empty substitution"
            else
                Str.join_with all_mappings ", "
        Err error ->
            when error is
                UnificationError msg -> "Unification error: $(msg)"
                SubtypeError { reason } -> "Subtype error: $(reason)"
                CyclicType t -> "Cyclic type: $(Num.to_str t)"
                RowError msg -> "Row error: $(msg)"
                UnsolvableConstraint c -> "Unsolvable constraint"
                ConflictingConstraints cs -> "Conflicting constraints"
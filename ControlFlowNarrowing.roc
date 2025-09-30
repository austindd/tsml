module [
    TypeGuard,
    NarrowingContext,
    RefineType,
    narrow_type,
    apply_guard,
    narrow_typeof,
    narrow_truthiness,
    narrow_equality,
    merge_contexts,
    narrow_in_branch,
]

import ComprehensiveTypeIndexed as T

# Type guards represent runtime checks that narrow types
TypeGuard : [
    TypeofGuard { var_name : Str, type_name : Str, negated : Bool },
    TruthinessGuard { var_name : Str, negated : Bool },
    EqualityGuard { var_name : Str, value : T.TypeId, negated : Bool },
    InstanceofGuard { var_name : Str, constructor : T.TypeId, negated : Bool },
    InGuard { property : Str, var_name : Str, negated : Bool },
    NullishGuard { var_name : Str, negated : Bool },
    AndGuard (List TypeGuard),
    OrGuard (List TypeGuard),
]

# Context for tracking narrowed types in control flow
NarrowingContext : {
    store : T.TypeStore,
    refinements : List { var_name : Str, original_type : T.TypeId, narrowed_type : T.TypeId },
}

# Type refinement result
RefineType : {
    refined_type : T.TypeId,
    store : T.TypeStore,
}

# Apply a type guard to narrow a type
narrow_type : T.TypeStore, T.TypeId, TypeGuard, Str -> RefineType
narrow_type = \store, type_id, guard, var_name ->
    when guard is
        TypeofGuard(data) ->
            if data.var_name == var_name then
                narrow_typeof(store, type_id, data.type_name, data.negated)
            else
                { refined_type: type_id, store: store }

        TruthinessGuard(data) ->
            if data.var_name == var_name then
                narrow_truthiness(store, type_id, data.negated)
            else
                { refined_type: type_id, store: store }

        EqualityGuard(data) ->
            if data.var_name == var_name then
                narrow_equality(store, type_id, data.value, data.negated)
            else
                { refined_type: type_id, store: store }

        InstanceofGuard(data) ->
            if data.var_name == var_name then
                narrow_instanceof(store, type_id, data.constructor, data.negated)
            else
                { refined_type: type_id, store: store }

        InGuard(data) ->
            if data.var_name == var_name then
                narrow_in_property(store, type_id, data.property, data.negated)
            else
                { refined_type: type_id, store: store }

        NullishGuard(data) ->
            if data.var_name == var_name then
                narrow_nullish(store, type_id, data.negated)
            else
                { refined_type: type_id, store: store }

        AndGuard(guards) ->
            # Apply all guards in sequence (intersection)
            List.walk(guards, { refined_type: type_id, store: store }, \acc, g ->
                narrow_type(acc.store, acc.refined_type, g, var_name)
            )

        OrGuard(guards) ->
            # Apply guards and union the results
            results = List.map(guards, \g -> narrow_type(store, type_id, g, var_name))

            # Join all refined types
            (final_store, refined) = List.walk(
                results,
                (store, type_id),
                \(acc_store, acc_type), result ->
                    T.join(acc_store, acc_type, result.refined_type)
            )
            { refined_type: refined, store: final_store }

# Narrow type based on typeof check
narrow_typeof : T.TypeStore, T.TypeId, Str, Bool -> RefineType
narrow_typeof = \store, type_id, type_name, negated ->
    # Get the primitive type for the typeof check
    (store1, expected_type) = when type_name is
        "number" -> T.make_primitive(store, "number")
        "string" -> T.make_primitive(store, "string")
        "boolean" -> T.make_primitive(store, "boolean")
        "undefined" -> T.make_primitive(store, "undefined")
        "object" ->
            # typeof null === "object" in JavaScript
            (s1, null_type) = T.make_primitive(store, "null")
            (s2, obj_type) = T.make_any(s1)  # Using any as placeholder for object
            T.make_union(s2, [obj_type, null_type])
        "function" -> T.make_any(store)  # Placeholder for function type
        "symbol" -> T.make_primitive(store, "symbol")
        "bigint" -> T.make_primitive(store, "bigint")
        _ -> T.make_never(store)

    if negated then
        # Negated: type & !expected_type
        narrow_by_exclusion(store1, type_id, expected_type)
    else
        # Not negated: type & expected_type
        (store2, narrowed) = T.meet(store1, type_id, expected_type)
        { refined_type: narrowed, store: store2 }

# Narrow type based on truthiness check
narrow_truthiness : T.TypeStore, T.TypeId, Bool -> RefineType
narrow_truthiness = \store, type_id, negated ->
    # Falsy values in JavaScript: false, 0, -0, 0n, "", null, undefined, NaN
    (store1, false_lit) = T.make_literal(store, BoolLit(Bool.false))
    (store2, zero_lit) = T.make_literal(store1, NumLit("0"))
    (store3, empty_str) = T.make_literal(store2, StrLit(""))
    (store4, null_type) = T.make_primitive(store3, "null")
    (store5, undefined_type) = T.make_primitive(store4, "undefined")

    # Create union of all falsy types
    (store6, falsy_union) = T.make_union(store5, [false_lit, zero_lit, empty_str, null_type, undefined_type])

    if negated then
        # Checking for falsy: narrow to falsy types
        (store7, narrowed) = T.meet(store6, type_id, falsy_union)
        { refined_type: narrowed, store: store7 }
    else
        # Checking for truthy: exclude falsy types
        narrow_by_exclusion(store6, type_id, falsy_union)

# Narrow type based on equality check
narrow_equality : T.TypeStore, T.TypeId, T.TypeId, Bool -> RefineType
narrow_equality = \store, type_id, value_type, negated ->
    if negated then
        # Not equal: exclude the value type
        narrow_by_exclusion(store, type_id, value_type)
    else
        # Equal: narrow to the value type
        (store1, narrowed) = T.meet(store, type_id, value_type)
        { refined_type: narrowed, store: store1 }

# Narrow type based on instanceof check
narrow_instanceof : T.TypeStore, T.TypeId, T.TypeId, Bool -> RefineType
narrow_instanceof = \store, type_id, constructor_type, negated ->
    # For instanceof, we'd need to track constructor/prototype relationships
    # For now, just return the type as-is or never if negated
    if negated then
        narrow_by_exclusion(store, type_id, constructor_type)
    else
        (store1, narrowed) = T.meet(store, type_id, constructor_type)
        { refined_type: narrowed, store: store1 }

# Narrow type based on 'in' property check
narrow_in_property : T.TypeStore, T.TypeId, Str, Bool -> RefineType
narrow_in_property = \store, type_id, property, negated ->
    # Check if type has the property
    when T.get_type(store, type_id) is
        Ok(TObject(row_id)) ->
            if has_property(store, row_id, property) then
                if negated then
                    # Has property but we're checking it doesn't
                    (store1, never) = T.make_never(store)
                    { refined_type: never, store: store1 }
                else
                    # Has property and we're checking it does
                    { refined_type: type_id, store: store }
            else
                if negated then
                    # Doesn't have property and we're checking it doesn't
                    { refined_type: type_id, store: store }
                else
                    # Doesn't have property but we're checking it does
                    (store1, never) = T.make_never(store)
                    { refined_type: never, store: store1 }
        _ ->
            # Not an object type - can't narrow by property
            { refined_type: type_id, store: store }

# Check if a row type has a property
has_property : T.TypeStore, T.RowId, Str -> Bool
has_property = \store, row_id, property ->
    when T.get_row(store, row_id) is
        Ok(RExtend(extend)) ->
            if extend.label == property then
                Bool.true
            else
                has_property(store, extend.rest, property)
        _ -> Bool.false

# Narrow type based on nullish check (null or undefined)
narrow_nullish : T.TypeStore, T.TypeId, Bool -> RefineType
narrow_nullish = \store, type_id, negated ->
    (store1, null_type) = T.make_primitive(store, "null")
    (store2, undefined_type) = T.make_primitive(store1, "undefined")
    (store3, nullish) = T.make_union(store2, [null_type, undefined_type])

    if negated then
        # Not nullish: exclude null and undefined
        narrow_by_exclusion(store3, type_id, nullish)
    else
        # Is nullish: narrow to null or undefined
        (store4, narrowed) = T.meet(store3, type_id, nullish)
        { refined_type: narrowed, store: store4 }

# Helper: Narrow by excluding a type
narrow_by_exclusion : T.TypeStore, T.TypeId, T.TypeId -> RefineType
narrow_by_exclusion = \store, type_id, exclude_type ->
    # This is essentially type_id & !exclude_type
    # We need to handle this based on the type structure

    when T.get_type(store, type_id) is
        Ok(TUnion(types)) ->
            # Filter out types that are subtypes of exclude_type
            remaining = List.keep_if(types, \t ->
                Bool.not(T.is_subtype_of(store, t, exclude_type))
            )

            when remaining is
                [] ->
                    (store1, never) = T.make_never(store)
                    { refined_type: never, store: store1 }
                [single] ->
                    { refined_type: single, store: store }
                multiple ->
                    (store1, union) = T.make_union(store, multiple)
                    { refined_type: union, store: store1 }
        _ ->
            # For non-union types, check if it's a subtype of what we're excluding
            if T.is_subtype_of(store, type_id, exclude_type) then
                # The entire type is excluded
                (store1, never) = T.make_never(store)
                { refined_type: never, store: store1 }
            else
                # The type doesn't overlap with what we're excluding
                { refined_type: type_id, store: store }

# Apply a type guard to a context
apply_guard : NarrowingContext, TypeGuard -> NarrowingContext
apply_guard = \ctx, guard ->
    # Apply guard to all variables in context
    new_refinements = List.map(ctx.refinements, \refinement ->
        result = narrow_type(ctx.store, refinement.narrowed_type, guard, refinement.var_name)
        { refinement &
            narrowed_type: result.refined_type,
        }
    )

    # Get the final store from narrowing
    final_store = when List.last(ctx.refinements) is
        Ok(last) ->
            result = narrow_type(ctx.store, last.narrowed_type, guard, last.var_name)
            result.store
        Err(_) -> ctx.store

    { store: final_store, refinements: new_refinements }

# Narrow types in a branch based on a condition
narrow_in_branch : NarrowingContext, TypeGuard, Bool -> NarrowingContext
narrow_in_branch = \ctx, guard, is_then_branch ->
    # In the then branch, apply the guard as-is
    # In the else branch, apply the negated guard
    actual_guard = if is_then_branch then
        guard
    else
        negate_guard(guard)

    apply_guard(ctx, actual_guard)

# Negate a type guard
negate_guard : TypeGuard -> TypeGuard
negate_guard = \guard ->
    when guard is
        TypeofGuard(data) -> TypeofGuard({ data & negated: Bool.not(data.negated) })
        TruthinessGuard(data) -> TruthinessGuard({ data & negated: Bool.not(data.negated) })
        EqualityGuard(data) -> EqualityGuard({ data & negated: Bool.not(data.negated) })
        InstanceofGuard(data) -> InstanceofGuard({ data & negated: Bool.not(data.negated) })
        InGuard(data) -> InGuard({ data & negated: Bool.not(data.negated) })
        NullishGuard(data) -> NullishGuard({ data & negated: Bool.not(data.negated) })
        AndGuard(guards) -> OrGuard(List.map(guards, negate_guard))  # De Morgan's law
        OrGuard(guards) -> AndGuard(List.map(guards, negate_guard))  # De Morgan's law

# Merge contexts from different control flow branches
merge_contexts : List NarrowingContext -> NarrowingContext
merge_contexts = \contexts ->
    when contexts is
        [] ->
            { store: T.empty_store, refinements: [] }
        [single] ->
            single
        multiple ->
            # For each variable, join the types from all branches
            all_vars = List.walk(multiple, [], \acc, ctx ->
                List.walk(ctx.refinements, acc, \vars, refinement ->
                    if List.any(vars, \v -> v == refinement.var_name) then
                        vars
                    else
                        List.append(vars, refinement.var_name)
                )
            )

            # Get the last store
            final_store = when List.last(multiple) is
                Ok(last) -> last.store
                Err(_) -> T.empty_store

            # For each variable, join types from all contexts
            merged_refinements = List.map(all_vars, \var_name ->
                # Get this variable's type from each context
                types_and_originals = List.keep_oks(multiple, \ctx ->
                    List.find_first(ctx.refinements, \r -> r.var_name == var_name)
                )

                when types_and_originals is
                    [] ->
                        # Should not happen
                        (store1, unknown) = T.make_unknown(final_store)
                        { var_name: var_name, original_type: unknown, narrowed_type: unknown }
                    refinements ->
                        # Get the first original type (should be same across all)
                        original = when List.first(refinements) is
                            Ok(r) -> r.original_type
                            Err(_) ->
                                (s, u) = T.make_unknown(final_store)
                                u

                        # Join all narrowed types
                        narrowed_types = List.map(refinements, \r -> r.narrowed_type)
                        (joined_store, joined_type) = List.walk(
                            List.drop_first(narrowed_types, 1),
                            (final_store, List.first(narrowed_types) |> Result.with_default(original)),
                            \(store, current), next ->
                                T.join(store, current, next)
                        )

                        { var_name: var_name, original_type: original, narrowed_type: joined_type }
            )

            { store: final_store, refinements: merged_refinements }

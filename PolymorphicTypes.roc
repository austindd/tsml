module [
    TypeScheme,
    TypeParameter,
    Quantifier,
    Variance,
    InstantiationContext,
    quantify,
    instantiate,
    generalize,
    freshen_type_params,
    get_free_type_vars,
    substitute,
    occurs_check,
]

import ComprehensiveTypeIndexed as T

# Type schemes represent polymorphic types with quantified type variables
TypeScheme : {
    quantifiers : List TypeParameter,
    body : T.TypeId,
}

# Type parameters with bounds and variance
TypeParameter : {
    name : Str,
    id : T.TypeId,  # The type variable itself
    quantifier : Quantifier,
    upper_bound : Result T.TypeId [NoBound],  # T extends Upper
    lower_bound : Result T.TypeId [NoBound],  # T super Lower
    variance : Variance,
}

# Quantifier types
Quantifier : [
    Universal,    # ∀ - for all types
    Existential,  # ∃ - exists a type
]

# Variance for type parameters
Variance : [
    Invariant,     # Must match exactly
    Covariant,     # Can be subtype (return types)
    Contravariant, # Can be supertype (parameter types)
    Bivariant,     # Can be either (rarely used)
]

# Context for instantiating polymorphic types
InstantiationContext : {
    store : T.TypeStore,
    substitutions : List { param : T.TypeId, replacement : T.TypeId },
    fresh_counter : U32,  # For generating fresh type variables
}

# Quantify free type variables in a type to create a type scheme
quantify : T.TypeStore, T.TypeId, Quantifier -> (T.TypeStore, TypeScheme)
quantify = \store, type_id, quantifier ->
    # Find all free type variables in the type
    free_vars = get_free_type_vars(store, type_id, [])

    # Create type parameters for each free variable
    (final_store, params) = List.walk(free_vars, (store, []), \(s, ps), var_id ->
        param = {
            name: get_type_var_name(s, var_id),
            id: var_id,
            quantifier: quantifier,
            upper_bound: get_upper_bound(s, var_id),
            lower_bound: get_lower_bound(s, var_id),
            variance: infer_variance(s, type_id, var_id),
        }
        (s, List.append(ps, param))
    )

    scheme = {
        quantifiers: params,
        body: type_id,
    }

    (final_store, scheme)

# Instantiate a type scheme with fresh type variables or concrete types
instantiate : InstantiationContext, TypeScheme -> (InstantiationContext, T.TypeId)
instantiate = \ctx, scheme ->
    # For universal quantification: replace with fresh type variables
    # For existential quantification: keep as type variables with constraints

    (new_ctx, substitutions) = List.walk(
        scheme.quantifiers,
        (ctx, []),
        \(c, subs), param ->
            when param.quantifier is
                Universal ->
                    # Create a fresh type variable for universal quantification
                    (c1, fresh_var) = make_fresh_type_var(c, param.name)

                    # Apply bounds if they exist
                    c2 = when param.upper_bound is
                        Ok(bound) -> add_upper_bound_constraint(c1, fresh_var, bound)
                        Err(_) -> c1

                    c3 = when param.lower_bound is
                        Ok(bound) -> add_lower_bound_constraint(c2, fresh_var, bound)
                        Err(_) -> c2

                    sub = { param: param.id, replacement: fresh_var }
                    (c3, List.append(subs, sub))

                Existential ->
                    # For existential, we keep the type variable but track constraints
                    # This allows the type to be any type that satisfies constraints
                    (c, subs)
    )

    # Apply substitutions to the body type
    (final_ctx, instantiated) = substitute(new_ctx, scheme.body, substitutions)

    (final_ctx, instantiated)

# Generalize a type by quantifying free variables that aren't in the environment
generalize : T.TypeStore, T.TypeId, List T.TypeId -> (T.TypeStore, TypeScheme)
generalize = \store, type_id, env_types ->
    # Find free variables in the type
    type_free_vars = get_free_type_vars(store, type_id, [])

    # Find free variables in the environment
    env_free_vars = List.walk(env_types, [], \acc, env_type ->
        List.concat(acc, get_free_type_vars(store, env_type, []))
    )

    # Variables to generalize are those in type but not in environment
    vars_to_generalize = List.keep_if(type_free_vars, \var ->
        Bool.not(List.contains(env_free_vars, var))
    )

    # Create type parameters for variables to generalize
    (final_store, params) = List.walk(vars_to_generalize, (store, []), \(s, ps), var_id ->
        param = {
            name: get_type_var_name(s, var_id),
            id: var_id,
            quantifier: Universal,  # Generalization uses universal quantification
            upper_bound: get_upper_bound(s, var_id),
            lower_bound: get_lower_bound(s, var_id),
            variance: infer_variance(s, type_id, var_id),
        }
        (s, List.append(ps, param))
    )

    scheme = {
        quantifiers: params,
        body: type_id,
    }

    (final_store, scheme)

# Create fresh type parameters for a function call
freshen_type_params : InstantiationContext, List TypeParameter -> (InstantiationContext, List { param : T.TypeId, fresh : T.TypeId })
freshen_type_params = \ctx, params ->
    List.walk(params, (ctx, []), \(c, mappings), param ->
        (c1, fresh) = make_fresh_type_var(c, param.name)
        mapping = { param: param.id, fresh: fresh }
        (c1, List.append(mappings, mapping))
    )

# Get all free type variables in a type
get_free_type_vars : T.TypeStore, T.TypeId, List T.TypeId -> List T.TypeId
get_free_type_vars = \store, type_id, seen ->
    if List.contains(seen, type_id) then
        []
    else
        new_seen = List.append(seen, type_id)
        when T.get_type(store, type_id) is
            Ok(TypeVariable(_)) -> [type_id]

            Ok(TFunction(func)) ->
                param_vars = List.walk(func.params, [], \acc, p ->
                    List.concat(acc, get_free_type_vars(store, p.param_type, new_seen))
                )
                return_vars = get_free_type_vars(store, func.ret, new_seen)
                List.concat(param_vars, return_vars)

            Ok(TObject(row)) ->
                get_row_free_vars(store, row, new_seen)

            Ok(TArray(elem)) ->
                get_free_type_vars(store, elem, new_seen)

            Ok(TTuple(types)) ->
                List.walk(types, [], \acc, t ->
                    List.concat(acc, get_free_type_vars(store, t, new_seen))
                )

            Ok(TUnion(types)) ->
                List.walk(types, [], \acc, t ->
                    List.concat(acc, get_free_type_vars(store, t, new_seen))
                )

            Ok(TIntersection(types)) ->
                List.walk(types, [], \acc, t ->
                    List.concat(acc, get_free_type_vars(store, t, new_seen))
                )

            _ -> []

# Get free type variables in a row type
get_row_free_vars : T.TypeStore, T.RowId, List T.TypeId -> List T.TypeId
get_row_free_vars = \store, row_id, seen ->
    when T.get_row(store, row_id) is
        Ok(REmpty) -> []
        Ok(RExtend(ext)) ->
            field_vars = get_free_type_vars(store, ext.field_type, seen)
            rest_vars = get_row_free_vars(store, ext.rest, seen)
            List.concat(field_vars, rest_vars)
        Ok(RVar(_)) -> []  # Row variables handled separately
        _ -> []

# Substitute type variables with their replacements
substitute : InstantiationContext, T.TypeId, List { param : T.TypeId, replacement : T.TypeId } -> (InstantiationContext, T.TypeId)
substitute = \ctx, type_id, substitutions ->
    # Check if this type should be substituted
    when List.find_first(substitutions, \sub -> sub.param == type_id) is
        Ok(sub) -> (ctx, sub.replacement)
        Err(_) ->
            # Recursively substitute in compound types
            when T.get_type(ctx.store, type_id) is
                Ok(TFunction(func)) ->
                    (ctx1, new_params) = List.walk(func.params, (ctx, []), \(c, ps), p ->
                        (c1, new_p_type) = substitute(c, p.param_type, substitutions)
                        (c1, List.append(ps, { p & param_type: new_p_type }))
                    )
                    (ctx2, new_return) = substitute(ctx1, func.ret, substitutions)

                    (store2, new_func) = T.make_function(ctx2.store, new_params, new_return, func.type_params, func.is_async, func.is_generator)
                    ({ ctx2 & store: store2 }, new_func)

                Ok(TArray(elem)) ->
                    (ctx1, new_elem) = substitute(ctx, elem, substitutions)
                    (store1, new_array) = T.make_array(ctx1.store, new_elem)
                    ({ ctx1 & store: store1 }, new_array)

                Ok(TTuple(types)) ->
                    (ctx1, new_types) = List.walk(types, (ctx, []), \(c, ts), t ->
                        (c1, new_t) = substitute(c, t, substitutions)
                        (c1, List.append(ts, new_t))
                    )
                    (store1, new_tuple) = T.make_tuple(ctx1.store, new_types)
                    ({ ctx1 & store: store1 }, new_tuple)

                Ok(TUnion(types)) ->
                    (ctx1, new_types) = List.walk(types, (ctx, []), \(c, ts), t ->
                        (c1, new_t) = substitute(c, t, substitutions)
                        (c1, List.append(ts, new_t))
                    )
                    (store1, new_union) = T.make_union(ctx1.store, new_types)
                    ({ ctx1 & store: store1 }, new_union)

                _ -> (ctx, type_id)

# Occurs check: ensure a type variable doesn't occur in its own definition
occurs_check : T.TypeStore, T.TypeId, T.TypeId -> Bool
occurs_check = \store, var_id, type_id ->
    if var_id == type_id then
        Bool.true
    else
        when T.get_type(store, type_id) is
            Ok(TFunction(func)) ->
                List.any(func.params, \p -> occurs_check(store, var_id, p.param_type)) ||
                occurs_check(store, var_id, func.ret)

            Ok(TArray(elem)) ->
                occurs_check(store, var_id, elem)

            Ok(TTuple(types)) | Ok(TUnion(types)) | Ok(TIntersection(types)) ->
                List.any(types, \t -> occurs_check(store, var_id, t))

            Ok(TObject(row)) ->
                occurs_check_row(store, var_id, row)

            _ -> Bool.false

# Helper: occurs check in row types
occurs_check_row : T.TypeStore, T.TypeId, T.RowId -> Bool
occurs_check_row = \store, var_id, row_id ->
    when T.get_row(store, row_id) is
        Ok(RExtend(ext)) ->
            occurs_check(store, var_id, ext.field_type) ||
            occurs_check_row(store, var_id, ext.rest)
        _ -> Bool.false

# Helper: get name for a type variable
get_type_var_name : T.TypeStore, T.TypeId -> Str
get_type_var_name = \store, type_id ->
    # For now, just use a generic name
    "T"

# Helper: get upper bound for a type variable
get_upper_bound : T.TypeStore, T.TypeId -> Result T.TypeId [NoBound]
get_upper_bound = \store, type_id ->
    # For now, return no bounds
    Err(NoBound)

# Helper: get lower bound for a type variable
get_lower_bound : T.TypeStore, T.TypeId -> Result T.TypeId [NoBound]
get_lower_bound = \store, type_id ->
    # For now, return no bounds
    Err(NoBound)

# Helper: infer variance of a type parameter
infer_variance : T.TypeStore, T.TypeId, T.TypeId -> Variance
infer_variance = \store, in_type, var_id ->
    # Analyze how the type variable is used to determine variance
    # This is a simplified version - full implementation would track positions
    when T.get_type(store, in_type) is
        Ok(TFunction(func)) ->
            # Check if var appears in parameters (contravariant) or return (covariant)
            in_params = List.any(func.params, \p -> type_contains_var(store, p.param_type, var_id))
            in_return = type_contains_var(store, func.ret, var_id)

            if in_params && in_return then
                Invariant  # Appears in both positions
            else if in_params then
                Contravariant
            else if in_return then
                Covariant
            else
                Invariant

        _ -> Invariant  # Default to invariant for safety

# Helper: check if a type contains a specific type variable
type_contains_var : T.TypeStore, T.TypeId, T.TypeId -> Bool
type_contains_var = \store, type_id, var_id ->
    if type_id == var_id then
        Bool.true
    else
        when T.get_type(store, type_id) is
            Ok(TFunction(func)) ->
                List.any(func.params, \p -> type_contains_var(store, p.param_type, var_id)) ||
                type_contains_var(store, func.ret, var_id)

            Ok(TArray(elem)) ->
                type_contains_var(store, elem, var_id)

            Ok(TTuple(types)) | Ok(TUnion(types)) | Ok(TIntersection(types)) ->
                List.any(types, \t -> type_contains_var(store, t, var_id))

            _ -> Bool.false

# Helper: create a fresh type variable
make_fresh_type_var : InstantiationContext, Str -> (InstantiationContext, T.TypeId)
make_fresh_type_var = \ctx, base_name ->
    # Use fresh_counter as the TypeVar (which is U64)
    (store1, fresh_var) = T.make_type_var(ctx.store, Num.to_u64(ctx.fresh_counter))

    new_ctx = { ctx &
        store: store1,
        fresh_counter: ctx.fresh_counter + 1,
    }

    (new_ctx, fresh_var)

# Helper: add upper bound constraint
add_upper_bound_constraint : InstantiationContext, T.TypeId, T.TypeId -> InstantiationContext
add_upper_bound_constraint = \ctx, var_id, bound ->
    # In a real implementation, this would add to a constraint store
    # For now, we just track it in the context
    ctx

# Helper: add lower bound constraint
add_lower_bound_constraint : InstantiationContext, T.TypeId, T.TypeId -> InstantiationContext
add_lower_bound_constraint = \ctx, var_id, bound ->
    # In a real implementation, this would add to a constraint store
    # For now, we just track it in the context
    ctx
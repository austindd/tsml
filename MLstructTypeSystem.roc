module [
    PrincipalType,
    TypeScheme,
    RowType,
    infer_principal_type,
]

# MLstruct + TypeScript Type System Design
# This module outlines what's needed for principal type inference

# ============================================
# CORE TYPE REPRESENTATION
# ============================================

# Type with row polymorphism and type variables
Type : [
    # Primitives
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,

    # Type variables (for polymorphism)
    TVar U32,

    # Polymorphic types
    TForall (List U32) Type,  # ∀α.τ

    # Object types with row polymorphism
    TObject RowType,

    # Array type
    TArray Type,

    # Function type
    TFunction (List Type) Type,

    # Union and Intersection
    TUnion (List Type),
    TIntersection (List Type),

    # Literal types
    TLiteral LiteralValue,

    # Top and bottom types
    TAny,
    TNever,
    TUnknown,
]

# Row types for structural typing
RowType : [
    # Empty row
    REmpty,

    # Row extension: field + rest of row
    RExtend Str Type RowType,

    # Row variable (for polymorphism)
    RVar U32,
]

# Literal values
LiteralValue : [
    NumLit F64,
    StrLit Str,
    BoolLit Bool,
]

# Type scheme (polymorphic type)
TypeScheme : {
    # Quantified variables
    quantified: List U32,

    # The type
    type: Type,

    # Row variables
    row_vars: List U32,
}

# Principal type result
PrincipalType : {
    # The most general type
    type: TypeScheme,

    # Substitution used
    substitution: List { var: U32, type: Type },

    # Row substitution
    row_substitution: List { var: U32, row: RowType },
}

# ============================================
# TYPE INFERENCE ENGINE
# ============================================

# Type environment
TypeEnv : {
    # Variable bindings
    bindings: List { name: Str, scheme: TypeScheme },

    # Current level for generalization
    level: U32,

    # Fresh variable counter
    next_var: U32,

    # Fresh row variable counter
    next_row_var: U32,
}

# Inference state
InferState : {
    env: TypeEnv,
    substitution: List { var: U32, type: Type },
    row_substitution: List { var: U32, row: RowType },
    constraints: List Constraint,
}

# Constraints for unification
Constraint : [
    # Type equality
    TypeEq Type Type,

    # Row equality
    RowEq RowType RowType,

    # Field presence constraint
    HasField RowType Str Type,

    # Subtype constraint
    SubType Type Type,

    # Instance constraint (for generics)
    Instance Type TypeScheme,
]

# ============================================
# PRINCIPAL TYPE INFERENCE
# ============================================

# Infer principal type for an expression
infer_principal_type : Ast.Node -> Result PrincipalType TypeError
infer_principal_type = \node ->
    initial_state = {
        env: empty_env(),
        substitution: [],
        row_substitution: [],
        constraints: [],
    }

    # Generate constraints
    (type, state) = generate_constraints(node, initial_state)

    # Solve constraints
    when solve_all_constraints(state.constraints, state) is
        Ok(final_state) ->
            # Generalize type
            scheme = generalize(type, final_state.env.level)

            Ok({
                type: scheme,
                substitution: final_state.substitution,
                row_substitution: final_state.row_substitution,
            })
        Err(e) -> Err(e)

# Generate constraints (Algorithm W style)
generate_constraints : Ast.Node, InferState -> (Type, InferState)
generate_constraints = \node, state ->
    when node is
        # Variable
        Identifier({ name }) ->
            when lookup_var(name, state.env) is
                Some(scheme) ->
                    # Instantiate polymorphic type
                    (instantiate(scheme, state))
                None ->
                    # Unbound variable - create fresh type var
                    fresh_var(state)

        # Lambda abstraction
        ArrowFunctionExpression({ params, body }) ->
            # Create fresh type vars for parameters
            (param_types, state1) = fresh_vars(List.len(params), state)

            # Extend environment with parameters
            env_with_params = extend_env_many(
                List.map2(params, param_types, \p, t -> (param_name(p), mono_scheme(t))),
                state1.env
            )

            # Infer body type
            (body_type, state2) = generate_constraints(body, { state1 & env: env_with_params })

            # Return function type
            (TFunction param_types body_type, state2)

        # Object literal
        ObjectExpression({ properties }) ->
            # Build row type
            (row_type, state1) = build_row_type(properties, state)
            (TObject row_type, state1)

        # Member access
        MemberExpression({ object, property }) ->
            # Generate constraint that object has property
            (obj_type, state1) = generate_constraints(object, state)
            (prop_type, state2) = fresh_var(state1)

            prop_name = get_property_name(property)
            constraint = HasField (get_row_type(obj_type)) prop_name prop_type

            (prop_type, add_constraint(constraint, state2))

        # Function call
        CallExpression({ callee, arguments }) ->
            (fun_type, state1) = generate_constraints(callee, state)
            (arg_types, state2) = generate_constraints_many(arguments, state1)
            (result_type, state3) = fresh_var(state2)

            # Add constraint that fun_type is a function
            constraint = TypeEq fun_type (TFunction arg_types result_type)

            (result_type, add_constraint(constraint, state3))

        _ -> fresh_var(state)  # Fallback

# ============================================
# CONSTRAINT SOLVING
# ============================================

# Solve all constraints
solve_all_constraints : List Constraint, InferState -> Result InferState TypeError
solve_all_constraints = \constraints, state ->
    List.walk(constraints, Ok(state), \acc_result, constraint ->
        when acc_result is
            Ok(s) -> solve_constraint(constraint, s)
            Err(e) -> Err(e)
    )

# Solve a single constraint
solve_constraint : Constraint, InferState -> Result InferState TypeError
solve_constraint = \constraint, state ->
    when constraint is
        TypeEq t1 t2 ->
            unify_types(t1, t2, state)

        RowEq r1 r2 ->
            unify_rows(r1, r2, state)

        HasField row field_name field_type ->
            # Ensure row has field with given type
            unify_rows(row, RExtend field_name field_type (fresh_row_var(state)), state)

        SubType t1 t2 ->
            # For now, treat as equality
            # Full implementation would handle subtyping
            unify_types(t1, t2, state)

        Instance t scheme ->
            inst_type = instantiate(scheme, state)
            unify_types(t, inst_type, state)

# ============================================
# ROW UNIFICATION
# ============================================

# Unify two row types
unify_rows : RowType, RowType, InferState -> Result InferState TypeError
unify_rows = \r1, r2, state ->
    when (r1, r2) is
        (REmpty, REmpty) -> Ok(state)

        (RVar v1, RVar v2) ->
            if v1 == v2 then
                Ok(state)
            else
                Err(RowMismatch)

        (RVar v, row) | (row, RVar v) ->
            # Check occurs check
            if occurs_in_row(v, row) then
                Err(InfiniteType)
            else
                Ok(add_row_subst(v, row, state))

        (RExtend k1 t1 rest1, RExtend k2 t2 rest2) ->
            if k1 == k2 then
                # Same field, unify types and rest
                when unify_types(t1, t2, state) is
                    Ok(s1) -> unify_rows(rest1, rest2, s1)
                    Err(e) -> Err(e)
            else
                # Different fields, need to commute
                # This is where row polymorphism shines
                handle_row_commutation(k1, t1, rest1, k2, t2, rest2, state)

        _ -> Err(RowMismatch)

# ============================================
# GENERALIZATION & INSTANTIATION
# ============================================

# Generalize a type to a type scheme
generalize : Type, U32 -> TypeScheme
generalize = \type, level ->
    # Find all free type variables above the level
    free_vars = find_free_vars_above_level(type, level)
    free_row_vars = find_free_row_vars_above_level(type, level)

    {
        quantified: free_vars,
        row_vars: free_row_vars,
        type: type,
    }

# Instantiate a type scheme
instantiate : TypeScheme, InferState -> (Type, InferState)
instantiate = \scheme, state ->
    # Create fresh variables for all quantified vars
    (fresh_vars, state1) = fresh_vars(List.len(scheme.quantified), state)
    (fresh_rows, state2) = fresh_row_vars(List.len(scheme.row_vars), state1)

    # Build substitution
    var_subst = List.map2(scheme.quantified, fresh_vars, \old, new -> { var: old, type: TVar new })
    row_subst = List.map2(scheme.row_vars, fresh_rows, \old, new -> { var: old, row: RVar new })

    # Apply substitution
    instantiated = apply_substs(scheme.type, var_subst, row_subst)

    (instantiated, state2)

# ============================================
# MISSING PIECES FOR FULL IMPLEMENTATION
# ============================================

# These would need to be implemented:
# 1. Complete unification with occurs check
# 2. Row commutation for field reordering
# 3. Level-based generalization for efficiency
# 4. Proper TypeScript type features:
#    - Conditional types
#    - Mapped types
#    - Template literal types
#    - Type guards and narrowing
# 5. Module system and declaration files
# 6. Bidirectional type checking for better inference
# 7. Variance annotations for generics
# 8. Recursive types with μ-types
# 9. Gradual typing features (any, unknown)
# 10. Effect tracking for async/await

# Helper functions (stubs)
empty_env : {} -> TypeEnv
empty_env = \{} -> {
    bindings: [],
    level: 0,
    next_var: 0,
    next_row_var: 0,
}

fresh_var : InferState -> (Type, InferState)
fresh_var = \state ->
    v = state.env.next_var
    new_env = { state.env & next_var: v + 1 }
    (TVar v, { state & env: new_env })

TypeError : [
    UnificationError,
    InfiniteType,
    RowMismatch,
    UnboundVariable,
]

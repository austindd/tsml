module [
    solve_constraints,
    generate_constraints,
    generalize,
    instantiate,
    to_simple_type,
    apply_subst,
    initial_state,
    Constraint,
    TypeScheme,
    SolverState,
]

import Ast
import SimpleComprehensiveType as Type
import Option exposing [Option]

# Type variable
TypeVar : U64

# Type with variables for inference
InferType : [
    # Base types
    INum,
    IStr,
    IBool,
    INull,
    IUndefined,
    IAny,
    INever,
    IUnknown,

    # Type variable
    IVar TypeVar,

    # Compound types
    IObject (List { key: Str, type: InferType }),
    IArray InferType,
    IFunction (List InferType) InferType,
    IUnion (List InferType),
]

# Type scheme for let-polymorphism
TypeScheme : {
    # Quantified variables (forall)
    quantified: List TypeVar,
    # The type body
    body: InferType,
}

# Constraint types
Constraint : [
    # Type equality constraint
    TypeEq InferType InferType,

    # Instance constraint (for let-bound polymorphism)
    Instance TypeVar TypeScheme,

    # Field constraint
    HasField InferType Str InferType,
]

# Substitution mapping variables to types
Substitution : List { var: TypeVar, type: InferType }

# Solver state
SolverState : {
    # Current substitution
    subst: Substitution,

    # Fresh variable counter
    next_var: U64,

    # Type environment for let-bound variables
    env: List { name: Str, scheme: TypeScheme },

    # Current nesting level for generalization
    level: U64,
}

# Create initial solver state
initial_state : SolverState
initial_state = {
    subst: [],
    next_var: 0,
    env: [],
    level: 0,
}

# Generate fresh type variable
fresh_var : SolverState -> (TypeVar, SolverState)
fresh_var = |state|
    var = state.next_var
    new_state = { state & next_var: var + 1 }
    (var, new_state)

# Apply substitution to type
apply_subst : InferType, Substitution -> InferType
apply_subst = |type, subst|
    when type is
        IVar(v) ->
            when List.find_first(subst, |s| s.var == v) is
                Ok(s) -> apply_subst(s.type, subst)  # Follow chains
                Err(_) -> IVar(v)

        IObject(fields) ->
            IObject(List.map(fields, |f| { f & type: apply_subst(f.type, subst) }))

        IArray(elem) ->
            IArray(apply_subst(elem, subst))

        IFunction(params, ret) ->
            IFunction(List.map(params, |p| apply_subst(p, subst)), apply_subst(ret, subst))

        IUnion(types) ->
            IUnion(List.map(types, |t| apply_subst(t, subst)))

        _ -> type

# Occurs check to prevent infinite types
occurs_check : TypeVar, InferType -> Bool
occurs_check = |var, type|
    when type is
        IVar(v) -> v == var
        IObject(fields) -> List.any(fields, |f| occurs_check(var, f.type))
        IArray(elem) -> occurs_check(var, elem)
        IFunction(params, ret) ->
            List.any(params, |p| occurs_check(var, p)) || occurs_check(var, ret)
        IUnion(types) -> List.any(types, |t| occurs_check(var, t))
        _ -> Bool.false

# Unify two types
unify : InferType, InferType, Substitution -> Result Substitution [UnificationError Str]
unify = |t1_orig, t2_orig, subst|
    # Apply current substitution
    t1 = apply_subst(t1_orig, subst)
    t2 = apply_subst(t2_orig, subst)

    when (t1, t2) is
        # Same types unify trivially
        (INum, INum) -> Ok(subst)
        (IStr, IStr) -> Ok(subst)
        (IBool, IBool) -> Ok(subst)
        (INull, INull) -> Ok(subst)
        (IUndefined, IUndefined) -> Ok(subst)
        (IAny, _) -> Ok(subst)
        (_, IAny) -> Ok(subst)
        (IUnknown, _) -> Ok(subst)
        (_, IUnknown) -> Ok(subst)

        # Variable unification
        (IVar(v1), IVar(v2)) ->
            if v1 == v2 then
                Ok(subst)
            else
                Ok(List.append(subst, { var: v1, type: IVar(v2) }))

        (IVar(v), t) ->
            if occurs_check(v, t) then
                Err(UnificationError("Infinite type"))
            else
                Ok(List.append(subst, { var: v, type: t }))

        (t, IVar(v)) ->
            if occurs_check(v, t) then
                Err(UnificationError("Infinite type"))
            else
                Ok(List.append(subst, { var: v, type: t }))

        # Array unification
        (IArray(e1), IArray(e2)) ->
            unify(e1, e2, subst)

        # Function unification
        (IFunction(p1, r1), IFunction(p2, r2)) ->
            if List.len(p1) != List.len(p2) then
                Err(UnificationError("Function arity mismatch"))
            else
                # Unify parameters
                params_result = List.walk(List.map2(p1, p2, |a, b| (a, b)), Ok(subst), |acc, (param1, param2)|
                    when acc is
                        Ok(s) -> unify(param1, param2, s)
                        Err(e) -> Err(e))

                # Then unify returns
                when params_result is
                    Ok(s) -> unify(r1, r2, s)
                    Err(e) -> Err(e)

        # Object unification (structural)
        (IObject(fields1), IObject(fields2)) ->
            unify_objects(fields1, fields2, subst)

        # Union unification (simplified)
        (IUnion(types1), IUnion(types2)) ->
            # For now, just check if they're compatible
            Ok(subst)

        _ -> Err(UnificationError("Type mismatch"))

# Unify object types structurally
unify_objects : List { key: Str, type: InferType }, List { key: Str, type: InferType }, Substitution -> Result Substitution [UnificationError Str]
unify_objects = |fields1, fields2, subst|
    # Check all fields in fields1 exist in fields2
    List.walk(fields1, Ok(subst), |acc, f1|
        when acc is
            Ok(s) ->
                when List.find_first(fields2, |f2| f2.key == f1.key) is
                    Ok(f2) -> unify(f1.type, f2.type, s)
                    Err(_) -> Err(UnificationError("Missing field $(f1.key)"))
            Err(e) -> Err(e))

# Solve a single constraint
solve_constraint : Constraint, SolverState -> Result SolverState [SolverError Str]
solve_constraint = |constraint, state|
    when constraint is
        TypeEq(t1, t2) ->
            when unify(t1, t2, state.subst) is
                Ok(new_subst) -> Ok({ state & subst: new_subst })
                Err(UnificationError(msg)) -> Err(SolverError(msg))

        Instance(var, scheme) ->
            # Instantiate the type scheme
            (inst_type, new_state) = instantiate_scheme(scheme, state)
            # Unify with the variable
            when unify(IVar(var), inst_type, new_state.subst) is
                Ok(new_subst) -> Ok({ new_state & subst: new_subst })
                Err(UnificationError(msg)) -> Err(SolverError(msg))

        HasField(obj_type, field_name, field_type) ->
            # Create object type with required field
            (fresh_fields_var, state1) = fresh_var(state)
            required_obj = IObject([{ key: field_name, type: field_type }])
            # Unify with actual object
            when unify(obj_type, required_obj, state1.subst) is
                Ok(new_subst) -> Ok({ state1 & subst: new_subst })
                Err(UnificationError(msg)) -> Err(SolverError(msg))

# Solve all constraints
solve_constraints : List Constraint, SolverState -> Result SolverState [SolverError Str]
solve_constraints = |constraints, initial|
    List.walk(constraints, Ok(initial), |acc, constraint|
        when acc is
            Ok(state) -> solve_constraint(constraint, state)
            Err(e) -> Err(e))

# Get free variables in a type
free_vars : InferType -> List TypeVar
free_vars = |type|
    when type is
        IVar(v) -> [v]
        IObject(fields) ->
            List.walk(fields, [], |acc, f| List.concat(acc, free_vars(f.type)))
        IArray(elem) -> free_vars(elem)
        IFunction(params, ret) ->
            param_vars = List.walk(params, [], |acc, p| List.concat(acc, free_vars(p)))
            List.concat(param_vars, free_vars(ret))
        IUnion(types) ->
            List.walk(types, [], |acc, t| List.concat(acc, free_vars(t)))
        _ -> []

# Generalize a type (for let-polymorphism)
generalize : InferType, SolverState -> TypeScheme
generalize = |type, state|
    # Apply current substitution
    final_type = apply_subst(type, state.subst)

    # Find free variables that can be generalized
    type_vars = free_vars(final_type)

    # Don't generalize variables that appear in the environment
    env_vars = List.walk(state.env, [], |acc, binding|
        List.concat(acc, free_vars(binding.scheme.body)))

    # Variables to quantify are those in type but not in environment
    quantifiable = List.keep_if(type_vars, |v|
        Bool.not(List.contains(env_vars, v)))

    { quantified: quantifiable, body: final_type }

# Instantiate a type scheme
instantiate_scheme : TypeScheme, SolverState -> (InferType, SolverState)
instantiate_scheme = |scheme, state|
    if List.is_empty(scheme.quantified) then
        (scheme.body, state)
    else
        # Create fresh variables for each quantified variable
        (fresh_vars, new_state) = List.walk(scheme.quantified, ([], state), |(vars, s), _|
            (new_var, s1) = fresh_var(s)
            (List.append(vars, new_var), s1))

        # Build substitution from old to new variables
        var_subst = List.map2(scheme.quantified, fresh_vars, |old, new|
            { var: old, type: IVar(new) })

        # Apply substitution to body
        inst_type = apply_subst(scheme.body, var_subst)
        (inst_type, new_state)

# Instantiate a type scheme (simpler version for external use)
instantiate : TypeScheme -> InferType
instantiate = |scheme|
    (inst_type, _) = instantiate_scheme(scheme, initial_state)
    inst_type

# Generate constraints from AST (simplified)
generate_constraints : Ast.Node, SolverState -> (InferType, List Constraint, SolverState)
generate_constraints = |node, state|
    when node is
        NumberLiteral(_) -> (INum, [], state)
        StringLiteral(_) -> (IStr, [], state)
        BooleanLiteral(_) -> (IBool, [], state)
        NullLiteral(_) -> (INull, [], state)
        UndefinedLiteral(_) -> (IUndefined, [], state)

        Identifier({ name }) ->
            # Look up in environment
            when List.find_first(state.env, |b| b.name == name) is
                Ok(binding) ->
                    # Instantiate the type scheme
                    (inst_type, new_state) = instantiate_scheme(binding.scheme, state)
                    (inst_type, [], new_state)
                Err(_) ->
                    # Unknown variable - create fresh type var
                    (var, new_state) = fresh_var(state)
                    (IVar(var), [], new_state)

        BinaryExpression({ left, operator, right }) ->
            # Generate constraints for operands
            (left_type, left_constraints, state1) = generate_constraints(left, state)
            (right_type, right_constraints, state2) = generate_constraints(right, state1)

            # Determine result type based on operator
            when operator is
                Plus ->
                    # Could be string concatenation or addition
                    (result_var, state3) = fresh_var(state2)
                    constraints = List.concat(left_constraints, right_constraints)
                    (IVar(result_var), constraints, state3)

                Minus | Star | Slash | Percent ->
                    # Numeric operators
                    constraints = List.concat(left_constraints,
                        List.concat(right_constraints, [
                            TypeEq(left_type, INum),
                            TypeEq(right_type, INum),
                        ]))
                    (INum, constraints, state2)

                LessThan | LessThanEqual | GreaterThan | GreaterThanEqual |
                EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
                    # Comparison operators
                    constraints = List.concat(left_constraints, right_constraints)
                    (IBool, constraints, state2)

                _ ->
                    (IUnknown, List.concat(left_constraints, right_constraints), state2)

        ArrayExpression({ elements }) ->
            # Generate constraints for elements
            (elem_var, state1) = fresh_var(state)

            # # Process only non-null elements
            # non_null_elems = List.keep_if(elements, |elem_opt|
            #     when elem_opt is
            #         Some(_) -> Bool.true
            #         None -> Bool.false
            # ) |> List.map(|elem_opt|
            #     when elem_opt is
            #         Some(elem) -> elem
            #         None -> Identifier({ name: "" })
            # )

            (elem_constraints, state2) = List.walk(elements, ([], state1), |(constraints, s), elem|
                (elem_type, elem_cs, s1) = generate_constraints(elem, s)
                # Each element should unify with the element type
                new_constraints = List.append(elem_cs, TypeEq(elem_type, IVar(elem_var)))
                (List.concat(constraints, new_constraints), s1))

            (IArray(IVar(elem_var)), elem_constraints, state2)

        _ ->
            # Default case for unhandled nodes
            (IUnknown, [], state)

# Convert InferType to SimpleComprehensiveType
to_simple_type : InferType -> Type.Type
to_simple_type = |type|
    when type is
        INum -> Type.mk_number
        IStr -> Type.mk_string
        IBool -> Type.mk_boolean
        INull -> Type.mk_null
        IUndefined -> Type.mk_undefined
        IAny -> Type.mk_any
        INever -> Type.mk_never
        IUnknown -> Type.mk_unknown
        IVar(_) -> Type.mk_unknown  # Unresolved variable
        IObject(fields) ->
            simple_fields = List.map(fields, |f| { key: f.key, value_type: to_simple_type(f.type) })
            Type.mk_object(simple_fields)
        IArray(elem) -> Type.mk_array(to_simple_type(elem))
        IFunction(_, _) -> Type.mk_unknown  # No function type in SimpleComprehensiveType
        IUnion(types) -> Type.mk_union(List.map(types, to_simple_type))

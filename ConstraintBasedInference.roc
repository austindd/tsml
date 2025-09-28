module [
    infer_program,
    Constraint,
    TypeVariable,
    Solution,
]

import Ast
import SimpleComprehensiveType as Type
import Option exposing [Option]

# Type variable for unknowns
TypeVariable : U64

# Constraints between types
Constraint : [
    # t1 = t2
    Equality TypeVariable TypeVariable,
    # t1 is a subtype of t2
    Subtype TypeVariable Type.Type,
    # t1 has property with type t2
    HasProperty TypeVariable Str TypeVariable,
    # t1 is a function from t2 to t3
    Function TypeVariable (List TypeVariable) TypeVariable,
    # t1 is an array of t2
    Array TypeVariable TypeVariable,
    # t1 is assignable to a concrete type
    Concrete TypeVariable Type.Type,
]

# Solution mapping variables to types
Solution : List { var: TypeVariable, type: Type.Type }

# Constraint generation context
GenContext : {
    constraints: List Constraint,
    next_var: TypeVariable,
    var_map: List { node: Str, var: TypeVariable }, # Map nodes to their type variables
}

# Create a fresh type variable
fresh_var : GenContext -> (TypeVariable, GenContext)
fresh_var = |ctx|
    var = ctx.next_var
    new_ctx = { ctx & next_var: ctx.next_var + 1 }
    (var, new_ctx)

# Generate constraints for a program
infer_program : Ast.Node -> (List Constraint, Solution)
infer_program = |node|
    initial_ctx = {
        constraints: [],
        next_var: 0,
        var_map: [],
    }

    # Generate constraints
    (_, final_ctx) = gen_constraints(node, initial_ctx)

    # Solve constraints
    solution = solve_constraints(final_ctx.constraints)

    (final_ctx.constraints, solution)

# Generate constraints for an AST node
gen_constraints : Ast.Node, GenContext -> (TypeVariable, GenContext)
gen_constraints = |node, ctx|
    when node is
        Program({ body, sourceType }) ->
            # Process all statements
            List.walk(body, (0, ctx), |acc, stmt|
                (_, acc_ctx) = acc
                gen_constraints(stmt, acc_ctx))

        NumberLiteral({ value }) ->
            (var, new_ctx) = fresh_var(ctx)
            constraint = Concrete var Type.mk_number
            (var, { new_ctx & constraints: List.append(new_ctx.constraints, constraint) })

        StringLiteral({ value }) ->
            (var, new_ctx) = fresh_var(ctx)
            constraint = Concrete var Type.mk_string
            (var, { new_ctx & constraints: List.append(new_ctx.constraints, constraint) })

        BooleanLiteral({ value }) ->
            (var, new_ctx) = fresh_var(ctx)
            constraint = Concrete var Type.mk_boolean
            (var, { new_ctx & constraints: List.append(new_ctx.constraints, constraint) })

        BinaryExpression({ left, right, operator }) ->
            # Generate constraints for operands
            (left_var, ctx1) = gen_constraints(left, ctx)
            (right_var, ctx2) = gen_constraints(right, ctx1)
            (result_var, ctx3) = fresh_var(ctx2)

            # Add operator-specific constraints
            new_constraints = when operator is
                Plus ->
                    # Result is string if either operand could be string
                    # Otherwise result is number
                    []
                Minus | Star | Slash | Percent ->
                    # Operands must be numbers
                    [
                        Concrete left_var Type.mk_number,
                        Concrete right_var Type.mk_number,
                        Concrete result_var Type.mk_number
                    ]
                LessThan | LessThanEqual | GreaterThan | GreaterThanEqual |
                EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
                    # Result is always boolean
                    [Concrete result_var Type.mk_boolean]
                _ ->
                    []

            final_ctx = { ctx3 &
                constraints: List.concat(ctx3.constraints, new_constraints)
            }
            (result_var, final_ctx)

        Identifier({ name }) ->
            # Look up or create type variable for identifier
            when List.find_first(ctx.var_map, |entry| entry.node == name) is
                Ok(entry) ->
                    (entry.var, ctx)
                Err(_) ->
                    (var, new_ctx) = fresh_var(ctx)
                    entry = { node: name, var: var }
                    (var, { new_ctx & var_map: List.append(new_ctx.var_map, entry) })

        VariableDeclarator({ id, init }) ->
            # Get variable name
            var_name = when id is
                Identifier({ name }) -> name
                _ -> "_unknown"

            # Generate constraints for initializer
            when init is
                Some(init_expr) ->
                    (init_var, ctx1) = gen_constraints(init_expr, ctx)

                    # Create or find variable for the identifier
                    (id_var, ctx2) = when List.find_first(ctx1.var_map, |e| e.node == var_name) is
                        Ok(entry) ->
                            (entry.var, ctx1)
                        Err(_) ->
                            (new_var, new_ctx) = fresh_var(ctx1)
                            entry = { node: var_name, var: new_var }
                            (new_var, { new_ctx & var_map: List.append(new_ctx.var_map, entry) })

                    # Add equality constraint
                    constraint = Equality id_var init_var
                    (id_var, { ctx2 & constraints: List.append(ctx2.constraints, constraint) })
                None ->
                    (var, new_ctx) = fresh_var(ctx)
                    constraint = Concrete var Type.mk_undefined
                    (var, { new_ctx & constraints: List.append(new_ctx.constraints, constraint) })

        ArrayExpression({ elements }) ->
            (arr_var, ctx1) = fresh_var(ctx)
            (elem_var, ctx2) = fresh_var(ctx1)

            # For now, just create array constraint without processing elements
            array_constraint = Array arr_var elem_var
            (arr_var, { ctx2 & constraints: List.append(ctx2.constraints, array_constraint) })

        _ ->
            # Default case: fresh variable with no constraints
            fresh_var(ctx)

# Solve constraints using unification
solve_constraints : List Constraint -> Solution
solve_constraints = |constraints|
    # For now, return a simple solution
    # A full implementation would use unification algorithm
    []

# Apply a substitution to a type variable
substitute : TypeVariable, Solution -> Type.Type
substitute = |var, solution|
    when List.find_first(solution, |s| s.var == var) is
        Ok(s) -> s.type
        Err(_) -> Type.mk_unknown

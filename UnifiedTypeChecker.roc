module [
    check_program,
    infer_type,
    TypeCheckResult,
    TypeContext,
    TypeError,
]

import Ast
import Option exposing [Option]
import ComprehensiveTypeIndexed as T
import PolymorphicTypes as Poly
import ConstraintSolver as CS
import ControlFlowNarrowing as CFN
import ControlFlowNarrowing exposing [TypeGuard]

# Result of type checking
TypeCheckResult : {
    type : T.TypeId,
    store : T.TypeStore,
    errors : List TypeError,
    warnings : List Str,
}

# Type checking context
TypeContext : {
    store : T.TypeStore,
    env : List { name : Str, type : T.TypeId, mutable : Bool },
    constraints : List CS.Constraint,
    return_type : Result T.TypeId [NoReturn],
    in_loop : Bool,
    in_function : Bool,
    type_params : List Poly.TypeParameter,
    fresh_counter : U32,
}

# Type errors
TypeError : [
    TypeMismatch { expected : T.TypeId, actual : T.TypeId, location : Str },
    UnboundVariable { name : Str, location : Str },
    InvalidOperation { operation : Str, types : List T.TypeId, location : Str },
    ConstraintViolation { constraint : CS.Constraint, location : Str },
    RecursiveType { type : T.TypeId, location : Str },
    Other Str,
]

# Main entry point - check an entire program
check_program : Ast.Node -> TypeCheckResult
check_program = \ast ->
    # Initialize context
    store0 = T.empty_store
    (store1, unknown) = T.make_unknown(store0)

    initial_ctx = {
        store: store1,
        env: [],
        constraints: [],
        return_type: Err(NoReturn),
        in_loop: Bool.false,
        in_function: Bool.false,
        type_params: [],
        fresh_counter: 1,
    }

    # Type check the program
    inference_result = infer_node(initial_ctx, ast)
    when inference_result is
        Ok((final_ctx, type_id)) ->
            constraint_set = {
                store: final_ctx.store,
                type_vars: List.map(final_ctx.type_params, |tp| tp.id),
                constraints: final_ctx.constraints,
            }
            solved_constraints = CS.solve_constraints(constraint_set)
            when solved_constraints is
                Ok(solution) ->
                    # Apply substitutions from constraint solving
                    subs = solution.substitutions |> List.map(|s| { var: s.var, solution: s.type_id })
                    (final_store, final_type) = apply_solution(solution.store, type_id, subs)

                    {
                        type: final_type,
                        store: final_store,
                        errors: [],
                        warnings: [],
                    }

                Err(error) ->
                    {
                        type: unknown,
                        store: final_ctx.store,
                        errors: [Other("Constraint solving failed: ${error}")],
                        warnings: [],
                    }

        Err(error) ->
            {
                type: unknown,
                store: initial_ctx.store,
                errors: [error],
                warnings: [],
            }

# Infer type for a node
infer_type : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_type = \ctx, node ->
    infer_node(ctx, node)

# Internal: infer type for any AST node
infer_node : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_node = \ctx, node ->
    when node is
        # Programs and statements
        Program(details) ->
            infer_statement_list(ctx, details.value.body)

        BlockStatement(details) ->
            infer_statement_list(ctx, details.value.body)

        # Declarations
        VariableDeclaration(details) ->
            infer_variable_declarations(ctx, details.value.declarations, details.value.kind)

        FunctionDeclaration(details) ->
            infer_function_declaration(ctx, details.value)

        # Expressions
        Identifier(details) ->
            lookup_variable(ctx, details.value.name)

        Literal(details) ->
            infer_literal(ctx, details.value)

        BinaryExpression(details) ->
            infer_binary_expression(ctx, details.value)

        UnaryExpression(details) ->
            infer_unary_expression(ctx, details.value)

        AssignmentExpression(details) ->
            infer_assignment(ctx, details.value)

        CallExpression(details) ->
            infer_call_expression(ctx, details.value)

        MemberExpression(details) ->
            infer_member_expression(ctx, details.value)

        ArrayExpression(details) ->
            infer_array_expression(ctx, details.value.elements)

        ObjectExpression(details) ->
            infer_object_expression(ctx, details.value.properties)

        ArrowFunctionExpression(details) ->
            infer_arrow_function(ctx, details.value)

        ConditionalExpression(details) ->
            infer_conditional(ctx, details.value)

        # Control flow
        IfStatement(details) ->
            infer_if_statement(ctx, details.value)

        WhileStatement(details) ->
            infer_while_statement(ctx, details.value)

        ForStatement(details) ->
            infer_for_statement(ctx, details.value)

        ReturnStatement(details) ->
            infer_return_statement(ctx, details.value.argument)

        # Default
        _ ->
            # Return unknown for unhandled nodes
            (store, unknown) = T.make_unknown(ctx.store)
            Ok(({ ctx & store: store }, unknown))

# Infer types for a list of statements
infer_statement_list : TypeContext, List Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_statement_list = \ctx, statements ->
    List.walk_until(statements, Ok((ctx, TypeId(0))), \result, stmt ->
        when result is
            Ok((current_ctx, _)) ->
                when infer_node(current_ctx, stmt) is
                    Ok((new_ctx, type_id)) -> Continue(Ok((new_ctx, type_id)))
                    Err(error) -> Break(Err(error))
            Err(error) -> Break(Err(error))
    )

# Infer types for variable declarations (List VariableDeclarator, Ast.VariableDeclarationKind)
infer_variable_declarations : TypeContext, List Ast.Node, Ast.VariableDeclarationKind -> Result (TypeContext, T.TypeId) TypeError
infer_variable_declarations = \ctx, declarators, kind ->
    is_mutable = when kind is
        Let -> Bool.false
        Const -> Bool.false
        Var -> Bool.true

    List.walk_until(declarators, Ok((ctx, TypeId(0))), \result, decl ->
        when result is
            Ok((current_ctx, _)) ->
                when infer_variable_declarator(current_ctx, decl, is_mutable) is
                    Ok((new_ctx, type_id)) -> Continue(Ok((new_ctx, type_id)))
                    Err(error) -> Break(Err(error))
            Err(error) -> Break(Err(error))
    )

# Infer type for a single variable declarator (VariableDeclarator)
infer_variable_declarator : TypeContext, Ast.Node, Bool -> Result (TypeContext, T.TypeId) TypeError
infer_variable_declarator = \ctx, declarator, is_mutable ->
    when declarator is
        VariableDeclarator(decl) ->
            when decl.id is
                Identifier({ name }) ->
                    # Infer type from initializer or use unknown
                    when decl.init is
                        Some(init_expr) ->
                            when infer_node(ctx, init_expr) is
                                Ok((ctx1, init_type)) ->
                                    # For non-mutable bindings, generalize the type
                                    final_type = if is_mutable then
                                        init_type
                                    else
                                        # Generalize for let-polymorphism
                                        env_types = List.map(ctx1.env, \binding -> binding.type)
                                        (store2, scheme) = Poly.generalize(ctx1.store, init_type, env_types)
                                        # For now, keep the type as-is (full polymorphism needs more work)
                                        init_type

                                    # Add to environment
                                    new_binding = { name: name, type: final_type, mutable: is_mutable }
                                    new_env = List.append(ctx1.env, new_binding)
                                    Ok(({ ctx1 & env: new_env }, final_type))

                                Err(error) -> Err(error)

                        None ->
                            # No initializer - use unknown type
                            (store1, unknown) = T.make_unknown(ctx.store)
                            new_binding = { name: name, type: unknown, mutable: is_mutable }
                            new_env = List.append(ctx.env, new_binding)
                            Ok(({ ctx & store: store1, env: new_env }, unknown))

                _ ->
                    # Complex patterns not supported yet
                    (store1, unknown) = T.make_unknown(ctx.store)
                    Ok(({ ctx & store: store1 }, unknown))

        _ -> Err(TypeError("Invalid variable declaration"))

# Infer type for function declaration (FunctionDeclaration)
infer_function_declaration : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_function_declaration = \ctx, func_decl_node ->
    when func_decl_node is
        FunctionDeclaration(func) ->
            # Create type parameters if any
            (ctx1, type_params) = when func.type_parameters is
                Some(params) -> create_type_parameters(ctx, params)
                None -> (ctx, [])

            # Create parameter types
            (ctx2, param_types) = create_parameter_types(ctx1, func.params)

            # Set up function context
            func_ctx = { ctx2 &
                return_type: Ok(TypeId(0)),  # Will be inferred
                in_function: Bool.true,
                type_params: type_params,
            }

            # Infer body type
            when infer_node(func_ctx, func.body) is
                Ok((body_ctx, _)) ->
                    # Get the actual return type
                    return_type = when body_ctx.return_type is
                        Ok(ret_type) -> ret_type
                        Err(_) ->
                            # No return statement - void/undefined
                            (s, undef) = T.make_primitive(body_ctx.store, "undefined")
                            undef

                    # Create function type
                    (store3, func_type) = T.make_function(
                        body_ctx.store,
                        param_types,
                        return_type,
                        [],  # Type params handled separately
                        func.async,
                        func.generator,
                    )

                    # Add function to environment if it has a name
                    final_ctx = when func.id is
                        Some(Identifier({ name })) ->
                            new_binding = { name: name, type: func_type, mutable: Bool.false }
                            { body_ctx & env: List.append(body_ctx.env, new_binding), store: store3 }
                        _ ->
                            { body_ctx & store: store3 }

                    Ok((final_ctx, func_type))

                Err(error) -> Err(error)
        _ -> Err(TypeError("Invalid function declaration"))

# Infer type for arrow function (ArrowFunctionExpression)
infer_arrow_function : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_arrow_function = \ctx, arrow_func_expr_node ->
    when arrow_func_expr_node is
        ArrowFunctionExpression(func) ->
            # Similar to function declaration but simpler
            # Create parameter types
            (ctx1, param_types) = create_parameter_types(ctx, func.params)

            # Set up function context
            func_ctx = { ctx1 &
                return_type: Ok(TypeId(0)),
                in_function: Bool.true,
            }

            # Infer body type
            when infer_node(func_ctx, func.body) is
                Ok((body_ctx, body_type)) ->
                    # For expression bodies, the body type is the return type
                    return_type = when func.body is
                        BlockStatement(_) ->
                            when body_ctx.return_type is
                                Ok(ret) -> ret
                                Err(_) -> body_type
                        _ -> body_type

                    # Create function type
                    (store2, func_type) = T.make_function(
                        body_ctx.store,
                        param_types,
                        return_type,
                        [],
                        func.async,
                        Bool.false,  # Arrow functions can't be generators
                    )

                    Ok(({ body_ctx & store: store2 }, func_type))

                Err(error) -> Err(error)
        _ -> Err(TypeError("Invalid arrow function"))

# Infer type for literals (<LiteralNodes>)
infer_literal : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_literal = \ctx, lit_node ->
    when lit_node is
        StringLiteral(str) ->
            (store1, str_lit) = T.make_literal(ctx.store, StrLit(str))
            Ok(({ ctx & store: store1 }, str_lit))

        NumericLiteral(num) ->
            (store1, num_lit) = T.make_literal(ctx.store, NumLit(num))
            Ok(({ ctx & store: store1 }, num_lit))

        BooleanLiteral(bool) ->
            (store1, bool_lit) = T.make_literal(ctx.store, BoolLit(bool))
            Ok(({ ctx & store: store1 }, bool_lit))

        NullLiteral ->
            (store1, null_type) = T.make_primitive(ctx.store, "null")
            Ok(({ ctx & store: store1 }, null_type))

        _ ->
            (store1, unknown) = T.make_unknown(ctx.store)
            Ok(({ ctx & store: store1 }, unknown))

# Infer type for binary expressions (BinaryExpression)
infer_binary_expression : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_binary_expression = \ctx, binary_expr_node ->
    when binary_expr_node is
        BinaryExpression(expr) ->
            # Infer left and right types
            when infer_node(ctx, expr.left) is
                Ok((ctx1, left_type)) ->
                    when infer_node(ctx1, expr.right) is
                        Ok((ctx2, right_type)) ->
                            # Determine result type based on operator
                            when expr.operator is
                                "+" | "-" | "*" | "/" | "%" ->
                                    # Numeric operations
                                    (store3, number) = T.make_primitive(ctx2.store, "number")
                                    # Add constraints that operands should be numbers
                                    constraint1 = Subtype({ sub: left_type, super: number, source: BinaryOp })
                                    constraint2 = Subtype({ sub: right_type, super: number, source: BinaryOp })
                                    new_constraints = List.concat(ctx2.constraints, [constraint1, constraint2])
                                    Ok(({ ctx2 & store: store3, constraints: new_constraints }, number))

                                "===" | "!==" | "==" | "!=" ->
                                    # Equality - returns boolean
                                    (store3, boolean) = T.make_primitive(ctx2.store, "boolean")
                                    Ok(({ ctx2 & store: store3 }, boolean))

                                "<" | ">" | "<=" | ">=" ->
                                    # Comparison - returns boolean
                                    (store3, boolean) = T.make_primitive(ctx2.store, "boolean")
                                    Ok(({ ctx2 & store: store3 }, boolean))

                                "&&" | "||" ->
                                    # Logical operations - return union of operand types
                                    (store3, union) = T.make_union(ctx2.store, [left_type, right_type])
                                    Ok(({ ctx2 & store: store3 }, union))

                                _ ->
                                    # Unknown operator
                                    (store3, unknown) = T.make_unknown(ctx2.store)
                                    Ok(({ ctx2 & store: store3 }, unknown))

                        Err(error) -> Err(error)

                Err(error) -> Err(error)
        _ -> Err(TypeError("Invalid binary expression"))

# Infer type for unary expressions (UnaryExpression)
infer_unary_expression : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_unary_expression = \ctx, unary_expr_node ->
    when unary_expr_node is
        UnaryExpression(expr) ->
            when infer_node(ctx, expr.argument) is
                Ok((ctx1, arg_type)) ->
                    when expr.operator is
                        "!" ->
                            # Logical not - returns boolean
                            (store2, boolean) = T.make_primitive(ctx1.store, "boolean")
                            Ok(({ ctx1 & store: store2 }, boolean))

                        "-" | "+" ->
                            # Numeric operators - return number
                            (store2, number) = T.make_primitive(ctx1.store, "number")
                            # Add constraint that operand should be numeric
                            constraint = Subtype({ sub: arg_type, super: number, source: UnaryOp })
                            new_constraints = List.append(ctx1.constraints, constraint)
                            Ok(({ ctx1 & store: store2, constraints: new_constraints }, number))

                        "typeof" ->
                            # typeof - returns string
                            (store2, string) = T.make_primitive(ctx1.store, "string")
                            Ok(({ ctx1 & store: store2 }, string))

                        _ ->
                            # Unknown operator
                            (store2, unknown) = T.make_unknown(ctx1.store)
                            Ok(({ ctx1 & store: store2 }, unknown))

                Err(error) -> Err(error)
        _ -> Err(TypeError("Invalid unary expression"))

# Infer type for call expressions (CallExpression)
infer_call_expression : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_call_expression = \ctx, call_expr_node ->
    when call_expr_node is
        CallExpression(expr) ->
            # Infer callee type
            when infer_node(ctx, expr.callee) is
                Ok((ctx1, callee_type)) ->
                    # Infer argument types
                    when infer_argument_types(ctx1, expr.arguments) is
                        Ok((ctx2, arg_types)) ->
                            # Check if callee is a function
                            when T.get_type(ctx2.store, callee_type) is
                                Ok(TFunction(func)) ->
                                    # Check argument count and types
                                    # For now, just return the return type
                                    Ok((ctx2, func.ret))

                                _ ->
                                    # Not a function - could be unknown or polymorphic
                                    # Create a fresh type variable for the result
                                    (store3, result_var) = T.make_type_var(ctx2.store, ctx2.fresh_counter)

                                    # Create expected function type
                                    param_types = List.map(arg_types, \arg_type ->
                                        { name: "", param_type: arg_type, optional: Bool.false }
                                    )
                                    (store4, expected_func) = T.make_function(
                                        store3, param_types, result_var, [], Bool.false, Bool.false
                                    )

                                    # Add constraint that callee should be a function
                                    constraint = Subtype({
                                        sub: callee_type,
                                        super: expected_func,
                                        source: FunctionCall
                                    })
                                    new_constraints = List.append(ctx2.constraints, constraint)

                                    Ok(({ ctx2 &
                                        store: store4,
                                        constraints: new_constraints,
                                        fresh_counter: ctx2.fresh_counter + 1,
                                    }, result_var))

                        Err(error) -> Err(error)

                Err(error) -> Err(error)
        _ -> Err(TypeError("Invalid call expression"))

# Infer types for arguments
infer_argument_types : TypeContext, List Ast.Node -> Result (TypeContext, List T.TypeId) TypeError
infer_argument_types = \ctx, arguments ->
    List.walk_until(arguments, Ok((ctx, [])), \result, arg ->
        when result is
            Ok((current_ctx, types)) ->
                when infer_node(current_ctx, arg) is
                    Ok((new_ctx, arg_type)) ->
                        Continue(Ok((new_ctx, List.append(types, arg_type))))
                    Err(error) -> Break(Err(error))
            Err(error) -> Break(Err(error))
    )

# Infer type for member expressions (MemberExpression)
infer_member_expression : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_member_expression = \ctx, member_expr_node ->
    when member_expr_node is
        MemberExpression(expr) ->
            # Infer object type
            when infer_node(ctx, expr.object) is
                Ok((ctx1, obj_type)) ->
                    # Get property name
                    prop_name = when expr.property is
                        Identifier({ name }) -> name
                        _ -> "unknown"

                    # Create a type variable for the member type
                    (store2, member_var) = T.make_type_var(ctx1.store, ctx1.fresh_counter)

                    # Add constraint that object has this member
                    constraint = HasMember({
                        object: obj_type,
                        member: prop_name,
                        member_type: member_var,
                        source: MemberAccess,
                    })
                    new_constraints = List.append(ctx1.constraints, constraint)

                    Ok(({ ctx1 &
                        store: store2,
                        constraints: new_constraints,
                        fresh_counter: ctx1.fresh_counter + 1,
                    }, member_var))

                Err(error) -> Err(error)
        _ -> Err(TypeError("Invalid member expression"))

# Infer type for if statements with control flow narrowing (IfStatement)
infer_if_statement : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_if_statement = \ctx, if_stmt ->
    when if_stmt is
        IfStatement(stmt) ->
            # Infer condition type
            when infer_node(ctx, stmt.test) is
                Ok((ctx1, cond_type)) ->
                    # Extract type guards from condition
                    guards = extract_type_guards(stmt.test)

                    # Apply guards for then branch
                    then_ctx = apply_type_guards(ctx1, guards, Bool.true)

                    # Type check then branch
                    when infer_node(then_ctx, stmt.consequent) is
                        Ok((then_result_ctx, then_type)) ->
                            # Type check else branch if present
                            when stmt.alternate is
                                Some(else_stmt) ->
                                    # Apply negated guards for else branch
                                    else_ctx = apply_type_guards(ctx1, guards, Bool.false)

                                    when infer_node(else_ctx, else_stmt) is
                                        Ok((else_result_ctx, else_type)) ->
                                            # Merge contexts after branches
                                            merged_ctx = merge_contexts(ctx1, then_result_ctx, else_result_ctx)

                                            # Result type is union of branch types
                                            (store2, union) = T.make_union(merged_ctx.store, [then_type, else_type])
                                            Ok(({ merged_ctx & store: store2 }, union))

                                        Err(error) -> Err(error)

                                None ->
                                    # No else branch
                                    (store2, void_type) = T.make_primitive(then_result_ctx.store, "undefined")
                                    Ok(({ then_result_ctx & store: store2 }, void_type))

                        Err(error) -> Err(error)

                Err(error) -> Err(error)
        _ -> Err(TypeError("Invalid if statement"))

# Helper functions

# Look up a variable in the environment
lookup_variable : TypeContext, Str -> Result (TypeContext, T.TypeId) TypeError
lookup_variable = \ctx, name ->
    when List.find_first(ctx.env, \binding -> binding.name == name) is
        Ok(binding) -> Ok((ctx, binding.type))
        Err(_) -> Err(UnboundVariable({ name: name, location: "unknown" }))

# Create type parameters (List TSTypeParameter)
create_type_parameters : TypeContext, List Ast.Node -> (TypeContext, List Poly.TypeParameter)
create_type_parameters = |ctx, type_params|
    # For now, just return empty list - full implementation needs more work
    (ctx, [])

# Create parameter types from function parameters (List [Identifier, ObjectPattern, ArrayPattern, etc.])
create_parameter_types : TypeContext, List Ast.Node -> (TypeContext, List { name: Str, param_type: T.TypeId, optional: Bool })
create_parameter_types = |ctx, params|
    List.walk(params, (ctx, []), |(current_ctx, types), param|
        when param is
            Identifier({ name }) ->
                # Create a fresh type variable for the parameter
                (store1, param_var) = T.make_type_var(current_ctx.store, current_ctx.fresh_counter)

                # Add to environment
                new_binding = { name: name, type: param_var, mutable: Bool.false }
                new_env = List.append(current_ctx.env, new_binding)

                # Add to parameter types
                param_type = { name: name, param_type: param_var, optional: Bool.false }
                new_types = List.append(types, param_type)

                ({ current_ctx &
                    store: store1,
                    env: new_env,
                    fresh_counter: current_ctx.fresh_counter + 1,
                }, new_types)

            _ ->
                # Complex patterns not supported yet
                (current_ctx, types)
    )

# Extract type guards from a condition expression
extract_type_guards : Ast.Node -> List TypeGuard
extract_type_guards = \node ->
    when node is
        BinaryExpression({ operator, left, right }) ->
            when operator is
                "===" | "==" ->
                    # Equality check
                    when left is
                        UnaryExpression({ operator: "typeof", argument }) ->
                            when argument is
                                Identifier({ name }) ->
                                    when right is
                                        Literal(StringLiteral(type_name)) ->
                                            [TypeofGuard({ var_name: name, type_name: type_name, negated: Bool.false })]
                                        _ -> []
                                _ -> []
                        _ -> []

                "!==" | "!=" ->
                    # Inequality check
                    when left is
                        UnaryExpression({ operator: "typeof", argument }) ->
                            when argument is
                                Identifier({ name }) ->
                                    when right is
                                        Literal(StringLiteral(type_name)) ->
                                            [TypeofGuard({ var_name: name, type_name: type_name, negated: Bool.true })]
                                        _ -> []
                                _ -> []
                        _ -> []

                _ -> []

        UnaryExpression({ operator: "!", argument }) ->
            # Negated truthiness
            when argument is
                Identifier({ name }) ->
                    [TruthinessGuard({ var_name: name, negated: Bool.true })]
                _ -> []

        Identifier({ name }) ->
            # Direct truthiness check
            [TruthinessGuard({ var_name: name, negated: Bool.false })]

        _ -> []

# Apply type guards to narrow types in environment
apply_type_guards : TypeContext, List TypeGuard, Bool -> TypeContext
apply_type_guards = \ctx, guards, is_then_branch ->
    List.walk(guards, ctx, \current_ctx, guard ->
        # Apply guard to each variable in environment
        new_env = List.map(current_ctx.env, \binding ->
            actual_guard = if is_then_branch then guard else negate_guard(guard)
            result = CFN.narrow_type(current_ctx.store, binding.type, actual_guard, binding.name)
            { binding & type: result.refined_type }
        )
        { current_ctx & env: new_env }
    )

# Negate a type guard
negate_guard : TypeGuard -> TypeGuard
negate_guard = \guard ->
    when guard is
        TypeofGuard(data) -> TypeofGuard({ data & negated: Bool.not(data.negated) })
        TruthinessGuard(data) -> TruthinessGuard({ data & negated: Bool.not(data.negated) })
        EqualityGuard(data) -> EqualityGuard({ data & negated: Bool.not(data.negated) })
        _ -> guard

# Merge contexts from different branches
merge_contexts : TypeContext, TypeContext, TypeContext -> TypeContext
merge_contexts = \original, then_ctx, else_ctx ->
    # For now, just return the original context with accumulated constraints
    all_constraints = List.concat(
        original.constraints,
        List.concat(then_ctx.constraints, else_ctx.constraints)
    )
    { original & constraints: all_constraints }

# Apply solution from constraint solver
apply_solution : T.TypeStore, T.TypeId, List { var: T.TypeId, solution: T.TypeId } -> (T.TypeStore, T.TypeId)
apply_solution = \store, type_id, substitutions ->
    # Apply substitutions to get final type
    ctx = {
        store: store,
        substitutions: List.map(substitutions, \sub -> { param: sub.var, replacement: sub.solution }),
        fresh_counter: 1,
    }
    (final_ctx, final_type) = Poly.substitute(ctx, type_id, ctx.substitutions)
    (final_ctx.store, final_type)

# Infer types for other statement types (stubs for now)

# Infer type for a while statement (WhileStatement)
infer_while_statement : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_while_statement = \ctx, while_stmt ->
    when while_stmt is
        WhileStatement(stmt) ->
            # Type check condition
            when infer_node(ctx, stmt.test) is
                Ok((ctx1, _)) ->
                    # Type check body in loop context
                    loop_ctx = { ctx1 & in_loop: Bool.true }
                    when infer_node(loop_ctx, stmt.body) is
                        Ok((ctx2, _)) ->
                            (store3, void_type) = T.make_primitive(ctx2.store, "undefined")
                            Ok(({ ctx2 & store: store3 }, void_type))
                        Err(error) -> Err(error)
                Err(error) -> Err(error)

        _ -> Err(TypeError("Invalid while statement"))

# Infer type for a for statement (ForStatement)
infer_for_statement : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_for_statement = \ctx, stmt_node ->
    when stmt_node is
        ForStatement(stmt) ->
            # Type check init
            ctx1 = when stmt.init is
                Some(init) ->
                    when infer_node(ctx, init) is
                        Ok((new_ctx, _)) -> new_ctx
                        Err(_) -> ctx
                None -> ctx

            # Type check in loop context
            loop_ctx = { ctx1 & in_loop: Bool.true }

            # Type check test
            ctx2 = when stmt.test is
                Some(test) ->
                    when infer_node(loop_ctx, test) is
                        Ok((new_ctx, _)) -> new_ctx
                        Err(_) -> loop_ctx
                None -> loop_ctx

            # Type check body
            ctx3 = when infer_node(ctx2, stmt.body) is
                Ok((new_ctx, _)) -> new_ctx
                Err(_) -> ctx2

            # Type check update
            ctx4 = when stmt.update is
                Some(update) ->
                    when infer_node(ctx3, update) is
                        Ok((new_ctx, _)) -> new_ctx
                        Err(_) -> ctx3
                None -> ctx3

            (store5, void_type) = T.make_primitive(ctx4.store, "undefined")
            Ok(({ ctx4 & store: store5 }, void_type))
        _ -> Err(TypeError("Invalid for statement"))

infer_return_statement : TypeContext, Option Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_return_statement = \ctx, argument ->
    if Bool.not(ctx.in_function) then
        Err(Other("Return statement outside of function"))
    else
        when argument is
            Some(expr) ->
                when infer_node(ctx, expr) is
                    Ok((ctx1, ret_type)) ->
                        # Update return type in context
                        new_ctx = { ctx1 & return_type: Ok(ret_type) }
                        Ok((new_ctx, ret_type))
                    Err(error) -> Err(error)

            None ->
                # Return undefined
                (store1, undef) = T.make_primitive(ctx.store, "undefined")
                new_ctx = { ctx & store: store1, return_type: Ok(undef) }
                Ok((new_ctx, undef))

infer_assignment : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_assignment = |ctx, expr|
    # Infer right-hand side type
    when infer_node(ctx, expr.right) is
        Ok((ctx1, right_type)) ->
            # Check left-hand side
            when expr.left is
                Identifier({ name }) ->
                    # Look up variable
                    when List.find_first(ctx1.env, \binding -> binding.name == name) is
                        Ok(binding) ->
                            if binding.mutable then
                                # Add constraint that types should match
                                constraint = Equality({
                                    left: binding.type,
                                    right: right_type,
                                    source: Assignment,
                                })
                                new_constraints = List.append(ctx1.constraints, constraint)
                                Ok(({ ctx1 & constraints: new_constraints }, right_type))
                            else
                                Err(Other("Cannot assign to const/let binding: $(name)"))
                        Err(_) ->
                            Err(UnboundVariable({ name: name, location: "assignment" }))

                _ ->
                    # Complex assignment targets not supported yet
                    Ok((ctx1, right_type))

        Err(error) -> Err(error)

infer_array_expression : TypeContext, List (Option Ast.Node) -> Result (TypeContext, T.TypeId) TypeError
infer_array_expression = \ctx, elements ->
    # Infer element types
    when infer_array_elements(ctx, elements) is
        Ok((ctx1, elem_types)) ->
            # Create union of all element types
            when elem_types is
                [] ->
                    # Empty array - array of unknown
                    (store2, unknown) = T.make_unknown(ctx1.store)
                    (store3, array_type) = T.make_array(store2, unknown)
                    Ok(({ ctx1 & store: store3 }, array_type))

                [single] ->
                    # Single element type
                    (store2, array_type) = T.make_array(ctx1.store, single)
                    Ok(({ ctx1 & store: store2 }, array_type))

                multiple ->
                    # Multiple types - create union
                    (store2, union) = T.make_union(ctx1.store, multiple)
                    (store3, array_type) = T.make_array(store2, union)
                    Ok(({ ctx1 & store: store3 }, array_type))

        Err(error) -> Err(error)

infer_array_elements : TypeContext, List (Option Ast.Node) -> Result (TypeContext, List T.TypeId) TypeError
infer_array_elements = |ctx, elements|
    List.walk_until(elements, Ok((ctx, [])), \result, elem ->
        when result is
            Ok((current_ctx, types)) ->
                when elem is
                    Some(node) ->
                        when infer_node(current_ctx, node) is
                            Ok((new_ctx, elem_type)) ->
                                Continue(Ok((new_ctx, List.append(types, elem_type))))
                            Err(error) -> Break(Err(error))
                    None ->
                        # Sparse array element
                        (store1, undef) = T.make_primitive(current_ctx.store, "undefined")
                        Continue(Ok(({ current_ctx & store: store1 }, List.append(types, undef))))
            Err(error) -> Break(Err(error))
    )

infer_object_expression : TypeContext, List Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_object_expression = |ctx, property_nodes|
    # Build object type from properties
    when infer_object_properties(ctx, property_nodes) is
        Ok((ctx1, prop_types)) ->
            # Create object type with all properties
            when create_object_type(ctx1.store, prop_types) is
                Ok((store2, obj_type)) ->
                    Ok(({ ctx1 & store: store2 }, obj_type))
                Err(_) ->
                    (store2, unknown) = T.make_unknown(ctx1.store)
                    Ok(({ ctx1 & store: store2 }, unknown))

        Err(error) -> Err(error)

infer_object_properties : TypeContext, List Ast.Node -> Result (TypeContext, List { key: Str, value: T.TypeId }) TypeError
infer_object_properties = |ctx, properties|
    properties
        |> List.walk_until(Ok((ctx, [])), |result, prop_node|
            when prop_node is
                Property(prop) ->
                    when result is
                        Ok((current_ctx, props)) ->
                            when prop.key is
                                Identifier({ name }) ->
                                    when infer_node(current_ctx, prop.value) is
                                        Ok((new_ctx, value_type)) ->
                                            new_prop = { key: name, value: value_type }
                                            Continue(Ok((new_ctx, List.append(props, new_prop))))
                                        Err(error) -> Break(Err(error))

                                Literal(StringLiteral(name)) ->
                                    when infer_node(current_ctx, prop.value) is
                                        Ok((new_ctx, value_type)) ->
                                            new_prop = { key: name, value: value_type }
                                            Continue(Ok((new_ctx, List.append(props, new_prop))))
                                        Err(error) -> Break(Err(error))

                                _ ->
                                    # Complex keys not supported
                                    Continue(Ok((current_ctx, props)))

                        Err(error) -> Break(Err(error))
                _ -> Break(Err(TypeError("Invalid object property")))
        )

create_object_type : T.TypeStore, List { key: Str, value: T.TypeId } -> Result (T.TypeStore, T.TypeId) [CreateObjectError]
create_object_type = \store, props ->
    # Create empty row
    (store1, empty_row) = T.make_empty_row(store)

    # Add each property to the row
    (final_store, final_row) = List.walk(props, (store1, empty_row), \(s, row), prop ->
        T.make_row_extend(s, prop.key, prop.value, Bool.false, Bool.false, row)
    )

    # Create object with the row
    (store2, obj) = T.make_object(final_store, final_row)
    Ok((store2, obj))

infer_conditional : TypeContext, Ast.Node -> Result (TypeContext, T.TypeId) TypeError
infer_conditional = \ctx, cond_expr ->
    when cond_expr is
        ConditionalExpression({ test, consequent, alternate }) ->
            # Infer test type
            when infer_node(ctx, test) is
                Ok((ctx1, _)) ->
                    # Extract guards and apply to branches
                    guards = extract_type_guards(test)

                    # Type check consequent with guards applied
                    then_ctx = apply_type_guards(ctx1, guards, Bool.true)
                    when infer_node(then_ctx, consequent) is
                        Ok((_, then_type)) ->
                            # Type check alternate with negated guards
                            else_ctx = apply_type_guards(ctx1, guards, Bool.false)
                            when infer_node(else_ctx, alternate) is
                                Ok((ctx3, else_type)) ->
                                    # Result is union of branch types
                                    (store4, union) = T.make_union(ctx3.store, [then_type, else_type])
                                    Ok(({ ctx3 & store: store4 }, union))

                                Err(error) -> Err(error)

                        Err(error) -> Err(error)

                Err(error) -> Err(error)
        _ -> Err("Unexpected node type. Expected ConditionalExpression.")

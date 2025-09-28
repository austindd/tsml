module [
    check_program,
    TypeCheckResult,
    TypeError,
]

import Ast
import Parser
import Token
import SimpleComprehensiveType as Type
import LetPolymorphicConstraintSolver as Solver
import BidirectionalTypeChecker as Bidir
import ControlFlowAnalysis as CFA
import Option exposing [Option]

# Type checking result
TypeCheckResult : {
    # Inferred types for each node
    types: List { node_id: U64, type: Type.Type },

    # Errors found
    errors: List TypeError,

    # Warnings
    warnings: List TypeWarning,
}

# Type error
TypeError : {
    message: Str,
    location: Option { line: U64, column: U64 },
    severity: [Error, Warning, Info],
}

# Type warning
TypeWarning : {
    message: Str,
    location: Option { line: U64, column: U64 },
}

# Check an entire program
check_program : Str -> TypeCheckResult
check_program = |source|
    # Step 1: Tokenize
    tokens = Token.tokenize_str(source)

    # Step 2: Filter trivia tokens
    non_trivia = List.keep_if(tokens, |token|
        when token is
            Whitespace(_) -> Bool.false
            BlockComment(_) -> Bool.false
            LineComment(_) -> Bool.false
            _ -> Bool.true)

    # Step 3: Parse to AST
    when Parser.parse_program(non_trivia) is
        Ok(ast) ->
            # Step 4: Type check the AST
            check_ast(ast)
        Err(_) ->
            # Return parse error as type error
            {
                types: [],
                errors: [{
                    message: "Parse error",
                    location: None,
                    severity: Error,
                }],
                warnings: [],
            }

# Check an AST
check_ast : Ast.Node -> TypeCheckResult
check_ast = |ast|
    when ast is
        Program({ body, sourceType }) ->
            # Initialize solver state
            initial_solver = Solver.initial_state

            # Process each statement
            (results, final_state) = List.walk body ([], initial_solver) \(acc, state), stmt ->
                (stmt_type, stmt_constraints, new_state) = check_statement(stmt, state)

                # Solve constraints for this statement
                when Solver.solve_constraints(stmt_constraints, new_state) is
                    Ok(solved_state) ->
                        # Apply substitution to get final type
                        final_type = Solver.apply_subst(stmt_type, solved_state.subst)
                        simple_type = Solver.to_simple_type(final_type)
                        (
                            List.append(
                                acc,
                                { node_id: 0, type: simple_type }
                            ),
                            solved_state
                        )
                    Err(SolverError(msg)) ->
                        # Add error but continue
                        error = { message: msg, location: None, severity: Error }
                        (acc, state)

            # Collect results
            {
                types: results,
                errors: [],
                warnings: [],
            }

        _ ->
            # Not a program node
            {
                types: [],
                errors: [{
                    message: "Expected Program node",
                    location: None,
                    severity: Error,
                }],
                warnings: [],
            }

# Check a statement
check_statement : Ast.Node, Solver.SolverState -> (Solver.InferType, List Solver.Constraint, Solver.SolverState)
check_statement = |stmt, state|
    when stmt is
        # Variable declaration
        VariableDeclaration({ declarations, kind }) ->
            # Process each declarator
            List.walk(declarations, (Solver.IUndefined, [], state), \(_, constraints, s), decl ->
                when decl is
                    VariableDeclarator({ id, init, typeAnnotation }) ->
                        when (id, init) is
                            (Identifier({ name }), Some(init_expr)) ->
                                # Generate constraints for initializer
                                (init_type, init_constraints, s1) = Solver.generate_constraints(init_expr, s)

                                # Handle let-polymorphism for 'const'
                                when kind is
                                    Const ->
                                        # Generalize the type
                                        scheme = Solver.generalize(init_type, s1)
                                        # Add to environment
                                        new_env = List.append(s1.env, { name, scheme })
                                        s2 = { s1 & env: new_env }
                                        (init_type, List.concat(constraints, init_constraints), s2)
                                    _ ->
                                        # No generalization for let/var
                                        mono_scheme = { quantified: [], body: init_type }
                                        new_env = List.append(s1.env, { name, scheme: mono_scheme })
                                        s2 = { s1 & env: new_env }
                                        (init_type, List.concat(constraints, init_constraints), s2)
                            _ ->
                                (Solver.IUndefined, constraints, s)
                    _ ->
                        (Solver.IUndefined, constraints, s))

        # Expression statement
        Directive({ expression }) ->
            Solver.generate_constraints(expression, state)

        # Block statement
        BlockStatement({ body }) ->
            # Enter new scope
            new_state = { state & level: state.level + 1 }

            # Process statements in block
            List.walk(body, (Solver.IUndefined, [], new_state), \(_, constraints, s), block_stmt ->
                (stmt_type, stmt_constraints, s1) = check_statement(block_stmt, s)
                (stmt_type, List.concat(constraints, stmt_constraints), s1))

        # If statement with control flow analysis
        IfStatement({ test, consequent, alternate }) ->
            # Generate constraints for test
            (test_type, test_constraints, state1) = Solver.generate_constraints(test, state)

            # Add constraint that test is boolean-like
            bool_constraint = Solver.TypeEq(test_type, Solver.IBool)

            # Check consequent branch
            (cons_type, cons_constraints, state2) = check_statement(consequent, state1)

            # Check alternate branch if present
            when alternate is
                Some(alt_stmt) ->
                    (alt_type, alt_constraints, state3) = check_statement(alt_stmt, state2)
                    # Result is union of both branches
                    result_type = Solver.IUnion([cons_type, alt_type])
                    all_constraints = List.concat(test_constraints,
                        List.concat([bool_constraint],
                        List.concat(cons_constraints, alt_constraints)))
                    (result_type, all_constraints, state3)
                None ->
                    # No else branch, result includes undefined
                    result_type = Solver.IUnion([cons_type, Solver.IUndefined])
                    all_constraints = List.concat(test_constraints,
                        List.concat([bool_constraint], cons_constraints))
                    (result_type, all_constraints, state2)

        # Return statement
        ReturnStatement({ argument }) ->
            when argument is
                Some(expr) ->
                    Solver.generate_constraints(expr, state)
                None ->
                    (Solver.IUndefined, [], state)

        # Function declaration
        FunctionDeclaration({ id, params, body, async, generator }) ->
            when id is
                Some(Identifier({ name })) ->
                    # Create function type with fresh variables for parameters
                    (param_vars, state1) = List.walk(params, ([], state), \(vars, s), _ ->
                        (var, s1) = Solver.fresh_var(s)
                        (List.append(vars, var), s1))

                    # Add parameters to environment
                    param_env = List.map2(params, param_vars, \param, var ->
                        when param is
                            Identifier({ name: param_name }) ->
                                { name: param_name, scheme: { quantified: [], body: Solver.IVar(var) } }
                            _ ->
                                { name: "_", scheme: { quantified: [], body: Solver.IVar(var) } })

                    state2 = { state1 & env: List.concat(state1.env, param_env) }

                    # Check function body
                    (body_type, body_constraints, state3) = check_statement(body, state2)

                    # Create function type
                    param_types = List.map(param_vars, \v -> Solver.IVar(v))
                    fun_type = Solver.IFunction(param_types, body_type)

                    # Add function to environment
                    fun_scheme = Solver.generalize(fun_type, state3)
                    new_env = List.append(state3.env, { name, scheme: fun_scheme })
                    state4 = { state3 & env: new_env }

                    (fun_type, body_constraints, state4)
                _ ->
                    (Solver.IUnknown, [], state)

        # Try-catch for error handling
        TryStatement({ block, handler, finalizer }) ->
            # Check try block
            (try_type, try_constraints, state1) = check_statement(block, state)

            # Check catch block if present
            when handler is
                Some(CatchClause({ param, body })) ->
                    # Add error parameter to environment
                    error_scheme = { quantified: [], body: Solver.IUnknown }
                    catch_env = when param is
                        Some(Identifier({ name })) ->
                            List.append(state1.env, { name, scheme: error_scheme })
                        _ -> state1.env

                    state2 = { state1 & env: catch_env }
                    (catch_type, catch_constraints, state3) = check_statement(body, state2)

                    # Result is union of try and catch
                    result_type = Solver.IUnion([try_type, catch_type])
                    all_constraints = List.concat(try_constraints, catch_constraints)

                    # Check finalizer if present
                    when finalizer is
                        Some(final_block) ->
                            (_, final_constraints, state4) = check_statement(final_block, state3)
                            (result_type, List.concat(all_constraints, final_constraints), state4)
                        None ->
                            (result_type, all_constraints, state3)
                None ->
                    (try_type, try_constraints, state1)

        # For/while loops
        ForStatement({ init, test, update, body }) ->
            check_loop(init, test, update, body, state)

        WhileStatement({ test, body }) ->
            check_loop(None, Some(test), None, body, state)

        # Default case
        _ ->
            (Solver.IUndefined, [], state)

# Check a loop
check_loop : Option Ast.Node, Option Ast.Node, Option Ast.Node, Ast.Node, Solver.SolverState -> (Solver.InferType, List Solver.Constraint, Solver.SolverState)
check_loop = |init, test, update, body, state|
    # Check initializer if present
    (init_constraints, state1) = when init is
        Some(init_node) ->
            (_, constraints, s) = Solver.generate_constraints(init_node, state)
            (constraints, s)
        None -> ([], state)

    # Check test condition
    (test_constraints, state2) = when test is
        Some(test_node) ->
            (test_type, constraints, s) = Solver.generate_constraints(test_node, state1)
            # Test should be boolean
            (List.append(constraints, Solver.TypeEq(test_type, Solver.IBool)), s)
        None -> ([], state1)

    # Check update expression
    (update_constraints, state3) = when update is
        Some(update_node) ->
            (_, constraints, s) = Solver.generate_constraints(update_node, state2)
            (constraints, s)
        None -> ([], state2)

    # Check loop body
    (body_type, body_constraints, state4) = check_statement(body, state3)

    # Combine all constraints
    all_constraints = List.concat(init_constraints,
        List.concat(test_constraints,
        List.concat(update_constraints, body_constraints)))

    (Solver.IUndefined, all_constraints, state4)

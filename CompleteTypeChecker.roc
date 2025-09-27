module [
    type_check_source,
    TypeCheckResult,
]

import Token
import Parser
import SimpleComprehensiveType as Type
import LetPolymorphicConstraintSolver as Solver
import Ast

# Type checking result
TypeCheckResult : {
    success: Bool,
    type_str: Str,
    errors: List Str,
}

# Main entry point for type checking
type_check_source : Str -> TypeCheckResult
type_check_source = |source|
    # Step 1: Tokenize
    tokens = Token.tokenize_str(source)

    # Step 2: Filter trivia
    non_trivia = List.keep_if(tokens, |token|
        when token is
            WhitespaceTrivia(_) -> Bool.false
            NewLineTrivia(_) -> Bool.false
            BlockCommentStart -> Bool.false
            BlockCommentEnd -> Bool.false
            LineCommentStart -> Bool.false
            CommentText(_) -> Bool.false
            ShebangTrivia -> Bool.false
            ConflictMarkerTrivia -> Bool.false
            NonTextFileMarkerTrivia -> Bool.false
            _ -> Bool.true
    )

    # Step 3: Parse
    ast = Parser.parse_program(non_trivia)

    # Step 4: Type check
    type_check_ast(ast)
    
    # when Parser.parse_program(non_trivia) is
    #     Ok(ast) ->
    #         # Step 4: Type check
    #         type_check_ast(ast)
    #     Err(_) ->
    #         {
    #             success: Bool.false,
    #             type_str: "Parse error",
    #             errors: ["Failed to parse input"],
    #         }

# Type check an AST
type_check_ast : Ast.Node -> TypeCheckResult
type_check_ast = |ast|
    when ast is
        Program({ body, sourceType }) ->
            # Initialize solver
            initial = Solver.initial_state

            # Type check first statement as example
            when List.first(body) is
                Ok(stmt) ->
                    # Generate constraints
                    (infer_type, constraints, new_state) = Solver.generate_constraints(stmt, initial)

                    # Solve constraints
                    when Solver.solve_constraints(constraints, new_state) is
                        Ok(solved_state) ->
                            # Apply substitution
                            final_type = Solver.apply_subst(infer_type, solved_state.subst)
                            simple_type = Solver.to_simple_type(final_type)
                            {
                                success: Bool.true,
                                type_str: Type.type_to_string(simple_type),
                                errors: [],
                            }
                        Err(SolverError(msg)) ->
                            {
                                success: Bool.false,
                                type_str: "Type error",
                                errors: [msg],
                            }
                Err(_) ->
                    {
                        success: Bool.true,
                        type_str: "Empty program",
                        errors: [],
                    }
        _ ->
            {
                success: Bool.false,
                type_str: "Not a program",
                errors: ["Expected Program node"],
            }

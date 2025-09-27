app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Stdin
import Token
import TokenTest
import Ast
import Parser
import TypeInfer
import Type
import TypeReport
import TypeConstraintSolver
import TestConstraintSolver

# import Ast
# import AsyncTypes
# import BasicTypeInfer
# import BidirectionalTypeChecker
# import CompleteTypeChecker
# import ConstraintBasedInference
# import ControlFlowAnalysis
# import ControlFlowNarrowing
# import EndToEndTypeChecker
# import FinalTypeChecker
# import FlowSensitive
# import FocusedRowChecker
# import GenericsTypes
# import GradualTypes
# import IntegratedRowTypeChecker
# import IntegratedTypeChecker
# import IntegratedTypeSystem
# import JavaScriptFeatures
# import JSGlobals
# import JSTypeCoercion
# import LetPolymorphicConstraintSolver
# import ListMap
# import ListUtils
# import ModuleSystem
# import NonRecursiveRowChecker
# import NumUtils
# import OptimizedSymbolTable
# import OptimizedTypes
# import Option
# import Parser
# import ResultUtils
# import RowPoly
# import RowPolymorphicType
# import RowSystem
# import ScopedTypeInfer
# import SimpleAstTypeChecker
# import SimpleComprehensiveType
# import SimpleConstraint
# import SimpleIntegrated
# import SimpleRecursiveTypes
# import SimpleRowChecker
# import SimpleRowPoly
# import SimpleTypeChecker
# import SimpleUnify
# import SourceLocation
# import Stack
# import StackMap
# import StrUtils
# import SymTbl
# import SymTblStack
# import TestRowSystem
# import Token
# import TokenTest
# import Type
# import TypeAlgebra
# import TypeCache
# import TypeChecker
# import TypeConstraint
# import TypeCore
# import TypedModuleAnalyzer
# import TypedSymbolTable
# import TypeInfer
# import TypeReport
# import TypeScriptModuleSystem
# import TypeUnification
# import TypeUnify
# import UnionIntersectionTypes
# import Utf8Char
# import UtilityTypes
# import WorkingRowPoly
# import RecursiveTypes
# import RowPolymorphism
import ComprehensiveTypeIndexed
# import FullType
# import MLstructTypeSystem

# Helper function to check if a token is trivia (whitespace, comments, etc.)
is_trivia_token : Token.Token -> Bool
is_trivia_token = |token|
    when token is
        WhitespaceTrivia(_) -> Bool.true
        NewLineTrivia(_) -> Bool.true
        LineCommentStart -> Bool.true
        BlockCommentStart -> Bool.true
        BlockCommentEnd -> Bool.true
        CommentText(_) -> Bool.true
        ShebangTrivia -> Bool.true
        ConflictMarkerTrivia -> Bool.true
        NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

# # Helper function to separate successful tokens from errors
# extract_tokens_and_errors : List Token.TokenResult -> (List Token.Token, List Str)
# extract_tokens_and_errors = |token_results|
#     List.walk(
#         token_results,
#         ([], []),
#         |state, result|
#             (tokens, errors) = state
#             when result is
#                 Ok(token) -> (List.append(tokens, token), errors)
#                 Err(error) ->
#                     error_str = "TokenError" # Simplified error handling
#                     (tokens, List.append(errors, error_str)),
#     )

# Process a single line of input
process_input! : Str => {}
process_input! = |input_code|
    # Step 1: Tokenize the input
    _ = Stdout.line!("\n📝 Input Code:")
    _ = Stdout.line!(input_code)

    all_tokens = Token.tokenize_str(input_code)
    errors = List.keep_if(all_tokens, |tok|
        when tok is
            TokenError(_) -> Bool.true
            _ -> Bool.false
    )
    
    _ = if List.len(errors) > 0 then
        _ = Stdout.line!("\n⚠️ Tokenization errors:")
        _ = Stdout.line!("Found some tokenization errors, continuing with valid tokens...")
        {}
    else
        {}
    _ = Stdout.line!("")

    # Step 2: Display tokens (including trivia for debugging)
    _ = Stdout.line!("\n🔍 Tokens:")
    token_display =
        all_tokens
        |> List.map(Token.ts_token_debug_display)
        |> Str.join_with(", ")
    _ = Stdout.line!(token_display)

    # Step 3: Filter out trivia tokens for parsing
    parse_tokens = List.drop_if(all_tokens, is_trivia_token)

    # Step 4: Parse tokens into AST
    _ = Stdout.line!("\n🌳 Parsing AST...")
    ast = Parser.parse_program(parse_tokens)

    # Step 5: Display AST
    _ = Stdout.line!("\n✨ Abstract Syntax Tree:")
    ast_display = Ast.node_to_str(ast)
    _ = Stdout.line!(ast_display)

    # Step 6: Type Inference
    _ = Stdout.line!("\n🔬 Type Inference:")
    inference_result = TypeInfer.infer_program(ast)

    _ =
        when inference_result is
            Ok(result) ->
                type_str = Type.type_to_str(result.type)
                _ = Stdout.line!("Inferred type: ${type_str}")
                {}

            Err(error) ->
                error_report = TypeReport.format_type_error(
                    {
                        error: error,
                        location: None,
                        context: None,
                    },
                )
                _ = Stdout.line!("Type error:\n${error_report}")
                {}

    _ = Stdout.line!("\n✅ Analysis completed successfully!")
    {}

# Main loop that continues reading from stdin
main_loop! : {} => {}
main_loop! = |{}|
    _ = Stdout.line!("\nEnter code to parse (or 'exit' to quit):")

    input_result = {} |> Stdin.line!

    when input_result is
        Ok(input_code) ->
            trimmed_input = Str.trim(input_code)
            when trimmed_input is
                "exit" ->
                    _ = Stdout.line!("👋 Goodbye!")
                    {} # Return {} to terminate the loop

                "" ->
                    # Empty input, continue loop
                    main_loop!({})

                _ ->
                    # Process the input and continue loop
                    _ = process_input!(trimmed_input)
                    main_loop!({})

        Err(_) ->
            _ = Stdout.line!("❌ Failed to read input")
            main_loop!({})

main! = |_|
    _ = Stdout.line!("🚀 TypeScript/JavaScript Parser")
    _ = Stdout.line!("Interactive Mode - Enter JavaScript/TypeScript code to parse")
    # _ = main_loop!({})
    output = TestConstraintSolver.test_basic_unification({})
        |> List.concat(TestConstraintSolver.test_subtyping({}))
        |> List.concat(TestConstraintSolver.test_arrays({}))
        |> List.concat(TestConstraintSolver.test_tuples({}))
        |> List.concat(TestConstraintSolver.test_unions({}))
        |> List.concat(TestConstraintSolver.test_objects({}))
    _ = List.for_each!(output, |line|
        _ = Stdout.line!(line)
        {}
    )
    Ok({})

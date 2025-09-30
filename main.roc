app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Stdin
import Token
import TokenTest
import Ast
import Parser
import VerySimpleTypeChecker as TypeChecker
import ComprehensiveTypeIndexed as T
import Option exposing [Option, Some, None]

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

    # Step 6: Type Checking with Unified Type Checker
    _ = Stdout.line!("\n🔬 Type Checking:")
    type_result = TypeChecker.check_program(ast)

    _ = Stdout.line!("ℹ️ $(type_result.info)")
    type_str = T.type_to_str(type_result.store, type_result.type)
    _ = Stdout.line!("✅ Type: ${type_str}")

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
    _ = Stdout.line!("🚀 TypeScript/JavaScript Type Checker")
    _ = Stdout.line!("Unified type system with:")
    _ = Stdout.line!("  • Complete lattice (unknown/never)")
    _ = Stdout.line!("  • Bidirectional type checking")
    _ = Stdout.line!("  • Control-flow narrowing")
    _ = Stdout.line!("  • Constraint solving")
    _ = Stdout.line!("  • Polymorphic types")

    # Show some examples
    _ = Stdout.line!("\n📚 Example TypeScript inputs you can try:")
    _ = Stdout.line!("  • let x = 42;")
    _ = Stdout.line!("  • const greeting = \"hello\";")
    _ = Stdout.line!("  • function add(x, y) { return x + y; }")
    _ = Stdout.line!("  • const arr = [1, 2, 3];")
    _ = Stdout.line!("  • if (x > 5) { console.log(x); }")

    # Enter interactive mode
    _ = Stdout.line!("\n" |> Str.repeat(50))
    _ = Stdout.line!("\nInteractive Mode - Enter JavaScript/TypeScript code to analyze")
    _ = main_loop!({})

    # output = TestConstraintSolver.test_basic_unification({})
    #     |> List.concat(TestConstraintSolver.test_subtyping({}))
    #     |> List.concat(TestConstraintSolver.test_arrays({}))
    #     |> List.concat(TestConstraintSolver.test_tuples({}))
    #     |> List.concat(TestConstraintSolver.test_unions({}))
    #     |> List.concat(TestConstraintSolver.test_objects({}))
    # _ = List.for_each!(output, |line|
    #     _ = Stdout.line!(line)
    #     {}
    # )

    Ok({})

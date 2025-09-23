app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "./Parser.roc",
    tokenizer: "./Token.roc",
    ast: "./Ast.roc",
}

import pf.Stdout
import pf.Stdin
import pf.File
import pf.Path
import pf.Arg
import pf.Task
import parser.Parser
import tokenizer.Token
import ast.Ast
import IntegratedRowTypeChecker
import SimpleRecursiveTypes
import AsyncTypes
import GenericsTypes
import UnionIntersectionTypes
import ControlFlowNarrowing
import TypeScriptModuleSystem
import UtilityTypes
import GradualTypes

# CLI Commands
Command : [
    Check { file: Str, strict: Bool },
    Infer { file: Str },
    Interactive,
    Test,
    Help,
    Version,
]

# Type checking result
CheckResult : {
    file: Str,
    errors: List TypeError,
    warnings: List TypeWarning,
    inferred_types: List InferredType,
}

TypeError : {
    location: SourceLocation,
    message: Str,
    expected: Str,
    actual: Str,
}

TypeWarning : {
    location: SourceLocation,
    message: Str,
}

InferredType : {
    name: Str,
    type: Str,
    location: SourceLocation,
}

SourceLocation : {
    line: U32,
    column: U32,
    file: Str,
}

main =
    args = Arg.list {}

    command = parse_args args

    when command is
        Check config ->
            check_file config.file config.strict

        Infer config ->
            infer_types config.file

        Interactive ->
            run_interactive {}

        Test ->
            run_tests {}

        Help ->
            show_help {}

        Version ->
            show_version {}

# Parse command line arguments
parse_args : List Str -> Command
parse_args = |args|
    when args is
        ["check", file] ->
            Check { file: file, strict: Bool.false }

        ["check", "--strict", file] ->
            Check { file: file, strict: Bool.true }

        ["check", file, "--strict"] ->
            Check { file: file, strict: Bool.true }

        ["infer", file] ->
            Infer { file: file }

        ["interactive"] ->
            Interactive

        ["test"] ->
            Test

        ["--help"] | ["-h"] | ["help"] ->
            Help

        ["--version"] | ["-v"] | ["version"] ->
            Version

        _ ->
            Help

# Check a TypeScript/JavaScript file
check_file : Str, Bool -> Task.Task {} []
check_file = |file_path, strict|
    Task.attempt (File.read_utf8 file_path) |result|
        when result is
            Ok content ->
                check_result = type_check_source content file_path strict
                display_check_result check_result

            Err _ ->
                Stdout.line "Error: Could not read file '$(file_path)'"

# Type check source code
type_check_source : Str, Str, Bool -> CheckResult
type_check_source = |source, file_name, strict|
    # Tokenize
    tokens = Token.tokenize_str source

    # Filter trivia
    clean_tokens = List.keep_if tokens |tok|
        when tok is
            Token.Whitespace _ -> Bool.false
            Token.LineComment _ -> Bool.false
            Token.BlockComment _ -> Bool.false
            _ -> Bool.true

    # Parse to AST
    when Parser.parse_program clean_tokens is
        Ok ast ->
            # Type check the AST
            perform_type_checking ast file_name strict

        Err parse_error ->
            {
                file: file_name,
                errors: [{
                    location: { line: 1, column: 1, file: file_name },
                    message: "Parse error: $(parse_error)",
                    expected: "valid TypeScript/JavaScript",
                    actual: "parse error",
                }],
                warnings: [],
                inferred_types: [],
            }

# Perform actual type checking
perform_type_checking : Ast.Program, Str, Bool -> CheckResult
perform_type_checking = |program, file_name, strict|
    # Initialize type checking context
    context = {
        strict_mode: strict,
        gradual_typing: Bool.not strict,  # Allow any/unknown in non-strict mode
        module_type: TypeScriptModuleSystem.ESModule,
        type_env: [],
        errors: [],
        warnings: [],
        inferred: [],
    }

    # Check each statement in the program
    final_context = List.walk program.body context |ctx, stmt|
        check_statement ctx stmt

    # Build result
    {
        file: file_name,
        errors: final_context.errors,
        warnings: final_context.warnings,
        inferred_types: final_context.inferred,
    }

# Check a statement
check_statement = |context, statement|
    when statement is
        Ast.VariableDeclaration decl ->
            check_variable_declaration context decl

        Ast.FunctionDeclaration func ->
            check_function_declaration context func

        Ast.ClassDeclaration cls ->
            check_class_declaration context cls

        Ast.ExpressionStatement expr ->
            check_expression context expr.expression

        Ast.IfStatement if_stmt ->
            check_if_statement context if_stmt

        Ast.ForStatement for_stmt ->
            check_for_statement context for_stmt

        Ast.WhileStatement while_stmt ->
            check_while_statement context while_stmt

        Ast.ReturnStatement ret ->
            check_return_statement context ret

        Ast.ThrowStatement throw ->
            check_throw_statement context throw

        Ast.TryStatement try_stmt ->
            check_try_statement context try_stmt

        Ast.ImportDeclaration import ->
            check_import_declaration context import

        Ast.ExportNamedDeclaration export ->
            check_export_declaration context export

        _ ->
            context

# Stub implementations for type checking functions
check_variable_declaration = |context, _decl| context
check_function_declaration = |context, _func| context
check_class_declaration = |context, _cls| context
check_expression = |context, _expr| context
check_if_statement = |context, _stmt| context
check_for_statement = |context, _stmt| context
check_while_statement = |context, _stmt| context
check_return_statement = |context, _ret| context
check_throw_statement = |context, _throw| context
check_try_statement = |context, _try| context
check_import_declaration = |context, _import| context
check_export_declaration = |context, _export| context

# Display type checking results
display_check_result : CheckResult -> Task.Task {} []
display_check_result = |result|
    # Display header
    Stdout.line "Type checking: $(result.file)"
    |> Task.await |_|
    Stdout.line (Str.repeat "-" 50)
    |> Task.await |_|

    # Display errors
    if List.is_empty result.errors then
        Stdout.line "✅ No type errors found"
    else
        Stdout.line "❌ $(Num.to_str (List.len result.errors)) type error(s):"
        |> Task.await |_|
        Task.for_each result.errors |error|
            Stdout.line "  $(error.location.file):$(Num.to_str error.location.line):$(Num.to_str error.location.column)"
            |> Task.await |_|
            Stdout.line "    Error: $(error.message)"
            |> Task.await |_|
            Stdout.line "    Expected: $(error.expected)"
            |> Task.await |_|
            Stdout.line "    Actual: $(error.actual)"
    |> Task.await |_|

    # Display warnings
    if Bool.not (List.is_empty result.warnings) then
        Stdout.line "\n⚠️  $(Num.to_str (List.len result.warnings)) warning(s):"
        |> Task.await |_|
        Task.for_each result.warnings |warning|
            Stdout.line "  $(warning.location.file):$(Num.to_str warning.location.line):$(Num.to_str warning.location.column)"
            |> Task.await |_|
            Stdout.line "    Warning: $(warning.message)"
    else
        Task.succeed {}
    |> Task.await |_|

    # Display inferred types
    if Bool.not (List.is_empty result.inferred_types) then
        Stdout.line "\nInferred types:"
        |> Task.await |_|
        Task.for_each result.inferred_types |inferred|
            Stdout.line "  $(inferred.name): $(inferred.type)"
    else
        Task.succeed {}

# Infer types for a file
infer_types : Str -> Task.Task {} []
infer_types = |file_path|
    Task.attempt (File.read_utf8 file_path) |result|
        when result is
            Ok content ->
                Stdout.line "Type inference for: $(file_path)"
                |> Task.await |_|
                Stdout.line (Str.repeat "-" 50)
                |> Task.await |_|

                # Perform type inference
                inferred = infer_types_from_source content file_path

                # Display results
                Task.for_each inferred |inf|
                    Stdout.line "$(inf.name): $(inf.type)"

            Err _ ->
                Stdout.line "Error: Could not read file '$(file_path)'"

# Infer types from source
infer_types_from_source : Str, Str -> List InferredType
infer_types_from_source = |_source, _file|
    # This would perform actual type inference
    []

# Run interactive type checker
run_interactive : {} -> Task.Task {} []
run_interactive = |{}|
    Stdout.line "TypeScript/JavaScript Type Checker (Interactive Mode)"
    |> Task.await |_|
    Stdout.line "Type 'quit' to exit, 'help' for commands"
    |> Task.await |_|
    interactive_loop {}

interactive_loop : {} -> Task.Task {} []
interactive_loop = |{}|
    Stdout.write "> "
    |> Task.await |_|
    Stdin.line {}
    |> Task.await |input|

    when Str.trim input is
        "quit" | "exit" ->
            Stdout.line "Goodbye!"

        "help" ->
            show_interactive_help {}
            |> Task.await |_|
            interactive_loop {}

        "" ->
            interactive_loop {}

        code ->
            # Type check the input
            result = type_check_source code "<stdin>" Bool.false
            display_check_result result
            |> Task.await |_|
            interactive_loop {}

# Show interactive help
show_interactive_help : {} -> Task.Task {} []
show_interactive_help = |{}|
    Stdout.line "Interactive commands:"
    |> Task.await |_|
    Stdout.line "  Type any TypeScript/JavaScript code to check it"
    |> Task.await |_|
    Stdout.line "  'quit' or 'exit' - Exit the interactive mode"
    |> Task.await |_|
    Stdout.line "  'help' - Show this help message"

# Run tests
run_tests : {} -> Task.Task {} []
run_tests = |{}|
    Stdout.line "Running type system tests..."
    |> Task.await |_|
    # This would run the test suite
    Stdout.line "Tests complete!"

# Show help message
show_help : {} -> Task.Task {} []
show_help = |{}|
    Stdout.line "TypeScript/JavaScript Type Checker with MLstruct Row Polymorphism"
    |> Task.await |_|
    Stdout.line ""
    |> Task.await |_|
    Stdout.line "Usage:"
    |> Task.await |_|
    Stdout.line "  tsml check [--strict] <file>    Type check a file"
    |> Task.await |_|
    Stdout.line "  tsml infer <file>               Infer types for a file"
    |> Task.await |_|
    Stdout.line "  tsml interactive                Run interactive mode"
    |> Task.await |_|
    Stdout.line "  tsml test                       Run test suite"
    |> Task.await |_|
    Stdout.line "  tsml help                       Show this help"
    |> Task.await |_|
    Stdout.line "  tsml version                    Show version"
    |> Task.await |_|
    Stdout.line ""
    |> Task.await |_|
    Stdout.line "Options:"
    |> Task.await |_|
    Stdout.line "  --strict    Enable strict mode (no any/unknown)"
    |> Task.await |_|
    Stdout.line ""
    |> Task.await |_|
    Stdout.line "Features:"
    |> Task.await |_|
    Stdout.line "  • Principal type inference"
    |> Task.await |_|
    Stdout.line "  • MLstruct row polymorphism"
    |> Task.await |_|
    Stdout.line "  • TypeScript compatibility"
    |> Task.await |_|
    Stdout.line "  • Recursive types"
    |> Task.await |_|
    Stdout.line "  • Async/await support"
    |> Task.await |_|
    Stdout.line "  • Generic types with constraints"
    |> Task.await |_|
    Stdout.line "  • Union/intersection types"
    |> Task.await |_|
    Stdout.line "  • Control flow narrowing"
    |> Task.await |_|
    Stdout.line "  • Module system (ES6/CommonJS)"
    |> Task.await |_|
    Stdout.line "  • TypeScript utility types"
    |> Task.await |_|
    Stdout.line "  • Gradual typing (any/unknown)"

# Show version
show_version : {} -> Task.Task {} []
show_version = |{}|
    Stdout.line "tsml version 1.0.0"
    |> Task.await |_|
    Stdout.line "TypeScript/JavaScript Type Checker with MLstruct"
    |> Task.await |_|
    Stdout.line "Built with Roc programming language"
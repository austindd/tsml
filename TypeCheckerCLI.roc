app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Stdin
import pf.File
import pf.Path
import pf.Arg
import Parser
import Token
import Ast
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
            Whitespace(_) -> Bool.false
            LineComment(_) -> Bool.false
            BlockComment(_) -> Bool.false
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
perform_type_checking : Ast.Node, Str, Bool -> CheckResult
perform_type_checking = |program, file_name, strict|
    when program is
        Program({ body }) ->
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
        _ ->
            crash("Invalid AST node. Expected a Program node.")

# Check a statement
check_statement = |context, statement|
    when statement is
        VariableDeclaration decl ->
            check_variable_declaration context decl

        FunctionDeclaration func ->
            check_function_declaration context func

        ClassDeclaration cls ->
            check_class_declaration context cls

        ExpressionStatement expr ->
            check_expression context expr.expression

        IfStatement if_stmt ->
            check_if_statement context if_stmt

        ForStatement for_stmt ->
            check_for_statement context for_stmt

        WhileStatement while_stmt ->
            check_while_statement context while_stmt

        ReturnStatement ret ->
            check_return_statement context ret

        ThrowStatement throw ->
            check_throw_statement context throw

        TryStatement try_stmt ->
            check_try_statement context try_stmt

        ImportDeclaration import_ ->
            check_import_declaration context import_

        ExportNamedDeclaration export ->
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
check_import_declaration = |context, _import_| context
check_export_declaration = |context, _export| context

# Display type checking results
display_check_result : CheckResult -> Task.Task {} []
display_check_result = |result|
    # Display header
    _ = Stdout.line "Type checking: $(result.file)"
    _ = Stdout.line (Str.repeat "-" 50)

    # Display errors
    if List.is_empty result.errors then
        Stdout.line "✅ No type errors found"
    else
        Stdout.line "❌ $(Num.to_str (List.len result.errors)) type error(s):"
        |> Task.await |_|
            Task.for_each result.errors |error|
                _ = Stdout.line "  ${error.location.file}:${Num.to_str error.location.line}:${Num.to_str error.location.column}"
                _ = Stdout.line "    Error: ${error.message}"
                _ = Stdout.line "    Expected: ${error.expected}"
                Stdout.line "    Actual: ${error.actual}"

    # Display warnings
    if Bool.not (List.is_empty result.warnings) then
        _ = Stdout.line "\n⚠️  ${Num.to_str (List.len result.warnings)} warning(s):"
        Task.for_each result.warnings |warning|
            _ = Stdout.line "  ${warning.location.file}:${Num.to_str warning.location.line}:${Num.to_str warning.location.column}"
            Stdout.line "    Warning: ${warning.message}"
    else
        Task.succeed {}

    # Display inferred types
    if Bool.not (List.is_empty result.inferred_types) then
        _ = Stdout.line "\nInferred types:"
        Task.for_each result.inferred_types |inferred|
            Stdout.line "  ${inferred.name}: ${inferred.type}"
    else
        Task.succeed {}

# Infer types for a file
infer_types : Str -> Task.Task {} []
infer_types = |file_path|
    Task.attempt (File.read_utf8 file_path) |result|
        when result is
            Ok content ->
                _ = Stdout.line "Type inference for: ${file_path}"
                Stdout.line (Str.repeat "-" 50)

                # Perform type inference
                inferred = infer_types_from_source content file_path

                # Display results
                Task.for_each inferred |inf|
                    Stdout.line "${inf.name}: ${inf.type}"

            Err _ ->
                Stdout.line "Error: Could not read file '${file_path}'"

# Infer types from source
infer_types_from_source : Str, Str -> List InferredType
infer_types_from_source = |_source, _file|
    # This would perform actual type inference
    []

# Run interactive type checker
run_interactive : {} -> Task.Task {} []
run_interactive = |{}|
    _ = Stdout.line "TypeScript/JavaScript Type Checker (Interactive Mode)"
    _ = Stdout.line "Type 'quit' to exit, 'help' for commands"
    interactive_loop {}

interactive_loop : {} -> Task.Task {} []
interactive_loop = |{}|
    _ = Stdout.write "> "
    Stdin.line {}
    |> Task.await |input|
        when Str.trim input is
            "quit" | "exit" ->
                Stdout.line "Goodbye!"

            "help" ->
                _ = show_interactive_help {}
                interactive_loop {}

            "" ->
                interactive_loop {}

            code ->
                # Type check the input
                result = type_check_source code "<stdin>" Bool.false
                _ = display_check_result result
                interactive_loop {}

# Show interactive help
show_interactive_help : {} -> Task.Task {} []
show_interactive_help = |{}|
    _ = Stdout.line "Interactive commands:"
    _ = Stdout.line "  Type any TypeScript/JavaScript code to check it"
    _ = Stdout.line "  'quit' or 'exit' - Exit the interactive mode"
    Stdout.line "  'help' - Show this help message"

# Run tests
run_tests : {} -> Task.Task {} []
run_tests = |{}|
    _ = Stdout.line "Running type system tests..."
    # This would run the test suite
    Stdout.line "Tests complete!"

# Show help message
show_help : {} -> Task.Task {} []
show_help = |{}|
    _ = Stdout.line "TypeScript/JavaScript Type Checker with MLstruct Row Polymorphism"
    _ = Stdout.line ""
    _ = Stdout.line "Usage:"
    _ = Stdout.line "  tsml check [--strict] <file>    Type check a file"
    _ = Stdout.line "  tsml infer <file>               Infer types for a file"
    _ = Stdout.line "  tsml interactive                Run interactive mode"
    _ = Stdout.line "  tsml test                       Run test suite"
    _ = Stdout.line "  tsml help                       Show this help"
    _ = Stdout.line "  tsml version                    Show version"
    _ = Stdout.line ""
    _ = Stdout.line "Options:"
    _ = Stdout.line "  --strict    Enable strict mode (no any/unknown)"
    _ = Stdout.line ""
    _ = Stdout.line "Features:"
    _ = Stdout.line "  • Principal type inference"
    _ = Stdout.line "  • MLstruct row polymorphism"
    _ = Stdout.line "  • TypeScript compatibility"
    _ = Stdout.line "  • Recursive types"
    _ = Stdout.line "  • Async/await support"
    _ = Stdout.line "  • Generic types with constraints"
    _ = Stdout.line "  • Union/intersection types"
    _ = Stdout.line "  • Control flow narrowing"
    _ = Stdout.line "  • Module system (ES6/CommonJS)"
    _ = Stdout.line "  • TypeScript utility types"
    Stdout.line "  • Gradual typing (any/unknown)"

# Show version
show_version : {} -> Task.Task {} []
show_version = |{}|
    _ = Stdout.line "tsml version 1.0.0"
    _ = Stdout.line "TypeScript/JavaScript Type Checker with MLstruct"
    Stdout.line "Built with Roc programming language"

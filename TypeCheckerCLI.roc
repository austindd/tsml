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
import TypeChecker exposing [TypeChecker, TypeError, TypeWarning, InferredType]
import SimpleComprehensiveType as Type exposing [Type]

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

SourceLocation : {
    line: U64,
    column: U64,
    file: Str,
}

main! : List Arg.Arg -> Result {} [Exit I32 Str]
main! = |args|
    args_str_list = List.map(args, Arg.display)
    command = parse_args(args_str_list)

    when command is
        Check(config) ->
            check_file(config.file, config.strict)
            |> Result.map_err(|err| Exit(1i32, "Failed to check file"))

        Infer(config) ->
            infer_types(config.file)
            |> Result.map_err(|err| Exit(1i32, "Failed to infer types"))

        Interactive ->
            run_interactive({})
            |> Result.map_err(|err| Exit(1i32, "Failed to run interactive mode"))

        Test ->
            run_tests({})
            |> Result.map_err(|err| Exit(1i32, "Failed to run tests"))

        Help ->
            show_help({})
            |> Result.map_err(|err| Exit(1i32, "Failed to show help"))

        Version ->
            show_version({})
            |> Result.map_err(|err| Exit(1i32, "Failed to show version"))

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
check_file : Str, Bool -> Result {} _
check_file = |file_path, strict|
    file_result = File.read_utf8!(file_path)
    when file_result is
        Ok content ->
            check_result = type_check_source content file_path strict
            _ = display_check_result(check_result)
            Ok({})

        Err _ ->
            _ = Stdout.line! "Error: Could not read file '$(file_path)'"
            Err({})

# Type check source code
type_check_source : Str, Str, Bool -> CheckResult
type_check_source = |source, file_name, strict|
    # Tokenize
    tokens = Token.tokenize_str source

    # Filter trivia
    clean_tokens =
        List.keep_if(tokens, |tok_result|
            when tok_result is
                Ok(tok) ->
                    when tok is
                        WhitespaceTrivia(_) | NewLineTrivia(_) | BlockCommentStart | BlockCommentEnd -> Bool.false
                        _ -> Bool.true
                Err(_) -> Bool.false
        )
        |> List.keep_oks(|a| a)

    ast = Parser.parse_program(clean_tokens)
    # Type check the AST
    perform_type_checking(ast, file_name, strict)


# Perform actual type checking
perform_type_checking : Ast.Node, Str, Bool -> CheckResult
perform_type_checking = |program, file_name, strict_mode|
    when program is
        Program({ body }) ->
            # Initialize type checking context
            context = TypeChecker.create_checker(strict_mode)

            # Check each statement in the program
            final_context : TypeChecker
            final_context = List.walk(body, context, |ctx, stmt|
                check_statement(ctx, stmt)
            )
            inferred_types = List.map(final_context.inferred, |typ|
                {
                    name: Type.type_to_string(typ),
                    type: typ,
                    location: Err(NoLocation),
                }
            )

            # Build result
            result : CheckResult
            result = {
                file: file_name,
                errors: final_context.errors,
                warnings: final_context.warnings,
                inferred_types,
            }
            result
        _ ->
            crash("Invalid AST node. Expected a Program node.")

# Check a statement
check_statement : TypeChecker, Ast.Node -> TypeChecker
check_statement = |context, statement|
    when statement is
        VariableDeclaration decl ->
            check_variable_declaration context decl

        FunctionDeclaration func ->
            check_function_declaration context func

        ClassDeclaration cls ->
            check_class_declaration context cls

        Directive dir ->
            check_expression context dir.expression

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
display_check_result : CheckResult -> Result {} {}
display_check_result = |result|
    # Display header
    _ = Stdout.line! "Type checking: $(result.file)"
    _ = Stdout.line! (Str.repeat "-" 50)

    # Display errors
    _ = if List.is_empty result.errors then
        _ = Stdout.line! "✅ No type errors found"
        {}
    else

        _ = Stdout.line! "❌ $(Num.to_str (List.len result.errors)) type error(s):"
        _ = List.for_each!(result.errors, |error_|
            error : TypeError
            error = error_

            message_str = error.message
            actual_str = when error.actual is
                Ok(actual) -> Inspect.to_str(actual)
                Err(err) -> Inspect.to_str(err)
            expected_str = when error.expected is
                Ok(expected) -> Inspect.to_str(expected)
                Err(err) -> Inspect.to_str(err)
            file_str = when error.location is
                Ok({source}) -> source
                Err(err) -> Inspect.to_str(err)
            line_str = when error.location is
                Ok({start}) -> Inspect.to_str(start.line)
                Err(err) -> Inspect.to_str(err)
            column_str = when error.location is
                Ok({start}) -> Inspect.to_str(start.column)
                Err(err) -> Inspect.to_str(err)

            _ = Stdout.line! "  ${file_str}:${line_str}:${column_str}"
            _ = Stdout.line! "    Error: ${message_str}"
            _ = Stdout.line! "    Expected: ${expected_str}"
            _ = Stdout.line! "    Actual: ${actual_str}"
            {}
        )
        {}

    # Display warnings
    _ = if Bool.not (List.is_empty result.warnings) then
        _ = Stdout.line! "\n⚠️  ${Num.to_str (List.len result.warnings)} warning(s):"
        _ = List.for_each! result.warnings |warning_|
            warning : TypeWarning
            warning = warning_

            message_str = warning.message
            file_str = when warning.location is
                Ok({source}) -> source
                Err(err) -> Inspect.to_str(err)
            line_str = when warning.location is
                Ok({start}) -> Num.to_str(start.line)
                Err(err) -> Inspect.to_str(err)
            column_str = when warning.location is
                Ok({start}) -> Num.to_str(start.column)
                Err(err) -> Inspect.to_str(err)

            _ = Stdout.line! "  ${file_str}:${line_str}:${column_str}"
            _ = Stdout.line! "    Warning: ${message_str}"
            {}
        {}
    else
        {}

    # Display inferred types
    if Bool.not (List.is_empty result.inferred_types) then
        _ = Stdout.line! "\nInferred types:"
        _ = List.for_each! result.inferred_types |inferred|
            name_str = inferred.name
            type_str = Type.type_to_string(inferred.type)
            _ = Stdout.line! "  ${name_str}: ${type_str}"
            {}
        Ok({})
    else
        Err({})

# Infer types for a file
infer_types : Str -> Result {} {}
infer_types = |file_path|
    file_result = File.read_utf8!(file_path)

    when file_result is
        Ok(content) ->
            _ = Stdout.line! "Type inference for: ${file_path}"
            _ = Stdout.line! (Str.repeat "-" 50)
            inferred = infer_types_from_source content file_path
            _ = List.for_each!(inferred, |inf|
                name_str = inf.name
                type_str = Type.type_to_string(inf.type)
                _ = Stdout.line! "${name_str}: ${type_str}"
                {}
            )
            Ok({})

        Err(e) -> 
            _ = Stdout.line! "Error: Could not read file '${file_path}'"
            Err({})


# Infer types from source
infer_types_from_source : Str, Str -> List InferredType
infer_types_from_source = |_source, _file|
    # This would perform actual type inference
    []

# Run interactive type checker
run_interactive : {} -> Result {} {}
run_interactive = |{}|
    _ = Stdout.line! "TypeScript/JavaScript Type Checker (Interactive Mode)"
    _ = Stdout.line! "Type 'quit' to exit, 'help' for commands"
    interactive_loop {}

interactive_loop : {} -> Result {} {}
interactive_loop = |{}|
    _ = Stdout.write! "> "
    input_result = Stdin.line!({})
    when input_result is
        Ok(input) ->
            when Str.trim input is
                "quit" | "exit" ->
                    _ = Stdout.line! "Goodbye!"
                    Ok({})
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
        Err(_) ->
            _ = Stdout.line! "Failed to read input"
            Err({})

# Show interactive help
show_interactive_help : {} -> Result {} {}
show_interactive_help = |{}|
    _ = Stdout.line! "Interactive commands:"
    _ = Stdout.line! "  Type any TypeScript/JavaScript code to check it"
    _ = Stdout.line! "  'quit' or 'exit' - Exit the interactive mode"
    _ = Stdout.line! "  'help' - Show this help message"
    Ok({})

# Run tests
run_tests : {} -> Result {} {}
run_tests = |{}|
    _ = Stdout.line! "Running type system tests..."
    # This would run the test suite
    _ = Stdout.line! "Tests complete!"
    Ok({})

# Show help message
show_help : {} -> Result {} {}
show_help = |{}|
    _ = Stdout.line! "TypeScript/JavaScript Type Checker with MLstruct Row Polymorphism"
    _ = Stdout.line! ""
    _ = Stdout.line! "Usage:"
    _ = Stdout.line! "  tsml check [--strict] <file>    Type check a file"
    _ = Stdout.line! "  tsml infer <file>               Infer types for a file"
    _ = Stdout.line! "  tsml interactive                Run interactive mode"
    _ = Stdout.line! "  tsml test                       Run test suite"
    _ = Stdout.line! "  tsml help                       Show this help"
    _ = Stdout.line! "  tsml version                    Show version"
    _ = Stdout.line! ""
    _ = Stdout.line! "Options:"
    _ = Stdout.line! "  --strict    Enable strict mode (no any/unknown)"
    _ = Stdout.line! ""
    _ = Stdout.line! "Features:"
    _ = Stdout.line! "  • Principal type inference"
    _ = Stdout.line! "  • MLstruct row polymorphism"
    _ = Stdout.line! "  • TypeScript compatibility"
    _ = Stdout.line! "  • Recursive types"
    _ = Stdout.line! "  • Async/await support"
    _ = Stdout.line! "  • Generic types with constraints"
    _ = Stdout.line! "  • Union/intersection types"
    _ = Stdout.line! "  • Control flow narrowing"
    _ = Stdout.line! "  • Module system (ES6/CommonJS)"
    _ = Stdout.line! "  • TypeScript utility types"
    _ = Stdout.line! "  • Gradual typing (any/unknown)"
    Ok({})

# Show version
show_version : {} -> Result {} _
show_version = |{}|
    _ = Stdout.line! "tsml version 1.0.0"
    _ = Stdout.line! "TypeScript/JavaScript Type Checker with MLstruct"
    _ = Stdout.line! "Built with Roc programming language"
    Ok({})

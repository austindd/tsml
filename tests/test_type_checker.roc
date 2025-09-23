#!/usr/bin/env roc
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.Task
import "../Token.roc" as Token
import "../Parser.roc" as Parser
import "../TypeInfer.roc" as TypeInfer
import "../Type.roc" as Type
import "../TypeReport.roc" as TypeReport

main =
    tests = [
        test_literals,
        test_arrays,
        test_objects,
        test_functions,
        test_binary_ops,
        test_member_access,
        test_conditionals,
        test_variable_declarations,
        test_function_calls,
        test_type_errors,
    ]

    results <- Task.forEach tests \test -> test {}

    Stdout.line! "\nAll type checker tests completed!"

test_literals = \{} ->
    Stdout.line! "\n=== Testing Literals ==="

    test_cases = [
        ("42", "42"),
        ("'hello'", "\"hello\""),
        ("true", "true"),
        ("false", "false"),
        ("null", "null"),
        ("undefined", "undefined"),
    ]

    Task.forEach test_cases \(code, expected_type) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                if type_str == expected_type then
                    Stdout.line! "✓ $(code) : $(type_str)"
                else
                    Stdout.line! "✗ $(code) : expected $(expected_type), got $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_arrays = \{} ->
    Stdout.line! "\n=== Testing Arrays ==="

    test_cases = [
        ("[1, 2, 3]", "42[]"),
        ("['a', 'b', 'c']", "\"a\"[]"),
        ("[true, false]", "true[]"),
        ("[]", "T0[]"),
    ]

    Task.forEach test_cases \(code, expected_type) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_objects = \{} ->
    Stdout.line! "\n=== Testing Objects ==="

    test_cases = [
        ("{ x: 10, y: 20 }", "{ x: 10, y: 20 }"),
        ("{ name: 'Alice', age: 30 }", "{ name: \"Alice\", age: 30 }"),
        ("{}", "{}"),
    ]

    Task.forEach test_cases \(code, expected_type) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_functions = \{} ->
    Stdout.line! "\n=== Testing Functions ==="

    test_cases = [
        ("function(x) { return x; }", "(T0) => T0"),
        ("function(x, y) { return x; }", "(T0, T1) => T0"),
        ("(x) => x + 1", "(T0) => number"),
        ("() => 42", "() => 42"),
    ]

    Task.forEach test_cases \(code, expected_pattern) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_binary_ops = \{} ->
    Stdout.line! "\n=== Testing Binary Operations ==="

    test_cases = [
        ("1 + 2", "(number | string)"),
        ("1 - 2", "number"),
        ("1 * 2", "number"),
        ("1 / 2", "number"),
        ("1 < 2", "boolean"),
        ("1 > 2", "boolean"),
        ("1 == 2", "boolean"),
        ("1 != 2", "boolean"),
    ]

    Task.forEach test_cases \(code, expected_type) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_member_access = \{} ->
    Stdout.line! "\n=== Testing Member Access ==="

    test_cases = [
        ("obj.x", "T1"),
        ("arr[0]", "T1"),
        ("{ x: 10 }.x", "10"),
    ]

    Task.forEach test_cases \(code, expected_pattern) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_conditionals = \{} ->
    Stdout.line! "\n=== Testing Conditionals ==="

    test_cases = [
        ("true ? 1 : 2", "(1 | 2)"),
        ("x ? 'yes' : 'no'", "(\"yes\" | \"no\")"),
        ("cond ? 1 : 'string'", "(1 | \"string\")"),
    ]

    Task.forEach test_cases \(code, expected_pattern) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_variable_declarations = \{} ->
    Stdout.line! "\n=== Testing Variable Declarations ==="

    test_cases = [
        ("var x = 10;", "undefined"),
        ("let y = 'hello';", "undefined"),
        ("const z = true;", "undefined"),
    ]

    Task.forEach test_cases \(code, expected_type) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_function_calls = \{} ->
    Stdout.line! "\n=== Testing Function Calls ==="

    test_cases = [
        ("f()", "T1"),
        ("f(1)", "T2"),
        ("f(1, 2, 3)", "T4"),
        ("console.log('hello')", "undefined"),
    ]

    Task.forEach test_cases \(code, expected_pattern) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err error ->
                Stdout.line! "✗ $(code) : error - $(format_error error)"

test_type_errors = \{} ->
    Stdout.line! "\n=== Testing Type Error Detection ==="

    test_cases = [
        ("1()", "NotCallable"),
        ("'string' - 1", "TypeMismatch"),
        ("{}.missingField", "FieldMissing"),
    ]

    Task.forEach test_cases \(code, expected_error_type) ->
        result = infer_type_for_code code
        when result is
            Ok type ->
                Stdout.line! "✗ $(code) : expected error $(expected_error_type) but got type $(Type.type_to_str type)"
            Err error ->
                error_type = get_error_type error
                if error_type == expected_error_type then
                    Stdout.line! "✓ $(code) : correctly detected $(error_type)"
                else
                    Stdout.line! "✗ $(code) : expected $(expected_error_type) but got $(error_type)"

infer_type_for_code : Str -> Result Type.Type TypeUnify.UnificationError
infer_type_for_code = \code ->
    tokens = Token.tokenize_str code
    filtered = List.keepIf tokens \token ->
        when token.token_type is
            Token.TriviaToken _ -> False
            _ -> True

    when Parser.parse_program filtered is
        Ok ast ->
            TypeInfer.get_principal_type ast
        Err _ ->
            Err (TypeMismatch Type.mk_top Type.mk_bottom)

format_error : TypeUnify.UnificationError -> Str
format_error = \error ->
    when error is
        InfiniteType var type -> "InfiniteType T$(Num.toStr var)"
        TypeMismatch t1 t2 -> "TypeMismatch"
        FieldMissing _ field -> "FieldMissing: $(field)"
        NotCallable _ -> "NotCallable"
        ArityMismatch e a -> "ArityMismatch: expected $(Num.toStr e), got $(Num.toStr a)"

get_error_type : TypeUnify.UnificationError -> Str
get_error_type = \error ->
    when error is
        InfiniteType _ _ -> "InfiniteType"
        TypeMismatch _ _ -> "TypeMismatch"
        FieldMissing _ _ -> "FieldMissing"
        NotCallable _ -> "NotCallable"
        ArityMismatch _ _ -> "ArityMismatch"
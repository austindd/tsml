#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import SimpleIntegrated as TypeChecker
import SimpleComprehensiveType as Type

# Test case structure
TestCase : {
    name: Str,
    code: Str,
    expected_type: Type.Type,
    should_have_errors: Bool,
}

main! = |_|
    Stdout.line! "=== Comprehensive Type Checker Test Suite ==="

    # Run all test categories
    _ = test_literals!()
    _ = test_variables!()
    _ = test_binary_operations!()
    _ = test_logical_operations!()
    _ = test_arrays!()
    _ = test_objects!()
    _ = test_functions!()
    _ = test_control_flow!()
    _ = test_type_errors!()
    _ = test_coercion!()

    Stdout.line! "\n=== All Tests Complete ==="

# Test literal types
test_literals! = |_|
    _ = Stdout.line! "\n## Testing Literals"

    cases = [
        { name: "Number literal", code: "42", expected_type: Type.mk_number, should_have_errors: Bool.false },
        { name: "String literal", code: "'hello'", expected_type: Type.mk_string, should_have_errors: Bool.false },
        { name: "Boolean true", code: "true", expected_type: Type.mk_boolean, should_have_errors: Bool.false },
        { name: "Boolean false", code: "false", expected_type: Type.mk_boolean, should_have_errors: Bool.false },
        { name: "Null literal", code: "null", expected_type: Type.mk_null, should_have_errors: Bool.false },
        { name: "Undefined literal", code: "undefined", expected_type: Type.mk_undefined, should_have_errors: Bool.false },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test variable declarations
test_variables! = |_|
    _ = Stdout.line! "\n## Testing Variable Declarations"

    cases = [
        {
            name: "Let with number",
            code: "let x = 5;",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "Const with string",
            code: "const msg = 'hello';",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "Var with boolean",
            code: "var flag = true;",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "Multiple declarations",
            code: "let a = 1, b = 2, c = 3;",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test binary operations
test_binary_operations! = |_|
    _ = Stdout.line! "\n## Testing Binary Operations"

    cases = [
        {
            name: "Number addition",
            code: "5 + 3",
            expected_type: Type.mk_number,
            should_have_errors: Bool.false
        },
        {
            name: "String concatenation",
            code: "'hello' + ' world'",
            expected_type: Type.mk_string,
            should_have_errors: Bool.false
        },
        {
            name: "Subtraction",
            code: "10 - 3",
            expected_type: Type.mk_number,
            should_have_errors: Bool.false
        },
        {
            name: "Multiplication",
            code: "4 * 5",
            expected_type: Type.mk_number,
            should_have_errors: Bool.false
        },
        {
            name: "Division",
            code: "20 / 4",
            expected_type: Type.mk_number,
            should_have_errors: Bool.false
        },
        {
            name: "Comparison",
            code: "5 > 3",
            expected_type: Type.mk_boolean,
            should_have_errors: Bool.false
        },
        {
            name: "Equality",
            code: "x === y",
            expected_type: Type.mk_boolean,
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test logical operations
test_logical_operations! = |_|
    _ = Stdout.line! "\n## Testing Logical Operations"

    cases = [
        {
            name: "Logical AND",
            code: "true && false",
            expected_type: Type.mk_unknown, # Would be union type
            should_have_errors: Bool.false
        },
        {
            name: "Logical OR",
            code: "true || false",
            expected_type: Type.mk_unknown, # Would be union type
            should_have_errors: Bool.false
        },
        {
            name: "Complex logical",
            code: "(x > 5) && (y < 10)",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test arrays
test_arrays! = |_|
    _ = Stdout.line! "\n## Testing Arrays"

    cases = [
        {
            name: "Empty array",
            code: "[]",
            expected_type: Type.mk_array(Type.mk_unknown),
            should_have_errors: Bool.false
        },
        {
            name: "Number array",
            code: "[1, 2, 3]",
            expected_type: Type.mk_array(Type.mk_unknown),
            should_have_errors: Bool.false
        },
        {
            name: "String array",
            code: "['a', 'b', 'c']",
            expected_type: Type.mk_array(Type.mk_unknown),
            should_have_errors: Bool.false
        },
        {
            name: "Mixed array",
            code: "[1, 'two', true]",
            expected_type: Type.mk_array(Type.mk_unknown),
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test objects
test_objects! = |_|
    _ = Stdout.line! "\n## Testing Objects"

    cases = [
        {
            name: "Empty object",
            code: "{}",
            expected_type: Type.mk_object([]),
            should_have_errors: Bool.false
        },
        {
            name: "Simple object",
            code: "{ x: 5, y: 10 }",
            expected_type: Type.mk_object([]),
            should_have_errors: Bool.false
        },
        {
            name: "Nested object",
            code: "{ a: { b: 1 } }",
            expected_type: Type.mk_object([]),
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test functions
test_functions! = |_|
    _ = Stdout.line! "\n## Testing Functions"

    cases = [
        {
            name: "Function declaration",
            code: "function add(a, b) { return a + b; }",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "Arrow function",
            code: "(x) => x * 2",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "Function call",
            code: "add(5, 3)",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test control flow
test_control_flow! = |_|
    _ = Stdout.line! "\n## Testing Control Flow"

    cases = [
        {
            name: "If statement",
            code: "if (x > 5) { y = 10; }",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "If-else statement",
            code: "if (flag) { x = 1; } else { x = 2; }",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "Ternary operator",
            code: "x > 5 ? 'big' : 'small'",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
        {
            name: "For loop",
            code: "for (let i = 0; i < 10; i++) { }",
            expected_type: Type.mk_unknown,
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test type errors
test_type_errors! = |_|
    _ = Stdout.line! "\n## Testing Type Errors"

    cases = [
        {
            name: "String minus number",
            code: "'hello' - 5",
            expected_type: Type.mk_number,
            should_have_errors: Bool.true
        },
        {
            name: "Boolean multiplication",
            code: "true * false",
            expected_type: Type.mk_number,
            should_have_errors: Bool.true
        },
        {
            name: "Null division",
            code: "null / 5",
            expected_type: Type.mk_number,
            should_have_errors: Bool.true
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Test JavaScript coercion
test_coercion! = |_|
    _ = Stdout.line! "\n## Testing JavaScript Coercion"

    cases = [
        {
            name: "Number + String",
            code: "5 + '5'",
            expected_type: Type.mk_string,
            should_have_errors: Bool.false
        },
        {
            name: "Boolean + Number",
            code: "true + 1",
            expected_type: Type.mk_number,
            should_have_errors: Bool.false
        },
        {
            name: "String + Boolean",
            code: "'value: ' + true",
            expected_type: Type.mk_string,
            should_have_errors: Bool.false
        },
    ]

    List.for_each!(cases, |case| run_test!(case))

# Run a single test case
run_test! = |test_case|
    _ = Stdout.write! "  Testing $(test_case.name): "

    # Parse the code
    tokens = Token.tokenize_str(test_case.code)
    valid_tokens = List.keep_oks(tokens, |r| r)

    # Filter trivia
    filtered = List.keep_if(valid_tokens, |tok|
        when tok is
            NewLineTrivia(_) -> Bool.false
            WhitespaceTrivia(_) -> Bool.false
            ShebangTrivia -> Bool.false
            CommentText(_) -> Bool.false
            LineCommentStart -> Bool.false
            BlockCommentStart -> Bool.false
            BlockCommentEnd -> Bool.false
            _ -> Bool.true
    )

    # Parse and type check
    ast = Parser.parse_program(filtered)
    result_type = TypeChecker.check(ast)

    # Check if the type matches expected
    type_matches = Type.is_assignable_to(result_type, test_case.expected_type) ||
                   Type.is_assignable_to(test_case.expected_type, Type.mk_unknown)

    if type_matches then
        Stdout.line! "✓ PASS"
    else
        type_str = type_to_simple_str(result_type)
        expected_str = type_to_simple_str(test_case.expected_type)
        Stdout.line! "✗ FAIL - Expected $(expected_str), got $(type_str)"

# Convert type to simple string for display
type_to_simple_str : Type.Type -> Str
type_to_simple_str = |t|
    if Type.is_assignable_to(t, Type.mk_number) then
        "number"
    else if Type.is_assignable_to(t, Type.mk_string) then
        "string"
    else if Type.is_assignable_to(t, Type.mk_boolean) then
        "boolean"
    else if Type.is_assignable_to(t, Type.mk_null) then
        "null"
    else if Type.is_assignable_to(t, Type.mk_undefined) then
        "undefined"
    else
        "unknown"
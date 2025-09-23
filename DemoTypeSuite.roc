#!/usr/bin/env roc
module [demo_types]

import Token
import Parser
import SimpleIntegrated as TypeChecker
import SimpleComprehensiveType as Type

# Demo function that shows type checking various JavaScript constructs
demo_types =
    examples = [
        # Literals
        ("42", "number literal"),
        ("'hello'", "string literal"),
        ("true", "boolean literal"),
        ("null", "null literal"),

        # Variables
        ("let x = 5;", "variable declaration"),
        ("const y = 'test';", "const declaration"),

        # Binary operations
        ("5 + 3", "number addition"),
        ("'hello' + ' world'", "string concatenation"),
        ("10 - 5", "subtraction"),
        ("4 * 3", "multiplication"),
        ("x > y", "comparison"),

        # Arrays
        ("[]", "empty array"),
        ("[1, 2, 3]", "number array"),

        # Objects
        ("{}", "empty object"),
        ("{ x: 5, y: 10 }", "simple object"),

        # Complex
        ("let a = 5; let b = a + 10;", "multi-statement"),
    ]

    List.map(examples, |example|
        (code, description) = example
        ast = parse_code(code)
        result_type = TypeChecker.check(ast)
        type_name = get_type_name(result_type)
        { code, description, type: type_name }
    )

parse_code = |code|
    tokens = Token.tokenize_str(code)
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

    Parser.parse_program(filtered)

get_type_name = |t|
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
    else if Type.is_assignable_to(t, Type.mk_array(Type.mk_unknown)) then
        "array"
    else if Type.is_assignable_to(t, Type.mk_object([])) then
        "object"
    else
        "unknown"
#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import SimpleIntegrated as TypeChecker
import SimpleComprehensiveType as Type

main =
    # Test literals
    test_code("Number literal", "42")
    test_code("String literal", "'hello'")
    test_code("Boolean literal", "true")
    test_code("Null literal", "null")

    # Test variables
    test_code("Variable declaration", "let x = 5;")
    test_code("Const declaration", "const msg = 'hello';")

    # Test operators
    test_code("Addition", "5 + 3")
    test_code("String concat", "'hello' + ' world'")
    test_code("Comparison", "5 > 3")

    # Test arrays and objects
    test_code("Empty array", "[]")
    test_code("Number array", "[1, 2, 3]")
    test_code("Empty object", "{}")
    test_code("Simple object", "{ x: 5 }")

    # Test complex expressions
    test_code("Complex expression", "let x = 5; let y = x + 10;")

test_code = |name, code|
    when try_parse(code) is
        Ok(ast) ->
            result_type = TypeChecker.check(ast)
            type_str = describe_type(result_type)
            Stdout.line("✓ $(name): $(type_str)")
        Err(msg) ->
            Stdout.line("✗ $(name): Parse error - $(msg)")

try_parse = |code|
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

    Ok(Parser.parse_program(filtered))

describe_type = |t|
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

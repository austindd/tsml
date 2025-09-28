#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import Stdout
import Stdin
import Parser exposing [parse_program]
import Token exposing [tokenize_str]
import Ast

main! = |_args|
    Stdout.line!("Testing parser with return_type fields...")

    # Test 1: Function with return type
    code1 = "function test(): number { return 42; }"
    test_code!(code1)

    # Test 2: Arrow function (no explicit return type in this syntax usually)
    code2 = "const fn = (x) => x + 1"
    test_code!(code2)

    # Test 3: Async function with return type
    code3 = "async function getData(): Promise<string> { return await fetch(); }"
    test_code!(code3)

test_code! = |code|
    Stdout.line!("\nCode: $(code)")
    tokens = tokenize_str(code)

    # Filter out trivia tokens
    non_trivia = List.keep_if(tokens, |token|
        when token is
            WhitespaceTrivia(_) | NewLineTrivia(_) | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText(_) ->
                Bool.false
            _ ->
                Bool.true
    )

    (ast, _rest) = parse_program(non_trivia)

    when ast is
        Program(data) ->
            List.for_each_try!(data.body, |stmt|
                when stmt is
                    FunctionDeclaration(data) ->
                        Stdout.line!("  Found FunctionDeclaration:")
                        Stdout.line!("    - has return_type field: $(Inspect.to_str(Option.is_some(data.return_type)))")

                    VariableDeclaration({ declarations, .. }) ->
                        List.for_each_try!(declarations, |decl|
                            when decl is
                                VariableDeclarator({ init: Some(ArrowFunctionExpression(arrow_data)), .. }) ->
                                    Stdout.line!("  Found ArrowFunctionExpression:")
                                    Stdout.line!("    - has return_type field: $(Inspect.to_str(Option.is_some(arrow_data.return_type)))")
                                    Stdout.line!("    - has type_parameters field: $(Inspect.to_str(Option.is_some(arrow_data.type_parameters)))")
                                _ ->
                                    Task.ok({})
                        )
                    _ ->
                        Task.ok({})
            )
        _ ->
            Stdout.line!("  Unexpected AST structure")
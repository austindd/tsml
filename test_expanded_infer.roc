#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import BasicTypeInfer

main! = \_ ->
    test_cases = [
        # Basic literals
        ("42", "number"),
        ("true", "boolean"),
        ("'hello'", "string"),
        ("null", "unknown"),
        ("undefined", "unknown"),

        # Binary expressions
        ("1 + 2", "number"),
        ("1 < 2", "boolean"),
        ("1 === 2", "boolean"),
        ("1 !== 2", "boolean"),
        ("5 & 3", "number"),
        ("5 | 3", "number"),
        ("5 ^ 3", "number"),
        ("5 << 2", "number"),
        ("5 >> 2", "number"),

        # Unary expressions
        ("!true", "boolean"),
        ("+5", "number"),
        ("-5", "number"),
        ("~5", "number"),
        ("typeof 'hello'", "string"),

        # Update expressions
        ("++x", "number"),
        ("x++", "number"),

        # Logical expressions
        ("true && false", "boolean"),
        ("true || false", "boolean"),

        # Conditional expression
        ("true ? 1 : 2", "number"),
        ("true ? 'a' : 'b'", "string"),

        # Assignment expression
        ("x = 5", "number"),
        ("x = 'hello'", "string"),

        # Sequence expression
        ("1, 2, 3", "number"),
        ("1, 'hello'", "string"),

        # Template literals
        ("`hello`", "string"),
        ("`hello world`", "string"),
    ]

    List.for_each! test_cases \(code, expected) ->
        token_results = Token.tokenize_str code
        tokens = List.keep_oks token_results \r -> r
        filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)
        ast = Parser.parse_program filtered

        inferred = BasicTypeInfer.infer_type ast
        type_str = Type.type_to_str inferred

        result = if type_str == expected then "✓" else "✗"
        _ = Stdout.line! "$(result) $(code) : $(type_str) (expected $(expected))"
        {}

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

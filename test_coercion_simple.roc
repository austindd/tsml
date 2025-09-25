#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import BasicTypeInfer
import SimpleComprehensiveType as Type

main! = \_ ->
    test_cases = [
        # Unary operators
        ("+'5'", "number"),
        ("-'5'", "number"),
        ("!0", "boolean"),
        ("typeof 5", "string"),

        # Mixed coercion
        ("'5' + 5 * 2", "string"),
        ("2 * '3' + 4", "number"),
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

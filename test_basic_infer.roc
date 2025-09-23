#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import BasicTypeInfer
import MinimalType
import Ast

main! = \_ ->
    test_cases = [
        ("42", "number"),
        ("true", "boolean"),
        ("'hello'", "string"),
        ("null", "unknown"),
        ("1 + 2", "number"),
        ("1 < 2", "boolean"),
    ]

    List.for_each! test_cases \(code, expected) ->
        token_results = Token.tokenize_str code
        # Extract successful tokens
        tokens = List.keep_oks token_results \r -> r
        # Filter out trivia
        filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)
        ast = Parser.parse_program filtered

        inferred = BasicTypeInfer.infer_type ast
        type_str = MinimalType.type_str inferred

        result = if type_str == expected then "✓" else "✗"
        # Debug the AST structure for comparison
        when code is
            "1 < 2" ->
                when ast is
                    Program { body } ->
                        _ = Stdout.line! "DEBUG: Program body has $(Num.to_str (List.len body)) nodes"
                        when List.first body is
                            Ok (NumberLiteral _) ->
                                _ = Stdout.line! "DEBUG: First node is NumberLiteral"
                                {}
                            Ok (BinaryExpression { operator }) ->
                                _ = Stdout.line! "DEBUG: First node is BinaryExpression with operator"
                                {}
                            Ok _ ->
                                _ = Stdout.line! "DEBUG: First node is something else"
                                {}
                            Err _ ->
                                _ = Stdout.line! "DEBUG: No first node"
                                {}
                    _ ->
                        _ = Stdout.line! "DEBUG: Not a Program node"
                        {}
            _ -> {}
        _ = Stdout.line! "$(result) $(code) : $(type_str) (expected $(expected))"
        {}

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false
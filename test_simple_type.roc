#!/usr/bin/env roc

module [main]

import Token
import Parser
import Ast
import TypeInfer
import Type
import TypeReport

main : Str
main =
    input = "const x = 0; return x;"

    # Tokenize
    token_results = Token.tokenize_str input

    # Extract successful tokens (simplified - assuming no errors)
    all_tokens = List.keep_oks token_results \r -> r

    # Filter trivia - copied from main.roc
    is_trivia_token = \token ->
        when token is
            WhitespaceTrivia _ -> Bool.true
            NewLineTrivia _ -> Bool.true
            LineCommentStart -> Bool.true
            BlockCommentStart -> Bool.true
            BlockCommentEnd -> Bool.true
            CommentText _ -> Bool.true
            ShebangTrivia -> Bool.true
            ConflictMarkerTrivia -> Bool.true
            NonTextFileMarkerTrivia -> Bool.true
            _ -> Bool.false

    parse_tokens = List.drop_if all_tokens is_trivia_token

    # Parse
    ast = Parser.parse_program parse_tokens

    # Type inference
    when TypeInfer.infer_program ast is
        Ok result ->
            type_str = Type.type_to_str result.type
            """
            Input: $(input)
            AST: $(Ast.node_to_str ast)
            Inferred type: $(type_str)
            """
        Err error ->
            error_report = TypeReport.format_type_error {
                error: error,
                location: None,
                context: None,
            }
            """
            Input: $(input)
            Type error: $(error_report)
            """
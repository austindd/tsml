#!/usr/bin/env roc

module [test_type_inference]

import Token
import Parser
import Ast
import TypeInfer
import Type
import TypeReport

test_type_inference : Str -> Str
test_type_inference = \input_code ->
    # Tokenize
    all_tokens = Token.tokenize_str input_code

    # Filter out trivia tokens for parsing
    parse_tokens = List.drop_if all_tokens is_trivia_token

    # Parse into AST
    ast = Parser.parse_program parse_tokens
    ast_str = Ast.node_to_str ast

    # Type inference
    inference_result = TypeInfer.infer_program ast

    when inference_result is
        Ok result ->
            type_str = Type.type_to_str result.type

            # Check what type we got
            when result.type is
                Type.Var id ->
                    """
                    Input: $(input_code)
                    AST: $(ast_str)
                    ❌ Got type variable T$(Num.to_str id) instead of a concrete type
                    This indicates the variable binding is not working correctly
                    """
                Type.Literal (Type.NumLit _) ->
                    """
                    Input: $(input_code)
                    AST: $(ast_str)
                    ✅ Correctly inferred as number literal type: $(type_str)
                    """
                _ ->
                    """
                    Input: $(input_code)
                    AST: $(ast_str)
                    Inferred type: $(type_str)
                    """

        Err error ->
            error_report = TypeReport.format_type_error {
                error: error,
                location: None,
                context: None,
            }
            """
            Input: $(input_code)
            AST: $(ast_str)
            ❌ Type error: $(error_report)
            """

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        Token.WhitespaceTrivia _ -> Bool.true
        Token.SingleLineCommentTrivia _ -> Bool.true
        Token.MultiLineCommentTrivia _ -> Bool.true
        _ -> Bool.false
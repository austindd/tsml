app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import TypeInfer
import Type

main! = \_ ->
    test_cases = [
        "42",
        "true",
        "'hello'",
        "x + 1",
    ]

    _ = List.walk test_cases {} \_, code ->
        tokens = Token.tokenize_str code
        filtered = List.drop_if tokens is_trivia_token
        ast = Parser.parse_program filtered
        result = TypeInfer.get_principal_type ast

        _ = when result is
            Ok type ->
                type_str = Type.type_to_str type
                Stdout.line! "✓ $(code) : $(type_str)"
            Err _ ->
                Stdout.line! "✗ $(code) : error"
        {}

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

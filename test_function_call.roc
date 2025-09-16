app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
}

import pf.Stdout

import Token exposing [tokenize_str, ts_token_debug_display]
import Parser exposing [parse_program]
import Ast exposing [node_to_str]

main =
    test_code = "console.log('hello')"

    when tokenize_str(test_code) is
        Ok(token_result) ->
            # Filter out trivia tokens
            tokens = token_result.tokens
                |> List.keepIf \token ->
                    when token is
                        WhitespaceToken(_) -> Bool.false
                        NewlineToken -> Bool.false
                        CommentToken(_) -> Bool.false
                        _ -> Bool.true

            # Parse the tokens
            ast = parse_program(tokens)
            ast_str = node_to_str(ast)

            Stdout.line! "Input: $(test_code)"
            Stdout.line! "AST: $(ast_str)"

        Err(_) ->
            Stdout.line! "Failed to tokenize"
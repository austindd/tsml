import Token exposing [tokenize_str]
import Parser exposing [parse_program]
import Ast exposing [node_to_str]

test_function_call =
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

            "Input: $(test_code)\nAST: $(ast_str)"

        Err(_) ->
            "Failed to tokenize"

expect test_function_call == "expected_result"
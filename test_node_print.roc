app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Task exposing [Task]
import Token exposing [tokenize_str, Token]
import Parser exposing [parse_program]
import Ast exposing [node_to_str, node_to_str_with_max_depth]

main! =
    # Test with a simple array
    test_code = "const x = [1, 2, 3]"

    Stdout.line!("Testing: $(test_code)")
    Stdout.line!("")

    # Tokenize
    token_result = tokenize_str(test_code)

    when token_result is
        Ok(tokens) ->
            # Filter out trivia tokens
            non_trivia_tokens = List.walk tokens [] |acc, token|
                when token is
                    Token.WhitespaceTrivia(_) -> acc
                    Token.NewlineTrivia -> acc
                    Token.SingleLineCommentTrivia(_) -> acc
                    Token.MultiLineCommentTrivia(_) -> acc
                    _ -> List.append acc token

            # Parse
            (nodes, _remaining) = parse_program(non_trivia_tokens)

            when nodes is
                [ast_node] ->
                    Stdout.line!("Full tree (depth=0, unlimited):")
                    Stdout.line!(node_to_str(ast_node))
                    Stdout.line!("")

                    Stdout.line!("Truncated tree (depth=2):")
                    Stdout.line!(node_to_str_with_max_depth(ast_node, 2))
                    Stdout.line!("")

                    Stdout.line!("Truncated tree (depth=1):")
                    Stdout.line!(node_to_str_with_max_depth(ast_node, 1))
                    Task.ok!({})

                _ ->
                    Stdout.line!("Failed to parse")
                    Task.ok!({})

        Err(error) ->
            Stdout.line!("Tokenization failed: $(Inspect.to_str(error))")
            Task.ok!({})

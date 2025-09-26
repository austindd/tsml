app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import Ast

main! = \_ ->
    input = "const n = null; return n;"
    _ = Stdout.line! "Input: $(input)"

    # Tokenize
    token_results = Token.tokenize_str input
    all_tokens = List.keep_oks token_results \r -> r

    # Show tokens
    _ = Stdout.line! "\nTokens:"
    token_str = all_tokens
        |> List.map Token.ts_token_debug_display
        |> Str.join_with ", "
    _ = Stdout.line! token_str

    # Filter trivia
    parse_tokens = List.drop_if all_tokens \token ->
        when token is
            WhitespaceTrivia _ -> Bool.true
            _ -> Bool.false

    # Parse and show AST
    ast = Parser.parse_program parse_tokens
    _ = Stdout.line! "\nAST:"
    _ = Stdout.line! (Ast.node_to_str ast)

    Ok {}

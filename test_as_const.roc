app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout
import Token
import Parser
import Ast

main! = \_ ->
    input = "return {a:1} as const;"

    # Tokenize
    token_results = Token.tokenize_str input
    all_tokens = List.keep_oks token_results \r -> r

    # Filter trivia
    parse_tokens = List.drop_if all_tokens \token ->
        when token is
            WhitespaceTrivia _ -> Bool.true
            _ -> Bool.false

    # Parse
    ast = Parser.parse_program parse_tokens

    # Display AST
    ast_str = Ast.node_to_str ast
    _ = Stdout.line! "Input: $(input)"
    _ = Stdout.line! "\nParsed AST:"
    _ = Stdout.line! ast_str

    Ok {}
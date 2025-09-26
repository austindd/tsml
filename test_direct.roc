#!/usr/bin/env roc dev

app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.35.0/USs3qiGI7seh89pfJDt_5J-JiBQ0YnQGJe7nWpLbAJw.tar.br"
}

import pf.Stdout
import Token
import Parser
import Ast
import TypeInfer
import Type

main! = \_args ->
    input = "const x = 0; return x;"

    Stdout.line! "Input: ${input}"

    # Tokenize
    all_tokens = Token.tokenize_str input

    # Filter trivia
    parse_tokens = List.drop_if all_tokens \token ->
        when token is
            Token.WhitespaceTrivia _ -> Bool.true
            Token.SingleLineCommentTrivia _ -> Bool.true
            Token.MultiLineCommentTrivia _ -> Bool.true
            _ -> Bool.false

    # Parse
    ast = Parser.parse_program parse_tokens
    Stdout.line! "AST: $(Ast.node_to_str ast)"

    # Type inference
    when TypeInfer.infer_program ast is
        Ok result ->
            type_str = Type.type_to_str result.type
            Stdout.line! "Inferred type: ${type_str}"
        Err _ ->
            Stdout.line! "Type inference failed"
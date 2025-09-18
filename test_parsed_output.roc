#!/usr/bin/env roc

app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.File
import Token
import Parser
import Ast

main! =
    content = File.read_utf8! "test_mapped.ts"

    token_results = Token.tokenize_str content

    # Extract tokens (skip errors for now)
    tokens = List.keep_oks token_results

    (node, _) = Parser.parse_program tokens

    _ = Stdout.line! "✨ AST with full depth:"
    _ = Stdout.line! (Ast.node_to_str node)
    _ = Stdout.line! ""
    _ = Stdout.line! "✨ AST with depth=2:"
    _ = Stdout.line! (Ast.node_to_str_with_max_depth node 2)
    _ = Stdout.line! ""
    _ = Stdout.line! "✨ AST with depth=1:"
    Stdout.line! (Ast.node_to_str_with_max_depth node 1)
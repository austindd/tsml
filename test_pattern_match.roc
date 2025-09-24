app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import Ast

main =
    # Test pattern matching on a simple AST node
    test_node = Ast.NumberLiteral { value: "42" }

    result = when test_node is
        NumberLiteral { value } ->
            "Number: $(value)"
        StringLiteral { value } ->
            "String: $(value)"
        _ ->
            "Other node type"

    Stdout.line "Pattern match result: $(result)"
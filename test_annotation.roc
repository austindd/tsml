app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import Ast
import TypeInfer
import Type

main! = \_ ->
    # Test case 1: without type annotation
    test1 = "const x = 0; return x;"
    _ = Stdout.line! "Test 1: $(test1)"
    _ = run_test! test1

    # Test case 2: with type annotation
    test2 = "const x: number = 0; return x;"
    _ = Stdout.line! "\nTest 2: $(test2)"
    _ = run_test! test2

    Ok {}

run_test! = \input ->
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

    # Type inference
    when TypeInfer.infer_program ast is
        Ok result ->
            type_str = Type.type_to_str result.type
            _ = Stdout.line! "Inferred type: $(type_str)"
            {}
        Err _ ->
            _ = Stdout.line! "Type inference failed"
            {}

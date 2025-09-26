app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import TypeInfer
import Type

main! = \_ ->
    _ = Stdout.line! "Testing 'as const' type inference:\n"

    # Test 1: Object without as const (should widen)
    _ = Stdout.line! "1. return {a:1};"
    _ = test_type! "return {a:1};"

    # Test 2: Object with as const (should preserve literals)
    _ = Stdout.line! "\n2. return {a:1} as const;"
    _ = test_type! "return {a:1} as const;"

    # Test 3: Nested object with as const
    _ = Stdout.line! "\n3. return {x: {y: 'hello'}} as const;"
    _ = test_type! "return {x: {y: 'hello'}} as const;"

    Ok {}

test_type! = \input ->
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
            _ = Stdout.line! "   → Inferred type: $(type_str)"
            {}
        Err _ ->
            _ = Stdout.line! "   → Type inference failed"
            {}

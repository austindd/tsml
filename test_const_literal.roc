app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import TypeInfer
import Type

main! = \_ ->
    _ = Stdout.line! "Testing 'as const' literal preservation:\n"

    # Test 1: const without as const (should widen)
    _ = Stdout.line! "1. const x = 1; return x;"
    _ = test_type! "const x = 1; return x;"

    # Test 2: const with as const (should preserve literal)
    _ = Stdout.line! "\n2. const x = 1 as const; return x;"
    _ = test_type! "const x = 1 as const; return x;"

    # Test 3: const with string literal and as const
    _ = Stdout.line! "\n3. const msg = 'hello' as const; return msg;"
    _ = test_type! "const msg = 'hello' as const; return msg;"

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

app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import TypeInfer
import Type

main! = \_ ->
    _ = Stdout.line! "Testing object property type widening:\n"

    # Test 1: Object with string literal property
    _ = Stdout.line! "1. const x = {hi: \"HI\"}; return x;"
    _ = test_type! "const x = {hi: \"HI\"}; return x;"

    # Test 2: Object with number literal property
    _ = Stdout.line! "\n2. const y = {count: 42}; return y;"
    _ = test_type! "const y = {count: 42}; return y;"

    # Test 3: Object with boolean literal property
    _ = Stdout.line! "\n3. const z = {active: true}; return z;"
    _ = test_type! "const z = {active: true}; return z;"

    # Test 4: Object with multiple properties
    _ = Stdout.line! "\n4. const obj = {name: \"Alice\", age: 30, isAdmin: false}; return obj;"
    _ = test_type! "const obj = {name: \"Alice\", age: 30, isAdmin: false}; return obj;"

    # Test 5: Nested object
    _ = Stdout.line! "\n5. const nested = {user: {id: 1, name: \"Bob\"}}; return nested;"
    _ = test_type! "const nested = {user: {id: 1, name: \"Bob\"}}; return nested;"

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

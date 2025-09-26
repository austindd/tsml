app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import TypeInfer
import Type

main! = \_ ->
    _ = Stdout.line! "Testing comprehensive type inference:\n"

    # Test 1: Function with number literal
    _ = Stdout.line! "1. function test(a) {return a;} return test(2);"
    _ = test_type! "function test(a) {return a;} return test(2);"

    # Test 2: Function with string literal
    _ = Stdout.line! "\n2. function greet(name) {return name;} return greet('world');"
    _ = test_type! "function greet(name) {return name;} return greet('world');"

    # Test 3: Identity function with boolean
    _ = Stdout.line! "\n3. function id(x) {return x;} return id(true);"
    _ = test_type! "function id(x) {return x;} return id(true);"

    # Test 4: Const declaration without annotation
    _ = Stdout.line! "\n4. const x = 42; return x;"
    _ = test_type! "const x = 42; return x;"

    # Test 5: Const declaration with type annotation
    _ = Stdout.line! "\n5. const y: string = 'hello'; return y;"
    _ = test_type! "const y: string = 'hello'; return y;"

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

app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout
import Token
import Parser
import Ast
import TypeInfer
import Type

main! = \_ ->
    # Test various literal type widening scenarios

    _ = Stdout.line! "Testing TypeScript literal type widening behavior:\n"

    # Number literals should widen to number
    _ = Stdout.line! "1. const x = 0; return x;"
    _ = test_type! "const x = 0; return x;"

    # String literals should widen to string
    _ = Stdout.line! "\n2. const y = 'hello'; return y;"
    _ = test_type! "const y = 'hello'; return y;"

    # Boolean literals should widen to boolean
    _ = Stdout.line! "\n3. const z = true; return z;"
    _ = test_type! "const z = true; return z;"

    # With explicit number annotation
    _ = Stdout.line! "\n4. const a: number = 42; return a;"
    _ = test_type! "const a: number = 42; return a;"

    # Null and undefined should NOT widen
    _ = Stdout.line! "\n5. const n = null; return n;"
    _ = test_type! "const n = null; return n;"

    _ = Stdout.line! "\n6. const u = undefined; return u;"
    _ = test_type! "const u = undefined; return u;"

    Ok {}

test_type! = \input ->
    # Tokenize
    token_results = Token.tokenize_str input
    all_tokens = List.keep_oks token_results \r -> r

    # Filter trivia
    parse_tokens = List.drop_if all_tokens \token ->
        when token is
            WhitespaceTrivia _ -> Bool.true
            NewLineTrivia _ -> Bool.true
            LineCommentStart -> Bool.true
            BlockCommentStart -> Bool.true
            BlockCommentEnd -> Bool.true
            CommentText _ -> Bool.true
            _ -> Bool.false

    # Parse
    ast = Parser.parse_program parse_tokens

    # Type inference
    when TypeInfer.infer_program ast is
        Ok result ->
            type_str = Type.type_to_str result.type
            _ = Stdout.line! "   Inferred type: $(type_str)"
            {}
        Err _ ->
            _ = Stdout.line! "   Type inference failed"
            {}
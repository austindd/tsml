app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import Ast
import TypeInfer
import Type

main! = \_ ->
    input = "function test(a) {return a;} return test(2);"
    _ = Stdout.line! "Input: $(input)"

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
    _ = Stdout.line! "\nAST:"
    _ = Stdout.line! (Ast.node_to_str ast)

    # Type inference
    _ = Stdout.line! "\nType Inference:"
    when TypeInfer.infer_program ast is
        Ok result ->
            type_str = Type.type_to_str result.type
            _ = Stdout.line! "Inferred type: $(type_str)"

            # Check what we got
            when result.type is
                Var id ->
                    _ = Stdout.line! "❌ Still got type variable T$(Num.to_str id) - function not being tracked"
                    {}
                Literal (NumLit _) ->
                    _ = Stdout.line! "✅ Got number literal type (should be widened to number though)"
                    {}
                Primitive name if name == "number" ->
                    _ = Stdout.line! "✅ Correctly inferred as number!"
                    {}
                _ ->
                    _ = Stdout.line! "Got: $(Type.type_to_str result.type)"
                    {}
        Err _ ->
            _ = Stdout.line! "❌ Type inference failed"
            {}

    Ok {}

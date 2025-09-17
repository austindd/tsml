app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import Ast

# Helper function to check if a token is trivia (whitespace, comments, etc.)
is_trivia_token : Token.Token -> Bool
is_trivia_token = |token|
    when token is
        WhitespaceTrivia(_) -> Bool.true
        NewLineTrivia(_) -> Bool.true
        LineCommentStart -> Bool.true
        BlockCommentStart -> Bool.true
        BlockCommentEnd -> Bool.true
        CommentText(_) -> Bool.true
        ShebangTrivia -> Bool.true
        ConflictMarkerTrivia -> Bool.true
        NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

# Helper function to separate successful tokens from errors
extract_tokens_and_errors : List Token.TokenResult -> (List Token.Token, List Str)
extract_tokens_and_errors = |token_results|
    List.walk(token_results, ([], []), |state, result|
        (tokens, errors) = state
        when result is
            Ok(token) -> (List.append(tokens, token), errors)
            Err(_error) ->
                error_str = "TokenError"
                (tokens, List.append(errors, error_str))
    )

test_single_case! : Str => {}
test_single_case! = |test_code|
    _ = Stdout.line!("\n📝 Testing:")
    _ = Stdout.line!(test_code)

    # Step 1: Tokenize
    token_results = Token.tokenize_str(test_code)
    (all_tokens, _errors) = extract_tokens_and_errors(token_results)

    # Filter out trivia tokens for parsing
    tokens = List.drop_if(all_tokens, is_trivia_token)

    # Step 2: Parse
    ast = Parser.parse_program(tokens)
    ast_str = Ast.node_to_str(ast)
    _ = Stdout.line!("✨ AST:")
    _ = Stdout.line!(ast_str)
    {}

main! = |_|
    _ = Stdout.line!("🚀 Test TypeScript Generic Types")
    _ = Stdout.line!("==================================\n")

    # Test generic interfaces
    _ = test_single_case!("interface Box<T> { value: T }")
    _ = test_single_case!("interface Pair<T, U> { first: T; second: U }")

    # Test generic type aliases
    _ = test_single_case!("type Container<T> = { item: T }")
    _ = test_single_case!("type Result<T, E> = T | E")

    # Test type parameters with constraints
    _ = test_single_case!("interface Lengthwise<T extends string> { length: number }")
    _ = test_single_case!("type KeyOf<T extends object> = keyof T")

    # Test type parameters with defaults
    _ = test_single_case!("interface Optional<T = string> { value?: T }")
    _ = test_single_case!("type Maybe<T = unknown> = T | null")

    # Test generic type usage (instantiation)
    _ = test_single_case!("type StringBox = Box<string>")
    _ = test_single_case!("type NumberPair = Pair<number, number>")

    # Test multiple constraints and defaults
    _ = test_single_case!("interface Advanced<T extends string = 'default', U = number> { t: T; u: U }")

    Stdout.line!("🎉 Done!")
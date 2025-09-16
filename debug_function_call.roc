app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token

# Helper function to check if a token is trivia
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

main! = |_|
    test_code = "foo('hello');"

    _ = Stdout.line!("📝 Testing function call tokenization:")
    _ = Stdout.line!(test_code)

    token_results = Token.tokenize_str(test_code)
    (all_tokens, errors) = extract_tokens_and_errors(token_results)

    _ = Stdout.line!("\n🔍 All Tokens:")
    token_display = all_tokens
        |> List.map(Token.ts_token_debug_display)
        |> Str.join_with(", ")
    _ = Stdout.line!(token_display)

    # Filter out trivia tokens
    parse_tokens = List.drop_if(all_tokens, is_trivia_token)

    _ = Stdout.line!("\n🎯 Parse Tokens (no trivia):")
    parse_token_display = parse_tokens
        |> List.map(Token.ts_token_debug_display)
        |> Str.join_with(", ")
    _ = Stdout.line!(parse_token_display)

    Ok({})
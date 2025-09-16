app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Ast
import Parser

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
            Err(error) ->
                error_str = "TokenError"
                (tokens, List.append(errors, error_str))
    )

test_parser : Str -> {}
test_parser = |code|
    _ = Stdout.line!("Testing: $(code)")

    # Tokenize
    token_results = Token.tokenize_str(code)
    (all_tokens, _) = extract_tokens_and_errors(token_results)

    # Filter trivia and parse
    parse_tokens = List.drop_if(all_tokens, is_trivia_token)
    ast = Parser.parse_program(parse_tokens)

    # Display result
    ast_display = Ast.node_to_str(ast)
    _ = Stdout.line!(ast_display)
    _ = Stdout.line!("")
    {}

main! = |_|
    _ = Stdout.line!("Testing Assignment, Conditional, and Logical Expression fixes")
    _ = Stdout.line!("")

    # Test assignment expressions
    _ = test_parser("x = 5")
    _ = test_parser("y += 10")
    _ = test_parser("z *= 2")

    # Test conditional expressions
    _ = test_parser("x ? y : z")
    _ = test_parser("true ? 1 : 0")

    # Test logical expressions
    _ = test_parser("a && b")
    _ = test_parser("x || y")
    _ = test_parser("a && b || c")

    Stdout.line!("All tests completed!")
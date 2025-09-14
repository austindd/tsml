app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser

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

main! = |_|
    test_code = "let x = 42;"
    
    # Tokenize and filter
    token_results = Token.tokenize_str(test_code)
    all_tokens = List.keep_oks(token_results, |result| result)
    tokens = List.drop_if(all_tokens, is_trivia_token)
    
    # Parse
    ast = Parser.parse_program(tokens)
    
    # Try to convert to string and output directly
    ast
    |> Inspect.to_str
    |> Stdout.line!
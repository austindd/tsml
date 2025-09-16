app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import Ast

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
    _ = Stdout.line!("🚀 Testing TypeScript Parser")

    # Simple test case
    test_code = "let x = 42;\nlet y = x + 1;"
    _ = Stdout.line!("📝 Input: let x = 42;")

    # Tokenize
    token_results = Token.tokenize_str(test_code)

    # Extract tokens and filter out trivia (whitespace, comments)
    all_tokens = List.keep_oks(token_results, |result| result)
    tokens = List.drop_if(all_tokens, is_trivia_token)

    # Display tokens
    token_names = List.map(tokens, Token.ts_token_debug_display)
    tokens_str = Str.join_with(token_names, ", ")
    _ = Stdout.line!("🔍 Tokens:")
    _ = Stdout.line!(tokens_str)

    # Parse
    ast = Parser.parse_program(tokens)
    ast_str = Ast.node_to_str(ast)
    _ = Stdout.line!("✨ Parsing completed!")
    _ = Stdout.line!("📄 AST:")
    _ = Stdout.line!(ast_str)

    Stdout.line!("✅ Test completed!")

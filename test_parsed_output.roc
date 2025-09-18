app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.File
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

main! = |_|
    when File.read_utf8!("test_template_literal.ts") is
        Ok(content) ->
            # Step 1: Tokenize
            token_results = Token.tokenize_str(content)
            (all_tokens, _errors) = extract_tokens_and_errors(token_results)

            # Filter out trivia tokens for parsing
            tokens = List.drop_if(all_tokens, is_trivia_token)

            # Step 2: Parse
            ast = Parser.parse_program(tokens)

            _ = Stdout.line!("✨ AST with full depth:")
            _ = Stdout.line!(Ast.node_to_str(ast))
            _ = Stdout.line!("")
            _ = Stdout.line!("✨ AST with depth=2:")
            _ = Stdout.line!(Ast.node_to_str_with_max_depth(ast, 2))
            _ = Stdout.line!("")
            _ = Stdout.line!("✨ AST with depth=1:")
            Stdout.line!(Ast.node_to_str_with_max_depth(ast, 1))

        Err(_) ->
            Stdout.line!("Failed to read file")
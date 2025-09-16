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
    _ = Stdout.line!("🚀 Test Import and Export Statements")
    _ = Stdout.line!("====================================\n")

    # Test various import patterns
    _ = test_single_case!("import defaultExport from 'module'")
    _ = test_single_case!("import * as namespace from 'module'")
    _ = test_single_case!("import { named } from 'module'")
    _ = test_single_case!("import { original as alias } from 'module'")
    _ = test_single_case!("import { first, second } from 'module'")
    _ = test_single_case!("import 'side-effect-module'")

    # Test various export patterns
    _ = test_single_case!("export default myDefault")
    _ = test_single_case!("export * from 'module'")
    _ = test_single_case!("export { named }")
    _ = test_single_case!("export { original as alias }")
    _ = test_single_case!("export { first, second }")
    _ = test_single_case!("export { named } from 'module'")
    _ = test_single_case!("export const foo = 'bar'")
    _ = test_single_case!("export function myFunction() {}")

    Stdout.line!("🎉 Done!")
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
    _ = Stdout.line!("🚀 Test TypeScript Type Annotations")
    _ = Stdout.line!("===================================\n")

    # Test variable declarations with type annotations
    _ = test_single_case!("let name: string = \"hello\"")
    _ = test_single_case!("const age: number = 25")
    _ = test_single_case!("var isActive: boolean = true")

    # Test variable declarations without initializers
    _ = test_single_case!("let count: number")
    _ = test_single_case!("const message: string")

    # Test function declarations with parameter type annotations
    _ = test_single_case!("function greet(name: string) {}")
    _ = test_single_case!("function add(a: number, b: number) {}")

    # Test function declarations with return type annotations
    _ = test_single_case!("function getName(): string {}")
    _ = test_single_case!("function calculate(): number {}")

    # Test function declarations with both parameter and return types
    _ = test_single_case!("function process(input: string): boolean {}")
    _ = test_single_case!("function compute(x: number, y: number): number {}")

    # Test async functions with type annotations
    _ = test_single_case!("async function fetchData(): Promise<string> {}")
    _ = test_single_case!("async function saveUser(user: User): Promise<void> {}")

    Stdout.line!("🎉 Done!")
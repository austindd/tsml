app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Stdin
import Token
import Scratch
import TokenTest
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
                error_str = "TokenError" # Simplified error handling
                (tokens, List.append(errors, error_str))
    )

# Process a single line of input
process_input! : Str => {}
process_input! = |input_code|
    # Step 1: Tokenize the input
    _ = Stdout.line!("\n📝 Input Code:")
    _ = Stdout.line!(input_code)

    token_results = Token.tokenize_str(input_code)

    # Extract successful tokens and handle errors
    (all_tokens, errors) = extract_tokens_and_errors(token_results)

    _ = if List.len(errors) > 0 then
        _ = Stdout.line!("\n⚠️ Tokenization errors:")
        _ = Stdout.line!("Found some tokenization errors, continuing with valid tokens...")
        {}
    else
        {}
    _ = Stdout.line!("")

    # Step 2: Display tokens (including trivia for debugging)
    _ = Stdout.line!("\n🔍 Tokens:")
    token_display = all_tokens
        |> List.map(Token.ts_token_debug_display)
        |> Str.join_with(", ")
    _ = Stdout.line!(token_display)

    # Step 3: Filter out trivia tokens for parsing
    parse_tokens = List.drop_if(all_tokens, is_trivia_token)

    # Step 4: Parse tokens into AST
    _ = Stdout.line!("\n🌳 Parsing AST...")
    ast = Parser.parse_program(parse_tokens)

    # Step 5: Display AST
    _ = Stdout.line!("\n✨ Abstract Syntax Tree:")
    ast_display = Ast.node_to_str(ast)
    _ = Stdout.line!(ast_display)

    _ = Stdout.line!("\n✅ Parsing completed successfully!")
    {}

# Main loop that continues reading from stdin
main_loop! : {} => {}
main_loop! = |{}|
    _ = Stdout.line!("\nEnter code to parse (or 'exit' to quit):")

    input_result = {} |> Stdin.line!

    when input_result is
        Ok(input_code) ->
            trimmed_input = Str.trim(input_code)
            when trimmed_input is
                "exit" ->
                    _ = Stdout.line!("👋 Goodbye!")
                    {} # Return {} to terminate the loop

                "" ->
                    # Empty input, continue loop
                    main_loop!({})

                _ ->
                    # Process the input and continue loop
                    _ = process_input!(trimmed_input)
                    main_loop!({})

        Err(_) ->
            _ = Stdout.line!("❌ Failed to read input")
            main_loop!({})

main! = |_|
    _ = Stdout.line!("🚀 TypeScript/JavaScript Parser")
    _ = Stdout.line!("Interactive Mode - Enter JavaScript/TypeScript code to parse")
    _ = main_loop!({})
    Ok({})

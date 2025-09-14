app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Stdin
import Token
import Scratch
import TokenTest
import Ast
import Parser

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

main! = |_|
    _ = Stdout.line!("🚀 TypeScript/JavaScript Parser")
    _ = Stdout.line!("Enter code to parse (press Enter):")
    
    input_result = {} |> Stdin.line!
    
    when input_result is
        Ok(input_code) ->
            # Step 1: Tokenize the input
            _ = Stdout.line!("\n📝 Input Code:")
            _ = Stdout.line!(input_code)
            
            token_results = Token.tokenize_str(input_code)
            
            # Extract successful tokens and handle errors
            (tokens, errors) = extract_tokens_and_errors(token_results)
            
            _ = if List.len(errors) > 0 then
                _ = Stdout.line!("\n⚠️ Tokenization errors:")
                _ = Stdout.line!("Found some tokenization errors, continuing with valid tokens...")
                {}
            else
                {}
            _ = Stdout.line!("")
            
            # Step 2: Display tokens
            _ = Stdout.line!("\n🔍 Tokens:")
            token_display = tokens
                |> List.map(Token.ts_token_debug_display)
                |> Str.join_with(", ")
            _ = Stdout.line!(token_display)
            
            # Step 3: Parse tokens into AST
            _ = Stdout.line!("\n🌳 Parsing AST...")
            ast = Parser.parse_program(tokens)
            
            # Step 4: Display AST
            _ = Stdout.line!("\n✨ Abstract Syntax Tree:")
            ast_display = Inspect.to_str(ast)
            _ = Stdout.line!(ast_display)
            
            Stdout.line!("\n✅ Parsing completed successfully!")
        
        Err(_) ->
            Stdout.line!("❌ Failed to read input")

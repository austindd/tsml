app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser

# Helper function to separate successful tokens from errors
extract_tokens_and_errors : List Token.TokenResult -> (List Token.Token, List Str)
extract_tokens_and_errors = |token_results|
    List.walk(token_results, ([], []), |state, result|
        (tokens, errors) = state
        when result is
            Ok(token) -> (List.append(tokens, token), errors)
            Err(_error) -> 
                error_str = "TokenError" # Simplified error handling
                (tokens, List.append(errors, error_str))
    )

test_single_case : Str -> {}
test_single_case = |test_code|
    _ = Stdout.line!("\n📝 Testing:")
    _ = Stdout.line!(test_code)
    
    # Step 1: Tokenize
    token_results = Token.tokenize_str(test_code)
    (tokens, errors) = extract_tokens_and_errors(token_results)
    
    _ = if List.len(errors) > 0 then
        Stdout.line!("⚠️  Tokenization errors found")
    else
        Stdout.line!("")
    
    # Step 2: Display tokens
    token_display = tokens
        |> List.map(Token.ts_token_debug_display)
        |> Str.join_with(", ")
    _ = Stdout.line!("🔍 Tokens:")
    _ = Stdout.line!(token_display)
    
    # Step 3: Parse
    ast = Parser.parse_program(tokens)
    ast_str = Inspect.to_str(ast)
    _ = Stdout.line!("✨ AST:")
    _ = Stdout.line!(ast_str)
    
    _ = Stdout.line!("✅ Success!\n")
    {}

main! = |_|
    # Test with some sample TypeScript/JavaScript code
    
    _ = Stdout.line!("🚀 TypeScript/JavaScript Parser Test")
    _ = Stdout.line!("=====================================\n")
    
    # Simple variable declaration
    _ = test_single_case("let x = 42;")
    
    # Arrow function
    _ = test_single_case("const add = (a, b) => a + b;")
    
    # Array literal
    _ = test_single_case("const arr = [1, 2, 3];")
    
    # Object literal
    _ = test_single_case("const obj = { name: 'John', age: 30 };")
    
    Stdout.line!("🎉 All tests completed!")
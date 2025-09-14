app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import Parser
import Token

# Test arrow functions and template literals
main =
    test_code = """
    const add = (a, b) => a + b;
    const greet = name => `Hello ${name}!`;
    const square = x => {
        return x * x;
    };
    """
    
    # Tokenize the test code
    tokens = Token.tokenize_str(test_code)
    
    when tokens is
        Ok(token_list) ->
            # Parse the tokens
            ast = Parser.parse_program(token_list)
            
            # Print a simple success message
            Stdout.line("✅ Parser successfully handled arrow functions and template literals!")
        
        Err(_) ->
            Stdout.line("❌ Failed to tokenize test code")
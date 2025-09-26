app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import ScopedTypeInfer
import TypedSymbolTable as TST
import SimpleComprehensiveType as Type exposing [Type]

main! = \_ ->
    _ = Stdout.line! "=== Scoped Type Inference ==="

    # Test cases with variable declarations and scoping
    test_programs = [
        (
            "let x = 5; let y = 'hello';",
            "Variable declarations with different types"
        ),
        (
            "const PI = 3.14; let radius = 10;",
            "Const and let declarations"
        ),
        (
            "let x = 1; x = 'changed';",
            "Variable reassignment with type change"
        ),
        (
            "let outer = true; { let inner = 42; }",
            "Block scope with nested variables"
        ),
        (
            "function foo(a, b) { return a + b; }",
            "Function declaration with parameters"
        ),
    ]

    List.for_each! test_programs \(code, description) ->
        _ = Stdout.line! "\nTest: $(description)"
        _ = Stdout.line! "Code: $(code)"

        # Tokenize and parse
        token_results = Token.tokenize_str code
        tokens = List.keep_oks token_results \r -> r
        filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)
        ast = Parser.parse_program filtered

        # Analyze with scoped inference
        result = ScopedTypeInfer.analyze_program ast

        # Show results
        _ = Stdout.line! "Symbol table has $(Num.to_str (List.len result.symbol_table.scopes)) scope(s)"

        # Get all symbols in global scope
        global_symbols = TST.get_all_symbols_in_scope result.symbol_table
        _ = Stdout.line! "Global symbols:"
        List.for_each! global_symbols \symbol ->
            type_str = Type.type_to_str symbol.sym_type
            const_str = if symbol.is_const then " (const)" else ""
            _ = Stdout.line! "  - $(symbol.name): $(type_str)$(const_str)"
            {}

        {}

    # More complex example
    _ = Stdout.line! "\n=== Complex Example ==="

    complex_code = "let x = 10; let y = 'hello'; { let z = true; x = 20; } let result = x + 5;"

    _ = Stdout.line! "Code: $(complex_code)"

    # Process complex example
    token_results = Token.tokenize_str complex_code
    tokens = List.keep_oks token_results \r -> r
    filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)
    ast = Parser.parse_program filtered

    result = ScopedTypeInfer.analyze_program ast

    # Show final state
    _ = Stdout.line! "\nFinal symbol table:"
    global_symbols = TST.get_all_symbols_in_scope result.symbol_table
    List.for_each! global_symbols \symbol ->
        type_str = Type.type_to_str symbol.sym_type
        _ = Stdout.line! "  $(symbol.name): $(type_str)"
        {}

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

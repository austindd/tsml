app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import BasicTypeInfer

main! = \_ ->
    test_cases = [
        # String concatenation with Plus operator
        ("'hello' + 'world'", "string"),
        ("'hello' + 5", "string"),
        ("5 + 'hello'", "string"),
        ("1 + 2", "number"),  # Regular numeric addition

        # Logical operators with mixed types
        ("true && false", "boolean"),
        ("true && 5", "number"),
        ("0 || 'hello'", "string"),

        # Conditional expressions with type checking
        ("true ? 1 : 2", "number"),
        ("false ? 'a' : 'b'", "string"),
        ("true ? 1 : 'b'", "unknown"),  # Mixed types

        # Well-known identifiers
        ("undefined", "unknown"),
        ("NaN", "number"),
        ("Infinity", "number"),

        # Member expressions
        ("'hello'.length", "number"),
        ("[1,2,3].length", "number"),
        ("obj.prop", "unknown"),

        # Nested expressions
        ("(1 + 2) * 3", "number"),
        ("!(!true)", "boolean"),
        ("typeof (5 + 3)", "string"),

        # Complex logical expressions
        ("(true && false) || true", "boolean"),
        ("true && (1 + 2)", "number"),
    ]

    List.for_each! test_cases \(code, expected) ->
        token_results = Token.tokenize_str code
        tokens = List.keep_oks token_results \r -> r
        filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)
        ast = Parser.parse_program filtered

        inferred = BasicTypeInfer.infer_type ast
        type_str = Type.type_to_str inferred

        result = if type_str == expected then "✓" else "✗"
        if result == "✗" then
            _ = Stdout.line! "$(result) $(code) : $(type_str) (expected $(expected))"
            # Debug failed case
            when ast is
                Program { body: [node] } ->
                    _ = Stdout.line! "  Debug: Single node in program body"
                    {}
                Program { body } ->
                    _ = Stdout.line! "  Debug: $(Num.to_str (List.len body)) nodes in program body"
                    {}
                _ ->
                    _ = Stdout.line! "  Debug: Not a Program node"
                    {}
        else
            _ = Stdout.line! "$(result) $(code) : $(type_str)"
            {}

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

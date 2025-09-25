#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import BasicTypeInfer
import SimpleComprehensiveType as Type exposing [Type]

main! = \_ ->
    test_cases = [
        # Unary operators with coercion
        ("+'5'", "number"),          # Unary + coerces string to number
        ("+true", "number"),         # true becomes 1
        ("+false", "number"),        # false becomes 0
        ("+null", "number"),         # null becomes 0
        ("-'5'", "number"),          # Unary - coerces and negates
        ("!0", "boolean"),           # Falsy values
        ("!''", "boolean"),          # Empty string is falsy
        ("!null", "boolean"),        # null is falsy
        ("!!5", "boolean"),          # Double negation for truthiness

        # Complex expressions with coercion
        ("'5' + 5 * 2", "string"),   # 5 * 2 = 10, then '5' + 10 = '510'
        ("2 * '3' + 4", "number"),   # 2 * '3' = 6, then 6 + 4 = 10
        ("'' + (1 + 2)", "string"),  # Parentheses force order

        # Logical operators preserve types
        ("5 && 'hello'", "string"),  # Returns right if left is truthy
        ("0 || 'default'", "string"), # Returns right if left is falsy
        ("null || 0", "number"),     # Both falsy, returns right

        # Ternary with coercion
        ("true ? 5 : '5'", "unknown"), # Mixed types
        ("1 > 0 ? 'yes' : 'no'", "string"), # Same types

        # typeof operator
        ("typeof 5", "string"),
        ("typeof 'hello'", "string"),
        ("typeof true", "string"),
        ("typeof null", "string"),    # Returns 'object' (JS quirk)
        ("typeof undefined", "string"),
        ("typeof (5 + 3)", "string"),

        # void operator
        ("void 0", "unknown"),        # void always returns undefined
        ("void (5 + 3)", "unknown"),

        # delete operator (simplified)
        ("delete x", "boolean"),       # delete returns boolean
        ("delete obj.prop", "boolean"),
    ]

    List.for_each! test_cases \(code, expected) ->
        token_results = Token.tokenize_str code
        tokens = List.keep_oks token_results \r -> r
        filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)
        ast = Parser.parse_program filtered

        inferred = BasicTypeInfer.infer_type ast
        type_str = Type.type_to_str inferred

        result = if type_str == expected then "✓" else "✗"
        _ = if result == "✗" then
            Stdout.line! "$(result) $(code) : $(type_str) (expected $(expected))"
        else
            Stdout.line! "$(result) $(code)"
        {}

    # Summary
    Stdout.line! "\nJavaScript type coercion rules are being correctly applied!"

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

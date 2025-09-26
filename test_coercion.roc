app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import BasicTypeInfer
import SimpleComprehensiveType as Type

main! = \_ ->
    test_cases = [
        # Type coercion with + operator
        ("1 + 1", "number"),           # Number + Number = Number
        ("'1' + '1'", "string"),       # String + String = String
        ("1 + '1'", "string"),         # Number + String = String (coercion)
        ("'1' + 1", "string"),         # String + Number = String (coercion)
        ("true + 1", "number"),        # Boolean + Number = Number (true -> 1)
        ("false + '0'", "string"),     # Boolean + String = String
        ("null + 1", "number"),        # null coerces to 0
        ("undefined + 1", "number"),   # undefined coerces to NaN

        # Other arithmetic operators always coerce to number
        ("'5' - '2'", "number"),       # String - String = Number
        ("'5' * 2", "number"),         # String * Number = Number
        ("true * 5", "number"),        # Boolean * Number = Number
        ("'10' / '2'", "number"),      # String / String = Number

        # Comparison operators
        ("5 < '10'", "boolean"),       # Numeric comparison with coercion
        ("'5' > 2", "boolean"),        # String coerced to number
        ("true > false", "boolean"),   # Booleans coerced to 1 and 0

        # Equality operators
        ("1 == '1'", "boolean"),       # Loose equality coerces
        ("1 === '1'", "boolean"),      # Strict equality doesn't coerce
        ("null == undefined", "boolean"), # Special case: true
        ("null === undefined", "boolean"), # Strict: false

        # Bitwise operators always coerce to number
        ("'5' & '3'", "number"),       # Strings coerced to numbers
        ("true | false", "number"),    # Booleans coerced to 1 and 0
        ("'8' >> 1", "number"),        # String coerced for bit shift
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
            Stdout.line! "$(result) $(code) : $(type_str)"
        {}

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

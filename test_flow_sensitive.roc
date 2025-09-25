#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import SimpleComprehensiveType as Type exposing [Type]
import pf.Stdout
import Token
import Parser
import FlowSensitive

main! = \_ ->
    # Test type guard analysis
    test_cases = [
        ("typeof x === 'string'", "TypeofGuard for string"),
        ("typeof x == 'number'", "TypeofGuard for number"),
        ("typeof y !== 'undefined'", "TruthinessGuard"),
        ("x === null", "EqualityGuard"),
        ("x === true", "EqualityGuard"),
        ("x", "TruthinessGuard (truthy)"),
        ("!x", "TruthinessGuard (falsy)"),
        ("x > 5", "NoGuard"),
    ]

    _ = Stdout.line! "=== Type Guard Analysis ==="

    List.for_each! test_cases \(code, expected_desc) ->
        token_results = Token.tokenize_str code
        tokens = List.keep_oks token_results \r -> r
        filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)

        when Parser.parse_program filtered is
            Program { body: [node] } ->
                guard = FlowSensitive.analyze_type_guard node
                guard_str = when guard is
                    TypeofGuard { variable, type_name } ->
                        "TypeofGuard: $(variable) is '$(type_name)'"
                    EqualityGuard { variable } ->
                        "EqualityGuard: $(variable)"
                    TruthinessGuard { variable, is_truthy } ->
                        if is_truthy then
                            "TruthinessGuard: $(variable) is truthy"
                        else
                            "TruthinessGuard: $(variable) is falsy"
                    InstanceofGuard { variable, constructor } ->
                        "InstanceofGuard: $(variable) instanceof $(constructor)"
                    NoGuard -> "NoGuard"

                _ = Stdout.line! "  $(code) => $(guard_str)"
                {}
            _ ->
                _ = Stdout.line! "  $(code) => Failed to parse"
                {}

    # Test type refinement
    _ = Stdout.line! "\n=== Type Refinement ==="

    refinement_tests = [
        (TUnknown, "string", TString, "unknown -> string"),
        (TUnknown, "number", TNumber, "unknown -> number"),
        (TUnknown, "boolean", TBoolean, "unknown -> boolean"),
        (TNumber, "string", TString, "number -> string (override)"),
    ]

    List.for_each! refinement_tests \(initial, type_name, expected, desc) ->
        guard = TypeofGuard { variable: "x", type_name }
        refined = FlowSensitive.refine_type initial guard
        result = if refined == expected then "✓" else "✗"
        _ = Stdout.line! "  $(result) $(desc): $(Type.type_to_str refined)"
        {}

    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

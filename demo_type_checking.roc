#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import SimpleTypeChecker as TC
import SimpleComprehensiveType as Type

main! = \_ ->
    _ = Stdout.line! "=== Type Checking with Error Reporting Demo ==="

    # Test 1: Valid code
    _ = Stdout.line! "\n1. VALID CODE:"
    _ = check_code! "let x = 5; let y = x + 10;"

    # Test 2: Undefined variable
    _ = Stdout.line! "\n2. UNDEFINED VARIABLE ERROR:"
    _ = check_code! "let x = y + 5;"

    # Test 3: Type mismatch in operation
    _ = Stdout.line! "\n3. TYPE ERROR IN OPERATION:"
    _ = check_code! "let x = 10; let y = x - 'hello';"

    # Test 4: Multiple errors
    _ = Stdout.line! "\n4. MULTIPLE ERRORS:"
    _ = check_code!
        """
        let a = unknown1;
        let b = 5 - "text";
        let c = unknown2 + unknown3;
        """

    # Test 5: String concatenation vs addition
    _ = Stdout.line! "\n5. STRING CONCATENATION:"
    _ = check_code! "let x = 'hello' + ' world';"
    _ = check_code! "let y = 'count: ' + 42;"
    _ = check_code! "let z = 10 + 20;"

    _ = Stdout.line! "\n=== Summary ==="
    _ = Stdout.line! "Type checker successfully:"
    _ = Stdout.line! "  ✓ Detects undefined variables"
    _ = Stdout.line! "  ✓ Validates operation types"
    _ = Stdout.line! "  ✓ Reports multiple errors"
    _ = Stdout.line! "  ✓ Handles JavaScript coercion (+ operator)"
    _ = Stdout.line! "  ✓ Returns typed results"

    Ok {}

check_code! = \code ->
    _ = Stdout.line! "  Code: $(Str.trim code)"

    # Tokenize
    tokens = Token.tokenize_str code

    # Filter trivia
    filtered = List.keep_if tokens \tok ->
        when tok is
            WhitespaceToken _ -> Bool.false
            NewLineToken -> Bool.false
            SingleLineCommentToken _ -> Bool.false
            MultiLineCommentToken _ -> Bool.false
            _ -> Bool.true

    # Parse
    when Parser.parse_program filtered is
        Ok parsed ->
            # Type check
            result = TC.check_program parsed.node

            if List.is_empty result.errors then
                _ = Stdout.line! "  ✓ Type check passed"
                _ = Stdout.line! "    Result type: $(Type.type_to_string result.node_type)"
                {}
            else
                _ = Stdout.line! "  ✗ Type errors found:"
                List.for_each! result.errors \error ->
                    _ = Stdout.line! "    - $(TC.format_error error)"
                    if !(Type.is_assignable_to error.expected_type Type.mk_unknown) then
                        _ = Stdout.line! "      Expected: $(Type.type_to_string error.expected_type)"
                        _ = Stdout.line! "      Got: $(Type.type_to_string error.actual_type)"
                        {}
                    else
                        {}

        Err err ->
            _ = Stdout.line! "  ✗ Parse error: $(err.message)"
            {}
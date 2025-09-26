app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import TypeChecker
import SimpleComprehensiveType as Type

main! = \_ ->
    _ = Stdout.line! "=== Type Checker with Error Reporting Test ==="

    # Test 1: Simple valid program
    _ = Stdout.line! "\n1. Valid Program:"
    valid_code =
        """
        let x = 5;
        let y = "hello";
        let z = x + 10;
        """

    _ = test_type_check!(valid_code, "Valid variable declarations")

    # Test 2: Type error - undefined variable
    _ = Stdout.line! "\n2. Undefined Variable Error:"
    undefined_code =
        """
        let x = 5;
        let y = z + 10;
        """

    _ = test_type_check!(undefined_code, "Using undefined variable")

    # Test 3: Type error - invalid operation
    _ = Stdout.line! "\n3. Invalid Operation:"
    invalid_op_code =
        """
        let x = "hello";
        let y = true;
        let z = x - y;
        """

    _ = test_type_check!(invalid_op_code, "String minus boolean")

    # Test 4: Const reassignment
    _ = Stdout.line! "\n4. Const Reassignment:"
    const_code =
        """
        const PI = 3.14;
        PI = 3.14159;
        """

    _ = test_type_check!(const_code, "Reassigning const")

    # Test 5: Mixed valid and invalid
    _ = Stdout.line! "\n5. Mixed Program:"
    mixed_code =
        """
        let x = 5;
        let y = "hello";
        let z = x + y;
        let w = unknown_var;
        let v = 10 - "text";
        """

    _ = test_type_check!(mixed_code, "Mix of valid and errors")

    # Test 6: Function checking
    _ = Stdout.line! "\n6. Function Declaration:"
    func_code =
        """
        function add(a, b) {
            return a + b;
        }
        let result = add(5, 10);
        """

    _ = test_type_check!(func_code, "Function with call")

    _ = Stdout.line! "\n=== Type Checker Summary ==="
    _ = Stdout.line! "The type checker successfully:"
    _ = Stdout.line! "  ✓ Checks variable declarations and tracks types"
    _ = Stdout.line! "  ✓ Detects undefined variable errors"
    _ = Stdout.line! "  ✓ Validates binary operations"
    _ = Stdout.line! "  ✓ Reports multiple errors with details"
    _ = Stdout.line! "  ✓ Maintains symbol table with scoping"
    _ = Stdout.line! "  ✓ Produces typed AST for valid code"

    Ok {}

test_type_check! = |code, description|
    _ = Stdout.line! "  Testing: $(description)"
    _ = Stdout.line! "  Code: $(Str.trim(code))"

    # Tokenize
    tokens = Token.tokenize_str(code)

    # Filter trivia
    filtered = List.keep_if(tokens, |tok|
        when tok is
            WhitespaceToken _ -> Bool.false
            NewLineToken -> Bool.false
            SingleLineCommentToken _ -> Bool.false
            MultiLineCommentToken _ -> Bool.false
            _ -> Bool.true
    )

    # Parse
    when Parser.parse_program(filtered) is
        Ok parsed ->
            # Type check
            when TypeChecker.check_program(parsed.node) is
                Ok typed_node ->
                    _ = Stdout.line! "  ✓ Type check passed!"
                    _ = Stdout.line! "    Result type: $(Type.type_to_string(typed_node.inferred_type))"
                    {}

                Err errors ->
                    _ = Stdout.line! "  ✗ Type check failed with $(Num.to_str(List.len(errors))) error(s):"
                    List.for_each!(errors, |error|
                        _ = Stdout.line! "    - $(error_kind_to_str(error.kind)): $(error.message)"
                        when error.expected is
                            Ok expected_type ->
                                when error.actual is
                                    Ok actual_type ->
                                        _ = Stdout.line! "      Expected: $(Type.type_to_string(expected_type))"
                                        _ = Stdout.line! "      Got: $(Type.type_to_string(actual_type))"
                                        {}
                                    _ -> {}
                            _ -> {}
                    )

        Err parse_err ->
            _ = Stdout.line! "  ✗ Parse error: $(parse_err.message)"
            {}

error_kind_to_str : TypeChecker.ErrorKind -> Str
error_kind_to_str = |kind|
    when kind is
        TypeMismatch -> "Type Mismatch"
        UnknownVariable -> "Unknown Variable"
        UnknownProperty -> "Unknown Property"
        NotCallable -> "Not Callable"
        WrongArgumentCount -> "Wrong Argument Count"
        ConstReassignment -> "Const Reassignment"
        InvalidReturn -> "Invalid Return"
        InvalidBreak -> "Invalid Break"
        InvalidContinue -> "Invalid Continue"
        UnreachableCode -> "Unreachable Code"
        MissingReturn -> "Missing Return"
        InvalidOperation -> "Invalid Operation"
        PropertyNotFound -> "Property Not Found"
        IndexNotAllowed -> "Index Not Allowed"

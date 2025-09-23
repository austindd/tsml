#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import IntegratedTypeChecker as TypeChecker

main! = |_|
    Stdout.line! "=== Integrated Type Checker Test ==="

    # Test 1: Basic types
    test_code1 =
        """
        let x = 5;
        let y = "hello";
        let z = true;
        """

    Stdout.line! "\nTest 1: Basic types"
    result1 = TypeChecker.check_program(test_code1)
    if List.is_empty(result1.errors) then
        Stdout.line! "✓ No errors"
    else
        List.for_each!(result1.errors, |err|
            Stdout.line! "  Error: $(err.message)")

    # Test 2: Type errors
    test_code2 =
        """
        let x = "hello";
        let y = x - 10;
        """

    Stdout.line! "\nTest 2: Type errors"
    result2 = TypeChecker.check_program(test_code2)
    if List.is_empty(result2.errors) then
        Stdout.line! "✓ No errors"
    else
        List.for_each!(result2.errors, |err|
            Stdout.line! "  Error: $(err.message)")

    # Test 3: Mixed operations
    test_code3 =
        """
        let a = 5;
        let b = 10;
        let c = a + b;
        let d = "hello";
        let e = d + " world";
        let f = a * d;
        """

    Stdout.line! "\nTest 3: Mixed operations"
    result3 = TypeChecker.check_program(test_code3)
    if List.is_empty(result3.errors) then
        Stdout.line! "✓ No errors"
    else
        List.for_each!(result3.errors, |err|
            Stdout.line! "  Error: $(err.message)")

    Stdout.line! "\n=== Test Complete ==="#
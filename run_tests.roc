#!/usr/bin/env roc

app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import cli.Stdout
import cli.Arg
import TestConstraintSolver

main : List Arg.Arg -> Result {} [Exit I32 Str]_
main = \_ ->
    # Run all tests
    results = TestConstraintSolver.run_all_tests {}

    # Print header
    Stdout.line? "=== TypeConstraintSolver Test Results ==="
    Stdout.line? ""

    # Print each result
    List.walk? results {} \_, result ->
        Stdout.line? result
        Ok {}

    # Count passed tests
    passed = List.count results \r -> Str.starts_with r "✓"
    total = List.len results

    Stdout.line? ""
    Stdout.line? "$(Num.to_str passed)/$(Num.to_str total) tests passed"

    if passed == total then
        Stdout.line? "✨ All tests passed!"
    else
        Stdout.line? "⚠️ Some tests failed"

    Ok {}
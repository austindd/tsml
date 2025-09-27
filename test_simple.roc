#!/usr/bin/env roc

# Simple test to verify TypeConstraintSolver compiles and works
app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import TypeConstraintSolver
import ComprehensiveTypeIndexed

main! = \_ ->
    # Create basic types
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"

    # Test unification
    result = when TypeConstraintSolver.unify_types store2 num_type num_type is
        Ok _ -> "✓ Number unifies with itself"
        Err _ -> "✗ Number should unify with itself"

    _ <- Stdout.line! result

    # Test non-unification
    result2 = when TypeConstraintSolver.unify_types store2 num_type str_type is
        Ok _ -> "✗ Number should not unify with string"
        Err _ -> "✓ Number does not unify with string"

    _ <- Stdout.line! result2

    # Test subtyping with special types
    (store3, any_type) = ComprehensiveTypeIndexed.make_any store2

    result3 = if TypeConstraintSolver.is_subtype_of store3 num_type any_type then
        "✓ Number is subtype of any"
    else
        "✗ Number should be subtype of any"

    Stdout.line! result3
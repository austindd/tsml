#!/usr/bin/env roc

# Final test to verify TypeConstraintSolver is working
app [main] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Arg
import TypeConstraintSolver
import ComprehensiveTypeIndexed

main : List Arg.Arg -> Result {} [Exit I32 Str]_
main = \_ ->
    # Create basic types
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"
    (store3, bool_type) = ComprehensiveTypeIndexed.make_primitive store2 "boolean"
    (store4, any_type) = ComprehensiveTypeIndexed.make_any store3
    (store5, never_type) = ComprehensiveTypeIndexed.make_never store4

    # Test Results
    tests = [
        # Test 1: Number unifies with itself
        when TypeConstraintSolver.unify_types store5 num_type num_type is
            Ok _ -> "✓ Number unifies with itself"
            Err _ -> "✗ Number should unify with itself",

        # Test 2: Number doesn't unify with string
        when TypeConstraintSolver.unify_types store5 num_type str_type is
            Ok _ -> "✗ Number should NOT unify with string"
            Err _ -> "✓ Number does not unify with string",

        # Test 3: Number is subtype of any
        if TypeConstraintSolver.is_subtype_of store5 num_type any_type then
            "✓ Number <: any"
        else
            "✗ Number should be subtype of any",

        # Test 4: Never is subtype of number
        if TypeConstraintSolver.is_subtype_of store5 never_type num_type then
            "✓ Never <: number"
        else
            "✗ Never should be subtype of number",

        # Test 5: String is not subtype of number
        if TypeConstraintSolver.is_subtype_of store5 str_type num_type then
            "✗ String should NOT be subtype of number"
        else
            "✓ String is not subtype of number",
    ]

    # Test arrays
    (store6, num_array) = ComprehensiveTypeIndexed.make_array store5 num_type
    (store7, str_array) = ComprehensiveTypeIndexed.make_array store6 str_type

    array_tests = [
        when TypeConstraintSolver.unify_types store7 num_array str_array is
            Ok _ -> "✗ number[] should NOT unify with string[]"
            Err _ -> "✓ number[] does not unify with string[]",
    ]

    # Test tuples
    (store8, tuple1) = ComprehensiveTypeIndexed.make_tuple store7 [num_type, str_type]
    (store9, tuple2) = ComprehensiveTypeIndexed.make_tuple store8 [num_type, str_type]
    (store10, tuple3) = ComprehensiveTypeIndexed.make_tuple store9 [str_type, num_type]

    tuple_tests = [
        when TypeConstraintSolver.unify_types store10 tuple1 tuple2 is
            Ok _ -> "✓ [number, string] unifies with itself"
            Err _ -> "✗ Same tuples should unify",

        when TypeConstraintSolver.unify_types store10 tuple1 tuple3 is
            Ok _ -> "✗ [number, string] should NOT unify with [string, number]"
            Err _ -> "✓ Different tuple orders don't unify",
    ]

    # Test unions
    (store11, union1) = ComprehensiveTypeIndexed.make_union store10 [num_type, str_type]

    union_tests = [
        if TypeConstraintSolver.is_subtype_of store11 num_type union1 then
            "✓ number <: (number | string)"
        else
            "✗ number should be subtype of union",

        if TypeConstraintSolver.is_subtype_of store11 str_type union1 then
            "✓ string <: (number | string)"
        else
            "✗ string should be subtype of union",

        if TypeConstraintSolver.is_subtype_of store11 bool_type union1 then
            "✗ boolean should NOT be subtype of (number | string)"
        else
            "✓ boolean is not subtype of (number | string)",
    ]

    # Test objects with rows
    (store12, empty_row) = ComprehensiveTypeIndexed.make_empty_row store11
    (store13, x_row) = ComprehensiveTypeIndexed.make_row_extend store12 "x" num_type Bool.false Bool.false empty_row
    (store14, obj_x) = ComprehensiveTypeIndexed.make_object store13 x_row

    (store15, empty_row2) = ComprehensiveTypeIndexed.make_empty_row store14
    (store16, y_row) = ComprehensiveTypeIndexed.make_row_extend store15 "y" str_type Bool.false Bool.false empty_row2
    (store17, xy_row) = ComprehensiveTypeIndexed.make_row_extend store16 "x" num_type Bool.false Bool.false y_row
    (store18, obj_xy) = ComprehensiveTypeIndexed.make_object store17 xy_row

    object_tests = [
        if TypeConstraintSolver.is_subtype_of store18 obj_xy obj_x then
            "✓ {x: number, y: string} <: {x: number}"
        else
            "✗ Object with extra property should be subtype",

        if TypeConstraintSolver.is_subtype_of store18 obj_x obj_xy then
            "✗ {x: number} should NOT be subtype of {x: number, y: string}"
        else
            "✓ Object missing property is not subtype",
    ]

    # Combine all test results
    all_tests = List.concat tests array_tests
        |> List.concat tuple_tests
        |> List.concat union_tests
        |> List.concat object_tests

    # Print results
    Stdout.line? "\n=== TypeConstraintSolver Test Results ==="
    List.walk? all_tests {} \_, test_result ->
        Stdout.line? test_result
        Ok {}

    # Count passed tests
    passed = List.count all_tests \result -> Str.starts_with result "✓"
    total = List.len all_tests

    Stdout.line? "\n$(Num.to_str passed)/$(Num.to_str total) tests passed"

    if passed == total then
        Stdout.line? "\n✨ All tests passed! The TypeConstraintSolver is working correctly."
    else
        Stdout.line? "\n⚠️  Some tests failed. Review the implementation."

    Ok {}
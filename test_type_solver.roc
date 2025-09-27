#!/usr/bin/env roc

# Simple test script for TypeConstraintSolver
app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import TypeConstraintSolver
import ComprehensiveTypeIndexed

main! = \_ ->
    # Test that the modules load and compile
    _ <- _ <- Stdout.line! "Testing TypeConstraintSolver module..."

    # Create a simple type store
    store = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"
    (store3, any_type) = ComprehensiveTypeIndexed.make_any store2
    (store4, never_type) = ComprehensiveTypeIndexed.make_never store3

    # Test basic subtyping
    _ <- Stdout.line! "Testing basic subtype relationships..."

    if TypeConstraintSolver.is_subtype_of store4 num_type any_type then
        _ <- Stdout.line! "✓ number <: any"
    else
        _ <- Stdout.line! "✗ number should be subtype of any"

    if TypeConstraintSolver.is_subtype_of store4 never_type num_type then
        _ <- Stdout.line! "✓ never <: number"
    else
        _ <- Stdout.line! "✗ never should be subtype of number"

    if TypeConstraintSolver.is_subtype_of store4 str_type num_type then
        _ <- Stdout.line! "✗ string should NOT be subtype of number"
    else
        _ <- Stdout.line! "✓ string is not subtype of number"

    # Test unification
    _ <- Stdout.line! "\nTesting type unification..."

    when TypeConstraintSolver.unify_types store4 num_type num_type is
        Ok _ -> _ <- Stdout.line! "✓ number unifies with itself"
        Err _ -> _ <- Stdout.line! "✗ number should unify with itself"

    when TypeConstraintSolver.unify_types store4 num_type str_type is
        Ok _ -> _ <- Stdout.line! "✗ number should NOT unify with string"
        Err _ -> _ <- Stdout.line! "✓ number does not unify with string"

    # Test arrays
    _ <- Stdout.line! "\nTesting array types..."
    (store5, num_array) = ComprehensiveTypeIndexed.make_array store4 num_type
    (store6, str_array) = ComprehensiveTypeIndexed.make_array store5 str_type

    when TypeConstraintSolver.unify_types store6 num_array str_array is
        Ok _ -> _ <- Stdout.line! "✗ number[] should NOT unify with string[]"
        Err _ -> _ <- Stdout.line! "✓ number[] does not unify with string[]"

    # Test tuples
    _ <- Stdout.line! "\nTesting tuple types..."
    (store7, tuple1) = ComprehensiveTypeIndexed.make_tuple store6 [num_type, str_type]
    (store8, tuple2) = ComprehensiveTypeIndexed.make_tuple store7 [num_type, str_type]
    (store9, tuple3) = ComprehensiveTypeIndexed.make_tuple store8 [str_type, num_type]

    when TypeConstraintSolver.unify_types store9 tuple1 tuple2 is
        Ok _ -> _ <- Stdout.line! "✓ [number, string] unifies with itself"
        Err _ -> _ <- Stdout.line! "✗ Same tuples should unify"

    when TypeConstraintSolver.unify_types store9 tuple1 tuple3 is
        Ok _ -> _ <- Stdout.line! "✗ [number, string] should NOT unify with [string, number]"
        Err _ -> _ <- Stdout.line! "✓ Different tuple orders don't unify"

    # Test unions
    _ <- Stdout.line! "\nTesting union types..."
    (store10, union1) = ComprehensiveTypeIndexed.make_union store9 [num_type, str_type]

    if TypeConstraintSolver.is_subtype_of store10 num_type union1 then
        _ <- Stdout.line! "✓ number <: (number | string)"
    else
        _ <- Stdout.line! "✗ number should be subtype of union"

    if TypeConstraintSolver.is_subtype_of store10 str_type union1 then
        _ <- Stdout.line! "✓ string <: (number | string)"
    else
        _ <- Stdout.line! "✗ string should be subtype of union"

    # Test objects with rows
    _ <- Stdout.line! "\nTesting object types..."
    (store11, empty_row) = ComprehensiveTypeIndexed.make_empty_row store10
    (store12, x_row) = ComprehensiveTypeIndexed.make_row_extend store11 "x" num_type Bool.false Bool.false empty_row
    (store13, obj_x) = ComprehensiveTypeIndexed.make_object store12 x_row

    (store14, empty_row2) = ComprehensiveTypeIndexed.make_empty_row store13
    (store15, y_row) = ComprehensiveTypeIndexed.make_row_extend store14 "y" str_type Bool.false Bool.false empty_row2
    (store16, xy_row) = ComprehensiveTypeIndexed.make_row_extend store15 "x" num_type Bool.false Bool.false y_row
    (store17, obj_xy) = ComprehensiveTypeIndexed.make_object store16 xy_row

    if TypeConstraintSolver.is_subtype_of store17 obj_xy obj_x then
        _ <- Stdout.line! "✓ {x: number, y: string} <: {x: number}"
    else
        _ <- Stdout.line! "✗ Object with extra property should be subtype"

    if TypeConstraintSolver.is_subtype_of store17 obj_x obj_xy then
        _ <- Stdout.line! "✗ {x: number} should NOT be subtype of {x: number, y: string}"
    else
        _ <- Stdout.line! "✓ Object missing property is not subtype"

    _ <- Stdout.line! "\n=== TypeConstraintSolver tests completed ==="
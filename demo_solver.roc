#!/usr/bin/env roc

# Demonstrate the TypeConstraintSolver functionality
module [demo_results]

import TypeConstraintSolver
import ComprehensiveTypeIndexed

# Demonstrate the solver working
demo_results : {} -> List Str
demo_results = \{} ->
    # Create a type store with basic types
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str) = ComprehensiveTypeIndexed.make_primitive store1 "string"
    (store3, any) = ComprehensiveTypeIndexed.make_any store2
    (store4, never) = ComprehensiveTypeIndexed.make_never store3

    # Test 1: Basic unification
    test1 = when TypeConstraintSolver.unify_types store4 num num is
        Ok _ -> "✓ PASS: number unifies with itself"
        Err _ -> "✗ FAIL: number should unify with itself"

    # Test 2: Non-unification
    test2 = when TypeConstraintSolver.unify_types store4 num str is
        Ok _ -> "✗ FAIL: number unified with string (shouldn't happen)"
        Err _ -> "✓ PASS: number correctly does not unify with string"

    # Test 3: Subtyping - any is top type
    test3 = if TypeConstraintSolver.is_subtype_of store4 num any then
        "✓ PASS: number <: any (correct)"
    else
        "✗ FAIL: number should be subtype of any"

    # Test 4: Subtyping - never is bottom type
    test4 = if TypeConstraintSolver.is_subtype_of store4 never num then
        "✓ PASS: never <: number (correct)"
    else
        "✗ FAIL: never should be subtype of everything"

    # Test 5: Union types
    (store5, union) = ComprehensiveTypeIndexed.make_union store4 [num, str]
    test5 = if TypeConstraintSolver.is_subtype_of store5 num union then
        "✓ PASS: number <: (number | string)"
    else
        "✗ FAIL: number should be subtype of union containing it"

    # Test 6: Object subtyping (width subtyping)
    (store6, empty1) = ComprehensiveTypeIndexed.make_empty_row store5
    (store7, x_row) = ComprehensiveTypeIndexed.make_row_extend store6 "x" num Bool.false Bool.false empty1
    (store8, obj_x) = ComprehensiveTypeIndexed.make_object store7 x_row

    (store9, empty2) = ComprehensiveTypeIndexed.make_empty_row store8
    (store10, y_row) = ComprehensiveTypeIndexed.make_row_extend store9 "y" str Bool.false Bool.false empty2
    (store11, xy_row) = ComprehensiveTypeIndexed.make_row_extend store10 "x" num Bool.false Bool.false y_row
    (store12, obj_xy) = ComprehensiveTypeIndexed.make_object store11 xy_row

    test6 = if TypeConstraintSolver.is_subtype_of store12 obj_xy obj_x then
        "✓ PASS: {x: number, y: string} <: {x: number} (width subtyping)"
    else
        "✗ FAIL: Object with extra property should be subtype"

    [test1, test2, test3, test4, test5, test6]
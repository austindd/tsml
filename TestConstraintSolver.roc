module [
    test_basic_unification,
    test_subtyping,
    test_arrays,
    test_tuples,
    test_unions,
    test_objects,
    run_all_tests,
]

import TypeConstraintSolver
import ComprehensiveTypeIndexed

# Test basic type unification
test_basic_unification : {} -> List Str
test_basic_unification = \{} ->
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"

    results = []

    # Test that number unifies with itself
    result1 = when TypeConstraintSolver.unify_types store2 num_type num_type is
        Ok _ -> List.append results "✓ Number unifies with itself"
        Err _ -> List.append results "✗ Number should unify with itself"

    # Test that number doesn't unify with string
    when TypeConstraintSolver.unify_types store2 num_type str_type is
        Ok _ -> List.append result1 "✗ Number should NOT unify with string"
        Err _ -> List.append result1 "✓ Number does not unify with string"

# Test subtyping relationships
test_subtyping : {} -> List Str
test_subtyping = \{} ->
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, any_type) = ComprehensiveTypeIndexed.make_any store1
    (store3, never_type) = ComprehensiveTypeIndexed.make_never store2
    (store4, lit_42) = ComprehensiveTypeIndexed.make_literal store3 (NumLit 42.0)

    results = []

    # Any is top type
    r1 = if TypeConstraintSolver.is_subtype_of store4 num_type any_type then
        List.append results "✓ Number <: any"
    else
        List.append results "✗ Number should be subtype of any"

    # Never is bottom type
    r2 = if TypeConstraintSolver.is_subtype_of store4 never_type num_type then
        List.append r1 "✓ Never <: number"
    else
        List.append r1 "✗ Never should be subtype of number"

    # Literal subtyping
    if TypeConstraintSolver.is_subtype_of store4 lit_42 num_type then
        List.append r2 "✓ Literal 42 <: number"
    else
        List.append r2 "✗ Literal 42 should be subtype of number"

# Test array covariance
test_arrays : {} -> List Str
test_arrays = \{} ->
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"
    (store3, num_array) = ComprehensiveTypeIndexed.make_array store2 num_type
    (store4, str_array) = ComprehensiveTypeIndexed.make_array store3 str_type

    when TypeConstraintSolver.unify_types store4 num_array str_array is
        Ok _ -> ["✗ number[] should NOT unify with string[]"]
        Err _ -> ["✓ number[] does not unify with string[]"]

# Test tuple types
test_tuples : {} -> List Str
test_tuples = \{} ->
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"
    (store3, tuple1) = ComprehensiveTypeIndexed.make_tuple store2 [num_type, str_type]
    (store4, tuple2) = ComprehensiveTypeIndexed.make_tuple store3 [num_type, str_type]
    (store5, tuple3) = ComprehensiveTypeIndexed.make_tuple store4 [str_type, num_type]

    results = []

    # Same tuple should unify
    r1 = when TypeConstraintSolver.unify_types store5 tuple1 tuple2 is
        Ok _ -> List.append results "✓ [number, string] unifies with itself"
        Err _ -> List.append results "✗ Same tuples should unify"

    # Different order should not unify
    when TypeConstraintSolver.unify_types store5 tuple1 tuple3 is
        Ok _ -> List.append r1 "✗ [number, string] should NOT unify with [string, number]"
        Err _ -> List.append r1 "✓ Different tuple orders don't unify"

# Test union types
test_unions : {} -> List Str
test_unions = \{} ->
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"
    (store3, bool_type) = ComprehensiveTypeIndexed.make_primitive store2 "boolean"
    (store4, union1) = ComprehensiveTypeIndexed.make_union store3 [num_type, str_type]

    results = []

    # Number should be subtype of union
    r1 = if TypeConstraintSolver.is_subtype_of store4 num_type union1 then
        List.append results "✓ number <: (number | string)"
    else
        List.append results "✗ number should be subtype of union"

    # String should be subtype of union
    r2 = if TypeConstraintSolver.is_subtype_of store4 str_type union1 then
        List.append r1 "✓ string <: (number | string)"
    else
        List.append r1 "✗ string should be subtype of union"

    # Boolean should NOT be subtype of union
    if TypeConstraintSolver.is_subtype_of store4 bool_type union1 then
        List.append r2 "✗ boolean should NOT be subtype of (number | string)"
    else
        List.append r2 "✓ boolean is not subtype of (number | string)"

# Test object structural typing
test_objects : {} -> List Str
test_objects = \{} ->
    store0 = ComprehensiveTypeIndexed.empty_store
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"

    # Create { x: number }
    (store3, empty_row1) = ComprehensiveTypeIndexed.make_empty_row store2
    (store4, x_row) = ComprehensiveTypeIndexed.make_row_extend store3 "x" num_type Bool.false Bool.false empty_row1
    (store5, obj_x) = ComprehensiveTypeIndexed.make_object store4 x_row

    # Create { x: number, y: string }
    (store6, empty_row2) = ComprehensiveTypeIndexed.make_empty_row store5
    (store7, y_row) = ComprehensiveTypeIndexed.make_row_extend store6 "y" str_type Bool.false Bool.false empty_row2
    (store8, xy_row) = ComprehensiveTypeIndexed.make_row_extend store7 "x" num_type Bool.false Bool.false y_row
    (store9, obj_xy) = ComprehensiveTypeIndexed.make_object store8 xy_row

    results = []

    # Width subtyping - object with more properties is subtype
    r1 = if TypeConstraintSolver.is_subtype_of store9 obj_xy obj_x then
        List.append results "✓ {x: number, y: string} <: {x: number}"
    else
        List.append results "✗ Object with extra property should be subtype"

    # Object missing property is not subtype
    if TypeConstraintSolver.is_subtype_of store9 obj_x obj_xy then
        List.append r1 "✗ {x: number} should NOT be subtype of {x: number, y: string}"
    else
        List.append r1 "✓ Object missing property is not subtype"

# Run all tests and return results
run_all_tests : {} -> List Str
run_all_tests = \{} ->
    List.concat (test_basic_unification {}) (test_subtyping {})
        |> List.concat (test_arrays {})
        |> List.concat (test_tuples {})
        |> List.concat (test_unions {})
        |> List.concat (test_objects {})
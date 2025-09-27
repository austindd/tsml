#!/usr/bin/env roc

# Test enum functionality
module [run_enum_tests]

import TypeConstraintSolver
import ComprehensiveTypeIndexed

run_enum_tests : {} -> List Str
run_enum_tests = \{} ->
    # Create store
    store0 = ComprehensiveTypeIndexed.empty_store

    # Create a numeric enum: enum Color { Red = 0, Green = 1, Blue = 2 }
    color_enum_def = {
        name: "Color",
        members: [
            { name: "Red", value: EnumNumValue 0.0 },
            { name: "Green", value: EnumNumValue 1.0 },
            { name: "Blue", value: EnumNumValue 2.0 },
        ],
        is_const: Bool.false,
    }
    (store1, color_enum) = ComprehensiveTypeIndexed.make_enum store0 color_enum_def
    (store2, enum_id) = when ComprehensiveTypeIndexed.get_type store1 color_enum is
        Ok (TEnum id) -> (store1, id)
        _ -> (store1, 0)  # Should not happen

    # Create enum members
    (store3, red_member) = ComprehensiveTypeIndexed.make_enum_member store2 enum_id "Red"
    (store4, green_member) = ComprehensiveTypeIndexed.make_enum_member store3 enum_id "Green"
    (store5, blue_member) = ComprehensiveTypeIndexed.make_enum_member store4 enum_id "Blue"

    # Create a string enum: enum Status { Success = "ok", Failure = "error" }
    status_enum_def = {
        name: "Status",
        members: [
            { name: "Success", value: EnumStrValue "ok" },
            { name: "Failure", value: EnumStrValue "error" },
        ],
        is_const: Bool.true,  # const enum
    }
    (store6, status_enum) = ComprehensiveTypeIndexed.make_enum store5 status_enum_def
    (store7, status_enum_id) = when ComprehensiveTypeIndexed.get_type store6 status_enum is
        Ok (TEnum id) -> (store6, id)
        _ -> (store6, 0)

    (store8, success_member) = ComprehensiveTypeIndexed.make_enum_member store7 status_enum_id "Success"
    (store9, failure_member) = ComprehensiveTypeIndexed.make_enum_member store8 status_enum_id "Failure"

    # Also get number and string types for testing
    (store10, num_type) = ComprehensiveTypeIndexed.make_primitive store9 "number"
    (store11, str_type) = ComprehensiveTypeIndexed.make_primitive store10 "string"
    (store12, str_ok_literal) = ComprehensiveTypeIndexed.make_literal store11 (StrLit "ok")
    (store13, str_error_literal) = ComprehensiveTypeIndexed.make_literal store12 (StrLit "error")

    results = []

    # Test 1: Enum members unify with themselves
    r1 = when TypeConstraintSolver.unify_types store13 red_member red_member is
        Ok _ -> List.append results "✓ Enum member unifies with itself"
        Err _ -> List.append results "✗ Enum member should unify with itself"

    # Test 2: Different enum members don't unify
    r2 = when TypeConstraintSolver.unify_types store13 red_member green_member is
        Ok _ -> List.append r1 "✗ Different enum members should NOT unify"
        Err _ -> List.append r1 "✓ Different enum members don't unify"

    # Test 3: Enum member is subtype of its enum
    r3 = if TypeConstraintSolver.is_subtype_of store13 red_member color_enum then
        List.append r2 "✓ Color.Red <: Color"
    else
        List.append r2 "✗ Enum member should be subtype of its enum"

    # Test 4: Enum member is NOT subtype of different enum
    r4 = if TypeConstraintSolver.is_subtype_of store13 red_member status_enum then
        List.append r3 "✗ Color.Red should NOT be subtype of Status"
    else
        List.append r3 "✓ Enum member not subtype of different enum"

    # Test 5: Const enum member subtyping with literals
    r5 = if TypeConstraintSolver.is_subtype_of store13 success_member str_type then
        List.append r4 "✓ Const enum Status.Success <: string"
    else
        List.append r4 "✗ Const enum member should be subtype of string"

    # Test 6: Const enum member matches literal value
    r6 = if TypeConstraintSolver.is_subtype_of store13 str_ok_literal status_enum then
        List.append r5 "✓ Literal 'ok' <: Status (matches Success value)"
    else
        List.append r5 "✗ Literal should match const enum value"

    # Test 7: Wrong literal doesn't match const enum
    r7 = if TypeConstraintSolver.is_subtype_of store13 str_error_literal success_member then
        List.append r6 "✗ Literal 'error' should NOT match Status.Success"
    else
        List.append r6 "✓ Wrong literal doesn't match const enum member"

    # Test 8: Same enum type unifies
    r8 = when TypeConstraintSolver.unify_types store13 color_enum color_enum is
        Ok _ -> List.append r7 "✓ Enum type unifies with itself"
        Err _ -> List.append r7 "✗ Enum type should unify with itself"

    # Test 9: Different enum types don't unify
    when TypeConstraintSolver.unify_types store13 color_enum status_enum is
        Ok _ -> List.append r8 "✗ Different enums should NOT unify"
        Err _ -> List.append r8 "✓ Different enum types don't unify"
app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout
import pf.Task
import TypeConstraintSolver exposing [
    empty_solver_state,
    add_constraint,
    solve_constraints,
    is_subtype_of,
    unify_types,
    Constraint,
    ConstraintKind,
    Equality,
    Subtype,
]
import ComprehensiveTypeIndexed exposing [
    TypeStore,
    TypeId,
    RowId,
    TypeDef,
    RowDef,
    LiteralValue,
    empty_store,
    add_type,
    add_row,
    make_primitive,
    make_literal,
    make_object,
    make_array,
    make_tuple,
    make_function,
    make_union,
    make_intersection,
    make_any,
    make_never,
    make_unknown,
    make_void,
    make_empty_row,
    make_row_extend,
    type_to_str,
]

# Test result type
TestResult : { name: Str, passed: Bool, message: Str }

# Run a test and return result
run_test : Str, ({} -> Result {} Str) -> TestResult
run_test = \name, test_fn ->
    when test_fn {} is
        Ok _ -> { name, passed: Bool.true, message: "✓" }
        Err msg -> { name, passed: Bool.false, message: "✗ $(msg)" }

# Test primitive type equality
test_primitive_equality : {} -> Result {} Str
test_primitive_equality = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, str_type) = make_primitive store1 "string"

    # Test that number = number succeeds
    when unify_types store2 num_type num_type is
        Ok _ ->
            # Test that number = string fails
            when unify_types store2 num_type str_type is
                Err _ -> Ok {}
                Ok _ -> Err "Number should not unify with string"
        Err _ -> Err "Number should unify with itself"

# Test literal subtyping
test_literal_subtyping : {} -> Result {} Str
test_literal_subtyping = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, lit_42) = make_literal store1 (NumLit 42.0)

    # Literal 42 should be subtype of number
    if is_subtype_of store2 lit_42 num_type then
        # But number should not be subtype of literal 42
        if is_subtype_of store2 num_type lit_42 then
            Err "Number should not be subtype of literal 42"
        else
            Ok {}
    else
        Err "Literal 42 should be subtype of number"

# Test array covariance
test_array_covariance : {} -> Result {} Str
test_array_covariance = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, lit_42) = make_literal store1 (NumLit 42.0)
    (store3, num_array) = make_array store2 num_type
    (store4, lit_array) = make_array store3 lit_42

    # Array<42> should be subtype of Array<number>
    if is_subtype_of store4 lit_array num_array then
        Ok {}
    else
        Err "Array<42> should be subtype of Array<number>"

# Test tuple types
test_tuple_types : {} -> Result {} Str
test_tuple_types = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, str_type) = make_primitive store1 "string"
    (store3, tuple1) = make_tuple store2 [num_type, str_type]
    (store4, tuple2) = make_tuple store3 [num_type, str_type]
    (store5, tuple3) = make_tuple store4 [str_type, num_type]

    # Same tuples should unify
    when unify_types store5 tuple1 tuple2 is
        Ok _ ->
            # Different order should not unify
            when unify_types store5 tuple1 tuple3 is
                Err _ -> Ok {}
                Ok _ -> Err "Tuples with different element order should not unify"
        Err _ -> Err "Same tuples should unify"

# Test union types
test_union_types : {} -> Result {} Str
test_union_types = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, str_type) = make_primitive store1 "string"
    (store3, bool_type) = make_primitive store2 "boolean"
    (store4, union1) = make_union store3 [num_type, str_type]

    # number should be subtype of number | string
    if is_subtype_of store4 num_type union1 then
        # string should be subtype of number | string
        if is_subtype_of store4 str_type union1 then
            # boolean should NOT be subtype of number | string
            if is_subtype_of store4 bool_type union1 then
                Err "Boolean should not be subtype of number | string"
            else
                Ok {}
        else
            Err "String should be subtype of number | string"
    else
        Err "Number should be subtype of number | string"

# Test intersection types
test_intersection_types : {} -> Result {} Str
test_intersection_types = \{} ->
    store0 = empty_store
    (store1, any_type) = make_any store0
    (store2, unknown_type) = make_unknown store1
    (store3, num_type) = make_primitive store2 "number"
    (store4, intersection) = make_intersection store3 [any_type, num_type]

    # number & any should be subtype of number
    if is_subtype_of store4 intersection num_type then
        Ok {}
    else
        Err "number & any should simplify to number"

# Test object structural typing (width subtyping)
test_object_width_subtyping : {} -> Result {} Str
test_object_width_subtyping = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, str_type) = make_primitive store1 "string"

    # Create { x: number, y: string }
    (store3, empty_row1) = make_empty_row store2
    (store4, row_y) = make_row_extend store3 "y" str_type Bool.false Bool.false empty_row1
    (store5, row_xy) = make_row_extend store4 "x" num_type Bool.false Bool.false row_y
    (store6, obj_xy) = make_object store5 row_xy

    # Create { x: number }
    (store7, empty_row2) = make_empty_row store6
    (store8, row_x) = make_row_extend store7 "x" num_type Bool.false Bool.false empty_row2
    (store9, obj_x) = make_object store8 row_x

    # { x: number, y: string } should be subtype of { x: number }
    if is_subtype_of store9 obj_xy obj_x then
        # But not the other way
        if is_subtype_of store9 obj_x obj_xy then
            Err "{ x: number } should not be subtype of { x: number, y: string }"
        else
            Ok {}
    else
        Err "{ x: number, y: string } should be subtype of { x: number }"

# Test function contravariance/covariance
test_function_variance : {} -> Result {} Str
test_function_variance = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, lit_42) = make_literal store1 (NumLit 42.0)

    # Create (x: number) => 42
    (store3, func1) = make_function store2
        [{ name: "x", param_type: num_type, optional: Bool.false }]
        lit_42 [] Bool.false Bool.false

    # Create (x: 42) => number
    (store4, func2) = make_function store3
        [{ name: "x", param_type: lit_42, optional: Bool.false }]
        num_type [] Bool.false Bool.false

    # func1 should be subtype of func2 (contravariant params, covariant return)
    if is_subtype_of store4 func1 func2 then
        Ok {}
    else
        Err "Function variance not working correctly"

# Test special types
test_special_types : {} -> Result {} Str
test_special_types = \{} ->
    store0 = empty_store
    (store1, any_type) = make_any store0
    (store2, never_type) = make_never store1
    (store3, unknown_type) = make_unknown store2
    (store4, num_type) = make_primitive store3 "number"

    # Everything is subtype of any
    if !is_subtype_of store4 num_type any_type then
        Err "Number should be subtype of any"
    else if !is_subtype_of store4 never_type any_type then
        Err "Never should be subtype of any"
    # Never is subtype of everything
    else if !is_subtype_of store4 never_type num_type then
        Err "Never should be subtype of number"
    # Everything is subtype of unknown
    else if !is_subtype_of store4 num_type unknown_type then
        Err "Number should be subtype of unknown"
    # Unknown is not subtype of other types (except any/unknown)
    else if is_subtype_of store4 unknown_type num_type then
        Err "Unknown should not be subtype of number"
    else
        Ok {}

# Test void and undefined relationship
test_void_undefined : {} -> Result {} Str
test_void_undefined = \{} ->
    store0 = empty_store
    (store1, void_type) = make_void store0
    (store2, undef_type) = make_primitive store1 "undefined"

    # void should be subtype of undefined
    if is_subtype_of store2 void_type undef_type then
        Ok {}
    else
        Err "Void should be subtype of undefined"

# Test constraint solver with multiple constraints
test_constraint_solver : {} -> Result {} Str
test_constraint_solver = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"
    (store2, str_type) = make_primitive store1 "string"

    solver_state = empty_solver_state store2
        |> add_constraint (Equality num_type num_type) (Ok "num = num")
        |> add_constraint (Subtype num_type num_type) (Ok "num <: num")

    when solve_constraints solver_state is
        Ok _ -> Ok {}
        Err _ -> Err "Valid constraints should solve successfully"

# Test optional properties in objects
test_optional_properties : {} -> Result {} Str
test_optional_properties = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"

    # Create { x: number, y?: number }
    (store2, empty_row1) = make_empty_row store1
    (store3, row_y_opt) = make_row_extend store2 "y" num_type Bool.true Bool.false empty_row1
    (store4, row_xy_opt) = make_row_extend store3 "x" num_type Bool.false Bool.false row_y_opt
    (store5, obj_xy_opt) = make_object store4 row_xy_opt

    # Create { x: number }
    (store6, empty_row2) = make_empty_row store5
    (store7, row_x) = make_row_extend store6 "x" num_type Bool.false Bool.false empty_row2
    (store8, obj_x) = make_object store7 row_x

    # { x: number } should be subtype of { x: number, y?: number }
    if is_subtype_of store8 obj_x obj_xy_opt then
        Ok {}
    else
        Err "Object without optional property should be subtype of object with optional property"

# Test readonly properties
test_readonly_properties : {} -> Result {} Str
test_readonly_properties = \{} ->
    store0 = empty_store
    (store1, num_type) = make_primitive store0 "number"

    # Create { readonly x: number }
    (store2, empty_row1) = make_empty_row store1
    (store3, row_x_ro) = make_row_extend store2 "x" num_type Bool.false Bool.true empty_row1
    (store4, obj_x_ro) = make_object store3 row_x_ro

    # Create { x: number }
    (store5, empty_row2) = make_empty_row store4
    (store6, row_x) = make_row_extend store5 "x" num_type Bool.false Bool.false empty_row2
    (store7, obj_x) = make_object store6 row_x

    # { readonly x: number } should be subtype of { x: number }
    # (readonly is more restrictive, so it's a subtype)
    if is_subtype_of store7 obj_x_ro obj_x then
        # But not the other way
        if is_subtype_of store7 obj_x obj_x_ro then
            Err "Mutable property should not be subtype of readonly"
        else
            Ok {}
    else
        Err "Readonly property should be subtype of mutable"

# Main test runner
main =
    tests = [
        run_test "Primitive equality" test_primitive_equality,
        run_test "Literal subtyping" test_literal_subtyping,
        run_test "Array covariance" test_array_covariance,
        run_test "Tuple types" test_tuple_types,
        run_test "Union types" test_union_types,
        run_test "Intersection types" test_intersection_types,
        run_test "Object width subtyping" test_object_width_subtyping,
        run_test "Function variance" test_function_variance,
        run_test "Special types (any, never, unknown)" test_special_types,
        run_test "Void and undefined" test_void_undefined,
        run_test "Constraint solver" test_constraint_solver,
        run_test "Optional properties" test_optional_properties,
        run_test "Readonly properties" test_readonly_properties,
    ]

    passed = List.count tests \t -> t.passed
    total = List.len tests

    # Print test results
    Stdout.line! "\n=== TypeConstraintSolver Test Results ===\n"

    List.walk! tests {} \_, test ->
        if test.passed then
            Stdout.line! "  $(test.message) $(test.name)"
        else
            Stdout.line! "  $(test.message) $(test.name)"

    Stdout.line! "\n$(Num.to_str passed)/$(Num.to_str total) tests passed"

    if passed == total then
        Stdout.line! "\n✨ All tests passed! ✨"
        Task.ok {}
    else
        Stdout.line! "\n❌ Some tests failed"
        Task.err 1
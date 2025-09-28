#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ComprehensiveTypeIndexed as T

# Advanced tests for the type lattice operations
main! = |_args|
    Stdout.line!("Testing Advanced Type Lattice Operations...")?
    Stdout.line!("=" |> Str.repeat(50))?

    store0 = T.empty_store

    # Create basic types
    (store1, unknown) = T.make_unknown(store0)
    (store2, never) = T.make_never(store1)
    (store3, any) = T.make_any(store2)
    (store4, number) = T.make_primitive(store3, "number")
    (store5, string) = T.make_primitive(store4, "string")
    (store6, boolean) = T.make_primitive(store5, "boolean")

    # Test 1: Any type in lattice
    Stdout.line!("\n--- Testing 'any' type ---")?
    any_sub_unknown = T.is_subtype_of(store6, any, unknown)
    Stdout.line!("any <: unknown: $(Inspect.to_str(any_sub_unknown))")?

    number_sub_any = T.is_subtype_of(store6, number, any)
    Stdout.line!("number <: any: $(Inspect.to_str(number_sub_any))")?

    (store7, join_num_any) = T.join(store6, number, any)
    join_any_str = T.type_to_str(store7, join_num_any)
    Stdout.line!("join(number, any) = $(join_any_str)")?

    # Test 2: Complex union/intersection operations
    Stdout.line!("\n--- Testing Complex Unions ---")?
    (store8, union_ns) = T.make_union(store7, [number, string])
    (store9, union_sb) = T.make_union(store8, [string, boolean])

    (store10, joined_unions) = T.join(store9, union_ns, union_sb)
    joined_unions_str = T.type_to_str(store10, joined_unions)
    Stdout.line!("join((number | string), (string | boolean)) = $(joined_unions_str)")?

    (store11, met_unions) = T.meet(store10, union_ns, union_sb)
    met_unions_str = T.type_to_str(store11, met_unions)
    Stdout.line!("meet((number | string), (string | boolean)) = $(met_unions_str)")?

    # Test 3: Intersection types
    Stdout.line!("\n--- Testing Intersections ---")?
    (store12, intersect_ns) = T.make_intersection(store11, [number, string])
    (store13, intersect_nb) = T.make_intersection(store12, [number, boolean])

    # Intersection of incompatible types should be never
    (store14, met_incompatible) = T.meet(store13, string, boolean)
    met_incompatible_str = T.type_to_str(store14, met_incompatible)
    Stdout.line!("meet(string, boolean) = $(met_incompatible_str)")?

    # Test 4: Function subtyping (contravariant params, covariant return)
    Stdout.line!("\n--- Testing Function Subtyping ---")?
    (store15, func_num_str) = T.make_function(store14, [{ name: "x", param_type: number, optional: Bool.false }], string, [], Bool.false, Bool.false)
    (store16, func_any_str) = T.make_function(store15, [{ name: "x", param_type: any, optional: Bool.false }], string, [], Bool.false, Bool.false)

    # (any) -> string <: (number) -> string (contravariance)
    func_contra = T.is_subtype_of(store16, func_any_str, func_num_str)
    Stdout.line!("(any) -> string <: (number) -> string: $(Inspect.to_str(func_contra))")?

    # Test 5: Nested array types
    Stdout.line!("\n--- Testing Nested Arrays ---")?
    (store17, arr_num) = T.make_array(store16, number)
    (store18, arr_arr_num) = T.make_array(store17, arr_num)
    (store19, arr_unknown) = T.make_array(store18, unknown)
    (store20, arr_arr_unknown) = T.make_array(store19, arr_unknown)

    nested_arr_sub = T.is_subtype_of(store20, arr_arr_num, arr_arr_unknown)
    Stdout.line!("Array[Array[number]] <: Array[Array[unknown]]: $(Inspect.to_str(nested_arr_sub))")?

    # Test 6: Object with optional properties
    Stdout.line!("\n--- Testing Object Optional Properties ---")?
    (store21, empty_row) = T.make_empty_row(store20)
    (store22, row_required) = T.make_row_extend(store21, "x", number, Bool.false, Bool.false, empty_row)
    (store23, obj_required) = T.make_object(store22, row_required)

    (store24, row_optional) = T.make_row_extend(store23, "x", number, Bool.true, Bool.false, empty_row)
    (store25, obj_optional) = T.make_object(store24, row_optional)

    # Object with required property is subtype of object with optional property
    req_sub_opt = T.is_subtype_of(store25, obj_required, obj_optional)
    Stdout.line!("{x: number} <: {x?: number}: $(Inspect.to_str(req_sub_opt))")?

    # Test 7: Recursive meet/join operations
    Stdout.line!("\n--- Testing Recursive Operations ---")?
    (store26, lit_true) = T.make_literal(store25, BoolLit(Bool.true))
    (store27, lit_false) = T.make_literal(store26, BoolLit(Bool.false))

    (store28, join_bools) = T.join(store27, lit_true, lit_false)
    join_bools_str = T.type_to_str(store28, join_bools)
    Stdout.line!("join(true, false) = $(join_bools_str)")?

    (store29, meet_bool_lit) = T.meet(store28, boolean, lit_true)
    meet_bool_lit_str = T.type_to_str(store29, meet_bool_lit)
    Stdout.line!("meet(boolean, true) = $(meet_bool_lit_str)")?

    # Test 8: Mixed tuple types
    Stdout.line!("\n--- Testing Mixed Tuples ---")?
    (store30, tup_concrete) = T.make_tuple(store29, [number, string])
    (store31, tup_mixed) = T.make_tuple(store30, [any, unknown])

    tup_mixed_sub = T.is_subtype_of(store31, tup_concrete, tup_mixed)
    Stdout.line!("[number, string] <: [any, unknown]: $(Inspect.to_str(tup_mixed_sub))")?

    (store32, join_tuples) = T.join(store31, tup_concrete, tup_mixed)
    join_tuples_str = T.type_to_str(store32, join_tuples)
    Stdout.line!("join([number, string], [any, unknown]) = $(join_tuples_str)")?

    Stdout.line!("\nAll advanced lattice tests passed! ✨")?
    Ok({})
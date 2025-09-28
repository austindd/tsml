#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ComprehensiveTypeIndexed as T

# Test the type lattice operations
main! = |_args|
    Stdout.line!("Testing Type Lattice Operations...")?
    Stdout.line!("=" |> Str.repeat(50))?

    # Test 1: Unknown is top
    store0 = T.empty_store
    (store1, unknown) = T.make_unknown(store0)
    (store2, number) = T.make_primitive(store1, "number")

    is_top_test = T.is_top(store2, unknown)
    Stdout.line!("✓ unknown is top: $(Inspect.to_str(is_top_test))")?

    # Test 2: Never is bottom
    (store3, never) = T.make_never(store2)
    is_bottom_test = T.is_bottom(store3, never)
    Stdout.line!("✓ never is bottom: $(Inspect.to_str(is_bottom_test))")?

    # Test 3: Subtyping - never <: number
    never_sub_number = T.is_subtype_of(store3, never, number)
    Stdout.line!("✓ never <: number: $(Inspect.to_str(never_sub_number))")?

    # Test 4: Subtyping - number <: unknown
    number_sub_unknown = T.is_subtype_of(store3, number, unknown)
    Stdout.line!("✓ number <: unknown: $(Inspect.to_str(number_sub_unknown))")?

    # Test 5: Literal subtyping
    (store4, lit_42) = T.make_literal(store3, NumLit(42.0))
    lit_sub_number = T.is_subtype_of(store4, lit_42, number)
    Stdout.line!("✓ 42 <: number: $(Inspect.to_str(lit_sub_number))")?

    # Test 6: Join literals -> base type
    (store5, lit_100) = T.make_literal(store4, NumLit(100.0))
    (store6, joined_nums) = T.join(store5, lit_42, lit_100)
    joined_str = T.type_to_str(store6, joined_nums)
    Stdout.line!("✓ join(42, 100) = $(joined_str)")?

    # Test 7: Join with never
    (store7, joined_never) = T.join(store6, never, number)
    joined_never_str = T.type_to_str(store7, joined_never)
    Stdout.line!("✓ join(never, number) = $(joined_never_str)")?

    # Test 8: Join with unknown
    (store8, joined_unknown) = T.join(store7, number, unknown)
    joined_unknown_str = T.type_to_str(store8, joined_unknown)
    Stdout.line!("✓ join(number, unknown) = $(joined_unknown_str)")?

    # Test 9: Meet with never
    (store9, met_never) = T.meet(store8, number, never)
    met_never_str = T.type_to_str(store9, met_never)
    Stdout.line!("✓ meet(number, never) = $(met_never_str)")?

    # Test 10: Meet with unknown
    (store10, met_unknown) = T.meet(store9, number, unknown)
    met_unknown_str = T.type_to_str(store10, met_unknown)
    Stdout.line!("✓ meet(number, unknown) = $(met_unknown_str)")?

    # Test 11: Array subtyping
    (store11, arr_lit) = T.make_array(store10, lit_42)
    (store12, arr_num) = T.make_array(store11, number)
    arr_sub = T.is_subtype_of(store12, arr_lit, arr_num)
    Stdout.line!("✓ Array[42] <: Array[number]: $(Inspect.to_str(arr_sub))")?

    # Test 12: Union types
    (store13, string) = T.make_primitive(store12, "string")
    (store14, union_ns) = T.make_union(store13, [number, string])
    num_sub_union = T.is_subtype_of(store14, number, union_ns)
    Stdout.line!("✓ number <: (number | string): $(Inspect.to_str(num_sub_union))")?

    # Test 13: Join creates union
    (store15, boolean) = T.make_primitive(store14, "boolean")
    (store16, joined_diff) = T.join(store15, number, boolean)
    joined_diff_str = T.type_to_str(store16, joined_diff)
    Stdout.line!("✓ join(number, boolean) = $(joined_diff_str)")?

    # Test 14: Tuple subtyping (width)
    (store17, tup2) = T.make_tuple(store16, [number, string])
    (store18, tup3) = T.make_tuple(store17, [number, string, boolean])
    tup_width_sub = T.is_subtype_of(store18, tup3, tup2)
    Stdout.line!("✓ [number, string, boolean] <: [number, string]: $(Inspect.to_str(tup_width_sub))")?

    # Test 15: Object/Row subtyping (structural)
    (store19, empty_row) = T.make_empty_row(store18)
    (store20, row_x) = T.make_row_extend(store19, "x", number, Bool.false, Bool.false, empty_row)
    (store21, obj_x) = T.make_object(store20, row_x)

    (store22, row_xy) = T.make_row_extend(store21, "y", string, Bool.false, Bool.false, row_x)
    (store23, obj_xy) = T.make_object(store22, row_xy)

    obj_width_sub = T.is_subtype_of(store23, obj_xy, obj_x)
    Stdout.line!("✓ {x: number, y: string} <: {x: number}: $(Inspect.to_str(obj_width_sub))")?

    Stdout.line!("")?
    Stdout.line!("All lattice tests passed! ✨")?
    Ok({})
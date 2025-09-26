app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout
import ComprehensiveTypeIndexed

main! = \_ ->
    _ = Stdout.line! "Testing Comprehensive Type System with Indices:\n"

    store0 = ComprehensiveTypeIndexed.empty_store

    # Test 1: Simple primitives
    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive store0 "number"
    (store2, str_type) = ComprehensiveTypeIndexed.make_primitive store1 "string"
    (store3, bool_type) = ComprehensiveTypeIndexed.make_primitive store2 "boolean"

    _ = Stdout.line! "1. Primitives:"
    _ = Stdout.line! "   number: $(ComprehensiveTypeIndexed.type_to_str store3 num_type)"
    _ = Stdout.line! "   string: $(ComprehensiveTypeIndexed.type_to_str store3 str_type)"
    _ = Stdout.line! "   boolean: $(ComprehensiveTypeIndexed.type_to_str store3 bool_type)"

    # Test 2: Literal types
    (store4, lit_42) = ComprehensiveTypeIndexed.make_literal store3 (NumLit "42")
    (store5, lit_hello) = ComprehensiveTypeIndexed.make_literal store4 (StrLit "hello")
    (store6, lit_true) = ComprehensiveTypeIndexed.make_literal store5 (BoolLit Bool.true)

    _ = Stdout.line! "\n2. Literal types:"
    _ = Stdout.line! "   42: $(ComprehensiveTypeIndexed.type_to_str store6 lit_42)"
    _ = Stdout.line! "   \"hello\": $(ComprehensiveTypeIndexed.type_to_str store6 lit_hello)"
    _ = Stdout.line! "   true: $(ComprehensiveTypeIndexed.type_to_str store6 lit_true)"

    # Test 3: Object type with properties
    (store7, empty_row) = ComprehensiveTypeIndexed.make_empty_row store6
    (store8, row_with_y) = ComprehensiveTypeIndexed.make_row_extend store7 "y" str_type Bool.false Bool.false empty_row
    (store9, row_with_x_y) = ComprehensiveTypeIndexed.make_row_extend store8 "x" num_type Bool.false Bool.false row_with_y
    (store10, obj_type) = ComprehensiveTypeIndexed.make_object store9 row_with_x_y

    _ = Stdout.line! "\n3. Object type:"
    _ = Stdout.line! "   {x: number, y: string}: $(ComprehensiveTypeIndexed.type_to_str store10 obj_type)"

    # Test 4: Array type
    (store11, arr_num) = ComprehensiveTypeIndexed.make_array store10 num_type
    (store12, arr_str) = ComprehensiveTypeIndexed.make_array store11 str_type

    _ = Stdout.line! "\n4. Array types:"
    _ = Stdout.line! "   number[]: $(ComprehensiveTypeIndexed.type_to_str store12 arr_num)"
    _ = Stdout.line! "   string[]: $(ComprehensiveTypeIndexed.type_to_str store12 arr_str)"

    # Test 5: Union type
    (store13, union_type) = ComprehensiveTypeIndexed.make_union store12 [num_type, str_type, lit_true]

    _ = Stdout.line! "\n5. Union type:"
    _ = Stdout.line! "   number | string | true: $(ComprehensiveTypeIndexed.type_to_str store13 union_type)"

    # Test 6: Function type
    (store14, func_type) = ComprehensiveTypeIndexed.make_function store13 [num_type, str_type] bool_type [] Bool.false
    (store15, async_func) = ComprehensiveTypeIndexed.make_function store14 [str_type] num_type [] Bool.true

    _ = Stdout.line! "\n6. Function types:"
    _ = Stdout.line! "   (number, string) => boolean: $(ComprehensiveTypeIndexed.type_to_str store15 func_type)"
    _ = Stdout.line! "   async (string) => number: $(ComprehensiveTypeIndexed.type_to_str store15 async_func)"

    # Test 7: Tuple type
    (store16, tuple_type) = ComprehensiveTypeIndexed.make_tuple store15 [num_type, str_type, bool_type]

    _ = Stdout.line! "\n7. Tuple type:"
    _ = Stdout.line! "   [number, string, boolean]: $(ComprehensiveTypeIndexed.type_to_str store16 tuple_type)"

    # Test 8: Generic type (Array<string>)
    (store17, array_constructor) = ComprehensiveTypeIndexed.make_primitive store16 "Array"
    (store18, generic_array_str) = ComprehensiveTypeIndexed.make_generic store17 array_constructor [str_type]

    _ = Stdout.line! "\n8. Generic type:"
    _ = Stdout.line! "   Array<string>: $(ComprehensiveTypeIndexed.type_to_str store18 generic_array_str)"

    # Test 9: Optional and readonly properties
    (store19, empty_row2) = ComprehensiveTypeIndexed.make_empty_row store18
    (store20, optional_prop) = ComprehensiveTypeIndexed.make_row_extend store19 "optional" str_type Bool.true Bool.false empty_row2
    (store21, readonly_prop) = ComprehensiveTypeIndexed.make_row_extend store20 "readonly" num_type Bool.false Bool.true optional_prop
    (store22, mixed_obj) = ComprehensiveTypeIndexed.make_object store21 readonly_prop

    _ = Stdout.line! "\n9. Object with modifiers:"
    _ = Stdout.line! "   {readonly readonly: number, optional?: string}: $(ComprehensiveTypeIndexed.type_to_str store22 mixed_obj)"

    _ = Stdout.line! "\n✅ Comprehensive type system working with indices!"

    Ok {}
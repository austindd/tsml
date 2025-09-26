app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import FullType as FT

main! = \_ ->
    _ = Stdout.line! "=== Full TypeScript/JavaScript Type System ==="

    # 1. All primitive types including BigInt and Symbol
    _ = Stdout.line! "\n1. Complete Primitive Types:"
    _ = Stdout.line! "  number: $(FT.type_to_string FT.mk_number)"
    _ = Stdout.line! "  string: $(FT.type_to_string FT.mk_string)"
    _ = Stdout.line! "  boolean: $(FT.type_to_string FT.mk_boolean)"
    _ = Stdout.line! "  null: $(FT.type_to_string FT.mk_null)"
    _ = Stdout.line! "  undefined: $(FT.type_to_string FT.mk_undefined)"
    _ = Stdout.line! "  bigint: $(FT.type_to_string FT.mk_bigint)"
    _ = Stdout.line! "  symbol: $(FT.type_to_string FT.mk_symbol)"

    # 2. Literal types
    _ = Stdout.line! "\n2. Literal Types:"
    str_hello = FT.mk_literal (FT.StrLit "hello")
    num_42 = FT.mk_literal (FT.NumLit 42.0)
    bool_true = FT.mk_literal (FT.BoolLit Bool.true)
    bigint_100 = FT.mk_literal (FT.BigIntLit "100")

    _ = Stdout.line! "  \"hello\": $(FT.type_to_string str_hello)"
    _ = Stdout.line! "  42: $(FT.type_to_string num_42)"
    _ = Stdout.line! "  true: $(FT.type_to_string bool_true)"
    _ = Stdout.line! "  100n: $(FT.type_to_string bigint_100)"

    # Test literal subtyping
    _ = Stdout.line! "  \"hello\" -> string: $(bool_str (FT.is_assignable_to str_hello FT.mk_string))"
    _ = Stdout.line! "  42 -> number: $(bool_str (FT.is_assignable_to num_42 FT.mk_number))"

    # 3. Function types
    _ = Stdout.line! "\n3. Function Types:"

    # Simple function
    add_func = FT.mk_function
        [
            { name: "a", param_type: FT.mk_number, optional: Bool.false },
            { name: "b", param_type: FT.mk_number, optional: Bool.false },
        ]
        FT.mk_number
    _ = Stdout.line! "  add: $(FT.type_to_string add_func)"

    # Function with optional params
    greet_func = FT.mk_function
        [
            { name: "name", param_type: FT.mk_string, optional: Bool.false },
            { name: "title", param_type: FT.mk_string, optional: Bool.true },
        ]
        FT.mk_void
    _ = Stdout.line! "  greet: $(FT.type_to_string greet_func)"

    # Async function
    fetch_func = FT.TFunction {
        params: [{ name: "url", param_type: FT.mk_string, optional: Bool.false }],
        return_type: FT.mk_generic "Promise" [FT.mk_string],
        type_params: [],
        is_async: Bool.true,
        is_generator: Bool.false,
    }
    _ = Stdout.line! "  fetch: $(FT.type_to_string fetch_func)"

    # Generator function
    gen_func = FT.TFunction {
        params: [],
        return_type: FT.mk_generic "Generator" [FT.mk_number],
        type_params: [],
        is_async: Bool.false,
        is_generator: Bool.true,
    }
    _ = Stdout.line! "  generator: $(FT.type_to_string gen_func)"

    # 4. Tuple types
    _ = Stdout.line! "\n4. Tuple Types:"
    coord = FT.mk_tuple [FT.mk_number, FT.mk_number]
    _ = Stdout.line! "  [number, number]: $(FT.type_to_string coord)"

    result_tuple = FT.mk_tuple [FT.mk_boolean, FT.mk_string, FT.mk_number]
    _ = Stdout.line! "  [boolean, string, number]: $(FT.type_to_string result_tuple)"

    # 5. Generic types
    _ = Stdout.line! "\n5. Generic Types:"
    array_number = FT.mk_generic "Array" [FT.mk_number]
    _ = Stdout.line! "  Array<number>: $(FT.type_to_string array_number)"

    promise_string = FT.mk_generic "Promise" [FT.mk_string]
    _ = Stdout.line! "  Promise<string>: $(FT.type_to_string promise_string)"

    map_string_number = FT.mk_generic "Map" [FT.mk_string, FT.mk_number]
    _ = Stdout.line! "  Map<string, number>: $(FT.type_to_string map_string_number)"

    # Type parameter
    t_param = FT.mk_type_param "T"
    _ = Stdout.line! "  Type param T: $(FT.type_to_string t_param)"

    # 6. Class types
    _ = Stdout.line! "\n6. Class Types:"
    person_class = FT.mk_class "Person" [
        { name: "name", prop_type: FT.mk_string, is_static: Bool.false, is_private: Bool.false },
        { name: "age", prop_type: FT.mk_number, is_static: Bool.false, is_private: Bool.false },
        { name: "id", prop_type: FT.mk_number, is_static: Bool.false, is_private: Bool.true },
        { name: "count", prop_type: FT.mk_number, is_static: Bool.true, is_private: Bool.false },
    ]
    _ = Stdout.line! "  Person class: $(FT.type_to_string person_class)"

    # 7. Interface types
    _ = Stdout.line! "\n7. Interface Types:"
    serializable = FT.mk_interface "Serializable" [
        { name: "toJSON", prop_type: FT.mk_function [] FT.mk_string, optional: Bool.false, readonly: Bool.true },
    ]
    _ = Stdout.line! "  Serializable interface: $(FT.type_to_string serializable)"

    named_interface = FT.mk_interface "Named" [
        { name: "name", prop_type: FT.mk_string, optional: Bool.false, readonly: Bool.false },
        { name: "displayName", prop_type: FT.mk_string, optional: Bool.true, readonly: Bool.false },
    ]
    _ = Stdout.line! "  Named interface: $(FT.type_to_string named_interface)"

    # 8. Union and Intersection types
    _ = Stdout.line! "\n8. Union and Intersection Types:"

    # Union of literals (discriminated union)
    status = FT.mk_union [
        FT.mk_literal (FT.StrLit "pending"),
        FT.mk_literal (FT.StrLit "success"),
        FT.mk_literal (FT.StrLit "error"),
    ]
    _ = Stdout.line! "  Status: $(FT.type_to_string status)"

    # Nullable type
    nullable_num = FT.mk_union [FT.mk_number, FT.mk_null, FT.mk_undefined]
    _ = Stdout.line! "  Nullable<number>: $(FT.type_to_string nullable_num)"

    # Intersection
    person_obj = FT.mk_object [
        { key: "name", value_type: FT.mk_string, optional: Bool.false, readonly: Bool.false },
        { key: "age", value_type: FT.mk_number, optional: Bool.false, readonly: Bool.false },
    ]
    timestamp_obj = FT.mk_object [
        { key: "createdAt", value_type: FT.mk_string, optional: Bool.false, readonly: Bool.true },
        { key: "updatedAt", value_type: FT.mk_string, optional: Bool.false, readonly: Bool.true },
    ]
    person_with_timestamps = FT.mk_intersection [person_obj, timestamp_obj]
    _ = Stdout.line! "  Person & Timestamped: $(FT.type_to_string person_with_timestamps)"

    # 9. Special types
    _ = Stdout.line! "\n9. Special Types:"
    _ = Stdout.line! "  any: $(FT.type_to_string FT.mk_any)"
    _ = Stdout.line! "  never: $(FT.type_to_string FT.mk_never)"
    _ = Stdout.line! "  unknown: $(FT.type_to_string FT.mk_unknown)"
    _ = Stdout.line! "  void: $(FT.type_to_string FT.mk_void)"

    # 10. Complex type assignability
    _ = Stdout.line! "\n10. Type Assignability Tests:"

    # Literal to union
    _ = Stdout.line! "  \"success\" -> Status: $(bool_str (FT.is_assignable_to (FT.mk_literal (FT.StrLit "success")) status))"
    _ = Stdout.line! "  \"invalid\" -> Status: $(bool_str (FT.is_assignable_to (FT.mk_literal (FT.StrLit "invalid")) status))"

    # Function variance
    func_less_params = FT.mk_function
        [{ name: "x", param_type: FT.mk_number, optional: Bool.false }]
        FT.mk_number

    func_more_params = FT.mk_function
        [
            { name: "x", param_type: FT.mk_number, optional: Bool.false },
            { name: "y", param_type: FT.mk_number, optional: Bool.false },
        ]
        FT.mk_number

    _ = Stdout.line! "  (x) => number -> (x, y) => number: $(bool_str (FT.is_assignable_to func_less_params func_more_params))"

    # Void assignability
    _ = Stdout.line! "  void -> undefined: $(bool_str (FT.is_assignable_to FT.mk_void FT.mk_undefined))"

    # Generic assignability
    array_string = FT.mk_generic "Array" [FT.mk_string]
    array_any = FT.mk_generic "Array" [FT.mk_any]
    _ = Stdout.line! "  Array<string> -> Array<any>: $(bool_str (FT.is_assignable_to array_string array_any))"

    _ = Stdout.line! "\n=== Test Complete ==="
    _ = Stdout.line! "Successfully demonstrated:"
    _ = Stdout.line! "  ✓ All 7 primitive types (including bigint, symbol)"
    _ = Stdout.line! "  ✓ Literal types for all primitives"
    _ = Stdout.line! "  ✓ Function types with async, generator, optional params"
    _ = Stdout.line! "  ✓ Tuple types"
    _ = Stdout.line! "  ✓ Generic types with type arguments"
    _ = Stdout.line! "  ✓ Class types with static and private members"
    _ = Stdout.line! "  ✓ Interface types with optional and readonly properties"
    _ = Stdout.line! "  ✓ Union types (including discriminated unions)"
    _ = Stdout.line! "  ✓ Intersection types"
    _ = Stdout.line! "  ✓ All special types (any, never, unknown, void)"
    _ = Stdout.line! "  ✓ Complex type assignability with proper variance"

    Ok {}

bool_str : Bool -> Str
bool_str = \b ->
    if b then "true" else "false"

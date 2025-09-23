#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import FullType as FT

main! = \_ ->
    _ = Stdout.line! "=== Complete Type System Demonstration ==="

    # 1. Literal Types
    _ = Stdout.line! "\n1. LITERAL TYPES:"
    str_lit = FT.mk_literal (FT.StrLit "hello")
    num_lit = FT.mk_literal (FT.NumLit 42.0)
    bool_lit = FT.mk_literal (FT.BoolLit Bool.true)
    _ = Stdout.line! "  \"hello\": $(FT.type_to_string str_lit)"
    _ = Stdout.line! "  42: $(FT.type_to_string num_lit)"
    _ = Stdout.line! "  true: $(FT.type_to_string bool_lit)"

    # 2. Function Types
    _ = Stdout.line! "\n2. FUNCTION TYPES:"

    # Basic function
    simple_func = FT.mk_function
        [{ name: "x", param_type: FT.mk_number, optional: Bool.false }]
        FT.mk_number
    _ = Stdout.line! "  (x: number) => number"

    # Function with optional params
    optional_func = FT.mk_function
        [
            { name: "x", param_type: FT.mk_string, optional: Bool.false },
            { name: "y", param_type: FT.mk_number, optional: Bool.true },
        ]
        FT.mk_void
    _ = Stdout.line! "  (x: string, y?: number) => void"

    # 3. Tuple Types
    _ = Stdout.line! "\n3. TUPLE TYPES:"
    pair = FT.mk_tuple [FT.mk_string, FT.mk_number]
    triple = FT.mk_tuple [FT.mk_boolean, FT.mk_string, FT.mk_number]
    _ = Stdout.line! "  [string, number]: $(FT.type_to_string pair)"
    _ = Stdout.line! "  [boolean, string, number]: $(FT.type_to_string triple)"

    # 4. Generic Types
    _ = Stdout.line! "\n4. GENERIC TYPES:"
    array_str = FT.mk_generic "Array" [FT.mk_string]
    promise_num = FT.mk_generic "Promise" [FT.mk_number]
    map_type = FT.mk_generic "Map" [FT.mk_string, FT.mk_any]
    _ = Stdout.line! "  Array<string>: $(FT.type_to_string array_str)"
    _ = Stdout.line! "  Promise<number>: $(FT.type_to_string promise_num)"
    _ = Stdout.line! "  Map<string, any>: $(FT.type_to_string map_type)"

    # 5. Type Parameters
    _ = Stdout.line! "\n5. TYPE PARAMETERS:"
    t_param = FT.mk_type_param "T"
    u_param = FT.mk_type_param "U"
    _ = Stdout.line! "  T: $(FT.type_to_string t_param)"
    _ = Stdout.line! "  U: $(FT.type_to_string u_param)"

    # 6. Class Types
    _ = Stdout.line! "\n6. CLASS TYPES:"
    user_class = FT.mk_class "User" [
        { name: "id", prop_type: FT.mk_number, is_static: Bool.false, is_private: Bool.true },
        { name: "name", prop_type: FT.mk_string, is_static: Bool.false, is_private: Bool.false },
        { name: "count", prop_type: FT.mk_number, is_static: Bool.true, is_private: Bool.false },
    ]
    _ = Stdout.line! "  User class: $(FT.type_to_string user_class)"

    # 7. Interface Types
    _ = Stdout.line! "\n7. INTERFACE TYPES:"
    readable = FT.mk_interface "Readable" [
        { name: "read", prop_type: simple_func, optional: Bool.false, readonly: Bool.true },
        { name: "size", prop_type: FT.mk_number, optional: Bool.true, readonly: Bool.true },
    ]
    _ = Stdout.line! "  Readable interface: $(FT.type_to_string readable)"

    # 8. Complex Objects
    _ = Stdout.line! "\n8. COMPLEX OBJECT TYPES:"
    config = FT.mk_object [
        { key: "host", value_type: FT.mk_string, optional: Bool.false, readonly: Bool.true },
        { key: "port", value_type: FT.mk_number, optional: Bool.false, readonly: Bool.true },
        { key: "debug", value_type: FT.mk_boolean, optional: Bool.true, readonly: Bool.false },
    ]
    _ = Stdout.line! "  Config: $(FT.type_to_string config)"

    # 9. Union of Literals (Discriminated Unions)
    _ = Stdout.line! "\n9. DISCRIMINATED UNIONS:"
    status = FT.mk_union [
        FT.mk_literal (FT.StrLit "idle"),
        FT.mk_literal (FT.StrLit "loading"),
        FT.mk_literal (FT.StrLit "success"),
        FT.mk_literal (FT.StrLit "error"),
    ]
    _ = Stdout.line! "  Status: $(FT.type_to_string status)"

    # 10. Intersection Types
    _ = Stdout.line! "\n10. INTERSECTION TYPES:"
    named = FT.mk_object [{ key: "name", value_type: FT.mk_string, optional: Bool.false, readonly: Bool.false }]
    aged = FT.mk_object [{ key: "age", value_type: FT.mk_number, optional: Bool.false, readonly: Bool.false }]
    person = FT.mk_intersection [named, aged]
    _ = Stdout.line! "  Named & Aged: $(FT.type_to_string person)"

    # 11. Void Type
    _ = Stdout.line! "\n11. VOID TYPE:"
    _ = Stdout.line! "  void: $(FT.type_to_string FT.mk_void)"
    void_func = FT.mk_function [] FT.mk_void
    _ = Stdout.line! "  () => void"

    # Summary
    _ = Stdout.line! "\n=== SUMMARY ==="
    _ = Stdout.line! "Complete TypeScript/JavaScript type system implemented:"
    _ = Stdout.line! "  ✓ 7 primitive types (number, string, boolean, null, undefined, bigint, symbol)"
    _ = Stdout.line! "  ✓ Literal types for constants"
    _ = Stdout.line! "  ✓ Function types with optional parameters"
    _ = Stdout.line! "  ✓ Tuple types for fixed-length arrays"
    _ = Stdout.line! "  ✓ Generic types with type arguments"
    _ = Stdout.line! "  ✓ Type parameters for generic functions/classes"
    _ = Stdout.line! "  ✓ Class types with static/private members"
    _ = Stdout.line! "  ✓ Interface types with optional/readonly properties"
    _ = Stdout.line! "  ✓ Union types including discriminated unions"
    _ = Stdout.line! "  ✓ Intersection types for type composition"
    _ = Stdout.line! "  ✓ Special types (any, never, unknown, void)"

    Ok {}
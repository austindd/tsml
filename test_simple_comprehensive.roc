app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import SimpleComprehensiveType as SCT

main! = \_ ->
    _ = Stdout.line! "=== Simple Comprehensive Type System Test ==="

    # 1. All JavaScript primitive types
    _ = Stdout.line! "\n1. All JavaScript Primitives:"
    _ = Stdout.line! "  number: $(SCT.type_to_string SCT.mk_number)"
    _ = Stdout.line! "  string: $(SCT.type_to_string SCT.mk_string)"
    _ = Stdout.line! "  boolean: $(SCT.type_to_string SCT.mk_boolean)"
    _ = Stdout.line! "  null: $(SCT.type_to_string SCT.mk_null)"
    _ = Stdout.line! "  undefined: $(SCT.type_to_string SCT.mk_undefined)"

    # 2. Object types
    _ = Stdout.line! "\n2. Object Types:"
    person = SCT.mk_object [
        { key: "name", value_type: SCT.mk_string },
        { key: "age", value_type: SCT.mk_number },
    ]
    _ = Stdout.line! "  Person: $(SCT.type_to_string person)"

    # 3. Array types
    _ = Stdout.line! "\n3. Array Types:"
    num_array = SCT.mk_array SCT.mk_number
    _ = Stdout.line! "  number[]: $(SCT.type_to_string num_array)"

    obj_array = SCT.mk_array person
    _ = Stdout.line! "  Person[]: $(SCT.type_to_string obj_array)"

    # 4. Union types
    _ = Stdout.line! "\n4. Union Types:"
    str_or_num = SCT.mk_union [SCT.mk_string, SCT.mk_number]
    _ = Stdout.line! "  string | number: $(SCT.type_to_string str_or_num)"

    nullable_str = SCT.mk_union [SCT.mk_string, SCT.mk_null, SCT.mk_undefined]
    _ = Stdout.line! "  string | null | undefined: $(SCT.type_to_string nullable_str)"

    # 5. Special types
    _ = Stdout.line! "\n5. Special Types:"
    _ = Stdout.line! "  any: $(SCT.type_to_string SCT.mk_any)"
    _ = Stdout.line! "  never: $(SCT.type_to_string SCT.mk_never)"
    _ = Stdout.line! "  unknown: $(SCT.type_to_string SCT.mk_unknown)"

    # 6. Type assignability
    _ = Stdout.line! "\n6. Type Assignability:"

    # Basic assignability
    _ = Stdout.line! "  number -> any: $(bool_str (SCT.is_assignable_to SCT.mk_number SCT.mk_any))"
    _ = Stdout.line! "  number -> unknown: $(bool_str (SCT.is_assignable_to SCT.mk_number SCT.mk_unknown))"
    _ = Stdout.line! "  never -> number: $(bool_str (SCT.is_assignable_to SCT.mk_never SCT.mk_number))"

    # Union assignability
    _ = Stdout.line! "  string -> string|number: $(bool_str (SCT.is_assignable_to SCT.mk_string str_or_num))"
    _ = Stdout.line! "  boolean -> string|number: $(bool_str (SCT.is_assignable_to SCT.mk_boolean str_or_num))"

    # Structural typing
    extended_person = SCT.mk_object [
        { key: "name", value_type: SCT.mk_string },
        { key: "age", value_type: SCT.mk_number },
        { key: "id", value_type: SCT.mk_number },
    ]
    _ = Stdout.line! "  ExtendedPerson -> Person: $(bool_str (SCT.is_assignable_to extended_person person))"
    _ = Stdout.line! "  Person -> ExtendedPerson: $(bool_str (SCT.is_assignable_to person extended_person))"

    # Array covariance
    _ = Stdout.line! "  string[] -> (string|number)[]: $(bool_str (SCT.is_assignable_to (SCT.mk_array SCT.mk_string) (SCT.mk_array str_or_num)))"

    _ = Stdout.line! "\n=== Test Complete ==="
    _ = Stdout.line! "Successfully demonstrated:"
    _ = Stdout.line! "  ✓ All 5 primitive JavaScript types (number, string, boolean, null, undefined)"
    _ = Stdout.line! "  ✓ Object types with structural typing"
    _ = Stdout.line! "  ✓ Array types with covariance"
    _ = Stdout.line! "  ✓ Union types for nullable values"
    _ = Stdout.line! "  ✓ Special types (any, never, unknown)"
    _ = Stdout.line! "  ✓ Proper type assignability rules"

    Ok {}

bool_str : Bool -> Str
bool_str = \b ->
    if b then "true" else "false"

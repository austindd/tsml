app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ComprehensiveType as CT

main! = \_ ->
    _ = Stdout.line! "=== Basic Comprehensive Type System Test ==="

    # 1. Test primitive types
    _ = Stdout.line! "\n1. Primitive Types:"
    num = CT.mk_number
    str = CT.mk_string
    bool = CT.mk_boolean

    _ = Stdout.line! "  number: $(CT.type_to_string num)"
    _ = Stdout.line! "  string: $(CT.type_to_string str)"
    _ = Stdout.line! "  boolean: $(CT.type_to_string bool)"

    # 2. Test arrays
    _ = Stdout.line! "\n2. Array Types:"
    num_array = CT.mk_array num
    _ = Stdout.line! "  number[]: $(CT.type_to_string num_array)"

    # 3. Test simple objects
    _ = Stdout.line! "\n3. Object Types:"
    obj = CT.mk_object [
        { key: "name", value_type: str, optional: Bool.false, readonly: Bool.false },
        { key: "age", value_type: num, optional: Bool.false, readonly: Bool.false },
    ]
    _ = Stdout.line! "  Person: $(CT.type_to_string obj)"

    # 4. Test literals
    _ = Stdout.line! "\n4. Literal Types:"
    str_lit = CT.mk_literal (CT.StrLit "hello")
    _ = Stdout.line! "  \"hello\": $(CT.type_to_string str_lit)"

    # 5. Test special types
    _ = Stdout.line! "\n5. Special Types:"
    _ = Stdout.line! "  any: $(CT.type_to_string CT.mk_any)"
    _ = Stdout.line! "  never: $(CT.type_to_string CT.mk_never)"
    _ = Stdout.line! "  unknown: $(CT.type_to_string CT.mk_unknown)"

    # 6. Basic assignability (avoiding complex cases)
    _ = Stdout.line! "\n6. Basic Assignability:"
    _ = Stdout.line! "  number assignable to any: $(bool_str (CT.is_assignable_to num CT.mk_any))"
    _ = Stdout.line! "  never assignable to number: $(bool_str (CT.is_assignable_to CT.mk_never num))"
    _ = Stdout.line! "  number assignable to unknown: $(bool_str (CT.is_assignable_to num CT.mk_unknown))"

    _ = Stdout.line! "\n=== Test Complete ==="
    Ok {}

bool_str : Bool -> Str
bool_str = \b ->
    if b then "true" else "false"

app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import SimpleComprehensiveType as Type
import BasicTypeInfer as TI
import JSTypeCoercion as JSC
import TypedSymbolTable as TST
import JSGlobals
import Ast exposing [Node]

main! = \_ ->
    _ = Stdout.line! "=== Comprehensive Type System Test Suite ==="

    # Test 1: Basic Type Inference
    _ = Stdout.line! "\n1. Basic Type Inference"
    test_basic_inference!

    # Test 2: JavaScript Type Coercion
    _ = Stdout.line! "\n2. JavaScript Type Coercion"
    test_type_coercion!

    # Test 3: Symbol Table Operations
    _ = Stdout.line! "\n3. Symbol Table Operations"
    test_symbol_table!

    # Test 4: JavaScript Globals
    _ = Stdout.line! "\n4. JavaScript Globals"
    test_js_globals!

    # Test 5: Scope Management
    _ = Stdout.line! "\n5. Scope Management"
    test_scope_management!

    _ = Stdout.line! "\n=== All Tests Complete ==="
    Ok {}

test_basic_inference! = \{} ->
    # Test numeric literal
    num_node = NumericLiteral { value: 42.0, raw: "42" }
    num_type = TI.infer_type num_node
    _ = Stdout.line! "  Numeric literal: $(Type.type_to_str num_type)"

    # Test string literal
    str_node = StringLiteral { value: "hello", raw: "\"hello\"" }
    str_type = TI.infer_type str_node
    _ = Stdout.line! "  String literal: $(Type.type_to_str str_type)"

    # Test boolean literal
    bool_node = BooleanLiteral { value: Bool.true }
    bool_type = TI.infer_type bool_node
    _ = Stdout.line! "  Boolean literal: $(Type.type_to_str bool_type)"

    # Test binary expression
    binary_node = BinaryExpression {
        operator: Plus,
        left: num_node,
        right: num_node,
    }
    binary_type = TI.infer_type binary_node
    _ = Stdout.line! "  42 + 42: $(Type.type_to_str binary_type)"

    {}

test_type_coercion! = \{} ->
    # Test string concatenation with +
    str_num = JSC.infer_binary_op Type.mk_string Type.mk_number "+"
    _ = Stdout.line! "  string + number: $(Type.type_to_str str_num)"

    # Test numeric addition
    num_num = JSC.infer_binary_op Type.mk_number Type.mk_number "+"
    _ = Stdout.line! "  number + number: $(Type.type_to_str num_num)"

    # Test comparison operators
    comp_result = JSC.infer_binary_op Type.mk_string Type.mk_number ">"
    _ = Stdout.line! "  string > number: $(Type.type_to_str comp_result)"

    # Test unary operators
    not_bool = JSC.infer_unary_op Type.mk_boolean "!"
    _ = Stdout.line! "  !boolean: $(Type.type_to_str not_bool)"

    typeof_num = JSC.infer_unary_op Type.mk_number "typeof"
    _ = Stdout.line! "  typeof number: $(Type.type_to_str typeof_num)"

    {}

test_symbol_table! = \{} ->
    # Create empty table
    table1 = TST.empty_table {}

    # Add a variable
    table2 = when TST.add_symbol table1 "myVar" Type.mk_number Bool.false is
        Ok t ->
            _ = Stdout.line! "  Added myVar: number"
            t
        Err _ ->
            _ = Stdout.line! "  Failed to add myVar"
            table1

    # Add a const
    table3 = when TST.add_symbol table2 "PI" Type.mk_number Bool.true is
        Ok t ->
            _ = Stdout.line! "  Added const PI: number"
            t
        Err _ ->
            _ = Stdout.line! "  Failed to add PI"
            table2

    # Try to update const (should fail)
    when TST.update_symbol_type table3 "PI" Type.mk_string is
        Ok _ ->
            _ = Stdout.line! "  ERROR: Allowed const reassignment"
            {}
        Err ConstReassignment ->
            _ = Stdout.line! "  Correctly prevented const reassignment"
            {}
        Err _ ->
            _ = Stdout.line! "  Other error updating PI"
            {}

    # Update mutable variable
    table4 = when TST.update_symbol_type table3 "myVar" Type.mk_string is
        Ok t ->
            _ = Stdout.line! "  Updated myVar to string"
            t
        Err _ ->
            _ = Stdout.line! "  Failed to update myVar"
            table3

    # Lookup variable
    when TST.lookup_symbol table4 "myVar" is
        Ok sym ->
            _ = Stdout.line! "  Found myVar: $(Type.type_to_str sym.sym_type)"
            {}
        Err _ ->
            _ = Stdout.line! "  Failed to find myVar"
            {}

    {}

test_js_globals! = \{} ->
    # Create table with JS globals
    global_table = TST.empty_table {}
        |> JSGlobals.add_js_globals

    # Test some common globals
    test_global = \table, name ->
        when TST.lookup_symbol table name is
            Ok sym ->
                const_str = if sym.is_const then "const" else "var"
                _ = Stdout.line! "  $(name): $(Type.type_to_str sym.sym_type) ($(const_str))"
                {}
            Err _ ->
                _ = Stdout.line! "  $(name): not found"
                {}

    test_global global_table "undefined"
    test_global global_table "NaN"
    test_global global_table "console"
    test_global global_table "Math"
    test_global global_table "window"

    {}

test_scope_management! = \{} ->
    # Start with global scope
    table1 = TST.empty_table {}

    # Add global variable
    table2 = when TST.add_symbol table1 "globalVar" Type.mk_number Bool.false is
        Ok t ->
            _ = Stdout.line! "  Added globalVar in global scope"
            t
        Err _ -> table1

    # Enter function scope
    table3 = TST.push_scope table2 FunctionScope
    _ = Stdout.line! "  Entered function scope"

    # Add function parameter
    table4 = when TST.add_symbol table3 "param" Type.mk_string Bool.false is
        Ok t ->
            _ = Stdout.line! "  Added param in function scope"
            t
        Err _ -> table3

    # Enter block scope
    table5 = TST.push_scope table4 BlockScope
    _ = Stdout.line! "  Entered block scope"

    # Add local variable
    table6 = when TST.add_symbol table5 "localVar" Type.mk_boolean Bool.false is
        Ok t ->
            _ = Stdout.line! "  Added localVar in block scope"
            t
        Err _ -> table5

    # Test variable resolution
    test_resolution = \table, name ->
        when TST.lookup_symbol table name is
            Ok sym ->
                _ = Stdout.line! "    $(name): found at level $(Num.to_str sym.scope_level)"
                {}
            Err _ ->
                _ = Stdout.line! "    $(name): not found"
                {}

    _ = Stdout.line! "  Variable resolution in block scope:"
    test_resolution table6 "localVar"
    test_resolution table6 "param"
    test_resolution table6 "globalVar"

    # Exit block scope
    table7 = when TST.pop_scope table6 is
        Ok t ->
            _ = Stdout.line! "  Exited block scope"
            t
        Err _ -> table6

    # localVar should not be accessible
    _ = Stdout.line! "  After exiting block:"
    test_resolution table7 "localVar"
    test_resolution table7 "param"
    test_resolution table7 "globalVar"

    {}

#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import TypedSymbolTable as TST
import JSGlobals
import MinimalType

main! = \_ ->
    _ = Stdout.line! "=== JavaScript Global Environment ==="

    # Create symbol table with JS globals
    empty_table = TST.empty_table {}
    global_table = JSGlobals.add_js_globals empty_table

    # Count how many globals were added
    globals = TST.get_all_symbols_in_scope global_table
    _ = Stdout.line! "\nAdded $(Num.to_str (List.len globals)) JavaScript globals"

    # Test looking up various globals
    _ = Stdout.line! "\n=== Common Globals Lookup ==="

    test_globals = [
        ("undefined", "Core language value"),
        ("NaN", "Not-a-Number"),
        ("Infinity", "Positive infinity"),
        ("console", "Console object"),
        ("Object", "Object constructor"),
        ("Array", "Array constructor"),
        ("Math", "Math namespace"),
        ("JSON", "JSON namespace"),
        ("Promise", "Promise constructor"),
        ("parseInt", "Parse integer function"),
        ("isNaN", "Check if NaN"),
        ("window", "Browser global"),
        ("document", "Browser DOM"),
        ("fetch", "Fetch API"),
        ("setTimeout", "Timer function"),
        ("process", "Node.js process"),
        ("Buffer", "Node.js Buffer"),
        ("__dirname", "Node.js directory"),
        ("require", "Node.js require"),
    ]

    List.for_each! test_globals \(name, description) ->
        when TST.lookup_symbol global_table name is
            Ok symbol ->
                type_str = MinimalType.type_str symbol.sym_type
                const_str = if symbol.is_const then " (const)" else " (var)"
                _ = Stdout.line! "  ✓ $(name): $(type_str)$(const_str) - $(description)"
                {}
            Err _ ->
                _ = Stdout.line! "  ✗ $(name): not found - $(description)"
                {}

    # Test type information
    _ = Stdout.line! "\n=== Type Information ==="

    typed_globals = [
        ("NaN", TNum, "number"),
        ("Infinity", TNum, "number"),
        ("__dirname", TStr, "string"),
        ("__filename", TStr, "string"),
        ("undefined", TUnknown, "unknown"),
        ("null", TUnknown, "unknown"),
    ]

    List.for_each! typed_globals \(name, expected_type, type_name) ->
        when TST.lookup_symbol global_table name is
            Ok symbol ->
                if symbol.sym_type == expected_type then
                    _ = Stdout.line! "  ✓ $(name) has type $(type_name)"
                    {}
                else
                    actual = MinimalType.type_str symbol.sym_type
                    _ = Stdout.line! "  ✗ $(name) expected $(type_name), got $(actual)"
                    {}
            Err _ ->
                _ = Stdout.line! "  ✗ $(name) not found"
                {}

    # Test const vs mutable
    _ = Stdout.line! "\n=== Const vs Mutable ==="

    const_test = [
        ("undefined", Bool.true, "is const"),
        ("NaN", Bool.true, "is const"),
        ("Math", Bool.true, "is const"),
        ("console", Bool.true, "is const"),
        ("window", Bool.false, "is mutable (can be reassigned)"),
        ("global", Bool.false, "is mutable"),
        ("exports", Bool.false, "is mutable"),
    ]

    List.for_each! const_test \(name, expected_const, description) ->
        when TST.lookup_symbol global_table name is
            Ok symbol ->
                if symbol.is_const == expected_const then
                    _ = Stdout.line! "  ✓ $(name) $(description)"
                    {}
                else
                    actual = if symbol.is_const then "const" else "mutable"
                    _ = Stdout.line! "  ✗ $(name) is $(actual)"
                    {}
            Err _ ->
                _ = Stdout.line! "  ✗ $(name) not found"
                {}

    # Test that we can shadow globals in nested scopes
    _ = Stdout.line! "\n=== Shadowing Globals ==="

    # Enter a function scope
    func_table = TST.push_scope global_table FunctionScope

    # Shadow some globals
    shadowed_table = when TST.add_symbol func_table "console" TStr Bool.false is
        Ok t -> t
        Err _ -> func_table

    # Check that local shadows global
    when TST.lookup_symbol shadowed_table "console" is
        Ok symbol ->
            type_str = MinimalType.type_str symbol.sym_type
            if symbol.sym_type == TStr then
                _ = Stdout.line! "  ✓ Local 'console' shadows global (type: $(type_str))"
                {}
            else
                _ = Stdout.line! "  ✗ Local 'console' has wrong type: $(type_str)"
                {}
        Err _ ->
            _ = Stdout.line! "  ✗ Failed to find shadowed console"
            {}

    # Exit scope and check global is back
    orig_table = when TST.pop_scope shadowed_table is
        Ok t -> t
        Err _ -> shadowed_table

    when TST.lookup_symbol orig_table "console" is
        Ok symbol ->
            if symbol.sym_type == TUnknown then
                _ = Stdout.line! "  ✓ Global 'console' accessible after leaving scope"
                {}
            else
                _ = Stdout.line! "  ✗ Global 'console' has unexpected type"
                {}
        Err _ ->
            _ = Stdout.line! "  ✗ Global console not found after scope exit"
            {}

    Ok {}
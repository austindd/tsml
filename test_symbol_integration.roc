#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import TypedSymbolTable as TST
import SimpleComprehensiveType as Type exposing [Type]

main! = \_ ->
    _ = Stdout.line! "=== Symbol Table Integration Demo ==="

    # Simulate a simple JavaScript program analysis
    # let x = 5;
    # function foo(a, b) {
    #   let local = x + a;
    #   return local + b;
    # }
    # x = 10;

    # Start with global scope
    table0 = TST.empty_table {}
    _ = Stdout.line! "\n1. Created global scope"

    # Add global variable x = 5
    table1 = when TST.add_symbol table0 "x" TNumber Bool.false is
        Ok t ->
            _ = Stdout.line! "2. Added global variable x: number"
            t
        Err _ -> table0

    # Enter function scope for foo
    table2 = TST.push_scope table1 FunctionScope
    _ = Stdout.line! "3. Entered function foo scope"

    # Add function parameters
    table3 = when TST.add_symbol table2 "a" TUnknown Bool.false is
        Ok t -> t
        Err _ -> table2

    table4 = when TST.add_symbol table3 "b" TUnknown Bool.false is
        Ok t ->
            _ = Stdout.line! "4. Added parameters a, b: unknown"
            t
        Err _ -> table3

    # Add local variable
    table5 = when TST.add_symbol table4 "local" TNumber Bool.false is
        Ok t ->
            _ = Stdout.line! "5. Added local variable local: number"
            t
        Err _ -> table4

    # Test symbol resolution inside function
    _ = Stdout.line! "\n=== Symbol Resolution Inside Function ==="
    test_symbols = ["local", "a", "b", "x", "notFound"]

    List.for_each! test_symbols \name ->
        when TST.lookup_symbol table5 name is
            Ok symbol ->
                type_str = Type.type_to_str symbol.sym_type
                scope_str = if symbol.scope_level == 0 then " (global)" else " (local)"
                _ = Stdout.line! "  ✓ $(name): $(type_str)$(scope_str)"
                {}
            Err _ ->
                _ = Stdout.line! "  ✗ $(name): not found"
                {}

    # Exit function scope
    table6 = when TST.pop_scope table5 is
        Ok t ->
            _ = Stdout.line! "\n6. Exited function scope"
            t
        Err _ -> table5

    # Update global x
    table7 = when TST.update_symbol_type table6 "x" TNumber is
        Ok t ->
            _ = Stdout.line! "7. Updated global x (simulating x = 10)"
            t
        Err _ -> table6

    # Verify function locals are gone
    _ = Stdout.line! "\n=== After Function Exit ==="
    when TST.lookup_symbol table7 "local" is
        Ok _ ->
            _ = Stdout.line! "  ✗ local is still accessible (shouldn't be!)"
            {}
        Err _ ->
            _ = Stdout.line! "  ✓ local is not accessible (correct)"
            {}

    when TST.lookup_symbol table7 "x" is
        Ok symbol ->
            type_str = Type.type_to_str symbol.sym_type
            _ = Stdout.line! "  ✓ x: $(type_str) (global still accessible)"
            {}
        Err _ ->
            _ = Stdout.line! "  ✗ x not found (should be!)"
            {}

    # Demo const handling
    _ = Stdout.line! "\n=== Const Handling ==="

    table8 = when TST.add_symbol table7 "PI" TNumber Bool.true is
        Ok t ->
            _ = Stdout.line! "Added const PI: number"
            t
        Err _ -> table7

    when TST.update_symbol_type table8 "PI" TString is
        Ok _ ->
            _ = Stdout.line! "  ✗ Allowed updating const (shouldn't!)"
            {}
        Err ConstReassignment ->
            _ = Stdout.line! "  ✓ Prevented const reassignment"
            {}
        Err _ ->
            _ = Stdout.line! "  ✗ Unexpected error"
            {}

    Ok {}

app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import TypedSymbolTable as TST
import SimpleComprehensiveType as Type exposing [Type]

main! = \_ ->
    _ = Stdout.line! "=== Symbol Table Management ==="

    # Start with empty global scope
    table0 = TST.empty_table {}
    _ = Stdout.line! "Created empty symbol table with global scope"

    # Add global variables
    table1 = when TST.add_symbol table0 "globalVar" TNumber Bool.false is
        Ok t -> t
        Err _ -> table0
    _ = Stdout.line! "Added globalVar: number to global scope"

    table2 = when TST.add_symbol table1 "globalConst" TString Bool.true is
        Ok t -> t
        Err _ -> table1
    _ = Stdout.line! "Added globalConst: string (const) to global scope"

    # Enter a function scope
    table3 = TST.push_scope table2 FunctionScope
    _ = Stdout.line! "\nEntered function scope"

    # Add function parameters and local variables
    table4 = when TST.add_symbol table3 "param1" TString Bool.false is
        Ok t -> t
        Err _ -> table3
    _ = Stdout.line! "Added param1: string to function scope"

    table5 = when TST.add_symbol table4 "localVar" TBoolean Bool.false is
        Ok t -> t
        Err _ -> table4
    _ = Stdout.line! "Added localVar: boolean to function scope"

    # Enter a block scope within the function
    table6 = TST.push_scope table5 BlockScope
    _ = Stdout.line! "\nEntered block scope"

    # Add block-scoped variable
    table7 = when TST.add_symbol table6 "blockVar" TNumber Bool.false is
        Ok t -> t
        Err _ -> table6
    _ = Stdout.line! "Added blockVar: number to block scope"

    # Test symbol lookup across scopes
    _ = Stdout.line! "\n=== Symbol Lookup Test ==="

    test_lookups = [
        ("blockVar", "Should find in current block scope"),
        ("localVar", "Should find in parent function scope"),
        ("param1", "Should find in parent function scope"),
        ("globalVar", "Should find in global scope"),
        ("globalConst", "Should find in global scope"),
        ("nonExistent", "Should not find"),
    ]

    List.for_each! test_lookups \(name, description) ->
        when TST.lookup_symbol table7 name is
            Ok symbol ->
                type_str = Type.type_to_str symbol.sym_type
                const_str = if symbol.is_const then " (const)" else ""
                _ = Stdout.line! "  ✓ $(name): $(type_str)$(const_str) - $(description)"
                {}
            Err _ ->
                _ = Stdout.line! "  ✗ $(name): not found - $(description)"
                {}

    # Test scope management
    _ = Stdout.line! "\n=== Scope Management ==="

    # Exit block scope
    table8 = when TST.pop_scope table7 is
        Ok t ->
            _ = Stdout.line! "Exited block scope"
            t
        Err _ ->
            _ = Stdout.line! "Failed to exit block scope"
            table7

    # Check that blockVar is no longer accessible
    when TST.lookup_symbol table8 "blockVar" is
        Ok _ ->
            _ = Stdout.line! "  ✗ blockVar still accessible after leaving block scope"
            {}
        Err _ ->
            _ = Stdout.line! "  ✓ blockVar not accessible after leaving block scope"
            {}

    # But function scope variables should still be accessible
    when TST.lookup_symbol table8 "localVar" is
        Ok _ ->
            _ = Stdout.line! "  ✓ localVar still accessible in function scope"
            {}
        Err _ ->
            _ = Stdout.line! "  ✗ localVar not accessible in function scope"
            {}

    # Test updating symbol types
    _ = Stdout.line! "\n=== Type Updates ==="

    # Try to update a regular variable
    table9 = when TST.update_symbol_type table8 "localVar" TNumber is
        Ok t ->
            _ = Stdout.line! "  ✓ Updated localVar from boolean to number"
            t
        Err _ ->
            _ = Stdout.line! "  ✗ Failed to update localVar type"
            table8

    # Try to update a const (should fail)
    when TST.update_symbol_type table9 "globalConst" TNumber is
        Ok _ ->
            _ = Stdout.line! "  ✗ Incorrectly allowed updating const"
            {}
        Err ConstReassignment ->
            _ = Stdout.line! "  ✓ Correctly prevented const reassignment"
            {}
        Err _ ->
            _ = Stdout.line! "  ✗ Unexpected error updating const"
            {}

    # Test duplicate detection
    _ = Stdout.line! "\n=== Duplicate Detection ==="

    when TST.add_symbol table9 "param1" TNumber Bool.false is
        Ok _ ->
            _ = Stdout.line! "  ✗ Incorrectly allowed duplicate symbol"
            {}
        Err DuplicateSymbol ->
            _ = Stdout.line! "  ✓ Correctly detected duplicate symbol in same scope"
            {}
        Err _ ->
            _ = Stdout.line! "  ✗ Unexpected error adding duplicate"
            {}

    # But shadowing in nested scope should work
    table10 = TST.push_scope table9 BlockScope
    when TST.add_symbol table10 "param1" TNumber Bool.false is
        Ok _ ->
            _ = Stdout.line! "  ✓ Allowed shadowing in nested scope"
            {}
        Err _ ->
            _ = Stdout.line! "  ✗ Failed to shadow in nested scope"
            {}

    Ok {}

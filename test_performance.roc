#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import OptimizedTypes as OT exposing [TType, TNum, TStr, TBool, TUnknown]
import OptimizedSymbolTable as OST
import TypeCache as TC
import MinimalType as MT exposing [TNum, TStr, TBool, TUnknown]
import TypedSymbolTable as TST
import Ast exposing [Node]

main! = \_ ->
    _ = Stdout.line! "=== Type System Performance Test ==="

    # Test 1: Type equality performance
    _ = Stdout.line! "\n1. Type Equality Checks"

    # Optimized version
    t1 = TNum
    t2 = TStr
    t3 = TNum

    _ = Stdout.line! "  Optimized type_eq:"
    _ = Stdout.line! "    TNum == TStr: $(bool_str (OT.type_eq t1 t2))"
    _ = Stdout.line! "    TNum == TNum: $(bool_str (OT.type_eq t1 t3))"

    # Original version for comparison
    mt1 = TNum
    mt2 = TStr
    mt3 = TNum

    _ = Stdout.line! "  Original type_eq:"
    _ = Stdout.line! "    TNum == TStr: $(bool_str (mt1 == mt2))"
    _ = Stdout.line! "    TNum == TNum: $(bool_str (mt1 == mt3))"

    # Test 2: Symbol table performance
    _ = Stdout.line! "\n2. Symbol Table Operations"

    # Optimized symbol table
    opt_table = OST.empty_table {}

    # Add 100 symbols
    opt_table1 = add_many_symbols_optimized opt_table 100
    _ = Stdout.line! "  Added 100 symbols to optimized table"

    # Lookup performance
    when OST.lookup_symbol opt_table1 "var_50" is
        Ok sym ->
            _ = Stdout.line! "  Found var_50 in optimized table: $(MT.type_str sym.sym_type)"
            {}
        Err _ ->
            _ = Stdout.line! "  Failed to find var_50"
            {}

    # Test 3: Type cache
    _ = Stdout.line! "\n3. Type Cache Performance"

    cache = TC.empty_cache 50

    # Create test nodes
    node1 = Identifier { name: "test1" }
    node2 = NumericLiteral { value: 42.0, raw: "42" }
    node3 = StringLiteral { value: "hello", raw: "\"hello\"" }

    # Insert into cache
    cache1 = TC.cache_insert cache node1 MT.TUnknown
    cache2 = TC.cache_insert cache1 node2 MT.TNum
    cache3 = TC.cache_insert cache2 node3 MT.TStr

    # Lookup from cache
    when TC.cache_lookup cache3 node2 is
        Ok (cached_type, cache4) ->
            _ = Stdout.line! "  Cache hit for node2: $(MT.type_str cached_type)"

            # Get stats
            stats = TC.cache_stats cache4
            _ = Stdout.line! "  Cache stats:"
            _ = Stdout.line! "    Hits: $(Num.to_str stats.hits)"
            _ = Stdout.line! "    Misses: $(Num.to_str stats.misses)"
            _ = Stdout.line! "    Size: $(Num.to_str stats.size)"
            {}
        Err _ ->
            _ = Stdout.line! "  Cache miss for node2"
            {}

    # Test 4: Scope management
    _ = Stdout.line! "\n4. Scope Management"

    scope_table = OST.empty_table {}
        |> OST.push_scope FunctionScope
        |> OST.push_scope BlockScope

    _ = Stdout.line! "  Pushed 2 scopes (function, block)"

    # Add symbol in block scope
    scope_table1 = when OST.add_symbol scope_table "local_var" MT.TStr Bool.false is
        Ok t ->
            _ = Stdout.line! "  Added local_var in block scope"
            t
        Err _ ->
            _ = Stdout.line! "  Failed to add local_var"
            scope_table

    # Pop block scope
    scope_table2 = when OST.pop_scope scope_table1 is
        Ok t ->
            _ = Stdout.line! "  Popped block scope"
            t
        Err _ ->
            _ = Stdout.line! "  Failed to pop scope"
            scope_table1

    # Try to find local_var (should fail)
    when OST.lookup_symbol scope_table2 "local_var" is
        Ok _ ->
            _ = Stdout.line! "  Error: local_var still accessible after scope pop"
            {}
        Err _ ->
            _ = Stdout.line! "  Correctly cannot access local_var after scope pop"
            {}

    # Test 5: Type operations
    _ = Stdout.line! "\n5. Type Operations"

    union1 = OT.type_union TNum TNum
    _ = Stdout.line! "  Union(TNum, TNum) = $(OT.type_str union1)"

    union2 = OT.type_union TNum TStr
    _ = Stdout.line! "  Union(TNum, TStr) = $(OT.type_str union2)"

    intersect1 = OT.type_intersect TNum TNum
    _ = Stdout.line! "  Intersect(TNum, TNum) = $(OT.type_str intersect1)"

    intersect2 = OT.type_intersect TUnknown TStr
    _ = Stdout.line! "  Intersect(TUnknown, TStr) = $(OT.type_str intersect2)"

    _ = Stdout.line! "\n=== Performance Test Complete ==="
    Ok {}

# Helper to add many symbols to optimized table
add_many_symbols_optimized : OST.SymbolTable, U64 -> OST.SymbolTable
add_many_symbols_optimized = \table, count ->
    List.range { start: At 0, end: Before (Num.to_nat count) }
    |> List.walk table \t, i ->
        name = "var_$(Num.to_str i)"
        sym_type = if i % 2 == 0 then TNum else TStr
        when OST.add_symbol t name sym_type Bool.false is
            Ok new_t -> new_t
            Err _ -> t

# Helper to convert bool to string
bool_str : Bool -> Str
bool_str = \b ->
    if b then "true" else "false"
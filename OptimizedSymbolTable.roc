module [
    SymbolTable,
    Symbol,
    ScopeType,
    empty_table,
    push_scope,
    pop_scope,
    add_symbol,
    lookup_symbol,
    update_symbol_type,
    get_all_symbols_in_scope,
]

Import SimpleComprehensiveType as Type exposing [Type]

# Symbol information
Symbol : {
    name : Str,
    sym_type : Type,
    is_const : Bool,
    scope_level : U64,
}

# Scope types
ScopeType : [GlobalScope, FunctionScope, BlockScope, ModuleScope]

# Optimized symbol table using a flat list with scope levels
# This avoids nested structures which can cause performance issues
SymbolTable : {
    symbols : List Symbol,         # All symbols in flat list
    scope_stack : List ScopeType,  # Stack of scope types
    current_level : U64,            # Current scope level
}

# Create empty symbol table
empty_table : {} -> SymbolTable
empty_table = \{} ->
    {
        symbols: [],
        scope_stack: [GlobalScope],
        current_level: 0,
    }

# Enter a new scope
push_scope : SymbolTable, ScopeType -> SymbolTable
push_scope = \table, scope_type ->
    {
        symbols: table.symbols,
        scope_stack: List.append table.scope_stack scope_type,
        current_level: table.current_level + 1,
    }

# Exit current scope
pop_scope : SymbolTable -> Result SymbolTable [AlreadyAtGlobalScope]
pop_scope = \table ->
    if table.current_level == 0 then
        Err AlreadyAtGlobalScope
    else
        # Filter out symbols from the current scope
        new_symbols = List.keep_if table.symbols \symbol ->
            symbol.scope_level < table.current_level

        # Remove the last scope type
        new_scope_stack = when List.drop_last table.scope_stack 1 is
            [] -> [GlobalScope]  # Shouldn't happen
            stack -> stack

        Ok {
            symbols: new_symbols,
            scope_stack: new_scope_stack,
            current_level: table.current_level - 1,
        }

# Add a symbol to current scope
add_symbol : SymbolTable, Str, Type, Bool -> Result SymbolTable [DuplicateSymbol]
add_symbol = \table, name, sym_type, is_const ->
    # Check for duplicate in current scope only
    existing = List.find_first table.symbols \sym ->
        sym.name == name && sym.scope_level == table.current_level

    when existing is
        Ok _ -> Err DuplicateSymbol
        Err _ ->
            new_symbol = {
                name,
                sym_type,
                is_const,
                scope_level: table.current_level,
            }
            Ok { table & symbols: List.append table.symbols new_symbol }

# Look up a symbol (searches from current scope upward)
lookup_symbol : SymbolTable, Str -> Result Symbol [SymbolNotFound]
lookup_symbol = \table, name ->
    # Find all symbols with this name
    matching = List.keep_if table.symbols \sym -> sym.name == name

    # Find the one with the highest scope level (most recent)
    List.walk matching (Err SymbolNotFound) \best, sym ->
        when best is
            Err _ -> Ok sym
            Ok best_sym ->
                if sym.scope_level > best_sym.scope_level then
                    Ok sym
                else
                    Ok best_sym

# Update symbol type if mutable
update_symbol_type : SymbolTable, Str, Type -> Result SymbolTable [SymbolNotFound, ConstReassignment]
update_symbol_type = \table, name, new_type ->
    when lookup_symbol table name is
        Ok symbol ->
            if symbol.is_const then
                Err ConstReassignment
            else
                # Update the symbol in place
                new_symbols = List.map table.symbols \sym ->
                    if sym.name == name && sym.scope_level == symbol.scope_level then
                        { sym & sym_type: new_type }
                    else
                        sym
                Ok { table & symbols: new_symbols }
        Err _ -> Err SymbolNotFound

# Get all symbols in current scope
get_all_symbols_in_scope : SymbolTable -> List Symbol
get_all_symbols_in_scope = \table ->
    List.keep_if table.symbols \sym ->
        sym.scope_level == table.current_level

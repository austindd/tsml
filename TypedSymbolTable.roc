module [
    SymbolTable,
    ScopeType,
    Symbol,
    empty_table,
    push_scope,
    pop_scope,
    add_symbol,
    lookup_symbol,
    update_symbol_type,
    get_current_scope,
    get_all_symbols_in_scope,
]

import SimpleComprehensiveType as Type exposing [Type]

# Symbol represents a variable or function in a scope
Symbol : {
    name : Str,
    sym_type : Type,
    is_const : Bool,  # const vs let/var
    is_function : Bool,
    scope_level : U64,
}

# Different types of scopes in JavaScript
ScopeType : [
    GlobalScope,
    FunctionScope,
    BlockScope,
    ForLoopScope,
    CatchScope,
    WithScope,
    ModuleScope,
]

# A single scope in the scope chain
Scope : {
    scope_type : ScopeType,
    symbols : List Symbol,
    parent_index : Result U64 [NoParent],
}

# Symbol table maintains a stack of scopes
SymbolTable : {
    scopes : List Scope,
    current_scope_index : U64,
}

# Create an empty symbol table with global scope
empty_table : {} -> SymbolTable
empty_table = \{} ->
    global_scope = {
        scope_type: GlobalScope,
        symbols: [],
        parent_index: Err NoParent,
    }
    {
        scopes: [global_scope],
        current_scope_index: 0,
    }

# Push a new scope onto the stack
push_scope : SymbolTable, ScopeType -> SymbolTable
push_scope = \table, scope_type ->
    new_scope = {
        scope_type,
        symbols: [],
        parent_index: Ok table.current_scope_index,
    }
    new_index = List.len table.scopes |> Num.to_u32
    {
        scopes: List.append table.scopes new_scope,
        current_scope_index: new_index,
    }

# Pop the current scope (return to parent)
pop_scope : SymbolTable -> Result SymbolTable [AlreadyAtGlobalScope]
pop_scope = \table ->
    if table.current_scope_index == 0 then
        Err AlreadyAtGlobalScope
    else
        current_scope = List.get table.scopes (Num.to_u64 table.current_scope_index)
        when current_scope is
            Ok scope ->
                when scope.parent_index is
                    Ok parent_idx ->
                        Ok { table & current_scope_index: parent_idx }
                    Err NoParent ->
                        Err AlreadyAtGlobalScope
            Err _ ->
                Err AlreadyAtGlobalScope

# Add a new symbol to the current scope
add_symbol : SymbolTable, Str, Type, Bool -> Result SymbolTable [DuplicateSymbol]
add_symbol = \table, name, sym_type, is_const ->
    current_idx = Num.to_u64 table.current_scope_index
    when List.get table.scopes current_idx is
        Ok current_scope ->
            # Check for duplicates in current scope
            existing = List.find_first current_scope.symbols \sym -> sym.name == name
            when existing is
                Ok _ -> Err DuplicateSymbol
                Err _ ->
                    new_symbol = {
                        name,
                        sym_type,
                        is_const,
                        is_function: Bool.false,
                        scope_level: table.current_scope_index,
                    }
                    updated_scope = { current_scope &
                        symbols: List.append current_scope.symbols new_symbol
                    }
                    updated_scopes = List.set table.scopes current_idx updated_scope
                    Ok { table & scopes: updated_scopes }
        Err _ ->
            Err DuplicateSymbol  # Should not happen

# Look up a symbol in the scope chain
lookup_symbol : SymbolTable, Str -> Result Symbol [SymbolNotFound]
lookup_symbol = \table, name ->
    lookup_in_scope table name table.current_scope_index

lookup_in_scope : SymbolTable, Str, U64 -> Result Symbol [SymbolNotFound]
lookup_in_scope = \table, name, scope_index ->
    when List.get table.scopes (Num.to_u64 scope_index) is
        Ok scope ->
            # First check current scope
            when List.find_first scope.symbols \sym -> sym.name == name is
                Ok symbol -> Ok symbol
                Err _ ->
                    # Check parent scope
                    when scope.parent_index is
                        Ok parent_idx ->
                            lookup_in_scope table name parent_idx
                        Err NoParent ->
                            Err SymbolNotFound
        Err _ ->
            Err SymbolNotFound

# Update the type of an existing symbol
update_symbol_type : SymbolTable, Str, Type -> Result SymbolTable [SymbolNotFound, ConstReassignment]
update_symbol_type = \table, name, new_type ->
    when lookup_symbol table name is
        Ok symbol ->
            if symbol.is_const then
                Err ConstReassignment
            else
                update_symbol_in_scope table name new_type symbol.scope_level
        Err _ ->
            Err SymbolNotFound

update_symbol_in_scope : SymbolTable, Str, Type, U64 -> Result SymbolTable [SymbolNotFound]
update_symbol_in_scope = \table, name, new_type, scope_level ->
    scope_idx = Num.to_u64 scope_level
    when List.get table.scopes scope_idx is
        Ok scope ->
            updated_symbols = List.map scope.symbols \sym ->
                if sym.name == name then
                    { sym & sym_type: new_type }
                else
                    sym
            updated_scope = { scope & symbols: updated_symbols }
            updated_scopes = List.set table.scopes scope_idx updated_scope
            Ok { table & scopes: updated_scopes }
        Err _ ->
            Err SymbolNotFound

# Get the current scope type
get_current_scope : SymbolTable -> Result ScopeType [InvalidScope]
get_current_scope = \table ->
    when List.get table.scopes (Num.to_u64 table.current_scope_index) is
        Ok scope -> Ok scope.scope_type
        Err _ -> Err InvalidScope

# Get all symbols in the current scope (not including parent scopes)
get_all_symbols_in_scope : SymbolTable -> List Symbol
get_all_symbols_in_scope = \table ->
    when List.get table.scopes (Num.to_u64 table.current_scope_index) is
        Ok scope -> scope.symbols
        Err _ -> []

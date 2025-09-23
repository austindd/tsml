module [
    infer_with_scope,
    analyze_program,
]

import MinimalType exposing [TType]
import Ast exposing [Node]
import TypedSymbolTable as TST
import BasicTypeInfer
import JSTypeCoercion
import Option exposing [Option, Some, None]

# Type inference with symbol table tracking
InferState : {
    symbol_table : TST.SymbolTable,
    current_type : TType,
}

# Analyze a program and build symbol table
analyze_program : Node -> InferState
analyze_program = \node ->
    initial_state = {
        symbol_table: TST.empty_table {},
        current_type: TUnknown,
    }
    analyze_node node initial_state

# Analyze a node and update state
analyze_node : Node, InferState -> InferState
analyze_node = \node, state ->
    when node is
        Program { body } ->
            List.walk body state analyze_node

        # Variable declarations
        VariableDeclaration { declarations, kind } ->
            is_const = when kind is
                Const -> Bool.true
                _ -> Bool.false

            List.walk declarations state \s, decl_node ->
                when decl_node is
                    VariableDeclarator { id, init } ->
                        # Get the type of the initializer
                        init_type = when init is
                            Some init_expr ->
                                BasicTypeInfer.infer_type init_expr
                            None ->
                                TUnknown

                        # Add variable to symbol table
                        when id is
                            Identifier { name } ->
                                new_table = when TST.add_symbol s.symbol_table name init_type is_const is
                                    Ok t -> t
                                    Err _ -> s.symbol_table  # Handle duplicate
                                { s & symbol_table: new_table }
                            _ ->
                                s  # Handle patterns later
                    _ ->
                        s

        # Function declarations
        FunctionDeclaration { id, params } ->
            # Add function to current scope
            func_state = when id is
                Some func_id ->
                    when func_id is
                        Identifier { name } ->
                            new_table = when TST.add_symbol state.symbol_table name TUnknown Bool.false is
                                Ok t -> t
                                Err _ -> state.symbol_table
                            { state & symbol_table: new_table }
                        _ ->
                            state
                _ ->
                    state

            # Enter function scope
            func_table = TST.push_scope func_state.symbol_table FunctionScope

            # Add parameters to function scope
            params_table = List.walk params func_table \table, param ->
                when param is
                    Identifier { name } ->
                        when TST.add_symbol table name TUnknown Bool.false is
                            Ok t -> t
                            Err _ -> table
                    _ ->
                        table

            { state & symbol_table: params_table }

        # Block statements create new scope
        BlockStatement { body } ->
            # Enter block scope
            block_table = TST.push_scope state.symbol_table BlockScope
            block_state = { state & symbol_table: block_table }

            # Process block body
            result_state = List.walk body block_state analyze_node

            # Exit block scope
            final_table = when TST.pop_scope result_state.symbol_table is
                Ok t -> t
                Err _ -> result_state.symbol_table

            { result_state & symbol_table: final_table }

        # Assignment expressions
        AssignmentExpression { left, right, operator } ->
            when left is
                Identifier { name } ->
                    # Get type of right side
                    right_type = BasicTypeInfer.infer_type right

                    # Update symbol type if it exists and is mutable
                    new_table = when TST.update_symbol_type state.symbol_table name right_type is
                        Ok t -> t
                        Err _ -> state.symbol_table

                    { state & symbol_table: new_table, current_type: right_type }
                _ ->
                    state

        # Identifier lookup
        Identifier { name } ->
            # Look up type in symbol table
            id_type = when TST.lookup_symbol state.symbol_table name is
                Ok symbol -> symbol.sym_type
                Err _ -> TUnknown

            { state & current_type: id_type }

        # For other expressions, use basic inference
        _ ->
            expr_type = BasicTypeInfer.infer_type node
            { state & current_type: expr_type }

# Main inference function that maintains scope
infer_with_scope : Node, TST.SymbolTable -> (TType, TST.SymbolTable)
infer_with_scope = \node, table ->
    state = analyze_node node { symbol_table: table, current_type: TUnknown }
    (state.current_type, state.symbol_table)
module [
    analyze_module,
    ModuleAnalysis,
]

import MinimalType exposing [TType]
import Ast exposing [Node]
import TypedSymbolTable as TST
import ModuleSystem as MS
import BasicTypeInfer
import JSGlobals
import Option exposing [Option, Some, None]

# Result of analyzing a module
ModuleAnalysis : {
    module_path : Str,
    registry : MS.ModuleRegistry,
    symbol_table : TST.SymbolTable,
    exported_types : List { name : Str, type : TType },
}

# Analyze a JavaScript/TypeScript module
analyze_module : Str, Node -> ModuleAnalysis
analyze_module = \module_path, ast ->
    # Initialize with JS globals
    initial_table = TST.empty_table {}
        |> JSGlobals.add_js_globals

    # Create module registry
    initial_registry = MS.empty_registry {}
        |> MS.register_module module_path

    # Process the AST
    result = process_ast ast initial_registry initial_table

    # Collect exported types
    exports = when MS.get_module_exports result.registry module_path is
        Ok module_exports ->
            List.map module_exports \export -> {
                name: export.name,
                type: export.export_type,
            }
        Err _ -> []

    {
        module_path,
        registry: result.registry,
        symbol_table: result.table,
        exported_types: exports,
    }

# Process AST nodes recursively
process_ast : Node, MS.ModuleRegistry, TST.SymbolTable -> { registry : MS.ModuleRegistry, table : TST.SymbolTable }
process_ast = \node, registry, table ->
    when node is
        Program { body } ->
            List.walk body { registry, table } \state, stmt ->
                process_ast stmt state.registry state.table

        # ES6 export default
        ExportDefaultDeclaration { declaration } ->
            # Infer type of exported expression
            export_type = BasicTypeInfer.infer_type declaration

            # Add to module exports
            new_registry = when MS.add_export registry "default" "default" export_type Bool.true is
                Ok r -> r
                Err _ -> registry

            { registry: new_registry, table }

        # ES6 named export: export { a, b as c }
        ExportNamedDeclaration { specifiers, source } ->
            when source is
                Some _ ->
                    # Re-export from another module
                    { registry, table }
                None ->
                    # Export local bindings
                    new_registry = List.walk specifiers registry \reg, spec ->
                        when spec is
                            ExportSpecifier { local, exported } ->
                                # Look up type in symbol table
                                local_name = extract_identifier_name local
                                export_name = extract_identifier_name exported

                                export_type = when TST.lookup_symbol table local_name is
                                    Ok symbol -> symbol.sym_type
                                    Err _ -> TUnknown

                                when MS.add_export reg export_name local_name export_type Bool.false is
                                    Ok r -> r
                                    Err _ -> reg
                            _ -> reg

                    { registry: new_registry, table }

        # ES6 import
        ImportDeclaration { specifiers, source } ->
            source_path = extract_literal_value source

            new_registry = List.walk specifiers registry \reg, spec ->
                when spec is
                    ImportDefaultSpecifier { local } ->
                        local_name = extract_identifier_name local
                        when MS.add_import reg source_path (MS.make_default_import local_name) is
                            Ok r -> r
                            Err _ -> reg

                    ImportSpecifier { imported, local } ->
                        imported_name = extract_identifier_name imported
                        local_name = extract_identifier_name local
                        when MS.add_import reg source_path (MS.make_named_import [{ imported: imported_name, local: local_name }]) is
                            Ok r -> r
                            Err _ -> reg

                    ImportNamespaceSpecifier { local } ->
                        local_name = extract_identifier_name local
                        when MS.add_import reg source_path (MS.make_namespace_import local_name) is
                            Ok r -> r
                            Err _ -> reg

                    _ -> reg

            { registry: new_registry, table }

        # Variable declarations - update symbol table
        VariableDeclaration { declarations, kind } ->
            is_const = when kind is
                Const -> Bool.true
                _ -> Bool.false

            new_table = List.walk declarations table \t, decl ->
                when decl is
                    VariableDeclarator { id, init } ->
                        var_name = extract_identifier_name id

                        # Infer type from initializer
                        var_type = when init is
                            Some init_expr -> BasicTypeInfer.infer_type init_expr
                            None -> TUnknown

                        # Add to symbol table
                        when TST.add_symbol t var_name var_type is_const is
                            Ok new_t -> new_t
                            Err _ -> t  # Handle duplicates
                    _ -> t

            { registry, table: new_table }

        # Function declarations
        FunctionDeclaration { id, params, body } ->
            func_name = when id is
                Some func_id -> extract_identifier_name func_id
                None -> ""

            # Add function to symbol table
            new_table1 = if func_name != "" then
                when TST.add_symbol table func_name TUnknown Bool.false is
                    Ok t -> t
                    Err _ -> table
            else
                table

            # Enter function scope
            func_table = TST.push_scope new_table1 FunctionScope

            # Add parameters
            params_table = List.walk params func_table \t, param ->
                param_name = extract_identifier_name param
                when TST.add_symbol t param_name TUnknown Bool.false is
                    Ok new_t -> new_t
                    Err _ -> t

            # Process function body
            result = process_ast body registry params_table

            # Exit function scope
            final_table = when TST.pop_scope result.table is
                Ok t -> t
                Err _ -> result.table

            { registry: result.registry, table: final_table }

        # Block statements
        BlockStatement { body } ->
            # Enter block scope
            block_table = TST.push_scope table BlockScope

            # Process statements
            result = List.walk body { registry, table: block_table } \state, stmt ->
                process_ast stmt state.registry state.table

            # Exit block scope
            final_table = when TST.pop_scope result.table is
                Ok t -> t
                Err _ -> result.table

            { registry: result.registry, table: final_table }

        # For other nodes, recurse on children
        _ ->
            { registry, table }

# Helper to extract identifier name
extract_identifier_name : Node -> Str
extract_identifier_name = \node ->
    when node is
        Identifier { name } -> name
        _ -> ""

# Helper to extract literal string value
extract_literal_value : Node -> Str
extract_literal_value = \node ->
    when node is
        StringLiteral { value } -> value
        TemplateLiteral { quasis } ->
            when List.first quasis is
                Ok quasi ->
                    when quasi is
                        TemplateElement { value } -> value.cooked
                        _ -> ""
                Err _ -> ""
        _ -> ""

# Helper to create import kinds
make_default_import : Str -> MS.ImportKind
make_default_import = \name ->
    DefaultImport name

make_named_import : List { imported : Str, local : Str } -> MS.ImportKind
make_named_import = \imports ->
    NamedImport imports

make_namespace_import : Str -> MS.ImportKind
make_namespace_import = \name ->
    NamespaceImport name
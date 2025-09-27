module [
    ModuleInfo,
    ModuleExport,
    ModuleImport,
    ModuleRegistry,
    ImportKind,
    empty_registry,
    register_module,
    add_export,
    add_import,
    resolve_import,
    get_module_exports,
    analyze_module_node,
    make_default_import,
    make_named_import,
    make_namespace_import,
    make_side_effect,
]

import SimpleComprehensiveType as Type exposing [Type]
import TypedSymbolTable as TST

# Information about a single export
ModuleExport : {
    name : Str,           # Export name (could be "default")
    local_name : Str,     # Local variable name
    export_type : Type,  # Type of the exported value
    is_default : Bool,    # True for default exports
}

# Information about a single import
ModuleImport : {
    source : Str,          # Module path/name being imported from
    imported : ImportKind, # What is being imported
}

ImportKind : [
    DefaultImport Str,                    # import foo from 'module'
    NamedImport (List { imported : Str, local : Str }), # import {a as b} from 'module'
    NamespaceImport Str,                  # import * as foo from 'module'
    SideEffect,                           # import 'module'
]

# Information about a module
ModuleInfo : {
    path : Str,                    # Module path/identifier
    exports : List ModuleExport,   # What this module exports
    imports : List ModuleImport,   # What this module imports
    symbol_table : TST.SymbolTable, # Module's symbol table
}

# Registry of all modules in the system
ModuleRegistry : {
    modules : List ModuleInfo,
    current_module : Result Str [NoModule],
}

make_default_import : Str -> ImportKind
make_default_import = \name ->
    DefaultImport name
    
make_named_import : List { imported : Str, local : Str } -> ImportKind
make_named_import = \imports ->
    NamedImport imports

make_namespace_import : Str -> ImportKind
make_namespace_import = \name ->
    NamespaceImport name

make_side_effect : {} -> ImportKind
make_side_effect = \{} ->
    SideEffect

# Create an empty module registry
empty_registry : {} -> ModuleRegistry
empty_registry = \{} ->
    {
        modules: [],
        current_module: Err NoModule,
    }

# Register a new module
register_module : ModuleRegistry, Str -> ModuleRegistry
register_module = \registry, path ->
    new_module = {
        path,
        exports: [],
        imports: [],
        symbol_table: TST.empty_table {} |> TST.push_scope ModuleScope,
    }
    {
        modules: List.append registry.modules new_module,
        current_module: Ok path,
    }

# Add an export to the current module
add_export : ModuleRegistry, Str, Str, Type, Bool -> Result ModuleRegistry [NoCurrentModule, DuplicateExport]
add_export = \registry, name, local_name, export_type, is_default ->
    when registry.current_module is
        Ok module_path ->
            when find_module registry module_path is
                Ok module_info ->
                    # Check for duplicate
                    existing = List.find_first module_info.exports \e -> e.name == name
                    when existing is
                        Ok _ -> Err DuplicateExport
                        Err _ ->
                            new_export = { name, local_name, export_type, is_default }
                            updated_module = { module_info &
                                exports: List.append module_info.exports new_export
                            }
                            updated_modules = update_module registry.modules module_path updated_module
                            Ok { registry & modules: updated_modules }
                Err _ -> Err NoCurrentModule
        Err _ -> Err NoCurrentModule

# Add an import to the current module
add_import : ModuleRegistry, Str, ImportKind -> Result ModuleRegistry [NoCurrentModule]
add_import = \registry, source, imported ->
    when registry.current_module is
        Ok module_path ->
            when find_module registry module_path is
                Ok module_info ->
                    new_import = { source, imported }
                    updated_module = { module_info &
                        imports: List.append module_info.imports new_import
                    }
                    updated_modules = update_module registry.modules module_path updated_module
                    Ok { registry & modules: updated_modules }
                Err _ -> Err NoCurrentModule
        Err _ -> Err NoCurrentModule

# Resolve an import - find what a module exports
resolve_import : ModuleRegistry, Str, Str -> Result ModuleExport [ModuleNotFound, ExportNotFound]
resolve_import = \registry, module_path, export_name ->
    when find_module registry module_path is
        Ok module_info ->
            when List.find_first module_info.exports \e -> e.name == export_name is
                Ok export -> Ok export
                Err _ -> Err ExportNotFound
        Err _ -> Err ModuleNotFound

# Get all exports from a module
get_module_exports : ModuleRegistry, Str -> Result (List ModuleExport) [ModuleNotFound]
get_module_exports = \registry, module_path ->
    when find_module registry module_path is
        Ok module_info -> Ok module_info.exports
        Err _ -> Err ModuleNotFound

# Helper: Find a module by path
find_module : ModuleRegistry, Str -> Result ModuleInfo [NotFound]
find_module = \registry, path ->
    List.find_first registry.modules \m -> m.path == path

# Helper: Update a module in the list
update_module : List ModuleInfo, Str, ModuleInfo -> List ModuleInfo
update_module = \modules, path, new_module ->
    List.map modules \m ->
        if m.path == path then new_module else m

# Analyze an import/export node and update registry
analyze_module_node : ModuleRegistry, ModuleNode -> ModuleRegistry
analyze_module_node = \registry, node ->
    when node is
        # ES6 default export: export default expr
        ExportDefault { expression, type_hint } ->
            export_type = type_hint |> Result.with_default TUnknown
            when add_export registry "default" "default" export_type Bool.true is
                Ok r -> r
                Err _ -> registry

        # ES6 named export: export { a, b as c }
        ExportNamed { specifiers } ->
            List.walk specifiers registry \reg, spec ->
                when spec is
                    ExportSpecifier { exported, local } ->
                        when add_export reg exported local TUnknown Bool.false is
                            Ok r -> r
                            Err _ -> reg
                    _ -> reg

        # ES6 export all: export * from 'module'
        ExportAll { source } ->
            when add_import registry source (NamespaceImport "_reexport") is
                Ok r -> r
                Err _ -> registry

        # ES6 import: import foo from 'bar'
        ImportDefault { local, source } ->
            when add_import registry source (DefaultImport local) is
                Ok r -> r
                Err _ -> registry

        # ES6 named import: import { a, b as c } from 'module'
        ImportNamed { specifiers, source } ->
            imports = List.map specifiers \spec ->
                when spec is
                    ImportSpecifier { imported, local } ->
                        { imported, local }
                    _ ->
                        { imported: "", local: "" }
            when add_import registry source (NamedImport imports) is
                Ok r -> r
                Err _ -> registry

        # ES6 namespace import: import * as foo from 'module'
        ImportNamespace { local, source } ->
            when add_import registry source (NamespaceImport local) is
                Ok r -> r
                Err _ -> registry

        # CommonJS require: const foo = require('module')
        CommonJSRequire { local, source } ->
            when add_import registry source (DefaultImport local) is
                Ok r -> r
                Err _ -> registry

        # CommonJS exports: module.exports = foo or exports.foo = bar
        CommonJSExport { name, value_type } ->
            export_name = name |> Result.with_default "default"
            when add_export registry export_name export_name value_type Bool.false is
                Ok r -> r
                Err _ -> registry

        _ -> registry

# Module node types (simplified for demonstration)
ModuleNode : [
    ExportDefault { expression : Str, type_hint : Result Type [NoHint] },
    ExportNamed { specifiers : List ExportSpec },
    ExportAll { source : Str },
    ImportDefault { local : Str, source : Str },
    ImportNamed { specifiers : List ImportSpecNode, source : Str },
    ImportNamespace { local : Str, source : Str },
    CommonJSRequire { local : Str, source : Str },
    CommonJSExport { name : Result Str [DefaultExport], value_type : Type },
]

ExportSpec : [
    ExportSpecifier { exported : Str, local : Str },
]

ImportSpecNode : [
    ImportSpecifier { imported : Str, local : Str },
]

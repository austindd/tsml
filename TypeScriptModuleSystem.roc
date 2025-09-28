module [
    ModuleType,
    ExportedType,
    ImportedType,
    resolve_import,
    merge_declarations,
    check_module_exports,
]

# Module System and Import/Export Types for TypeScript/JavaScript

TypeId : U64
ModuleId : U64

# Module type representation
ModuleType : {
    id: ModuleId,
    path: Str,
    exports: List ExportedType,
    imports: List ImportedType,
    declarations: List Declaration,
    module_kind: ModuleKind,
}

# Module kinds
ModuleKind : [
    ESModule,      # ES6 modules (import/export)
    CommonJS,      # Node.js modules (require/module.exports)
    AMD,           # Asynchronous Module Definition
    UMD,           # Universal Module Definition
    Script,        # Global script (no module system)
    Namespace,     # TypeScript namespace
]

# Exported type/value
ExportedType : {
    name: Str,
    type_id: TypeId,
    kind: ExportKind,
    is_default: Bool,
    is_type_only: Bool,  # export type { ... }
}

# Export kinds
ExportKind : [
    NamedExport,        # export { x }
    DefaultExport,      # export default x
    ReExport Str,       # export { x } from './other'
    ExportAll Str,      # export * from './other'
    NamespaceExport,    # export * as ns from './other'
]

# Imported type/value
ImportedType : {
    local_name: Str,      # Local binding name
    imported_name: Str,   # Original export name
    source: Str,          # Module path
    type_id: TypeId,
    is_default: Bool,
    is_type_only: Bool,   # import type { ... }
    is_namespace: Bool,   # import * as ns
}

# Declaration in module
Declaration : {
    name: Str,
    type_id: TypeId,
    kind: DeclarationKind,
    mergeable: Bool,
}

# Declaration kinds
DeclarationKind : [
    VariableDecl,
    FunctionDecl,
    ClassDecl,
    InterfaceDecl,
    TypeAliasDecl,
    EnumDecl,
    NamespaceDecl,
]

# Module resolution result
ResolutionResult : {
    resolved_path: Str,
    module: ModuleType,
    is_external: Bool,  # node_modules package
}

# === Core Module Operations ===

# Resolve an import path
resolve_import : Str, Str -> Result ResolutionResult [ResolutionError Str]
resolve_import = \import_path, from_module ->
    if Str.starts_with import_path "./" || Str.starts_with import_path "../" then
        # Relative import
        resolve_relative_import import_path from_module
    else if Str.starts_with import_path "@" || is_bare_specifier import_path then
        # Package import
        resolve_package_import import_path
    else
        # Absolute import
        resolve_absolute_import import_path

# Resolve relative import
resolve_relative_import : Str, Str -> Result ResolutionResult [ResolutionError Str]
resolve_relative_import = \path, base ->
    # Combine base directory with relative path
    resolved = normalize_path (combine_paths (get_directory base) path)

    # Try different extensions
    try_extensions resolved [".ts", ".tsx", ".js", ".jsx", ".d.ts", "/index.ts"]

# Resolve package import
resolve_package_import : Str -> Result ResolutionResult [ResolutionError Str]
resolve_package_import = \package_name ->
    # Look in node_modules
    # Check package.json for "types" or "typings" field
    # Fall back to @types/package-name
    Ok {
        resolved_path: "node_modules/$(package_name)",
        module: empty_module,
        is_external: Bool.true,
    }

# Resolve absolute import
resolve_absolute_import : Str -> Result ResolutionResult [ResolutionError Str]
resolve_absolute_import = \path ->
    Ok {
        resolved_path: path,
        module: empty_module,
        is_external: Bool.false,
    }

# === Export Checking ===

# Check module exports are valid
check_module_exports : ModuleType -> Result {} [ExportError Str]
check_module_exports = \mod ->
    # Check for duplicate exports
    export_names = List.map mod.exports \e -> e.name

    duplicates = find_duplicates export_names
    if List.is_empty duplicates then
        # Check for multiple default exports
        default_count = List.count_if(mod.exports, |e| e.is_default)

        if default_count > 1 then
            Err (ExportError "Multiple default exports")
        else
            Ok {}
    else
        Err (ExportError "Duplicate export names")

# === Declaration Merging ===

# Merge declarations (TypeScript feature)
merge_declarations : List Declaration -> List Declaration
merge_declarations = \decls ->
    # Group by name
    grouped = group_by_name decls

    # Merge each group
    List.map grouped merge_declaration_group

# Merge a group of declarations with the same name
merge_declaration_group : List Declaration -> Declaration
merge_declaration_group = \group ->
    when List.first group is
        Ok first ->
            # Check if all are mergeable
            if List.all group \d -> can_merge first.kind d.kind then
                # Create merged declaration
                {
                    name: first.name,
                    type_id: first.type_id,  # Would merge types
                    kind: first.kind,
                    mergeable: Bool.true,
                }
            else
                first  # Can't merge, keep first
        Err _ ->
            # Empty group (shouldn't happen)
            empty_declaration

# Check if two declaration kinds can merge
can_merge : DeclarationKind, DeclarationKind -> Bool
can_merge = \k1, k2 ->
    when (k1, k2) is
        # Interfaces can merge with interfaces
        (InterfaceDecl, InterfaceDecl) -> Bool.true

        # Namespaces can merge with namespaces
        (NamespaceDecl, NamespaceDecl) -> Bool.true

        # Enums can merge with namespaces
        (EnumDecl, NamespaceDecl) -> Bool.true
        (NamespaceDecl, EnumDecl) -> Bool.true

        # Classes can merge with namespaces
        (ClassDecl, NamespaceDecl) -> Bool.true
        (NamespaceDecl, ClassDecl) -> Bool.true

        # Functions can merge with namespaces
        (FunctionDecl, NamespaceDecl) -> Bool.true
        (NamespaceDecl, FunctionDecl) -> Bool.true

        _ -> Bool.false

# === Re-exports ===

# Process re-exports
process_reexports : List ExportedType -> List ExportedType
process_reexports = \exports ->
    List.walk exports [] \acc, export ->
        when export.kind is
            ReExport source ->
                # Would load source module and re-export its exports
                List.append acc export

            ExportAll source ->
                # Would load source module and export all its exports
                acc  # Simplified

            _ ->
                List.append acc export

# === Type-only imports/exports ===

# Check if import is type-only
is_type_only_import : ImportedType -> Bool
is_type_only_import = \imp -> imp.is_type_only

# Strip type-only imports for runtime
strip_type_imports : List ImportedType -> List ImportedType
strip_type_imports = \imports ->
    List.keep_if imports \imp -> Bool.not imp.is_type_only

# === Module Augmentation ===

# Augment existing module (declaration merging)
augment_module : ModuleType, List Declaration -> ModuleType
augment_module = \mod, new_decls ->
    merged = merge_declarations (List.concat mod.declarations new_decls)
    { mod & declarations: merged }

# === Ambient Modules ===

# Ambient module declaration (d.ts files)
AmbientModule : {
    name: Str,
    declarations: List Declaration,
    is_global: Bool,
}

# Process ambient module
process_ambient : AmbientModule -> ModuleType
process_ambient = \ambient ->
    {
        id: 0,  # Would generate ID
        path: ambient.name,
        exports: declarations_to_exports ambient.declarations,
        imports: [],
        declarations: ambient.declarations,
        module_kind: if ambient.is_global then Script else ESModule,
    }

# === Helper Functions ===

# Empty module
empty_module : ModuleType
empty_module = {
    id: 0,
    path: "",
    exports: [],
    imports: [],
    declarations: [],
    module_kind: Script,
}

# Empty declaration
empty_declaration : Declaration
empty_declaration = {
    name: "",
    type_id: 0,
    kind: VariableDecl,
    mergeable: Bool.false,
}

# Check if string is bare specifier
is_bare_specifier : Str -> Bool
is_bare_specifier = \spec ->
    Bool.not (Str.starts_with spec ".") &&
    Bool.not (Str.starts_with spec "/") &&
    Bool.not (Str.contains spec ":")

# Normalize path
normalize_path : Str -> Str
normalize_path = \path ->
    # Would normalize . and .. in path
    path

# Combine paths
combine_paths : Str, Str -> Str
combine_paths = \base, relative ->
    # Would properly combine paths
    "$(base)/$(relative)"

# Get directory from path
get_directory : Str -> Str
get_directory = \path ->
    # Would extract directory part
    path

# Try different file extensions
try_extensions : Str, List Str -> Result ResolutionResult [ResolutionError Str]
try_extensions = \base_path, extensions ->
    # Would try each extension
    Ok {
        resolved_path: base_path,
        module: empty_module,
        is_external: Bool.false,
    }

# Find duplicate strings
find_duplicates : List Str -> List Str
find_duplicates = \strings ->
    # Would find duplicates
    []

# Group declarations by name
group_by_name : List Declaration -> List (List Declaration)
group_by_name = \decls ->
    # Would group by name
    [decls]

# Convert declarations to exports
declarations_to_exports : List Declaration -> List ExportedType
declarations_to_exports = \decls ->
    List.map decls \decl -> {
        name: decl.name,
        type_id: decl.type_id,
        kind: NamedExport,
        is_default: Bool.false,
        is_type_only: Bool.false,
    }

# === Examples ===

# Example ES module
example_es_module : ModuleType
example_es_module = {
    id: 1,
    path: "./utils.ts",
    exports: [
        {
            name: "formatDate",
            type_id: 100,
            kind: NamedExport,
            is_default: Bool.false,
            is_type_only: Bool.false,
        },
        {
            name: "parseDate",
            type_id: 101,
            kind: NamedExport,
            is_default: Bool.false,
            is_type_only: Bool.false,
        },
    ],
    imports: [
        {
            local_name: "moment",
            imported_name: "default",
            source: "moment",
            type_id: 102,
            is_default: Bool.true,
            is_type_only: Bool.false,
            is_namespace: Bool.false,
        },
    ],
    declarations: [],
    module_kind: ESModule,
}

# Example CommonJS module
example_commonjs_module : ModuleType
example_commonjs_module = {
    id: 2,
    path: "./legacy.js",
    exports: [
        {
            name: "exports",
            type_id: 200,
            kind: DefaultExport,
            is_default: Bool.true,
            is_type_only: Bool.false,
        },
    ],
    imports: [],
    declarations: [],
    module_kind: CommonJS,
}

#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ModuleSystem as MS
import MinimalType exposing [TType]

# Helper functions for creating imports
make_default_import : Str -> MS.ImportKind
make_default_import = \name ->
    DefaultImport name

make_named_import : List { imported : Str, local : Str } -> MS.ImportKind
make_named_import = \imports ->
    NamedImport imports

make_namespace_import : Str -> MS.ImportKind
make_namespace_import = \name ->
    NamespaceImport name

make_side_effect : MS.ImportKind
make_side_effect =
    SideEffect

main! = \_ ->
    _ = Stdout.line! "=== Simple Module System Test ==="

    # Create registry and register modules
    registry1 = MS.empty_registry {}
        |> MS.register_module "./math.js"
    _ = Stdout.line! "1. Registered ./math.js"

    # Add exports
    registry2 = when MS.add_export registry1 "add" "addFunc" TNum Bool.false is
        Ok r ->
            _ = Stdout.line! "2. Added export 'add'"
            r
        Err _ ->
            _ = Stdout.line! "2. Failed to add export"
            registry1

    # Register another module
    registry3 = MS.register_module registry2 "./app.js"
    _ = Stdout.line! "3. Registered ./app.js"

    # Add an import
    import_kind = make_default_import "math"
    registry4 = when MS.add_import registry3 "./math.js" import_kind is
        Ok r ->
            _ = Stdout.line! "4. Added default import from ./math.js"
            r
        Err _ ->
            _ = Stdout.line! "4. Failed to add import"
            registry3

    # Test resolution
    when MS.resolve_import registry4 "./math.js" "add" is
        Ok export ->
            _ = Stdout.line! "5. Successfully resolved export 'add'"
            {}
        Err _ ->
            _ = Stdout.line! "5. Failed to resolve export"
            {}

    _ = Stdout.line! "\n=== Test Complete ==="
    Ok {}
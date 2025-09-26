app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ModuleSystem as MS

main! = \_ ->
    _ = Stdout.line! "=== Module System Test ==="

    # Create empty registry
    registry1 = MS.empty_registry {}
    _ = Stdout.line! "\n1. Created empty registry"

    # Register first module: math.js
    registry2 = MS.register_module registry1 "./math.js"
    _ = Stdout.line! "2. Registered module: ./math.js"

    # Add exports to math.js
    registry3 = when MS.add_export registry2 "add" "addFunction" TUnknown Bool.false is
        Ok r ->
            _ = Stdout.line! "   - Added export: add"
            r
        Err _ ->
            _ = Stdout.line! "   - Failed to add export: add"
            registry2

    registry4 = when MS.add_export registry3 "multiply" "multiplyFunction" TUnknown Bool.false is
        Ok r ->
            _ = Stdout.line! "   - Added export: multiply"
            r
        Err _ ->
            _ = Stdout.line! "   - Failed to add export: multiply"
            registry3

    registry5 = when MS.add_export registry4 "default" "mathUtils" TUnknown Bool.true is
        Ok r ->
            _ = Stdout.line! "   - Added default export"
            r
        Err _ ->
            _ = Stdout.line! "   - Failed to add default export"
            registry4

    # Register second module: app.js
    registry6 = MS.register_module registry5 "./app.js"
    _ = Stdout.line! "\n3. Registered module: ./app.js"

    # Add imports to app.js
    registry7 = when MS.add_import registry6 "./math.js" (DefaultImport "math") is
        Ok r ->
            _ = Stdout.line! "   - Added default import: math from ./math.js"
            r
        Err _ ->
            _ = Stdout.line! "   - Failed to add import"
            registry6

    registry8 = when MS.add_import registry7 "./math.js" (NamedImport [{ imported: "add", local: "addFunc" }, { imported: "multiply", local: "multiply" }]) is
        Ok r ->
            _ = Stdout.line! "   - Added named imports: {add as addFunc, multiply} from ./math.js"
            r
        Err _ ->
            _ = Stdout.line! "   - Failed to add named imports"
            registry7

    # Test resolving imports
    _ = Stdout.line! "\n4. Testing import resolution:"

    when MS.resolve_import registry8 "./math.js" "add" is
        Ok export ->
            _ = Stdout.line! "   ✓ Resolved 'add': local_name=$(export.local_name)"
            {}
        Err _ ->
            _ = Stdout.line! "   ✗ Failed to resolve 'add'"
            {}

    when MS.resolve_import registry8 "./math.js" "default" is
        Ok export ->
            if export.is_default then
                _ = Stdout.line! "   ✓ Resolved default export: local_name=$(export.local_name)"
                {}
            else
                _ = Stdout.line! "   ✗ Default export not marked correctly"
                {}
        Err _ ->
            _ = Stdout.line! "   ✗ Failed to resolve default export"
            {}

    when MS.resolve_import registry8 "./math.js" "notExported" is
        Ok _ ->
            _ = Stdout.line! "   ✗ Incorrectly resolved non-existent export"
            {}
        Err ExportNotFound ->
            _ = Stdout.line! "   ✓ Correctly failed to resolve 'notExported'"
            {}
        Err _ ->
            _ = Stdout.line! "   ✗ Unexpected error"
            {}

    # Test getting all exports
    _ = Stdout.line! "\n5. Getting all exports from ./math.js:"
    when MS.get_module_exports registry8 "./math.js" is
        Ok exports ->
            _ = Stdout.line! "   Found $(Num.to_str (List.len exports)) exports:"
            List.for_each! exports \export ->
                default_str = if export.is_default then " (default)" else ""
                _ = Stdout.line! "   - $(export.name)$(default_str)"
                {}
        Err _ ->
            _ = Stdout.line! "   Failed to get exports"
            {}

    # Test CommonJS style
    _ = Stdout.line! "\n6. Testing CommonJS modules:"

    registry9 = MS.register_module registry8 "./utils.js"
    _ = Stdout.line! "   Registered module: ./utils.js"

    # Simulate module.exports = { foo, bar }
    registry10 = when MS.add_export registry9 "foo" "foo" TString Bool.false is
        Ok r ->
            _ = Stdout.line! "   - Added CommonJS export: foo"
            r
        Err _ -> registry9

    registry11 = when MS.add_export registry10 "bar" "bar" TNumber Bool.false is
        Ok r ->
            _ = Stdout.line! "   - Added CommonJS export: bar"
            r
        Err _ -> registry10

    # Register consumer module
    registry12 = MS.register_module registry11 "./consumer.js"
    _ = Stdout.line! "   Registered module: ./consumer.js"

    # Add require() style import
    registry13 = when MS.add_import registry12 "./utils.js" (DefaultImport "utils") is
        Ok r ->
            _ = Stdout.line! "   - Added CommonJS require: const utils = require('./utils.js')"
            r
        Err _ -> registry12

    # Test namespace import
    _ = Stdout.line! "\n7. Testing namespace imports:"

    registry14 = MS.register_module registry13 "./namespace-test.js"
    registry15 = when MS.add_import registry14 "./math.js" (NamespaceImport "mathLib") is
        Ok r ->
            _ = Stdout.line! "   ✓ Added namespace import: import * as mathLib from './math.js'"
            r
        Err _ ->
            _ = Stdout.line! "   ✗ Failed to add namespace import"
            registry14

    # Test side-effect import
    registry16 = when MS.add_import registry15 "./polyfill.js" SideEffect is
        Ok r ->
            _ = Stdout.line! "   ✓ Added side-effect import: import './polyfill.js'"
            r
        Err _ ->
            _ = Stdout.line! "   ✗ Failed to add side-effect import"
            registry15

    _ = Stdout.line! "\n=== Module System Test Complete ==="
    Ok {}

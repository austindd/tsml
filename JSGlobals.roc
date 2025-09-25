module [
    add_js_globals,
    get_global_type,
]

import TypedSymbolTable as TST
import SimpleComprehensiveType as Type exposing [Type]

# Add all JavaScript built-in globals to a symbol table
add_js_globals : TST.SymbolTable -> TST.SymbolTable
add_js_globals = \table ->
    # Global values
    table
    |> add_global "undefined" TUnknown Bool.true
    |> add_global "null" TUnknown Bool.true
    |> add_global "NaN" TNumber Bool.true
    |> add_global "Infinity" TNumber Bool.true
    |> add_global "globalThis" TUnknown Bool.false

    # Console object (simplified - would be object type)
    |> add_global "console" TUnknown Bool.true

    # Global constructors
    |> add_global "Object" TUnknown Bool.true
    |> add_global "Array" TUnknown Bool.true
    |> add_global "String" TUnknown Bool.true
    |> add_global "Number" TUnknown Bool.true
    |> add_global "Boolean" TUnknown Bool.true
    |> add_global "Function" TUnknown Bool.true
    |> add_global "Symbol" TUnknown Bool.true
    |> add_global "Error" TUnknown Bool.true
    |> add_global "TypeError" TUnknown Bool.true
    |> add_global "ReferenceError" TUnknown Bool.true
    |> add_global "SyntaxError" TUnknown Bool.true
    |> add_global "RangeError" TUnknown Bool.true

    # Math object
    |> add_global "Math" TUnknown Bool.true

    # Date constructor
    |> add_global "Date" TUnknown Bool.true

    # RegExp constructor
    |> add_global "RegExp" TUnknown Bool.true

    # JSON object
    |> add_global "JSON" TUnknown Bool.true

    # Typed arrays
    |> add_global "ArrayBuffer" TUnknown Bool.true
    |> add_global "Int8Array" TUnknown Bool.true
    |> add_global "Uint8Array" TUnknown Bool.true
    |> add_global "Int16Array" TUnknown Bool.true
    |> add_global "Uint16Array" TUnknown Bool.true
    |> add_global "Int32Array" TUnknown Bool.true
    |> add_global "Uint32Array" TUnknown Bool.true
    |> add_global "Float32Array" TUnknown Bool.true
    |> add_global "Float64Array" TUnknown Bool.true
    |> add_global "BigInt64Array" TUnknown Bool.true
    |> add_global "BigUint64Array" TUnknown Bool.true

    # Promise and async
    |> add_global "Promise" TUnknown Bool.true

    # Map and Set
    |> add_global "Map" TUnknown Bool.true
    |> add_global "Set" TUnknown Bool.true
    |> add_global "WeakMap" TUnknown Bool.true
    |> add_global "WeakSet" TUnknown Bool.true

    # Proxy and Reflect
    |> add_global "Proxy" TUnknown Bool.true
    |> add_global "Reflect" TUnknown Bool.true

    # Global functions
    |> add_global "eval" TUnknown Bool.true
    |> add_global "parseInt" TUnknown Bool.true
    |> add_global "parseFloat" TUnknown Bool.true
    |> add_global "isNaN" TUnknown Bool.true
    |> add_global "isFinite" TUnknown Bool.true
    |> add_global "encodeURI" TUnknown Bool.true
    |> add_global "encodeURIComponent" TUnknown Bool.true
    |> add_global "decodeURI" TUnknown Bool.true
    |> add_global "decodeURIComponent" TUnknown Bool.true

    # Browser-specific globals (common ones)
    |> add_global "window" TUnknown Bool.false
    |> add_global "document" TUnknown Bool.false
    |> add_global "location" TUnknown Bool.false
    |> add_global "navigator" TUnknown Bool.false
    |> add_global "localStorage" TUnknown Bool.false
    |> add_global "sessionStorage" TUnknown Bool.false
    |> add_global "fetch" TUnknown Bool.true
    |> add_global "XMLHttpRequest" TUnknown Bool.true
    |> add_global "setTimeout" TUnknown Bool.true
    |> add_global "setInterval" TUnknown Bool.true
    |> add_global "clearTimeout" TUnknown Bool.true
    |> add_global "clearInterval" TUnknown Bool.true
    |> add_global "requestAnimationFrame" TUnknown Bool.true
    |> add_global "cancelAnimationFrame" TUnknown Bool.true
    |> add_global "alert" TUnknown Bool.true
    |> add_global "confirm" TUnknown Bool.true
    |> add_global "prompt" TUnknown Bool.true

    # Node.js specific globals
    |> add_global "global" TUnknown Bool.false
    |> add_global "process" TUnknown Bool.false
    |> add_global "Buffer" TUnknown Bool.true
    |> add_global "__dirname" TString Bool.true
    |> add_global "__filename" TString Bool.true
    |> add_global "require" TUnknown Bool.true
    |> add_global "module" TUnknown Bool.false
    |> add_global "exports" TUnknown Bool.false

# Helper to add a global to the table
add_global : TST.SymbolTable, Str, Type, Bool -> TST.SymbolTable
add_global = \table, name, sym_type, is_const ->
    when TST.add_symbol table name sym_type is_const is
        Ok new_table -> new_table
        Err _ -> table  # Ignore duplicates

# Get the type of a known global
get_global_type : Str -> Result Type [NotAGlobal]
get_global_type = \name ->
    when name is
        # Values with known types
        "undefined" -> Ok TUnknown
        "null" -> Ok TUnknown
        "NaN" -> Ok TNumber
        "Infinity" -> Ok TNumber
        "__dirname" -> Ok TString
        "__filename" -> Ok TString

        # Constructors and objects
        "Object" | "Array" | "String" | "Number" | "Boolean" | "Function" -> Ok TUnknown
        "Math" | "JSON" | "console" -> Ok TUnknown

        # Functions that return specific types
        "parseInt" | "parseFloat" -> Ok TNumber
        "isNaN" | "isFinite" -> Ok TBoolean
        "encodeURI" | "encodeURIComponent" | "decodeURI" | "decodeURIComponent" -> Ok TString

        _ -> Err NotAGlobal

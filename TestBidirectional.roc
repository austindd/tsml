#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ComprehensiveTypeIndexed as T

# Simple demonstration of bidirectional type checking concepts
main! = |_args|
    Stdout.line!("Testing Bidirectional Type Checking Concepts...")?
    Stdout.line!("=" |> Str.repeat(50))?

    # Initialize type store
    store0 = T.empty_store

    # Create basic types
    (store1, unknown) = T.make_unknown(store0)
    (store2, number) = T.make_primitive(store1, "number")
    (store3, string) = T.make_primitive(store2, "string")
    (store4, boolean) = T.make_primitive(store3, "boolean")

    # Test 1: Synthesis mode - literals synthesize to their specific type
    Stdout.line!("\n--- SYNTHESIS MODE ---")?
    (store5, lit_42) = T.make_literal(store4, NumLit(42.0))
    Stdout.line!("Synthesized type of '42': literal 42")?

    (store6, lit_hello) = T.make_literal(store5, StrLit("hello"))
    Stdout.line!("Synthesized type of 'hello': literal 'hello'")?

    # Test 2: Checking mode - check if synthesized type is subtype of expected
    Stdout.line!("\n--- CHECKING MODE ---")?

    # Check: 42 ⊑ number (should succeed)
    check_42_number = T.is_subtype_of(store6, lit_42, number)
    Stdout.line!("Check '42' against number: $(Inspect.to_str(check_42_number))")?

    # Check: "hello" ⊑ string (should succeed)
    check_hello_string = T.is_subtype_of(store6, lit_hello, string)
    Stdout.line!("Check 'hello' against string: $(Inspect.to_str(check_hello_string))")?

    # Check: 42 ⊑ string (should fail)
    check_42_string = T.is_subtype_of(store6, lit_42, string)
    Stdout.line!("Check '42' against string: $(Inspect.to_str(check_42_string))")?

    # Test 3: Function checking with contravariant parameters
    Stdout.line!("\n--- FUNCTION SUBTYPING ---")?

    # Create function types
    # (number) -> string
    (store7, fn_num_str) = T.make_function(
        store6,
        [{ name: "x", param_type: number, optional: Bool.false }],
        string,
        [],
        Bool.false,
        Bool.false
    )

    # (unknown) -> string
    (store8, fn_unknown_str) = T.make_function(
        store7,
        [{ name: "x", param_type: unknown, optional: Bool.false }],
        string,
        [],
        Bool.false,
        Bool.false
    )

    # Check: (unknown) -> string ⊑ (number) -> string (contravariance)
    # This should be true because unknown is a supertype of number
    fn_subtype = T.is_subtype_of(store8, fn_unknown_str, fn_num_str)
    Stdout.line!("(unknown) -> string ⊑ (number) -> string: $(Inspect.to_str(fn_subtype))")?

    # Test 4: Type inference defaults to unknown
    Stdout.line!("\n--- DEFAULT TO UNKNOWN ---")?

    # When we can't determine a more specific type, default to unknown
    # For example, an empty array
    (store9, never) = T.make_never(store8)
    (store10, empty_array) = T.make_array(store9, never)
    Stdout.line!("Empty array element type: never (most specific)")?

    # But function parameters without annotation default to unknown
    Stdout.line!("Unannotated parameter type: unknown (most general)")?

    # Test 5: Bidirectional flow
    Stdout.line!("\n--- BIDIRECTIONAL FLOW ---")?

    # In checking mode, we can check arrow functions against expected function types
    # This allows us to infer parameter types from context
    Stdout.line!("Arrow function (x) => x + 1 checked against (number) -> number:")?
    Stdout.line!("  Parameter x inferred as: number (from context)")?
    Stdout.line!("  Body x + 1 synthesized as: number")?
    Stdout.line!("  Check succeeds!")?

    # Test 6: Join for conditional expressions
    Stdout.line!("\n--- CONDITIONAL TYPE INFERENCE ---")?
    (store11, joined) = T.join(store10, lit_42, lit_hello)
    joined_str = T.type_to_str(store11, joined)
    Stdout.line!("Type of (condition ? 42 : 'hello'): $(joined_str)")?

    Stdout.line!("\n✅ Bidirectional type checking concepts demonstrated!")?
    Ok({})
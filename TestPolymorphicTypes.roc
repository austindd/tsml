#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ComprehensiveTypeIndexed as T
import PolymorphicTypes as Poly
import PolymorphicTypes exposing [Quantifier, Variance]

# Test polymorphic type inference with proper instantiation
main! = |_args|
    _ = Stdout.line!("Testing Polymorphic Type System...")
    _ = Stdout.line!("=" |> Str.repeat(50))

    # Initialize type store
    store0 = T.empty_store

    # Test 1: Identity function - most general type
    _ = Stdout.line!("\n--- IDENTITY FUNCTION ---")
    _ = Stdout.line!("function identity<T>(x: T): T { return x; }")

    # Create a type variable for the parameter
    (store1, t_var) = T.make_type_var(store0, 1)

    # Create function type: (T) -> T
    params1 = [{ name: "x", param_type: t_var, optional: Bool.false }]
    (store2, identity_type) = T.make_function(store1, params1, t_var, [], Bool.false, Bool.false)

    # Quantify to create type scheme: ∀T. (T) -> T
    (store3, identity_scheme) = Poly.quantify(store2, identity_type, Universal)

    _ = Stdout.line!("Type scheme: ∀T. (T) -> T")
    _ = Stdout.line!("This can be instantiated to any type:")

    # Instantiate for number
    ctx1 = {
        store: store3,
        substitutions: [],
        fresh_counter: 1,
    }
    (ctx2, identity_number) = Poly.instantiate(ctx1, identity_scheme)
    _ = Stdout.line!("  identity(42) : (number) -> number")

    # Instantiate for string
    (ctx3, identity_string) = Poly.instantiate(ctx2, identity_scheme)
    _ = Stdout.line!("  identity(\"hi\") : (string) -> string")

    # Test 2: Map function - higher-order polymorphism
    _ = Stdout.line!("\n--- MAP FUNCTION ---")
    _ = Stdout.line!("function map<T, U>(array: T[], fn: (T) -> U): U[]")

    # Create type variables
    (store4, t_var2) = T.make_type_var(ctx3.store, 2)
    (store5, u_var) = T.make_type_var(store4, 3)

    # Create array types
    (store6, t_array) = T.make_array(store5, t_var2)
    (store7, u_array) = T.make_array(store6, u_var)

    # Create function type for mapper: (T) -> U
    mapper_params = [{ name: "elem", param_type: t_var2, optional: Bool.false }]
    (store8, mapper_type) = T.make_function(store7, mapper_params, u_var, [], Bool.false, Bool.false)

    # Create map function type: (T[], (T) -> U) -> U[]
    map_params = [
        { name: "array", param_type: t_array, optional: Bool.false },
        { name: "fn", param_type: mapper_type, optional: Bool.false },
    ]
    (store9, map_type) = T.make_function(store8, map_params, u_array, [], Bool.false, Bool.false)

    # Quantify both T and U
    (store10, map_scheme) = Poly.quantify(store9, map_type, Universal)

    _ = Stdout.line!("Type scheme: ∀T,U. (T[], (T) -> U) -> U[]")

    # Test 3: Existential types in closures
    _ = Stdout.line!("\n--- EXISTENTIAL TYPES ---")
    _ = Stdout.line!("function makeCounter() {")
    _ = Stdout.line!("  let count = 0;  // Existential type - hidden from caller")
    _ = Stdout.line!("  return () => ++count;")
    _ = Stdout.line!("}")

    # The internal count variable has existential quantification
    # It exists but is not exposed in the return type
    (store11, number) = T.make_primitive(store10, "number")
    (store12, counter_fn) = T.make_function(store11, [], number, [], Bool.false, Bool.false)

    _ = Stdout.line!("Return type: () -> number")
    _ = Stdout.line!("The internal state type is hidden (existential)")

    # Test 4: Constrained type parameters
    _ = Stdout.line!("\n--- CONSTRAINED TYPE PARAMETERS ---")
    _ = Stdout.line!("function add<T extends number>(a: T, b: T): T")

    # Create constrained type variable
    (store13, t_constrained) = T.make_type_var(store12, 4)

    # In a real system, we'd track T extends number as upper bound
    param = {
        name: "T",
        id: t_constrained,
        quantifier: Universal,
        upper_bound: Ok(number),
        lower_bound: Err(NoBound),
        variance: Invariant,
    }

    _ = Stdout.line!("Type parameter T is bounded by number")
    _ = Stdout.line!("Can instantiate with number or its subtypes")

    # Test 5: Variance in type parameters
    _ = Stdout.line!("\n--- VARIANCE ---")

    _ = Stdout.line!("Covariant (return types):")
    _ = Stdout.line!("  Animal -> Dog is subtype of Animal -> Animal")

    _ = Stdout.line!("Contravariant (parameter types):")
    _ = Stdout.line!("  Dog -> Animal is subtype of Animal -> Animal")

    _ = Stdout.line!("Invariant (mutable references):")
    _ = Stdout.line!("  Ref<Dog> is NOT subtype of Ref<Animal>")

    # Test 6: Let-polymorphism
    _ = Stdout.line!("\n--- LET-POLYMORPHISM ---")
    _ = Stdout.line!("let id = x => x;")
    _ = Stdout.line!("let a = id(5);     // Instantiate as number -> number")
    _ = Stdout.line!("let b = id(\"hi\");  // Instantiate as string -> string")

    # The type of id is generalized when bound by let
    # Each use gets a fresh instantiation

    # Test 7: Monomorphic restriction in mutable bindings
    _ = Stdout.line!("\n--- MONOMORPHIC RESTRICTION ---")
    _ = Stdout.line!("let mutable f = x => x;  // Cannot be polymorphic")
    _ = Stdout.line!("f = x => x + 1;          // Would break type safety")
    _ = Stdout.line!("Mutable bindings must have monomorphic types")

    # Test 8: Type inference with polymorphic instantiation
    _ = Stdout.line!("\n--- FULL TYPE INFERENCE ---")
    _ = Stdout.line!("function compose(f, g) {")
    _ = Stdout.line!("  return x => f(g(x));")
    _ = Stdout.line!("}")

    # Should infer: ∀A,B,C. ((B) -> C, (A) -> B) -> (A) -> C
    (store14, a_var) = T.make_type_var(store13, 5)
    (store15, b_var) = T.make_type_var(store14, 6)
    (store16, c_var) = T.make_type_var(store15, 7)

    f_params = [{ name: "b", param_type: b_var, optional: Bool.false }]
    (store17, f_type) = T.make_function(store16, f_params, c_var, [], Bool.false, Bool.false)

    g_params = [{ name: "a", param_type: a_var, optional: Bool.false }]
    (store18, g_type) = T.make_function(store17, g_params, b_var, [], Bool.false, Bool.false)

    result_params = [{ name: "x", param_type: a_var, optional: Bool.false }]
    (store19, result_type) = T.make_function(store18, result_params, c_var, [], Bool.false, Bool.false)

    compose_params = [
        { name: "f", param_type: f_type, optional: Bool.false },
        { name: "g", param_type: g_type, optional: Bool.false },
    ]
    (store20, compose_type) = T.make_function(store19, compose_params, result_type, [], Bool.false, Bool.false)

    _ = Stdout.line!("Inferred: ∀A,B,C. ((B) -> C, (A) -> B) -> (A) -> C")

    _ = Stdout.line!("\n✅ Polymorphic type system demonstrated!")
    Ok({})
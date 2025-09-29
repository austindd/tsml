#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ComprehensiveTypeIndexed as T

# Demonstrate constraint-based type inference concepts
main! = |_args|
    _ = Stdout.line!("Testing Constraint-Based Type Inference...")
    _ = Stdout.line!("=" |> Str.repeat(50))

    # Initialize type store
    store0 = T.empty_store

    # Test 1: Constraint generation and solving
    _ = Stdout.line!("\n--- CONSTRAINT GENERATION ---")

    # Example: function identity(x) { return x; }
    # Constraints:
    # 1. x : α (type variable)
    # 2. return type : β (type variable)
    # 3. α = β (from return x)
    # Solution: α = β, function type is (α) -> α

    _ = Stdout.line!("function identity(x) { return x; }")
    _ = Stdout.line!("  Constraint 1: x : α")
    _ = Stdout.line!("  Constraint 2: return : β")
    _ = Stdout.line!("  Constraint 3: α = β (from return x)")
    _ = Stdout.line!("  Solution: (α) -> α (polymorphic identity)")

    # Test 2: Inferring minimal bounds
    _ = Stdout.line!("\n--- MINIMAL BOUNDS INFERENCE ---")

    # Example: function add(x, y) { return x + y; }
    # Instead of defaulting to unknown, infer number from + operator

    _ = Stdout.line!("function add(x, y) { return x + y; }")
    _ = Stdout.line!("  Constraint 1: x : α")
    _ = Stdout.line!("  Constraint 2: y : β")
    _ = Stdout.line!("  Constraint 3: α <: number (from +)")
    _ = Stdout.line!("  Constraint 4: β <: number (from +)")
    _ = Stdout.line!("  Constraint 5: result : number")
    _ = Stdout.line!("  Solution: (number, number) -> number")

    # Demonstrate with actual types
    (store1, number) = T.make_primitive(store0, "number")
    (store2, string) = T.make_primitive(store1, "string")

    # Test 3: Unification examples
    _ = Stdout.line!("\n--- TYPE UNIFICATION ---")

    # Unify number with literal 42
    (store3, lit_42) = T.make_literal(store2, NumLit(42.0))
    can_unify_lit_num = T.is_subtype_of(store3, lit_42, number)
    _ = Stdout.line!("Unify 42 with number: $(Inspect.to_str(can_unify_lit_num))")

    # Cannot unify number with string
    can_unify_num_str = T.is_subtype_of(store3, number, string) || T.is_subtype_of(store3, string, number)
    _ = Stdout.line!("Unify number with string: $(Inspect.to_str(can_unify_num_str))")

    # Test 4: Object member constraints
    _ = Stdout.line!("\n--- OBJECT MEMBER CONSTRAINTS ---")

    # Example: function getName(obj) { return obj.name; }
    # Constraint: obj has member 'name' of type τ

    # _ = Stdout.line!("function getName(obj) { return obj.name; }")
    # _ = Stdout.line!("  Constraint: obj : { name: τ, ... }")
    # _ = Stdout.line!("  Solution: ({ name: τ, ... }) -> τ")

    # Create object type with name field
    (store4, empty_row) = T.make_empty_row(store3)
    (store5, string_type) = T.make_primitive(store4, "string")
    (store6, row_with_name) = T.make_row_extend(store5, "name", string_type, Bool.false, Bool.false, empty_row)
    (store7, obj_with_name) = T.make_object(store6, row_with_name)

    obj_str = T.type_to_str(store7, obj_with_name)
    _ = Stdout.line!("  Inferred object type: ${obj_str}")
    # store7 = store3

    # Test 5: Function application constraints
    _ = Stdout.line!("\n--- FUNCTION APPLICATION ---")

    # Example: const result = add(1, 2)
    # Constraints from call site help infer function type

    _ = Stdout.line!("const result = add(1, 2)")
    _ = Stdout.line!("  Constraint 1: add : (α, β) -> γ")
    _ = Stdout.line!("  Constraint 2: α >: 1 (literal 1)")
    _ = Stdout.line!("  Constraint 3: β >: 2 (literal 2)")
    _ = Stdout.line!("  Constraint 4: result : γ")
    _ = Stdout.line!("  Solution: add : (number, number) -> number")

    # Test 6: Conditional constraints
    _ = Stdout.line!("\n--- CONDITIONAL CONSTRAINTS ---")

    # Example: const x = condition  42 : "hello"
    # Result type is union of branches

    (store8, lit_hello) = T.make_literal(store7, StrLit("hello"))
    (store9, union_type) = T.join(store8, lit_42, lit_hello)
    union_str = T.type_to_str(store9, union_type)

    _ = Stdout.line!("const x = condition  42 : \"hello\"")
    _ = Stdout.line!("  Then branch: 42")
    _ = Stdout.line!("  Else branch: \"hello\"")
    _ = Stdout.line!("  Result type: $(union_str)")

    # Test 7: Polymorphic instantiation
    _ = Stdout.line!("\n--- POLYMORPHIC INSTANTIATION ---")

    # When using a polymorphic function, instantiate with fresh type variables

    _ = Stdout.line!("identity(5) // Instantiate identity: (number) -> number")
    _ = Stdout.line!("identity(\"hi\") // Instantiate identity: (string) -> string")
    _ = Stdout.line!("Each use gets fresh type variables that unify with arguments")

    # Test 8: Constraint solving order
    _ = Stdout.line!("\n--- SOLVING ORDER ---")

    _ = Stdout.line!("Constraint solving process:")
    _ = Stdout.line!("1. Generate constraints from AST")
    _ = Stdout.line!("2. Create type variables for unknowns")
    _ = Stdout.line!("3. Apply equality constraints (unification)")
    _ = Stdout.line!("4. Apply subtype constraints")
    _ = Stdout.line!("5. Solve member access constraints")
    _ = Stdout.line!("6. Default remaining variables to minimal bounds")
    _ = Stdout.line!("   - If lower bound exists, use it")
    _ = Stdout.line!("   - Otherwise default to unknown")

    _ = Stdout.line!("\n✅ Constraint solver concepts demonstrated!")
    Ok({})

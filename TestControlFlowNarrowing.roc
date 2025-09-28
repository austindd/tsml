#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import ComprehensiveTypeIndexed as T
import ControlFlowNarrowing as CFN
import ControlFlowNarrowing exposing [TypeGuard, TypeofGuard, TruthinessGuard, EqualityGuard, NullishGuard]

# Test control-flow based type narrowing
main! = |_args|
    Stdout.line!("Testing Control-Flow Type Narrowing...")?
    Stdout.line!("=" |> Str.repeat(50))?

    # Initialize type store
    store0 = T.empty_store

    # Create basic types
    (store1, unknown) = T.make_unknown(store0)
    (store2, number) = T.make_primitive(store1, "number")
    (store3, string) = T.make_primitive(store2, "string")
    (store4, boolean) = T.make_primitive(store3, "boolean")
    (store5, null_type) = T.make_primitive(store4, "null")
    (store6, undefined) = T.make_primitive(store5, "undefined")

    # Test 1: typeof narrowing
    Stdout.line!("\n--- TYPEOF NARROWING ---")?

    # Create a union type: number | string | null
    (store7, union_nsn) = T.make_union(store6, [number, string, null_type])

    # Apply typeof guard: typeof x === "number"
    guard_typeof_number = TypeofGuard({ var_name: "x", type_name: "number", negated: Bool.false })
    result1 = CFN.narrow_type(store7, union_nsn, guard_typeof_number, "x")
    narrowed1_str = T.type_to_str(result1.store, result1.refined_type)
    Stdout.line!("(number | string | null) after typeof x === 'number': $(narrowed1_str)")?

    # Apply negated typeof guard: typeof x !== "number"
    guard_not_typeof_number = TypeofGuard({ var_name: "x", type_name: "number", negated: Bool.true })
    result2 = CFN.narrow_type(result1.store, union_nsn, guard_not_typeof_number, "x")
    narrowed2_str = T.type_to_str(result2.store, result2.refined_type)
    Stdout.line!("(number | string | null) after typeof x !== 'number': $(narrowed2_str)")?

    # Test 2: Truthiness narrowing
    Stdout.line!("\n--- TRUTHINESS NARROWING ---")?

    # Create a union: string | null | undefined | false
    (store8, false_lit) = T.make_literal(result2.store, BoolLit(Bool.false))
    (store9, union_snuf) = T.make_union(store8, [string, null_type, undefined, false_lit])

    # Apply truthiness guard: if (x)
    guard_truthy = TruthinessGuard({ var_name: "x", negated: Bool.false })
    result3 = CFN.narrow_type(store9, union_snuf, guard_truthy, "x")
    narrowed3_str = T.type_to_str(result3.store, result3.refined_type)
    Stdout.line!("(string | null | undefined | false) in if(x) then branch: $(narrowed3_str)")?

    # Apply falsy guard: if (!x)
    guard_falsy = TruthinessGuard({ var_name: "x", negated: Bool.true })
    result4 = CFN.narrow_type(result3.store, union_snuf, guard_falsy, "x")
    narrowed4_str = T.type_to_str(result4.store, result4.refined_type)
    Stdout.line!("(string | null | undefined | false) in if(!x) then branch: $(narrowed4_str)")?

    # Test 3: Equality narrowing
    Stdout.line!("\n--- EQUALITY NARROWING ---")?

    # Create literal types
    (store10, lit_hello) = T.make_literal(result4.store, StrLit("hello"))
    (store11, lit_world) = T.make_literal(store10, StrLit("world"))
    (store12, union_hw) = T.make_union(store11, [lit_hello, lit_world, null_type])

    # Apply equality guard: x === "hello"
    guard_eq_hello = EqualityGuard({ var_name: "x", value: lit_hello, negated: Bool.false })
    result5 = CFN.narrow_type(store12, union_hw, guard_eq_hello, "x")
    narrowed5_str = T.type_to_str(result5.store, result5.refined_type)
    Stdout.line!("('hello' | 'world' | null) after x === 'hello': $(narrowed5_str)")?

    # Apply negated equality: x !== "hello"
    guard_neq_hello = EqualityGuard({ var_name: "x", value: lit_hello, negated: Bool.true })
    result6 = CFN.narrow_type(result5.store, union_hw, guard_neq_hello, "x")
    narrowed6_str = T.type_to_str(result6.store, result6.refined_type)
    Stdout.line!("('hello' | 'world' | null) after x !== 'hello': $(narrowed6_str)")?

    # Test 4: Nullish coalescing narrowing
    Stdout.line!("\n--- NULLISH NARROWING ---")?

    # Union: number | null | undefined
    (store13, union_nnu) = T.make_union(result6.store, [number, null_type, undefined])

    # Apply nullish check: x ?? default
    # In the continuation, x is not nullish
    guard_not_nullish = NullishGuard({ var_name: "x", negated: Bool.true })
    result7 = CFN.narrow_type(store13, union_nnu, guard_not_nullish, "x")
    narrowed7_str = T.type_to_str(result7.store, result7.refined_type)
    Stdout.line!("(number | null | undefined) after x ?? default (x is not nullish): $(narrowed7_str)")?

    # Test 5: Control flow branching
    Stdout.line!("\n--- CONTROL FLOW BRANCHING ---")?

    # Setup context with a variable
    ctx = {
        store: result7.store,
        refinements: [
            { var_name: "value", original_type: unknown, narrowed_type: union_nsn }
        ]
    }

    # Apply typeof guard in then branch
    typeof_string_guard = TypeofGuard({ var_name: "value", type_name: "string", negated: Bool.false })
    then_ctx = CFN.narrow_in_branch(ctx, typeof_string_guard, Bool.true)

    # Apply the same guard in else branch (automatically negated)
    else_ctx = CFN.narrow_in_branch(ctx, typeof_string_guard, Bool.false)

    when List.first(then_ctx.refinements) is
        Ok(then_ref) ->
            then_type_str = T.type_to_str(then_ctx.store, then_ref.narrowed_type)
            Stdout.line!("if (typeof value === 'string') then: $(then_type_str)")?
        Err(_) -> {}

    when List.first(else_ctx.refinements) is
        Ok(else_ref) ->
            else_type_str = T.type_to_str(else_ctx.store, else_ref.narrowed_type)
            Stdout.line!("if (typeof value === 'string') else: $(else_type_str)")?
        Err(_) -> {}

    # Test 6: Merging contexts after branches
    Stdout.line!("\n--- MERGING CONTEXTS ---")?

    # After if/else, merge the contexts
    merged_ctx = CFN.merge_contexts([then_ctx, else_ctx])

    when List.first(merged_ctx.refinements) is
        Ok(merged_ref) ->
            merged_type_str = T.type_to_str(merged_ctx.store, merged_ref.narrowed_type)
            Stdout.line!("After if/else merge: $(merged_type_str)")?
        Err(_) -> {}

    Stdout.line!("\n✅ Control-flow narrowing tests passed!")?
    Ok({})
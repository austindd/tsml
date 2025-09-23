module [
    analyze_control_flow,
    TypeGuard,
    FlowNode,
    RefinedType,
]

import Ast
import SimpleComprehensiveType as Type
import Option exposing [Option]

# Type guards that narrow types
TypeGuard : [
    # typeof x === "string"
    TypeOfGuard Str Type.Type,
    # x === null
    EqualityGuard Ast.Node Type.Type,
    # x instanceof Array
    InstanceOfGuard Str Str,
    # x !== undefined
    InequalityGuard Ast.Node Type.Type,
    # !x (truthiness check)
    TruthinessGuard Str Bool,
    # x in obj
    InGuard Str Str,
    # Array.isArray(x)
    IsArrayGuard Str,
]

# Control flow graph node
FlowNode : [
    # Entry point of a function/block
    Entry,
    # Regular statement
    Statement Ast.Node,
    # Conditional branch
    Branch {
        condition: Ast.Node,
        true_branch: U32,  # Index to true flow node
        false_branch: U32, # Index to false flow node
    },
    # Merge point after branches
    Merge (List U32), # Indices of incoming branches
    # Loop
    Loop {
        condition: Ast.Node,
        body: U32,
        exit: U32,
    },
    # Return statement
    Return (Option Ast.Node),
    # Exit point
    Exit,
]

# Type after refinement
RefinedType : {
    base_type: Type.Type,
    refinements: List TypeGuard,
}

# Control flow context
FlowContext : {
    nodes: List FlowNode,
    current_index: U32,
    type_environment: List { name: Str, type: RefinedType },
    guards: List TypeGuard,
}

# Analyze control flow for type narrowing
analyze_control_flow : Ast.Node -> List { variable: Str, refined_type: Type.Type }
analyze_control_flow = |node|
    initial_context = {
        nodes: [Entry],
        current_index: 0,
        type_environment: [],
        guards: [],
    }

    final_context = analyze_node(node, initial_context)

    # Extract refined types from the environment
    List.map(final_context.type_environment, |entry|
        { variable: entry.name, refined_type: apply_refinements(entry.type) })

# Analyze an AST node for control flow
analyze_node : Ast.Node, FlowContext -> FlowContext
analyze_node = |node, ctx|
    when node is
        Program({ body, sourceType }) ->
            List.walk(body, ctx, |acc, stmt|
                analyze_node(stmt, acc))

        IfStatement({ test, consequent, alternate }) ->
            # Extract type guards from condition
            guards = extract_type_guards(test)

            # Create branch node
            branch_node = Branch {
                condition: test,
                true_branch: ctx.current_index + 1,
                false_branch: ctx.current_index + 2,
            }

            # Apply guards in true branch
            true_ctx = apply_guards(ctx, guards, Bool.true)
            true_ctx_after = analyze_node(consequent, true_ctx)

            # Apply negated guards in false branch
            false_ctx = apply_guards(ctx, guards, Bool.false)
            false_ctx_after = when alternate is
                Some(alt) -> analyze_node(alt, false_ctx)
                None -> false_ctx

            # Merge branches
            merge_contexts(true_ctx_after, false_ctx_after)

        WhileStatement({ test, body }) ->
            # Extract guards from loop condition
            guards = extract_type_guards(test)

            # Create loop node
            loop_node = Loop {
                condition: test,
                body: ctx.current_index + 1,
                exit: ctx.current_index + 2,
            }

            # Analyze loop body with guards applied
            loop_ctx = apply_guards(ctx, guards, Bool.true)
            analyze_node(body, loop_ctx)

        BinaryExpression({ left, right, operator }) ->
            # Check for type guards
            when operator is
                EqualEqualEqual | EqualEqual ->
                    when extract_typeof_check(left, right) is
                        Some(guard) ->
                            { ctx & guards: List.append(ctx.guards, guard) }
                        None -> ctx

                BangEqualEqual | BangEqual ->
                    when extract_typeof_check(left, right) is
                        Some(guard) ->
                            # Negate the guard
                            { ctx & guards: List.append(ctx.guards, negate_guard(guard)) }
                        None -> ctx
                _ -> ctx

        VariableDeclarator({ id, init }) ->
            # Add variable to environment
            var_name = when id is
                Identifier({ name }) -> name
                _ -> "_unknown"

            base_type = when init is
                Some(expr) -> infer_base_type(expr)
                None -> Type.mk_undefined

            entry = {
                name: var_name,
                type: { base_type, refinements: [] }
            }

            { ctx & type_environment: List.append(ctx.type_environment, entry) }

        _ -> ctx

# Extract type guards from a condition expression
extract_type_guards : Ast.Node -> List TypeGuard
extract_type_guards = |node|
    when node is
        BinaryExpression({ left, right, operator }) ->
            when operator is
                EqualEqualEqual | EqualEqual ->
                    when extract_typeof_check(left, right) is
                        Some(guard) -> [guard]
                        None -> []

                BangEqualEqual | BangEqual ->
                    when extract_typeof_check(left, right) is
                        Some(guard) -> [negate_guard(guard)]
                        None -> []
                _ -> []

        UnaryExpression({ operator, argument }) ->
            when operator is
                Bang ->
                    when argument is
                        Identifier({ name }) ->
                            [TruthinessGuard name Bool.false]
                        _ -> []
                _ -> []

        LogicalExpression({ left, right, operator }) ->
            left_guards = extract_type_guards(left)
            right_guards = extract_type_guards(right)

            when operator is
                LogicalAnd ->
                    # Both conditions must be true
                    List.concat(left_guards, right_guards)
                LogicalOr ->
                    # Either condition can be true
                    # This is more complex - would need union of refinements
                    []
                _ -> []

        CallExpression({ callee, arguments }) ->
            # Check for Array.isArray(x)
            when callee is
                MemberExpression({ object, property, computed }) ->
                    when object is
                        Identifier({ name }) ->
                            if name == "Array" then
                                when property is
                                    Identifier({ name: prop_name }) ->
                                        if prop_name == "isArray" then
                                            when List.first(arguments) is
                                                Ok(Identifier({ name: arg_name })) ->
                                                    [IsArrayGuard arg_name]
                                                _ -> []
                                        else []
                                    _ -> []
                            else []
                        _ -> []
                _ -> []

        _ -> []

# Extract typeof check from binary expression
extract_typeof_check : Ast.Node, Ast.Node -> Option TypeGuard
extract_typeof_check = |left, right|
    when left is
        UnaryExpression({ operator, argument }) ->
            when operator is
                Typeof ->
                    when argument is
                        Identifier({ name }) ->
                            when right is
                                StringLiteral({ value }) ->
                                    type = string_to_type(value)
                                    Some(TypeOfGuard name type)
                                _ -> None
                        _ -> None
                _ -> None
        _ -> None

# Convert string literal to type
string_to_type : Str -> Type.Type
string_to_type = |str|
    when str is
        "number" -> Type.mk_number
        "string" -> Type.mk_string
        "boolean" -> Type.mk_boolean
        "undefined" -> Type.mk_undefined
        "object" -> Type.mk_object([])
        _ -> Type.mk_unknown

# Apply type guards to context
apply_guards : FlowContext, List TypeGuard, Bool -> FlowContext
apply_guards = |ctx, guards, positive|
    refined_env = List.map(ctx.type_environment, |entry|
        applicable_guards = List.keep_if(guards, |guard|
            guard_applies_to(guard, entry.name))

        refined_guards = if positive then
            applicable_guards
        else
            List.map(applicable_guards, negate_guard)

        { entry &
            type: {
                base_type: entry.type.base_type,
                refinements: List.concat(entry.type.refinements, refined_guards)
            }
        })

    { ctx & type_environment: refined_env }

# Check if a guard applies to a variable
guard_applies_to : TypeGuard, Str -> Bool
guard_applies_to = |guard, var_name|
    when guard is
        TypeOfGuard name _ -> name == var_name
        TruthinessGuard name _ -> name == var_name
        IsArrayGuard name -> name == var_name
        _ -> Bool.false

# Negate a type guard
negate_guard : TypeGuard -> TypeGuard
negate_guard = |guard|
    when guard is
        TypeOfGuard name type ->
            # typeof x !== type means it could be any other type
            TypeOfGuard name Type.mk_unknown
        TruthinessGuard name val ->
            TruthinessGuard name (Bool.not(val))
        _ -> guard

# Merge two flow contexts at a join point
merge_contexts : FlowContext, FlowContext -> FlowContext
merge_contexts = |ctx1, ctx2|
    # Merge type environments
    # This is simplified - a real implementation would compute least upper bounds
    { ctx1 &
        type_environment: ctx1.type_environment,
        guards: []
    }

# Apply refinements to get final type
apply_refinements : RefinedType -> Type.Type
apply_refinements = |refined|
    # Apply all refinements to narrow the base type
    List.walk(refined.refinements, refined.base_type, |current_type, guard|
        when guard is
            TypeOfGuard _ guard_type ->
                # Return the more specific type from the guard
                guard_type
            IsArrayGuard _ ->
                Type.mk_array(Type.mk_unknown)
            _ -> current_type)

# Infer base type from expression
infer_base_type : Ast.Node -> Type.Type
infer_base_type = |node|
    when node is
        NumberLiteral(_) -> Type.mk_number
        StringLiteral(_) -> Type.mk_string
        BooleanLiteral(_) -> Type.mk_boolean
        NullLiteral({}) -> Type.mk_null
        UndefinedLiteral({}) -> Type.mk_undefined
        ArrayExpression(_) -> Type.mk_array(Type.mk_unknown)
        ObjectExpression(_) -> Type.mk_object([])
        _ -> Type.mk_unknown
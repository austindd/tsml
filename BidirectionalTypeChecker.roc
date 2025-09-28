module [
    check_type,
    synth_type,
    TypeContext,
    Mode,
]

import Ast
import SimpleComprehensiveType as Type
import Option exposing [Option]

# Type checking mode
Mode : [
    # Synthesis mode - infer type from expression
    Synthesis,
    # Checking mode - check expression against expected type
    Checking Type.Type,
]

# Type context for bidirectional checking
TypeContext : {
    # Variable bindings
    bindings: List { name: Str, type: Type.Type },

    # Current mode
    mode: Mode,

    # Parent context for nested scopes (using index instead of self-reference)
    parent_id: Option U64,
}

# Create empty context
empty_context : TypeContext
empty_context = {
    bindings: [],
    mode: Synthesis,
    parent_id: None,
}

# Add binding to context
add_binding : TypeContext, Str, Type.Type -> TypeContext
add_binding = |ctx, name, type|
    { ctx & bindings: List.append(ctx.bindings, { name, type }) }

# Lookup variable in context (simplified without parent traversal)
lookup_var : TypeContext, Str -> Option Type.Type
lookup_var = |ctx, name|
    when List.find_first(ctx.bindings, |b| b.name == name) is
        Ok(binding) -> Some(binding.type)
        Err(_) -> None

# Create nested context
nest_context : TypeContext -> TypeContext
nest_context = |ctx|
    { bindings: ctx.bindings, mode: ctx.mode, parent_id: Some(0) }

# Main type checking function (bidirectional)
check_type : Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
check_type = |node, ctx|
    when ctx.mode is
        Synthesis -> synth_type(node, ctx)
        Checking(expected_type) -> check_against(node, expected_type, ctx)

# Type synthesis (inference mode)
synth_type : Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_type = |node, ctx|
    when node is
        # Literals have obvious types
        NumberLiteral(_) -> Ok(Type.mk_number)
        StringLiteral(_) -> Ok(Type.mk_string)
        BooleanLiteral(_) -> Ok(Type.mk_boolean)
        NullLiteral(_) -> Ok(Type.mk_null)
        UndefinedLiteral(_) -> Ok(Type.mk_undefined)

        # Variable lookup
        Identifier({ name }) ->
            when lookup_var(ctx, name) is
                Some(type) -> Ok(type)
                None -> Err(TypeError("Undefined variable: $(name)"))

        # Binary expressions
        BinaryExpression({ left, operator, right }) ->
            synth_binary(left, operator, right, ctx)

        # Logical expressions
        LogicalExpression({ left, operator, right }) ->
            synth_logical(left, operator, right, ctx)

        # Member access
        MemberExpression({ object, property, computed }) ->
            synth_member(object, property, computed, ctx)

        # Function call
        CallExpression({ callee, arguments }) ->
            synth_call(callee, arguments, ctx)

        # Array literal
        ArrayExpression({ elements }) ->
            synth_array(elements, ctx)

        # Object literal
        ObjectExpression({ properties }) ->
            synth_object(properties, ctx)

        # Function expressions
        ArrowFunctionExpression({ params, body, async, generator }) ->
            synth_function(params, body, ctx)

        FunctionExpression({ id, params, body, async, generator }) ->
            synth_function(params, body, ctx)

        # Conditional expression
        ConditionalExpression({ test, consequent, alternate }) ->
            synth_conditional(test, consequent, alternate, ctx)

        # Assignment
        AssignmentExpression({ left, operator, right }) ->
            synth_assignment(left, operator, right, ctx)

        _ -> Ok(Type.mk_unknown)

# Type checking (checking mode)
check_against : Ast.Node, Type.Type, TypeContext -> Result Type.Type [TypeError Str]
check_against = |node, expected_type, ctx|
    when node is
        # Literals - check they match expected type
        NumberLiteral(_) ->
            if Type.is_assignable_to(Type.mk_number, expected_type) then
                Ok(Type.mk_number)
            else
                Err(TypeError("Expected $(Type.type_to_string(expected_type)), got number"))

        StringLiteral(_) ->
            if Type.is_assignable_to(Type.mk_string, expected_type) then
                Ok(Type.mk_string)
            else
                Err(TypeError("Expected $(Type.type_to_string(expected_type)), got string"))

        BooleanLiteral(_) ->
            if Type.is_assignable_to(Type.mk_boolean, expected_type) then
                Ok(Type.mk_boolean)
            else
                Err(TypeError("Expected $(Type.type_to_string(expected_type)), got boolean"))

        # Lambda expressions can be checked against function types
        ArrowFunctionExpression({ params, body, async, generator }) ->
            check_function(params, body, expected_type, ctx)

        # Array literals can be checked against array types
        ArrayExpression({ elements }) ->
            when expected_type is
                TArray(elem_type) ->
                    check_array_elements(elements, elem_type, ctx)
                _ ->
                    # Fall back to synthesis
                    synth_array(elements, ctx)

        # Object literals can be checked against object types
        ObjectExpression({ properties }) ->
            when expected_type is
                TObject(expected_props) ->
                    check_object_props(properties, expected_props, ctx)
                _ ->
                    # Fall back to synthesis
                    synth_object(properties, ctx)

        # For most expressions, synthesize and check subtype
        _ ->
            when synth_type(node, ctx) is
                Ok(actual_type) ->
                    if Type.is_assignable_to(actual_type, expected_type) then
                        Ok(actual_type)
                    else
                        Err(TypeError("Type mismatch: expected $(Type.type_to_string(expected_type)), got $(Type.type_to_string(actual_type))"))
                Err(e) -> Err(e)

# Synthesize binary expression type
synth_binary : Ast.Node, Ast.BinaryOperator, Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_binary = |left, op, right, ctx|
    when op is
        # Arithmetic operators
        Plus | Minus | Star | Slash | Percent ->
            # These produce numbers
            Ok(Type.mk_number)

        # Comparison operators
        LessThan | LessThanEqual | GreaterThan | GreaterThanEqual ->
            Ok(Type.mk_boolean)

        # Equality operators
        EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
            Ok(Type.mk_boolean)

        # Bitwise operators
        Ampersand | Pipe | Caret | LeftShift | RightShift | UnsignedRightShift ->
            Ok(Type.mk_number)

        # Special operators
        In | Instanceof ->
            Ok(Type.mk_boolean)

        _ -> Ok(Type.mk_unknown)

synth_logical : Ast.Node, Ast.LogicalOperator, Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_logical = |left, op, right, ctx|
    when op is
        # Logical operators
        LogicalAnd | LogicalOr ->
            # Return union of operand types
            when (synth_type(left, ctx), synth_type(right, ctx)) is
                (Ok(left_type), Ok(right_type)) ->
                    Ok(Type.mk_union([left_type, right_type]))
                _ -> Ok(Type.mk_unknown)

# Synthesize member access type
synth_member : Ast.Node, Ast.Node, Bool, TypeContext -> Result Type.Type [TypeError Str]
synth_member = |object, property, computed, ctx|
    when synth_type(object, ctx) is
        Ok(TObject(props)) ->
            when property is
                Identifier({ name }) ->
                    # Look up property
                    when List.find_first(props, |p| p.key == name) is
                        Ok(prop) -> Ok(prop.value_type)
                        Err(_) -> Ok(Type.mk_unknown)
                _ -> Ok(Type.mk_unknown)
        _ -> Ok(Type.mk_unknown)

# Synthesize call expression type
synth_call : Ast.Node, List Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_call = |callee, arguments, ctx|
    # For now, calls return unknown
    # A full implementation would need function types
    Ok(Type.mk_unknown)

# Synthesize array literal type
synth_array : List Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_array = |elements, ctx|
    # Create union of element types
    when elements is
        [] -> Ok(Type.mk_array(Type.mk_unknown))
        [single] ->
            when synth_type(single, ctx) is
                Ok(single_type) -> Ok(Type.mk_array(single_type))
                Err(_) -> Ok(Type.mk_array(Type.mk_unknown))
        multiple ->
            type_results = List.map(multiple, |elem| synth_type(elem, ctx))
            if List.any(type_results, |result| Result.is_err(result)) then
                Ok(Type.mk_array(Type.mk_unknown))
            else 
                types = List.map(type_results, |result| 
                    when result is
                        Ok(t) -> t
                        Err(_) -> Type.mk_unknown
                )
                Ok(Type.mk_array(Type.mk_union(types)))

# Synthesize object literal type
synth_object : List Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_object = |properties, ctx|
    props = List.keep_oks(properties, |prop|
        when prop is
            Property({ key, value, kind }) ->
                when key is
                    Identifier({ name }) ->
                        when value is
                            Some(val) ->
                                when synth_type(val, ctx) is
                                    Ok(val_type) -> Ok({ key: name, value_type: val_type })
                                    Err(_) -> Err(NotProperty)
                            None -> Ok({ key: name, value_type: Type.mk_unknown })
                    _ -> Err(NotProperty)
            _ -> Err(NotProperty))

    Ok(Type.mk_object(props))

# Synthesize function type
synth_function : List Ast.Node, Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_function = |params, body, ctx|
    # For now, functions return unknown
    # A full implementation would need function types
    Ok(Type.mk_unknown)

# Synthesize conditional expression type
synth_conditional : Ast.Node, Ast.Node, Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_conditional = |test, consequent, alternate, ctx|
    when (synth_type(consequent, ctx), synth_type(alternate, ctx)) is
        (Ok(cons_type), Ok(alt_type)) ->
            # Union of both branches
            Ok(Type.mk_union([cons_type, alt_type]))
        _ -> Ok(Type.mk_unknown)

# Synthesize assignment expression type
synth_assignment : Ast.Node, Ast.AssignmentOperator, Ast.Node, TypeContext -> Result Type.Type [TypeError Str]
synth_assignment = |left, op, right, ctx|
    synth_type(right, ctx)

# Check function against expected type
check_function : List Ast.Node, Ast.Node, Type.Type, TypeContext -> Result Type.Type [TypeError Str]
check_function = |params, body, expected, ctx|
    # For now, accept any function
    # Full implementation would check parameter and return types
    Ok(Type.mk_unknown)

# Check array elements against expected element type
check_array_elements : List Ast.Node, Type.Type, TypeContext -> Result Type.Type [TypeError Str]
check_array_elements = |elements, elem_type, ctx|
    # Check each element
    checking_ctx = { ctx & mode: Checking(elem_type) }

    results = List.walk(elements, Ok([]), |acc_result, elem|
        when acc_result is
            Ok(types) ->
                when check_type(elem, checking_ctx) is
                    Ok(t) -> Ok(List.append(types, t))
                    Err(e) -> Err(e)
            Err(e) -> Err(e))

    when results is
        Ok(_) -> Ok(Type.mk_array(elem_type))
        Err(e) -> Err(e)

# Check object properties against expected properties
check_object_props : List Ast.Node, List { key: Str, value_type: Type.Type }, TypeContext -> Result Type.Type [TypeError Str]
check_object_props = |actual_props, expected_props, ctx|
    # For each expected property, check it exists with right type
    results = List.walk(expected_props, Ok([]), |acc_result, exp_prop|
        when acc_result is
            Ok(checked) ->
                # Find corresponding actual property
                actual_prop_opt = List.find_first(actual_props, |p|
                    when p is
                        Property({ key, value, kind }) ->
                            when key is
                                Identifier({ name }) -> name == exp_prop.key
                                _ -> Bool.false
                        _ -> Bool.false)

                when actual_prop_opt is
                    Ok(Property({ key, value, kind })) ->
                        when value is
                            Some(val) ->
                                checking_ctx = { ctx & mode: Checking(exp_prop.value_type) }
                                when check_type(val, checking_ctx) is
                                    Ok(val_type) ->
                                        Ok(List.append(checked, { key: exp_prop.key, value_type: val_type }))
                                    Err(e) -> Err(e)
                            None ->
                                Ok(List.append(checked, { key: exp_prop.key, value_type: Type.mk_unknown }))
                    _ -> Err(TypeError("Missing required property: $(exp_prop.key)"))
            Err(e) -> Err(e))

    when results is
        Ok(props) -> Ok(Type.mk_object(props))
        Err(e) -> Err(e)

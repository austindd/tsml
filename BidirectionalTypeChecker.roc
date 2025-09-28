module [
    TypeContext,
    CheckResult,
    SynthResult,
    TypeMode,
    empty_context,
    extend_context,
    lookup_var,
    check,
    synth,
    apply_context,
    check_statement,
    synth_expression,
]

import ComprehensiveTypeIndexed as T
import Ast
import Option exposing [Option, Some, None]

# Type checking context - tracks variable bindings and type information
TypeContext : {
    store : T.TypeStore,
    bindings : List { name : Str, type_id : T.TypeId },
    return_type : Option T.TypeId,  # Expected return type for current function
    in_loop : Bool,  # Track if we're inside a loop (for break/continue)
}

# Results from type checking operations
CheckResult : Result {} TypeError
SynthResult : Result { type_id : T.TypeId, context : TypeContext } TypeError

TypeError : [
    TypeMismatch { expected : T.TypeId, actual : T.TypeId, context : TypeContext },
    UnboundVariable Str,
    CannotSynthesize Str,
    InvalidOperation Str,
    UnificationError Str,
    ReturnTypeMismatch { expected : T.TypeId, actual : T.TypeId },
    BreakOutsideLoop,
    ContinueOutsideLoop,
]

TypeMode : [
    Synthesis,  # Infer the type
    Checking T.TypeId,  # Check against expected type
]

# Create an empty type checking context
empty_context : T.TypeStore -> TypeContext
empty_context = \store ->
    {
        store: store,
        bindings: [],
        return_type: None,
        in_loop: Bool.false,
    }

# Add a variable binding to the context
extend_context : TypeContext, Str, T.TypeId -> TypeContext
extend_context = \ctx, name, type_id ->
    { ctx &
        bindings: List.append(ctx.bindings, { name: name, type_id: type_id })
    }

# Look up a variable's type in the context
lookup_var : TypeContext, Str -> Result T.TypeId TypeError
lookup_var = \ctx, name ->
    List.find_last(ctx.bindings, \binding -> binding.name == name)
    |> Result.map_ok(\binding -> binding.type_id)
    |> Result.map_err(\_ -> UnboundVariable(name))

# Main checking function - checks a term against an expected type
check : TypeContext, Ast.Node, T.TypeId -> Result TypeContext TypeError
check = \ctx, node, expected_type ->
    when node is
        # For literals, synthesize and check subtyping
        NumericLiteral(data) ->
            (ctx1, lit_type) = T.make_literal(ctx.store, NumLit(data.value))
            if T.is_subtype_of(ctx1, lit_type, expected_type) then
                Ok({ ctx & store: ctx1 })
            else
                Err(TypeMismatch({ expected: expected_type, actual: lit_type, context: { ctx & store: ctx1 } }))

        StringLiteral(data) ->
            (ctx1, lit_type) = T.make_literal(ctx.store, StrLit(data.value))
            if T.is_subtype_of(ctx1, lit_type, expected_type) then
                Ok({ ctx & store: ctx1 })
            else
                Err(TypeMismatch({ expected: expected_type, actual: lit_type, context: { ctx & store: ctx1 } }))

        BooleanLiteral(data) ->
            (ctx1, lit_type) = T.make_literal(ctx.store, BoolLit(data.value))
            if T.is_subtype_of(ctx1, lit_type, expected_type) then
                Ok({ ctx & store: ctx1 })
            else
                Err(TypeMismatch({ expected: expected_type, actual: lit_type, context: { ctx & store: ctx1 } }))

        # For identifiers, look up and check subtyping
        Identifier(data) ->
            when lookup_var(ctx, data.name) is
                Ok(var_type) ->
                    if T.is_subtype_of(ctx.store, var_type, expected_type) then
                        Ok(ctx)
                    else
                        Err(TypeMismatch({ expected: expected_type, actual: var_type, context: ctx }))
                Err(err) -> Err(err)

        # Lambda/function expressions can be checked against function types
        ArrowFunctionExpression(data) ->
            when T.get_type(ctx.store, expected_type) is
                Ok(TFunction(fn_type)) ->
                    check_function(ctx, data.params, data.body, fn_type.params, fn_type.return_type)
                _ ->
                    # Can't check arrow function against non-function type
                    synth(ctx, node)
                    |> Result.try(\result ->
                        if T.is_subtype_of(result.context.store, result.type_id, expected_type) then
                            Ok(result.context)
                        else
                            Err(TypeMismatch({ expected: expected_type, actual: result.type_id, context: result.context }))
                    )

        # For most expressions, synthesize then check subtyping
        _ ->
            synth(ctx, node)
            |> Result.try(\result ->
                if T.is_subtype_of(result.context.store, result.type_id, expected_type) then
                    Ok(result.context)
                else
                    Err(TypeMismatch({ expected: expected_type, actual: result.type_id, context: result.context }))
            )

# Main synthesis function - infers the type of a term
synth : TypeContext, Ast.Node -> SynthResult
synth = \ctx, node ->
    when node is
        # Literals synthesize to their literal types
        NumericLiteral(data) ->
            (ctx1, lit_type) = T.make_literal(ctx.store, NumLit(data.value))
            Ok({ type_id: lit_type, context: { ctx & store: ctx1 } })

        StringLiteral(data) ->
            (ctx1, lit_type) = T.make_literal(ctx.store, StrLit(data.value))
            Ok({ type_id: lit_type, context: { ctx & store: ctx1 } })

        BooleanLiteral(data) ->
            (ctx1, lit_type) = T.make_literal(ctx.store, BoolLit(data.value))
            Ok({ type_id: lit_type, context: { ctx & store: ctx1 } })

        NullLiteral(_) ->
            (ctx1, null_type) = T.make_primitive(ctx.store, "null")
            Ok({ type_id: null_type, context: { ctx & store: ctx1 } })

        # Identifiers look up their type
        Identifier(data) ->
            lookup_var(ctx, data.name)
            |> Result.map_ok(\type_id -> { type_id: type_id, context: ctx })

        # Binary operations
        BinaryExpression(data) ->
            synth_binary(ctx, data.left, data.operator, data.right)

        # Unary operations
        UnaryExpression(data) ->
            synth_unary(ctx, data.operator, data.argument)

        # Array expressions
        ArrayExpression(data) ->
            synth_array(ctx, data.elements)

        # Object expressions
        ObjectExpression(data) ->
            synth_object(ctx, data.properties)

        # Member access
        MemberExpression(data) ->
            synth_member(ctx, data.object, data.property, data.computed)

        # Function calls
        CallExpression(data) ->
            synth_call(ctx, data.callee, data.arguments)

        # Conditional expressions (ternary)
        ConditionalExpression(data) ->
            synth_conditional(ctx, data.test, data.consequent, data.alternate)

        # Assignment
        AssignmentExpression(data) ->
            synth_assignment(ctx, data.left, data.operator, data.right)

        # Arrow functions
        ArrowFunctionExpression(data) ->
            synth_arrow_function(ctx, data.params, data.body, data.return_type)

        # Function expressions
        FunctionExpression(data) ->
            synth_function_expr(ctx, data.params, data.body, data.return_type)

        _ ->
            Err(CannotSynthesize("Cannot synthesize type for $(Ast.node_to_str(node))"))

# Helper: Synthesize binary expression type
synth_binary : TypeContext, Ast.Node, Str, Ast.Node -> SynthResult
synth_binary = \ctx, left, op, right ->
    when op is
        "+" | "-" | "*" | "/" | "%" ->
            # Arithmetic operators
            synth(ctx, left)
            |> Result.try(\left_result ->
                synth(left_result.context, right)
                |> Result.try(\right_result ->
                    # Check if both operands are numeric
                    (store1, number_type) = T.make_primitive(right_result.context.store, "number")
                    if T.is_subtype_of(store1, left_result.type_id, number_type) &&
                       T.is_subtype_of(store1, right_result.type_id, number_type) then
                        Ok({ type_id: number_type, context: { right_result.context & store: store1 } })
                    else
                        # For +, could be string concatenation
                        if op == "+" then
                            (store2, string_type) = T.make_primitive(store1, "string")
                            if T.is_subtype_of(store2, left_result.type_id, string_type) ||
                               T.is_subtype_of(store2, right_result.type_id, string_type) then
                                Ok({ type_id: string_type, context: { right_result.context & store: store2 } })
                            else
                                Ok({ type_id: number_type, context: { right_result.context & store: store2 } })
                        else
                            Ok({ type_id: number_type, context: { right_result.context & store: store1 } })
                )
            )

        "===" | "!==" | "==" | "!=" ->
            # Equality operators always return boolean
            synth(ctx, left)
            |> Result.try(\left_result ->
                synth(left_result.context, right)
                |> Result.try(\right_result ->
                    (store1, bool_type) = T.make_primitive(right_result.context.store, "boolean")
                    Ok({ type_id: bool_type, context: { right_result.context & store: store1 } })
                )
            )

        "<" | ">" | "<=" | ">=" ->
            # Comparison operators return boolean
            synth(ctx, left)
            |> Result.try(\left_result ->
                synth(left_result.context, right)
                |> Result.try(\right_result ->
                    (store1, bool_type) = T.make_primitive(right_result.context.store, "boolean")
                    Ok({ type_id: bool_type, context: { right_result.context & store: store1 } })
                )
            )

        "&&" | "||" ->
            # Logical operators
            synth(ctx, left)
            |> Result.try(\left_result ->
                synth(left_result.context, right)
                |> Result.map_ok(\right_result ->
                    # Result is union of both operand types
                    (store1, union_type) = T.join(right_result.context.store, left_result.type_id, right_result.type_id)
                    { type_id: union_type, context: { right_result.context & store: store1 } }
                )
            )

        _ ->
            Err(InvalidOperation("Unknown binary operator: $(op)"))

# Helper: Synthesize unary expression type
synth_unary : TypeContext, Str, Ast.Node -> SynthResult
synth_unary = \ctx, op, argument ->
    when op is
        "!" ->
            # Logical not always returns boolean
            synth(ctx, argument)
            |> Result.try(\arg_result ->
                (store1, bool_type) = T.make_primitive(arg_result.context.store, "boolean")
                Ok({ type_id: bool_type, context: { arg_result.context & store: store1 } })
            )

        "-" | "+" ->
            # Numeric operators return number
            synth(ctx, argument)
            |> Result.try(\arg_result ->
                (store1, number_type) = T.make_primitive(arg_result.context.store, "number")
                Ok({ type_id: number_type, context: { arg_result.context & store: store1 } })
            )

        "typeof" ->
            # typeof always returns string
            synth(ctx, argument)
            |> Result.try(\arg_result ->
                (store1, string_type) = T.make_primitive(arg_result.context.store, "string")
                Ok({ type_id: string_type, context: { arg_result.context & store: store1 } })
            )

        "void" ->
            # void always returns undefined
            synth(ctx, argument)
            |> Result.try(\arg_result ->
                (store1, undefined_type) = T.make_primitive(arg_result.context.store, "undefined")
                Ok({ type_id: undefined_type, context: { arg_result.context & store: store1 } })
            )

        _ ->
            Err(InvalidOperation("Unknown unary operator: $(op)"))

# Helper: Synthesize array type
synth_array : TypeContext, List (Option Ast.Node) -> SynthResult
synth_array = \ctx, elements ->
    # Synthesize types for all elements
    List.walk(elements, Ok({ types: [], context: ctx }), \acc_result, elem_opt ->
        acc_result
        |> Result.try(\acc ->
            when elem_opt is
                Some(elem) ->
                    synth(acc.context, elem)
                    |> Result.map_ok(\elem_result ->
                        {
                            types: List.append(acc.types, elem_result.type_id),
                            context: elem_result.context
                        }
                    )
                None ->
                    # Spread element or hole - use unknown for now
                    (store1, unknown) = T.make_unknown(acc.context.store)
                    Ok({
                        types: List.append(acc.types, unknown),
                        context: { acc.context & store: store1 }
                    })
        )
    )
    |> Result.map_ok(\result ->
        # Join all element types to get the array element type
        when result.types is
            [] ->
                # Empty array - element type is never
                (store1, never) = T.make_never(result.context.store)
                (store2, array_type) = T.make_array(store1, never)
                { type_id: array_type, context: { result.context & store: store2 } }
            [single] ->
                (store1, array_type) = T.make_array(result.context.store, single)
                { type_id: array_type, context: { result.context & store: store1 } }
            _ ->
                # Multiple elements - join all types
                first_type = when List.first(result.types) is
                    Ok(t) -> t
                    Err(_) -> 0  # Should never happen since we checked list isn't empty
                (final_store, elem_type) = List.walk(
                    List.drop_first(result.types, 1),
                    (result.context.store, first_type),
                    \(store, current_type), next_type ->
                        T.join(store, current_type, next_type)
                )
                (store2, array_type) = T.make_array(final_store, elem_type)
                { type_id: array_type, context: { result.context & store: store2 } }
    )

# Helper: Synthesize object type
synth_object : TypeContext, List Ast.PropertyNode -> SynthResult
synth_object = \ctx, properties ->
    # Build up the object type by processing each property
    List.walk(properties, Ok({ row_id: 0, context: ctx }), \acc_result, prop ->
        acc_result
        |> Result.try(\acc ->
            when prop is
                Property(data) ->
                    # Get property name
                    prop_name = when data.key is
                        Identifier(id_data) -> id_data.name
                        StringLiteral(str_data) -> str_data.value
                        _ -> "unknown"

                    # Synthesize property value type
                    synth(acc.context, data.value)
                    |> Result.map_ok(\value_result ->
                        # Extend the row type
                        if acc.row_id == 0 then
                            # First property - create initial row
                            (store1, empty_row) = T.make_empty_row(value_result.context.store)
                            (store2, new_row) = T.make_row_extend(
                                store1,
                                prop_name,
                                value_result.type_id,
                                Bool.false,  # not optional
                                Bool.false,  # not readonly
                                empty_row
                            )
                            { row_id: new_row, context: { value_result.context & store: store2 } }
                        else
                            # Extend existing row
                            (store1, new_row) = T.make_row_extend(
                                value_result.context.store,
                                prop_name,
                                value_result.type_id,
                                Bool.false,
                                Bool.false,
                                acc.row_id
                            )
                            { row_id: new_row, context: { value_result.context & store: store1 } }
                    )

                SpreadElement(_) ->
                    # For spread properties, we'd need to handle them specially
                    # For now, just continue with current row
                    Ok(acc)
        )
    )
    |> Result.map_ok(\result ->
        if result.row_id == 0 then
            # Empty object
            (store1, empty_row) = T.make_empty_row(result.context.store)
            (store2, obj_type) = T.make_object(store1, empty_row)
            { type_id: obj_type, context: { result.context & store: store2 } }
        else
            (store1, obj_type) = T.make_object(result.context.store, result.row_id)
            { type_id: obj_type, context: { result.context & store: store1 } }
    )

# Helper: Synthesize member access type
synth_member : TypeContext, Ast.Node, Ast.Node, Bool -> SynthResult
synth_member = \ctx, object, property, _computed ->
    synth(ctx, object)
    |> Result.try(\obj_result ->
        when T.get_type(obj_result.context.store, obj_result.type_id) is
            Ok(TObject(row_id)) ->
                # Get property name
                prop_name = when property is
                    Identifier(data) -> data.name
                    StringLiteral(data) -> data.value
                    _ -> ""

                # Look up property in row
                lookup_row_field(obj_result.context.store, row_id, prop_name)
                |> Result.map_ok(\field_type ->
                    { type_id: field_type, context: obj_result.context }
                )
                |> Result.map_err(\_ ->
                    InvalidOperation("Property '$(prop_name)' not found")
                )
            _ ->
                # Not an object - return unknown
                (store1, unknown) = T.make_unknown(obj_result.context.store)
                Ok({ type_id: unknown, context: { obj_result.context & store: store1 } })
    )

# Helper: Look up field in row type
lookup_row_field : T.TypeStore, T.RowId, Str -> Result T.TypeId [NotFound]
lookup_row_field = \store, row_id, field_name ->
    when T.get_row(store, row_id) is
        Ok(RExtend(extend)) ->
            if extend.label == field_name then
                Ok(extend.field_type)
            else
                lookup_row_field(store, extend.rest, field_name)
        _ -> Err(NotFound)

# Helper: Synthesize function call type
synth_call : TypeContext, Ast.Node, List Ast.ArgumentNode -> SynthResult
synth_call = \ctx, callee, arguments ->
    synth(ctx, callee)
    |> Result.try(\callee_result ->
        when T.get_type(callee_result.context.store, callee_result.type_id) is
            Ok(TFunction(fn_type)) ->
                # Check arguments against parameters
                check_arguments(callee_result.context, arguments, fn_type.params)
                |> Result.map_ok(\new_context ->
                    { type_id: fn_type.return_type, context: new_context }
                )
            _ ->
                # Not a function - return unknown
                (store1, unknown) = T.make_unknown(callee_result.context.store)
                Ok({ type_id: unknown, context: { callee_result.context & store: store1 } })
    )

# Helper: Check function arguments
check_arguments : TypeContext, List Ast.ArgumentNode, List T.TypeId -> Result TypeContext TypeError
check_arguments = \ctx, args, param_types ->
    # For simplicity, just check positional arguments
    arg_exprs = List.map(args, \arg ->
        when arg is
            Argument(expr) -> expr
            SpreadArgument(_) -> Identifier({ name: "spread", type_annotation: None })  # placeholder
    )

    List.walk2(arg_exprs, param_types, Ok(ctx), \acc_result, arg, param_type ->
        acc_result
        |> Result.try(\acc_ctx ->
            check(acc_ctx, arg, param_type)
        )
    )

# Helper: Synthesize conditional expression type
synth_conditional : TypeContext, Ast.Node, Ast.Node, Ast.Node -> SynthResult
synth_conditional = \ctx, test, consequent, alternate ->
    # First check test is boolean-like
    synth(ctx, test)
    |> Result.try(\test_result ->
        # Synthesize both branches
        synth(test_result.context, consequent)
        |> Result.try(\cons_result ->
            synth(cons_result.context, alternate)
            |> Result.map_ok(\alt_result ->
                # Result type is join of both branches
                (store1, result_type) = T.join(alt_result.context.store, cons_result.type_id, alt_result.type_id)
                { type_id: result_type, context: { alt_result.context & store: store1 } }
            )
        )
    )

# Helper: Synthesize assignment expression type
synth_assignment : TypeContext, Ast.Node, Str, Ast.Node -> SynthResult
synth_assignment = \ctx, left, _operator, right ->
    # For now, just return the type of the right side
    synth(ctx, right)

# Helper: Synthesize arrow function type
synth_arrow_function : TypeContext, List Ast.Node, Ast.Node, Option Ast.TypeAnnotation -> SynthResult
synth_arrow_function = \ctx, params, body, return_type_opt ->
    # Create parameter types (unknown if not annotated)
    (store_with_params, param_types, new_ctx) = List.walk(params, (ctx.store, [], ctx), \(store, types, current_ctx), param ->
        when param is
            Identifier(data) ->
                param_type = when data.type_annotation is
                    Some(ann) -> type_annotation_to_type(store, ann)
                    None -> T.make_unknown(store)

                when param_type is
                    (new_store, type_id) ->
                        new_binding_ctx = extend_context(current_ctx, data.name, type_id)
                        (new_store, List.append(types, type_id), new_binding_ctx)
            _ ->
                # Non-identifier param - use unknown
                (new_store, unknown) = T.make_unknown(store)
                (new_store, List.append(types, unknown), current_ctx)
    )

    # Determine return type
    (store_with_return, expected_return) = when return_type_opt is
        Some(ann) -> type_annotation_to_type(store_with_params, ann)
        None -> T.make_unknown(store_with_params)

    # Set return type in context for checking body
    body_ctx = { new_ctx & store: store_with_return, return_type: Some(expected_return) }

    # Check body against return type
    check(body_ctx, body, expected_return)
    |> Result.map_ok(\final_ctx ->
        # Create function type
        param_records = List.map_with_index(param_types, \type_id, idx ->
            { name: "arg$(Num.to_str(idx))", param_type: type_id, optional: Bool.false }
        )
        (final_store, fn_type) = T.make_function(
            final_ctx.store,
            param_records,
            expected_return,
            [],  # type parameters
            Bool.false,  # async
            Bool.false   # generator
        )
        { type_id: fn_type, context: { final_ctx & store: final_store } }
    )

# Helper: Synthesize function expression type
synth_function_expr : TypeContext, List Ast.FunctionParam, Ast.Node, Option Ast.TypeAnnotation -> SynthResult
synth_function_expr = \ctx, params, body, return_type_opt ->
    # Similar to arrow function but with FunctionParam nodes
    (store_with_params, param_types, new_ctx) = List.walk(params, (ctx.store, [], ctx), \(store, types, current_ctx), param ->
        when param is
            FunctionParam(data) ->
                when data.name is
                    Identifier(id_data) ->
                        param_type = when data.type_annotation is
                            Some(ann) -> type_annotation_to_type(store, ann)
                            None -> T.make_unknown(store)

                        when param_type is
                            (new_store, type_id) ->
                                new_binding_ctx = extend_context(current_ctx, id_data.name, type_id)
                                (new_store, List.append(types, type_id), new_binding_ctx)
                    _ ->
                        (new_store, unknown) = T.make_unknown(store)
                        (new_store, List.append(types, unknown), current_ctx)
    )

    # Rest is similar to arrow function
    (store_with_return, expected_return) = when return_type_opt is
        Some(ann) -> type_annotation_to_type(store_with_params, ann)
        None -> T.make_unknown(store_with_params)

    body_ctx = { new_ctx & store: store_with_return, return_type: Some(expected_return) }

    check(body_ctx, body, expected_return)
    |> Result.map_ok(\final_ctx ->
        param_records = List.map_with_index(param_types, \type_id, idx ->
            { name: "arg$(Num.to_str(idx))", param_type: type_id, optional: Bool.false }
        )
        (final_store, fn_type) = T.make_function(
            final_ctx.store,
            param_records,
            expected_return,
            [],
            Bool.false,
            Bool.false
        )
        { type_id: fn_type, context: { final_ctx & store: final_store } }
    )

# Helper: Convert TypeAnnotation to TypeId
type_annotation_to_type : T.TypeStore, Ast.TypeAnnotation -> (T.TypeStore, T.TypeId)
type_annotation_to_type = \store, annotation ->
    when annotation is
        TSTypeReference(data) ->
            when data.type_name is
                "number" -> T.make_primitive(store, "number")
                "string" -> T.make_primitive(store, "string")
                "boolean" -> T.make_primitive(store, "boolean")
                "void" -> T.make_primitive(store, "void")
                "undefined" -> T.make_primitive(store, "undefined")
                "null" -> T.make_primitive(store, "null")
                "any" -> T.make_any(store)
                "unknown" -> T.make_unknown(store)
                "never" -> T.make_never(store)
                _ -> T.make_unknown(store)  # Unknown type name

        TSNumberKeyword -> T.make_primitive(store, "number")
        TSStringKeyword -> T.make_primitive(store, "string")
        TSBooleanKeyword -> T.make_primitive(store, "boolean")
        TSVoidKeyword -> T.make_primitive(store, "void")
        TSUndefinedKeyword -> T.make_primitive(store, "undefined")
        TSNullKeyword -> T.make_primitive(store, "null")
        TSAnyKeyword -> T.make_any(store)
        TSUnknownKeyword -> T.make_unknown(store)
        TSNeverKeyword -> T.make_never(store)

        TSArrayType(elem_ann) ->
            (store1, elem_type) = type_annotation_to_type(store, elem_ann)
            T.make_array(store1, elem_type)

        TSUnionType(anns) ->
            (final_store, types) = List.walk(anns, (store, []), \(acc_store, acc_types), ann ->
                (new_store, type_id) = type_annotation_to_type(acc_store, ann)
                (new_store, List.append(acc_types, type_id))
            )
            T.make_union(final_store, types)

        TSIntersectionType(anns) ->
            (final_store, types) = List.walk(anns, (store, []), \(acc_store, acc_types), ann ->
                (new_store, type_id) = type_annotation_to_type(acc_store, ann)
                (new_store, List.append(acc_types, type_id))
            )
            T.make_intersection(final_store, types)

        _ -> T.make_unknown(store)  # Unsupported annotation

# Helper: Check a function against expected parameter and return types
check_function : TypeContext, List Ast.Node, Ast.Node, List T.TypeId, T.TypeId -> Result TypeContext TypeError
check_function = \ctx, params, body, expected_params, expected_return ->
    # Bind parameters with their expected types
    new_ctx = List.walk2(params, expected_params, ctx, \acc_ctx, param, param_type ->
        when param is
            Identifier(data) ->
                extend_context(acc_ctx, data.name, param_type)
            _ -> acc_ctx
    )

    # Check body against expected return type
    body_ctx = { new_ctx & return_type: Some(expected_return) }
    check(body_ctx, body, expected_return)

# Check a statement (for use in block statements)
check_statement : TypeContext, Ast.Node -> Result TypeContext TypeError
check_statement = \ctx, stmt ->
    when stmt is
        VariableDeclaration(data) ->
            List.walk(data.declarations, Ok(ctx), \acc_result, decl ->
                acc_result
                |> Result.try(\acc_ctx ->
                    when decl is
                        VariableDeclarator(decl_data) ->
                            when decl_data.id is
                                Identifier(id_data) ->
                                    when decl_data.init is
                                        Some(init_expr) ->
                                            # Synthesize type of initializer
                                            synth(acc_ctx, init_expr)
                                            |> Result.map_ok(\init_result ->
                                                extend_context(init_result.context, id_data.name, init_result.type_id)
                                            )
                                        None ->
                                            # No initializer - type is undefined
                                            (store1, undefined) = T.make_primitive(acc_ctx.store, "undefined")
                                            Ok(extend_context({ acc_ctx & store: store1 }, id_data.name, undefined))
                                _ -> Ok(acc_ctx)
                )
            )

        ExpressionStatement(data) ->
            synth(ctx, data.expression)
            |> Result.map_ok(\result -> result.context)

        ReturnStatement(data) ->
            when data.argument is
                Some(arg) ->
                    when ctx.return_type is
                        Some(expected) ->
                            check(ctx, arg, expected)
                        None ->
                            # No expected return type - just synthesize
                            synth(ctx, arg)
                            |> Result.map_ok(\result -> result.context)
                None ->
                    # Return void/undefined
                    Ok(ctx)

        IfStatement(data) ->
            # Check condition
            synth(ctx, data.test)
            |> Result.try(\test_result ->
                # Check consequent
                check_statement(test_result.context, data.consequent)
                |> Result.try(\cons_ctx ->
                    when data.alternate is
                        Some(alt) ->
                            check_statement(cons_ctx, alt)
                        None ->
                            Ok(cons_ctx)
                )
            )

        BlockStatement(data) ->
            List.walk(data.body, Ok(ctx), \acc_result, stmt_node ->
                acc_result
                |> Result.try(\acc_ctx ->
                    check_statement(acc_ctx, stmt_node)
                )
            )

        _ ->
            # For other statements, just return context unchanged
            Ok(ctx)

# Synthesize type for an expression (public interface)
synth_expression : T.TypeStore, Ast.Node -> Result (T.TypeStore, T.TypeId) TypeError
synth_expression = \store, expr ->
    ctx = empty_context(store)
    synth(ctx, expr)
    |> Result.map_ok(\result ->
        (result.context.store, result.type_id)
    )

# Apply context updates (for testing/debugging)
apply_context : TypeContext, (TypeContext -> Result TypeContext TypeError) -> Result TypeContext TypeError
apply_context = \ctx, f -> f(ctx)
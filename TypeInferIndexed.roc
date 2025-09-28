module [
    InferenceResult,
    TypedNode,
    InferContext,
    infer_program,
    infer_expression,
    infer_statement,
    generalize,
    get_principal_type,
]

import Ast exposing [Node, BinaryOperator, UnaryOperator, AssignmentOperator, LogicalOperator]
import ComprehensiveTypeIndexed exposing [
    TypeStore,
    TypeId,
    TypeDef,
    RowId,
]
import TypeConstraintSolver exposing [
    Constraint,
    Substitution,
    SolverError,
    unify_types,
    solve_constraints,
    apply_substitution,
    is_subtype_of,
]
import Dict
import Set
import Option exposing [Option]
import Result exposing [Result]
import List
import Str
import Num

# Type environment mapping variable names to type schemes
TypeScheme : {
    forall: Set.Set U64,  # Set of universally quantified type variables
    type_id: TypeId,
}

TypeEnv : Dict.Dict Str TypeScheme

InferenceResult : {
    type_id: TypeId,
    substitution: Substitution,
    type_env: TypeEnv,
    store: TypeStore,
}

TypedNode : {
    node: Node,
    type_id: TypeId,
}

InferContext : {
    store: TypeStore,
    env: TypeEnv,
    next_var: U64,
    constraints: List Constraint,
}

# Main inference functions

infer_program : Node -> Result InferenceResult [InferenceError Str, SolverError SolverError]
infer_program = \ast ->
    initial_ctx = {
        store: build_initial_store {},
        env: build_initial_env {},
        next_var: 10000,  # Start with high number to avoid conflicts
        constraints: [],
    }

    when infer_node ast initial_ctx is
        Ok (type_id, final_ctx) ->
            # Solve accumulated constraints
            initial_solver_state =
                TypeConstraintSolver.empty_solver_state(final_ctx.store)
                |> TypeConstraintSolver.add_constraints(final_ctx.constraints)
            when TypeConstraintSolver.solve_constraints(initial_solver_state) is
                Ok substitution ->
                    # Apply substitution to get final type
                    final_type_id = apply_substitution_to_type_id final_ctx.store type_id substitution
                    final_env = apply_substitution_to_env final_ctx.store final_ctx.env substitution

                    Ok {
                        type_id: final_type_id,
                        substitution,
                        type_env: final_env,
                        store: final_ctx.store,
                    }
                Err solver_error ->
                    Err (SolverError solver_error)
        Err msg ->
            Err (InferenceError msg)

infer_expression : Node, TypeStore, TypeEnv -> Result TypeId [InferenceError Str, SolverError SolverError]
infer_expression = \expr, store, env ->
    ctx = {
        store,
        env,
        next_var: 10000,
        constraints: [],
    }

    when infer_node expr ctx is
        Ok (type_id, final_ctx) ->
            initial_solver_state =
                TypeConstraintSolver.empty_solver_state(final_ctx.store)
                |> TypeConstraintSolver.add_constraints(final_ctx.constraints)
            when TypeConstraintSolver.solve_constraints(initial_solver_state) is
                Ok substitution ->
                    final_type_id = apply_substitution_to_type_id(final_ctx.store, type_id, substitution)
                    Ok final_type_id
                Err solver_error ->
                    Err (SolverError solver_error)
        Err msg ->
            Err (InferenceError msg)

infer_statement : Node, TypeStore, TypeEnv -> Result (TypeId, TypeStore, TypeEnv) [InferenceError Str, SolverError SolverError]
infer_statement = \stmt, store, env ->
    ctx = {
        store,
        env,
        next_var: 10000,
        constraints: [],
    }

    when infer_node stmt ctx is
        Ok (type_id, final_ctx) ->
            initial_solver_state =
                TypeConstraintSolver.empty_solver_state(final_ctx.store)
                |> TypeConstraintSolver.add_constraints(final_ctx.constraints)
            subst_result = TypeConstraintSolver.solve_constraints(initial_solver_state)
            when subst_result is
                Ok substitution ->
                    final_type_id = apply_substitution_to_type_id final_ctx.store type_id substitution
                    final_env = apply_substitution_to_env final_ctx.store final_ctx.env substitution
                    Ok (final_type_id, final_ctx.store, final_env)
                Err solver_error ->
                    Err (SolverError solver_error)
        Err msg ->
            Err (InferenceError msg)

# Core inference algorithm

infer_node : Node, InferContext -> Result (TypeId, InferContext) Str
infer_node = \node, ctx ->
    when node is
        Program data ->
            infer_statement_list data.body ctx

        # Literals
        NumberLiteral data ->
            num_val = Str.to_f64 data.value |> Result.with_default 0.0
            (new_store, type_id) = ComprehensiveTypeIndexed.make_literal ctx.store (NumLit num_val)
            Ok (type_id, { ctx & store: new_store })

        StringLiteral data ->
            (new_store, type_id) = ComprehensiveTypeIndexed.make_literal ctx.store (StrLit data.value)
            Ok (type_id, { ctx & store: new_store })

        BooleanLiteral data ->
            (new_store, type_id) = ComprehensiveTypeIndexed.make_literal ctx.store (BoolLit data.value)
            Ok (type_id, { ctx & store: new_store })

        NullLiteral _ ->
            (new_store, type_id) = ComprehensiveTypeIndexed.make_null ctx.store
            Ok (type_id, { ctx & store: new_store })

        # Identifier lookup
        Identifier data ->
            # Check if this is the special 'undefined' identifier
            if data.name == "undefined" then
                (new_store, type_id) = ComprehensiveTypeIndexed.make_undefined ctx.store
                Ok (type_id, { ctx & store: new_store })
            else
                when Dict.get ctx.env data.name is
                    Ok scheme ->
                        (instantiated_type, new_ctx) = instantiate_scheme scheme ctx
                        Ok (instantiated_type, new_ctx)
                    Err _ ->
                        # Unknown identifier - create a type variable
                        (new_store, type_var) = ComprehensiveTypeIndexed.make_type_var ctx.store ctx.next_var
                        Ok (type_var, { ctx & store: new_store, next_var: ctx.next_var + 1 })

        # Array expressions
        ArrayExpression data ->
            when data.elements is
                [] ->
                    # Empty array - Array<never>
                    (store1, never_type) = ComprehensiveTypeIndexed.make_never ctx.store
                    (store2, array_type) = ComprehensiveTypeIndexed.make_array store1 never_type
                    Ok (array_type, { ctx & store: store2 })
                _ ->
                    # Infer types of all elements
                    result = List.walk data.elements (Ok ([], ctx)) \acc_result, expr ->
                        when acc_result is
                            Ok (types, curr_ctx) ->
                                when infer_node expr curr_ctx is
                                    Ok (elem_type, new_ctx) ->
                                        Ok (List.append types elem_type, new_ctx)
                                    Err e -> Err e
                            Err e -> Err e

                    when result is
                        Ok (elem_types, final_ctx) ->
                            # Create union of all element types
                            (union_store, elem_union) = if List.len elem_types == 1 then
                                when List.first elem_types is
                                    Ok t -> (final_ctx.store, t)
                                    Err _ -> ComprehensiveTypeIndexed.make_never final_ctx.store
                            else
                                ComprehensiveTypeIndexed.make_union final_ctx.store elem_types

                            (array_store, array_type) = ComprehensiveTypeIndexed.make_array union_store elem_union
                            Ok (array_type, { final_ctx & store: array_store })
                        Err e -> Err e

        # Object expressions with proper recursive type handling
        ObjectExpression { properties } ->
            # Create recursive object type
            infer_object_literal properties ctx

        # Function expressions
        FunctionExpression { id, params, body, async, generator, return_type, type_parameters } ->
            infer_function id params body async generator return_type type_parameters ctx

        ArrowFunctionExpression { params, body, async, generator, return_type, type_parameters } ->
            infer_function(None, params, body, async, generator, return_type, type_parameters, ctx)

        # Binary operations
        BinaryExpression { left, right, operator } ->
            infer_binary_op left right operator ctx

        # Unary operations
        UnaryExpression { argument, operator, prefix } ->
            infer_unary_op argument operator prefix ctx

        # Member access
        MemberExpression { object, property, computed } ->
            infer_member_access object property computed Bool.false ctx

        # Call expressions
        CallExpression { callee, arguments, type_args } ->
            infer_call callee arguments Bool.false type_args ctx

        # Assignment
        AssignmentExpression { left, right, operator } ->
            infer_assignment left right operator ctx

        # Update expressions (++, --)
        UpdateExpression { argument, operator, prefix } ->
            when infer_node argument ctx is
                Ok (arg_type, new_ctx) ->
                    # Ensure argument is number type
                    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive new_ctx.store "number"
                    constraint = {
                        kind: Equality(arg_type, num_type),
                        source: Err(NoSource),
                    }
                    Ok (num_type, { new_ctx &
                        store: store1,
                        constraints: List.append new_ctx.constraints constraint
                    })
                Err e -> Err e

        # Conditional expressions (ternary)
        ConditionalExpression { test, consequent, alternate } ->
            infer_conditional test consequent alternate ctx

        # This expression
        ThisExpression _ ->
            # Create a type variable for 'this'
            (new_store, this_var) = ComprehensiveTypeIndexed.make_type_var ctx.store ctx.next_var
            Ok (this_var, { ctx & store: new_store, next_var: ctx.next_var + 1 })

        # Sequence expressions
        SequenceExpression { expressions } ->
            when List.last expressions is
                Ok last_expr ->
                    # Type is the type of the last expression
                    infer_expression_list expressions ctx
                Err _ ->
                    (new_store, void_type) = ComprehensiveTypeIndexed.make_void ctx.store
                    Ok (void_type, { ctx & store: new_store })

        # Template literals
        TemplateLiteral { quasis, expressions } ->
            # Template literals are always strings
            (new_store, str_type) = ComprehensiveTypeIndexed.make_primitive ctx.store "string"
            # Still need to infer expression types for side effects
            result = List.walk expressions (Ok ctx) \acc_result, expr ->
                when acc_result is
                    Ok curr_ctx ->
                        when infer_node expr curr_ctx is
                            Ok (_, new_ctx) -> Ok new_ctx
                            Err e -> Err e
                    Err e -> Err e

            when result is
                Ok final_ctx -> Ok (str_type, { final_ctx & store: new_store })
                Err e -> Err e

        # New expressions
        NewExpression { callee, arguments, type_args } ->
            infer_new callee arguments type_args ctx

        # Await expressions
        AwaitExpression { argument } ->
            when infer_node argument ctx is
                Ok (promise_type, new_ctx) ->
                    # Extract the type parameter from Promise<T>
                    extract_promise_type promise_type new_ctx
                Err e -> Err e

        # Yield expressions
        YieldExpression { argument, delegate } ->
            when argument is
                Some arg ->
                    infer_node arg ctx
                None ->
                    (new_store, undef) = ComprehensiveTypeIndexed.make_undefined ctx.store
                    Ok (undef, { ctx & store: new_store })

        # Statements
        BlockStatement { body } ->
            infer_statement_list body ctx

        Directive { expression } ->
            infer_node expression ctx

        VariableDeclaration { declarations, kind } ->
            infer_variable_declarations declarations kind ctx

        FunctionDeclaration { id, params, body, async, generator, return_type, type_parameters } ->
            infer_function (Some id) params body async generator return_type type_parameters ctx

        ReturnStatement { argument } ->
            when argument is
                Some arg ->
                    infer_node arg ctx
                None ->
                    (new_store, undef) = ComprehensiveTypeIndexed.make_undefined ctx.store
                    Ok (undef, { ctx & store: new_store })

        IfStatement { test, consequent, alternate } ->
            when infer_node test ctx is
                Ok (_, test_ctx) ->
                    when infer_node consequent test_ctx is
                        Ok (_, cons_ctx) ->
                            when alternate is
                                Some alt ->
                                    infer_node alt cons_ctx
                                None ->
                                    (new_store, void_type) = ComprehensiveTypeIndexed.make_void cons_ctx.store
                                    Ok (void_type, { cons_ctx & store: new_store })
                        Err e -> Err e
                Err e -> Err e

        WhileStatement { test, body } ->
            when infer_node test ctx is
                Ok (_, test_ctx) ->
                    when infer_node body test_ctx is
                        Ok (_, body_ctx) ->
                            (new_store, void_type) = ComprehensiveTypeIndexed.make_void body_ctx.store
                            Ok (void_type, { body_ctx & store: new_store })
                        Err e -> Err e
                Err e -> Err e

        ForStatement { init, test, update, body } ->
            ctx1 = when init is
                Some i ->
                    when infer_node i ctx is
                        Ok (_, new_ctx) -> new_ctx
                        Err _ -> ctx
                None -> ctx

            ctx2 = when test is
                Some t ->
                    when infer_node t ctx1 is
                        Ok (_, new_ctx) -> new_ctx
                        Err _ -> ctx1
                None -> ctx1

            ctx3 = when update is
                Some u ->
                    when infer_node u ctx2 is
                        Ok (_, new_ctx) -> new_ctx
                        Err _ -> ctx2
                None -> ctx2

            when infer_node body ctx3 is
                Ok (_, body_ctx) ->
                    (new_store, void_type) = ComprehensiveTypeIndexed.make_void body_ctx.store
                    Ok (void_type, { body_ctx & store: new_store })
                Err e -> Err e

        # TypeScript specific
        TSTypeAnnotation { type_annotation } ->
            infer_node type_annotation ctx

        TSAsExpression { expression, type_annotation } ->
            # Type assertion - use the asserted type
            infer_node type_annotation ctx

        # TypeScript type keywords
        TSStringKeyword _ | TSNumberKeyword _ | TSBooleanKeyword _ |
        TSVoidKeyword _ | TSUndefinedKeyword _ | TSNullKeyword _ |
        TSAnyKeyword _ | TSUnknownKeyword _ | TSNeverKeyword _ |
        TSArrayType _ | TSUnionType _ | TSIntersectionType _ ->
            handle_ts_type_node node ctx

        _ ->
            # For unhandled nodes, return unknown type
            (new_store, unknown) = ComprehensiveTypeIndexed.make_unknown ctx.store
            Ok (unknown, { ctx & store: new_store })

# Helper function for object literal inference with recursive types and discriminants
infer_object_literal : List Node, InferContext -> Result (TypeId, InferContext) Str
infer_object_literal = \properties, ctx ->
    # First pass: collect all property names and identify potential discriminants
    analysis = List.walk properties { prop_vars: Dict.empty{}, discriminants: [] } \acc, prop ->
        when prop is
            Property { key, kind, value } ->
                key_name = extract_property_key key

                # Check if this is a potential discriminant (literal string value)
                is_discriminant = when value is
                    Some (StringLiteral data) -> Bool.true
                    _ -> Bool.false

                new_acc = { acc &
                    prop_vars: Dict.insert acc.prop_vars key_name ctx.next_var
                }

                if is_discriminant then
                    { new_acc & discriminants: List.append acc.discriminants key_name }
                else
                    new_acc
            _ -> acc

    # Create initial context with type variables for potential recursive references
    initial_ctx = { ctx & next_var: ctx.next_var + (Dict.len analysis.prop_vars |> Num.to_u64) }

    # Second pass: infer property types
    result = List.walk properties (Ok ([], initial_ctx)) \acc_result, prop ->
        when acc_result is
            Ok (fields, curr_ctx) ->
                when prop is
                    Property { key, kind, value } ->
                        key_name = extract_property_key key

                        when value is
                            Some val ->
                                when infer_node_with_recursion val curr_ctx analysis key_name is
                                    Ok (val_type, new_ctx) ->
                                        field = { name: key_name, type_id: val_type, optional: Bool.false, readonly: Bool.false }
                                        Ok (List.append fields field, new_ctx)
                                    Err e -> Err e
                            None ->
                                (new_store, undef) = ComprehensiveTypeIndexed.make_undefined curr_ctx.store
                                field = { name: key_name, type_id: undef, optional: Bool.true, readonly: Bool.false }
                                Ok (List.append fields field, { curr_ctx & store: new_store })

                    SpreadElement { argument } ->
                        # Handle spread properties
                        when infer_node argument curr_ctx is
                            Ok (spread_type, new_ctx) ->
                                # For now, we'll just track that there was a spread
                                # In a full implementation, we'd merge the spread type
                                Ok (fields, new_ctx)
                            Err e -> Err e

                    _ -> Ok (fields, curr_ctx)
            Err e -> Err e

    when result is
        Ok (fields, final_ctx) ->
            create_object_type_from_fields fields final_ctx
        Err e -> Err e

# Special helper for recursive type inference
infer_node_with_recursion : Node, InferContext, { prop_vars: Dict.Dict Str U64, discriminants: List Str }, Str -> Result (TypeId, InferContext) Str
infer_node_with_recursion = \node, ctx, analysis, current_key ->
    when node is
        Identifier { name } ->
            # Check if this refers to a property in the same object (recursive reference)
            when Dict.get analysis.prop_vars name is
                Ok var_id ->
                    (new_store, type_var) = ComprehensiveTypeIndexed.make_type_var ctx.store var_id
                    Ok (type_var, { ctx & store: new_store })
                Err _ ->
                    # Not a recursive reference, use normal inference
                    infer_node node ctx
        _ ->
            infer_node node ctx

# Create object type from fields
create_object_type_from_fields : List { name: Str, type_id: TypeId, optional: Bool, readonly: Bool }, InferContext -> Result (TypeId, InferContext) Str
create_object_type_from_fields = \fields, ctx ->
    # Build row type from fields
    result = List.walk fields (Ok (None, ctx.store)) \acc_result, field ->
        when acc_result is
            Ok (maybe_row, store) ->
                when maybe_row is
                    None ->
                        (store1, empty_row) = ComprehensiveTypeIndexed.make_empty_row store
                        (store2, new_row) = ComprehensiveTypeIndexed.make_row_extend store1 field.name field.type_id field.optional field.readonly empty_row
                        Ok (Some new_row, store2)
                    Some row ->
                        (new_store, new_row) = ComprehensiveTypeIndexed.make_row_extend store field.name field.type_id field.optional field.readonly row
                        Ok (Some new_row, new_store)
            Err e -> Err e

    when result is
        Ok (Some row_id, final_store) ->
            (object_store, object_type) = ComprehensiveTypeIndexed.make_object final_store row_id
            Ok (object_type, { ctx & store: object_store })
        Ok (None, final_store) ->
            # Empty object
            (store1, empty_row) = ComprehensiveTypeIndexed.make_empty_row final_store
            (store2, object_type) = ComprehensiveTypeIndexed.make_object store1 empty_row
            Ok (object_type, { ctx & store: store2 })
        Err e -> Err e

# Function inference
infer_function : Option Node, List Node, Node, Bool, Bool, Option Node, Option Node, InferContext -> Result (TypeId, InferContext) Str
infer_function = \id, params, body, is_async, is_generator, return_type, type_params, ctx ->
    # Create type variables for parameters
    param_result = List.walk params (Ok ([], [], ctx)) \acc_result, param ->
        when acc_result is
            Ok (param_types, param_names, curr_ctx) ->
                when param is
                    Identifier { name } ->
                        (new_store, param_var) = ComprehensiveTypeIndexed.make_type_var curr_ctx.store curr_ctx.next_var
                        Ok (
                            List.append param_types { name, param_type: param_var, optional: Bool.false },
                            List.append param_names name,
                            { curr_ctx & store: new_store, next_var: curr_ctx.next_var + 1 }
                        )
                    _ ->
                        # Handle patterns, rest parameters, etc.
                        (new_store, param_var) = ComprehensiveTypeIndexed.make_type_var curr_ctx.store curr_ctx.next_var
                        Ok (
                            List.append param_types { name: "_", param_type: param_var, optional: Bool.false },
                            List.append param_names "_",
                            { curr_ctx & store: new_store, next_var: curr_ctx.next_var + 1 }
                        )
            Err e -> Err e

    when param_result is
        Ok (param_types, param_names, param_ctx) ->
            # Add parameters to environment
            env_with_params = List.walk param_types param_ctx.env \env, param_type ->
                scheme = { forall: Set.empty {}, type_id: param_type.param_type }
                Dict.insert env param_type.name scheme

            # Infer body type
            body_ctx = { param_ctx & env: env_with_params }
            when infer_node body body_ctx is
                Ok (body_type, final_ctx) ->
                    # Handle return type annotation if present
                    return_type_id = when return_type is
                        Some ret_annotation ->
                            when infer_node ret_annotation final_ctx is
                                Ok (ret_type, _) -> ret_type
                                Err _ -> body_type
                        None -> body_type

                    # Create function type
                    (fn_store, fn_type) = ComprehensiveTypeIndexed.make_function(
                        final_ctx.store,
                        param_types,
                        return_type_id,
                        [], # TODO: Handle type parameters
                        is_async,
                        is_generator
                    )

                    # Add function to environment if it has a name
                    final_env = when id is
                        Some (Identifier { name }) ->
                            scheme = { forall: Set.empty {}, type_id: fn_type }
                            Dict.insert final_ctx.env name scheme
                        _ -> final_ctx.env

                    Ok (fn_type, { final_ctx & store: fn_store, env: final_env })
                Err e -> Err e
        Err e -> Err e

infer_logical_op : Node, Node, LogicalOperator, InferContext -> Result (TypeId, InferContext) Str
infer_logical_op = \left, right, operator, ctx ->
    when (infer_node left ctx, infer_node right ctx) is
        (Ok (left_type, left_ctx), Ok (right_type, right_ctx)) ->
            when operator is
                LogicalAnd | LogicalOr ->
                    # Logical operators - result is the type of one of the operands
                    # For simplicity, return a union type
                    (store1, union_type) = ComprehensiveTypeIndexed.make_union right_ctx.store [left_type, right_type]
                    Ok (union_type, { right_ctx & store: store1 })
        (Err e, _) -> Err e
        (_, Err e) -> Err e

# Binary operation inference
infer_binary_op : Node, Node, BinaryOperator, InferContext -> Result (TypeId, InferContext) Str
infer_binary_op = \left, right, operator, ctx ->
    when infer_node left ctx is
        Ok (left_type, left_ctx) ->
            when infer_node right left_ctx is
                Ok (right_type, right_ctx) ->
                    when operator is
                        Plus | Minus | Star | Slash | Percent ->
                            # Arithmetic operators - result is number
                            (store1, num_type) = ComprehensiveTypeIndexed.make_primitive right_ctx.store "number"
                            # Add constraints that operands should be numbers
                            constraint1 = { kind: Equality left_type num_type, source: Err NoSource }
                            constraint2 = { kind: Equality right_type num_type, source: Err NoSource }
                            Ok (num_type, { right_ctx &
                                store: store1,
                                constraints: List.concat right_ctx.constraints [constraint1, constraint2]
                            })

                        EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | In | Instanceof ->
                            # Comparison operators - result is boolean
                            (store1, bool_type) = ComprehensiveTypeIndexed.make_primitive right_ctx.store "boolean"
                            Ok (bool_type, { right_ctx & store: store1 })

                        LeftShift | RightShift | UnsignedRightShift | Ampersand | Pipe | Caret ->
                            # Bitwise operators - result is number
                            (store1, num_type) = ComprehensiveTypeIndexed.make_primitive right_ctx.store "number"
                            constraint1 = { kind: Equality left_type num_type, source: Err NoSource }
                            constraint2 = { kind: Equality right_type num_type, source: Err NoSource }
                            Ok (num_type, { right_ctx &
                                store: store1,
                                constraints: List.concat right_ctx.constraints [constraint1, constraint2]
                            })

                        NullishCoalesce ->
                            # ?? operator - left type without null/undefined, or right type
                            # For now, just return union
                            (store1, union_type) = ComprehensiveTypeIndexed.make_union right_ctx.store [left_type, right_type]
                            Ok (union_type, { right_ctx & store: store1 })
                Err e -> Err e
        Err e -> Err e

# Unary operation inference
infer_unary_op : Node, UnaryOperator, Bool, InferContext -> Result (TypeId, InferContext) Str
infer_unary_op = \argument, operator, prefix, ctx ->
    when infer_node argument ctx is
        Ok (arg_type, new_ctx) ->
            when operator is
                Bang | Delete ->
                    # These return boolean
                    (store1, bool_type) = ComprehensiveTypeIndexed.make_primitive new_ctx.store "boolean"
                    Ok (bool_type, { new_ctx & store: store1 })

                Plus | Minus | Tilde ->
                    # These return number
                    (store1, num_type) = ComprehensiveTypeIndexed.make_primitive new_ctx.store "number"
                    constraint = {
                        kind: Equality(arg_type, num_type),
                        source: Err(NoSource),
                    }
                    Ok (num_type, { new_ctx &
                        store: store1,
                        constraints: List.append new_ctx.constraints constraint
                    })

                Typeof ->
                    # typeof returns string
                    (store1, str_type) = ComprehensiveTypeIndexed.make_primitive new_ctx.store "string"
                    Ok (str_type, { new_ctx & store: store1 })

                Void ->
                    # void returns undefined
                    (store1, undef) = ComprehensiveTypeIndexed.make_undefined new_ctx.store
                    Ok (undef, { new_ctx & store: store1 })
        Err e -> Err e

# Member access inference
infer_member_access : Node, Node, Bool, Bool, InferContext -> Result (TypeId, InferContext) Str
infer_member_access = \object, property, computed, optional, ctx ->
    when infer_node object ctx is
        Ok (obj_type, obj_ctx) ->
            property_name = if computed then
                # Computed property access - need to evaluate
                when property is
                    StringLiteral { value } -> Some value
                    Identifier { name } -> Some name
                    _ -> None
            else
                # Direct property access
                when property is
                    Identifier { name } -> Some name
                    _ -> None

            when property_name is
                Some prop_name ->
                    # Create a type variable for the result
                    (store1, result_var) = ComprehensiveTypeIndexed.make_type_var obj_ctx.store obj_ctx.next_var
                    # For object field access, we need to handle it more carefully
                    # We'll generate a constraint that the object type has this field
                    # This is done by creating a row variable and constraints
                    (store2, row_var) = ComprehensiveTypeIndexed.make_row_var store1 obj_ctx.next_var
                    (store3, expected_obj) = ComprehensiveTypeIndexed.make_object store2 row_var
                    (store4, empty_row) = ComprehensiveTypeIndexed.make_empty_row store3
                    (store5, field_row) = ComprehensiveTypeIndexed.make_row_extend store4 prop_name result_var Bool.false Bool.false row_var
                    (store6, field_obj) = ComprehensiveTypeIndexed.make_object store5 field_row

                    # Constraint: obj_type must be subtype of object with this field
                    constraint = { kind: Subtype field_obj obj_type, source: Ok "field $(prop_name) access" }
                    Ok (result_var, { obj_ctx &
                        store: store6,  # Use the final store with all the types
                        next_var: obj_ctx.next_var + 2,  # We used two variables (result and row)
                        constraints: List.append obj_ctx.constraints constraint
                    })
                None ->
                    # Can't determine property name statically
                    (store1, unknown) = ComprehensiveTypeIndexed.make_unknown obj_ctx.store
                    Ok (unknown, { obj_ctx & store: store1 })
        Err e -> Err e

# Call expression inference
infer_call : Node, List Node, Bool, Option (List Node), InferContext -> Result (TypeId, InferContext) Str
infer_call = \callee, arguments, optional, type_args, ctx ->
    when infer_node callee ctx is
        Ok (fn_type, fn_ctx) ->
            # Infer argument types
            arg_result = List.walk arguments (Ok ([], fn_ctx)) \acc_result, arg ->
                when acc_result is
                    Ok (arg_types, curr_ctx) ->
                        when infer_node arg curr_ctx is
                            Ok (arg_type, new_ctx) ->
                                Ok (List.append arg_types arg_type, new_ctx)
                            Err e -> Err e
                    Err e -> Err e

            when arg_result is
                Ok (arg_types, args_ctx) ->
                    # Create type variable for result
                    (store1, result_var) = ComprehensiveTypeIndexed.make_type_var args_ctx.store args_ctx.next_var
                    # For function calls, we need to unify with a function type
                    # Create a function type with the argument types and result type
                    (store2, expected_fn_type) = ComprehensiveTypeIndexed.make_function store1 (List.map arg_types \t -> { name: "_", param_type: t, optional: Bool.false }) result_var [] Bool.false Bool.false
                    # Constraint: fn_type must equal this function type
                    constraint = { kind: Equality fn_type expected_fn_type, source: Ok "function call" }
                    Ok (result_var, { args_ctx &
                        store: store2,  # Use the store with the function type
                        next_var: args_ctx.next_var + 1,
                        constraints: List.append args_ctx.constraints constraint
                    })
                Err e -> Err e
        Err e -> Err e

# New expression inference
infer_new : Node, List Node, Option (List Node), InferContext -> Result (TypeId, InferContext) Str
infer_new = \callee, arguments, type_args, ctx ->
    when infer_node callee ctx is
        Ok (constructor_type, cons_ctx) ->
            # For 'new' expressions, we need to determine the instance type
            # This is complex in the general case, so we'll create a type variable
            (store1, instance_var) = ComprehensiveTypeIndexed.make_type_var cons_ctx.store cons_ctx.next_var
            Ok (instance_var, { cons_ctx & store: store1, next_var: cons_ctx.next_var + 1 })
        Err e -> Err e

# Assignment inference
infer_assignment : Node, Node, AssignmentOperator, InferContext -> Result (TypeId, InferContext) Str
infer_assignment = \left, right, operator, ctx ->
    when infer_node right ctx is
        Ok (right_type, right_ctx) ->
            when left is
                Identifier { name } ->
                    # Update environment with new binding
                    scheme = { forall: Set.empty {}, type_id: right_type }
                    new_env = Dict.insert right_ctx.env name scheme
                    Ok (right_type, { right_ctx & env: new_env })
                _ ->
                    # Complex assignment target
                    when infer_node left right_ctx is
                        Ok (left_type, left_ctx) ->
                            # Add constraint that types must be compatible
                            constraint = { kind: Subtype right_type left_type, source: Ok "assignment" }
                            Ok (right_type, { left_ctx &
                                constraints: List.append left_ctx.constraints constraint
                            })
                        Err e -> Err e
        Err e -> Err e

# Conditional expression inference
infer_conditional : Node, Node, Node, InferContext -> Result (TypeId, InferContext) Str
infer_conditional = \test, consequent, alternate, ctx ->
    when infer_node test ctx is
        Ok (_, test_ctx) ->
            when infer_node consequent test_ctx is
                Ok (cons_type, cons_ctx) ->
                    when infer_node alternate cons_ctx is
                        Ok (alt_type, alt_ctx) ->
                            # Result is union of consequent and alternate types
                            (store1, union_type) = ComprehensiveTypeIndexed.make_union alt_ctx.store [cons_type, alt_type]
                            Ok (union_type, { alt_ctx & store: store1 })
                        Err e -> Err e
                Err e -> Err e
        Err e -> Err e

# Variable declaration inference
infer_variable_declarations : List Node, Ast.VariableDeclarationKind, InferContext -> Result (TypeId, InferContext) Str
infer_variable_declarations = \declarations, kind, ctx ->
    result = List.walk declarations (Ok (None, ctx)) \acc_result, decl ->
        when acc_result is
            Ok (_, curr_ctx) ->
                when decl is
                    VariableDeclarator { id, init } ->
                        when id is
                            Identifier { name } ->
                                when init is
                                    Some init_expr ->
                                        when infer_node init_expr curr_ctx is
                                            Ok (init_type, new_ctx) ->
                                                scheme = { forall: Set.empty {}, type_id: init_type }
                                                new_env = Dict.insert new_ctx.env name scheme
                                                Ok (Some init_type, { new_ctx & env: new_env })
                                            Err e -> Err e
                                    None ->
                                        # No initializer - type is undefined
                                        (new_store, undef) = ComprehensiveTypeIndexed.make_undefined curr_ctx.store
                                        scheme = { forall: Set.empty {}, type_id: undef }
                                        new_env = Dict.insert curr_ctx.env name scheme
                                        Ok (Some undef, { curr_ctx & store: new_store, env: new_env })
                            _ ->
                                # Pattern destructuring - not yet handled
                                Ok (None, curr_ctx)
                    _ ->
                        Ok (None, curr_ctx)
            Err e -> Err e

    when result is
        Ok (Some type_id, final_ctx) ->
            Ok (type_id, final_ctx)
        Ok (None, final_ctx) ->
            (new_store, void_type) = ComprehensiveTypeIndexed.make_void final_ctx.store
            Ok (void_type, { final_ctx & store: new_store })
        Err e -> Err e

# Statement list inference
infer_statement_list : List Node, InferContext -> Result (TypeId, InferContext) Str
infer_statement_list = \statements, ctx ->
    when statements is
        [] ->
            (new_store, void_type) = ComprehensiveTypeIndexed.make_void ctx.store
            Ok (void_type, { ctx & store: new_store })
        _ ->
            result = List.walk statements (Ok (None, ctx)) \acc_result, stmt ->
                when acc_result is
                    Ok (_, curr_ctx) ->
                        when infer_node stmt curr_ctx is
                            Ok (stmt_type, new_ctx) ->
                                Ok (Some stmt_type, new_ctx)
                            Err e -> Err e
                    Err e -> Err e

            when result is
                Ok (Some last_type, final_ctx) ->
                    Ok (last_type, final_ctx)
                Ok (None, final_ctx) ->
                    (new_store, void_type) = ComprehensiveTypeIndexed.make_void final_ctx.store
                    Ok (void_type, { final_ctx & store: new_store })
                Err e -> Err e

# Expression list inference (for sequences)
infer_expression_list : List Node, InferContext -> Result (TypeId, InferContext) Str
infer_expression_list = \expressions, ctx ->
    when List.last expressions is
        Ok _ ->
            result = List.walk expressions (Ok (None, ctx)) \acc_result, expr ->
                when acc_result is
                    Ok (_, curr_ctx) ->
                        when infer_node expr curr_ctx is
                            Ok (expr_type, new_ctx) ->
                                Ok (Some expr_type, new_ctx)
                            Err e -> Err e
                    Err e -> Err e

            when result is
                Ok (Some last_type, final_ctx) ->
                    Ok (last_type, final_ctx)
                Ok (None, final_ctx) ->
                    (new_store, void_type) = ComprehensiveTypeIndexed.make_void final_ctx.store
                    Ok (void_type, { final_ctx & store: new_store })
                Err e -> Err e
        Err _ ->
            (new_store, void_type) = ComprehensiveTypeIndexed.make_void ctx.store
            Ok (void_type, { ctx & store: new_store })

# TypeScript type nodes are passed through infer_node
# This function handles TypeScript type keywords and constructs
handle_ts_type_node : Node, InferContext -> Result (TypeId, InferContext) Str
handle_ts_type_node = \node, ctx ->
    when node is
        TSStringKeyword _ ->
            (new_store, str_type) = ComprehensiveTypeIndexed.make_primitive ctx.store "string"
            Ok (str_type, { ctx & store: new_store })

        TSNumberKeyword _ ->
            (new_store, num_type) = ComprehensiveTypeIndexed.make_primitive ctx.store "number"
            Ok (num_type, { ctx & store: new_store })

        TSBooleanKeyword _ ->
            (new_store, bool_type) = ComprehensiveTypeIndexed.make_primitive ctx.store "boolean"
            Ok (bool_type, { ctx & store: new_store })

        TSVoidKeyword _ ->
            (new_store, void_type) = ComprehensiveTypeIndexed.make_void ctx.store
            Ok (void_type, { ctx & store: new_store })

        TSUndefinedKeyword _ ->
            (new_store, undef) = ComprehensiveTypeIndexed.make_undefined ctx.store
            Ok (undef, { ctx & store: new_store })

        TSNullKeyword _ ->
            (new_store, null) = ComprehensiveTypeIndexed.make_null ctx.store
            Ok (null, { ctx & store: new_store })

        TSAnyKeyword _ ->
            (new_store, any) = ComprehensiveTypeIndexed.make_any ctx.store
            Ok (any, { ctx & store: new_store })

        TSUnknownKeyword _ ->
            (new_store, unknown) = ComprehensiveTypeIndexed.make_unknown ctx.store
            Ok (unknown, { ctx & store: new_store })

        TSNeverKeyword _ ->
            (new_store, never) = ComprehensiveTypeIndexed.make_never ctx.store
            Ok (never, { ctx & store: new_store })

        TSArrayType({ elementType }) ->
            when infer_node(elementType, ctx) is
                Ok (elem_type, elem_ctx) ->
                    (new_store, array_type) = ComprehensiveTypeIndexed.make_array elem_ctx.store elem_type
                    Ok (array_type, { elem_ctx & store: new_store })
                Err e -> Err e

        TSUnionType { types } ->
            result = List.walk types (Ok ([], ctx)) \acc_result, t ->
                when acc_result is
                    Ok (union_types, curr_ctx) ->
                        when infer_node t curr_ctx is
                            Ok (t_type, new_ctx) ->
                                Ok (List.append union_types t_type, new_ctx)
                            Err e -> Err e
                    Err e -> Err e

            when result is
                Ok (union_types, final_ctx) ->
                    (new_store, union) = ComprehensiveTypeIndexed.make_union final_ctx.store union_types
                    Ok (union, { final_ctx & store: new_store })
                Err e -> Err e

        TSIntersectionType { types } ->
            result = List.walk types (Ok ([], ctx)) \acc_result, t ->
                when acc_result is
                    Ok (inter_types, curr_ctx) ->
                        when infer_node t curr_ctx is
                            Ok (t_type, new_ctx) ->
                                Ok (List.append inter_types t_type, new_ctx)
                            Err e -> Err e
                    Err e -> Err e

            when result is
                Ok (inter_types, final_ctx) ->
                    (new_store, inter) = ComprehensiveTypeIndexed.make_intersection final_ctx.store inter_types
                    Ok (inter, { final_ctx & store: new_store })
                Err e -> Err e

        _ ->
            # Not a TS type node
            Err "Not a TypeScript type node"

# Extract promise type helper
extract_promise_type : TypeId, InferContext -> Result (TypeId, InferContext) Str
extract_promise_type = \promise_type, ctx ->
    # For now, just return a type variable
    # In a full implementation, we'd extract the T from Promise<T>
    (new_store, result_var) = ComprehensiveTypeIndexed.make_type_var ctx.store ctx.next_var
    Ok (result_var, { ctx & store: new_store, next_var: ctx.next_var + 1 })

# Property key extraction helper
extract_property_key : Node -> Str
extract_property_key = \key ->
    when key is
        Identifier { name } -> name
        StringLiteral { value } -> value
        NumberLiteral { value } -> value
        _ -> "_unknown_"

# Scheme instantiation
instantiate_scheme : TypeScheme, InferContext -> (TypeId, InferContext)
instantiate_scheme = \scheme, ctx ->
    if Set.is_empty scheme.forall then
        (scheme.type_id, ctx)
    else
        # For now, just return the type as-is
        # In a full implementation, we'd instantiate with fresh type variables
        (scheme.type_id, ctx)

# Generalization
generalize : TypeId, TypeStore, TypeEnv -> TypeScheme
generalize = \type_id, store, env ->
    # Find free type variables in the type
    type_free_vars = find_free_vars_in_type type_id store

    # Find free type variables in the environment
    env_free_vars = Set.empty {}
    env_free_final = Dict.walk env env_free_vars \acc, _, scheme ->
        scheme_free = find_free_vars_in_type scheme.type_id store
        Set.union acc (Set.difference scheme_free scheme.forall)

    # Variables to generalize are those free in type but not in environment
    generalizable = Set.difference type_free_vars env_free_final

    { forall: generalizable, type_id }

# Find free variables in a type
find_free_vars_in_type : TypeId, TypeStore -> Set.Set U64
find_free_vars_in_type = \type_id, store ->
    when ComprehensiveTypeIndexed.get_type store type_id is
        Ok type_def ->
            when type_def is
                TypeVariable var -> Set.single var
                TFunction { params, ret } ->
                    param_vars = List.walk params (Set.empty {}) \acc, param ->
                        Set.union acc (find_free_vars_in_type param.param_type store)
                    Set.union param_vars (find_free_vars_in_type ret store)
                TArray elem -> find_free_vars_in_type elem store
                TObject row -> find_free_vars_in_row row store
                TUnion types ->
                    List.walk types (Set.empty {}) \acc, t ->
                        Set.union acc (find_free_vars_in_type t store)
                TIntersection types ->
                    List.walk types (Set.empty {}) \acc, t ->
                        Set.union acc (find_free_vars_in_type t store)
                _ -> Set.empty {}
        Err _ -> Set.empty {}

# Find free variables in a row type
find_free_vars_in_row : RowId, TypeStore -> Set.Set U64
find_free_vars_in_row = \row_id, store ->
    when ComprehensiveTypeIndexed.get_row store row_id is
        Ok row_def ->
            when row_def is
                REmpty -> Set.empty {}
                RExtend { field_type, rest } ->
                    field_vars = find_free_vars_in_type field_type store
                    rest_vars = find_free_vars_in_row rest store
                    Set.union field_vars rest_vars
                RVar var -> Set.single var
                RIndex { key_type, value_type, rest } ->
                    key_vars = find_free_vars_in_type key_type store
                    value_vars = find_free_vars_in_type value_type store
                    rest_vars = find_free_vars_in_row rest store
                    Set.union key_vars (Set.union value_vars rest_vars)
        Err _ -> Set.empty {}

# Apply substitution to type ID
apply_substitution_to_type_id : TypeStore, TypeId, Substitution -> TypeId
apply_substitution_to_type_id = \store, type_id, subst ->
    # Apply the substitution using TypeConstraintSolver
    TypeConstraintSolver.apply_substitution store type_id subst

# Apply substitution to environment
apply_substitution_to_env : TypeStore, TypeEnv, Substitution -> TypeEnv
apply_substitution_to_env = \store, env, subst ->
    Dict.map env \_, scheme ->
        { scheme & type_id: apply_substitution_to_type_id store scheme.type_id subst }

# Get principal type (simplified API)
get_principal_type : Node -> Result TypeId [InferenceError Str, SolverError SolverError]
get_principal_type = \node ->
    when infer_program node is
        Ok result -> Ok result.type_id
        Err e -> Err e

# Build initial type store with built-in types
build_initial_store : {} -> TypeStore
build_initial_store = \{} ->
    ComprehensiveTypeIndexed.empty_store

# Build initial environment with built-in bindings
build_initial_env : {} -> TypeEnv
build_initial_env = \{} ->
    # Start with empty environment
    # Built-ins will be added to the store as we encounter them
    # This keeps the initial environment lightweight
    Dict.empty {}
    |> add_builtin_binding "console" (create_console_type {})
    |> add_builtin_binding "Math" (create_math_type {})
    |> add_builtin_binding "undefined" (create_undefined_type {})
    |> add_builtin_binding "null" (create_null_type {})
    |> add_builtin_binding "true" (create_true_type {})
    |> add_builtin_binding "false" (create_false_type {})

# Helper to add built-in bindings
add_builtin_binding : TypeEnv, Str, TypeScheme -> TypeEnv
add_builtin_binding = \env, name, scheme ->
    Dict.insert env name scheme

# Create console type
create_console_type : {} -> TypeScheme
create_console_type = \{} ->
    # For now, return a simple scheme
    # In a full implementation, this would create the proper console object type
    { forall: Set.empty {}, type_id: 0 }

# Create Math type
create_math_type : {} -> TypeScheme
create_math_type = \{} ->
    { forall: Set.empty {}, type_id: 0 }

# Create literal types
create_undefined_type : {} -> TypeScheme
create_undefined_type = \{} ->
    { forall: Set.empty {}, type_id: 0 }

create_null_type : {} -> TypeScheme
create_null_type = \{} ->
    { forall: Set.empty {}, type_id: 0 }

create_true_type : {} -> TypeScheme
create_true_type = \{} ->
    { forall: Set.empty {}, type_id: 0 }

create_false_type : {} -> TypeScheme
create_false_type = \{} ->
    { forall: Set.empty {}, type_id: 0 }

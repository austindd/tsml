module [
    transform_to_cps,
]

import Ast exposing [
    Node,
    ProgramKind,
    VariableDeclarationKind,
    MethodKind,
    PropertyKind,
    AssignmentOperator,
    LogicalOperator,
    BinaryOperator,
    UnaryOperator,
    UpdateOperator,
]
import Option exposing [Option]

transform_to_cps : Node -> Node
transform_to_cps = |ast|
    ctx = initial_context
    (transformed, _) = transform_node(ast, ctx, make_final_continuation())
    transformed

CpsContext : {
    next_cont_id : U64,
    current_cont : Str,
}

initial_context : CpsContext
initial_context = {
    next_cont_id: 0,
    current_cont: "k",
}

fresh_cont_name : CpsContext -> (Str, CpsContext)
fresh_cont_name = |ctx|
    name = Str.concat("k", Num.to_str(ctx.next_cont_id))
    new_ctx = { ctx & next_cont_id: ctx.next_cont_id + 1 }
    (name, new_ctx)

fresh_var_name : CpsContext, Str -> (Str, CpsContext)
fresh_var_name = |ctx, prefix|
    name = Str.concat(prefix, Num.to_str(ctx.next_cont_id))
    new_ctx = { ctx & next_cont_id: ctx.next_cont_id + 1 }
    (name, new_ctx)

make_final_continuation : {} -> Node
make_final_continuation = |{}|
    FunctionExpression({
        id: None,
        params: [Identifier({ name: "result" })],
        body: BlockStatement({
            body: [ReturnStatement({ argument: Some(Identifier({ name: "result" })) })],
        }),
        generator: Bool.false,
        async: Bool.false,
        typeParameters: None,
    })

make_continuation : Str, { body : Node } -> Node
make_continuation = |param_name, data|
    FunctionExpression({
        id: None,
        params: [Identifier({ name: param_name })],
        body: data.body,
        generator: Bool.false,
        async: Bool.false,
        typeParameters: None,
    })

apply_continuation : Node, Node -> Node
apply_continuation = |cont, value|
    CallExpression({
        callee: cont,
        arguments: [value],
    })

transform_node : Node, CpsContext, Node -> (Node, CpsContext)
transform_node = |node, ctx, cont|
    when node is
        Program(data) ->
            (transformed_body, new_ctx) = transform_statements(data.body, ctx)
            wrapped_body = wrap_in_continuation_call(transformed_body, cont)
            (Program({ data & body: [wrapped_body] }), new_ctx)

        Identifier(_) ->
            (apply_continuation(cont, node), ctx)

        BooleanLiteral(_) ->
            (apply_continuation(cont, node), ctx)

        NumberLiteral(_) ->
            (apply_continuation(cont, node), ctx)

        StringLiteral(_) ->
            (apply_continuation(cont, node), ctx)

        NullLiteral(_) ->
            (apply_continuation(cont, node), ctx)

        UndefinedLiteral(_) ->
            (apply_continuation(cont, node), ctx)

        BinaryExpression(data) ->
            transform_binary_expression(data, ctx, cont)

        UnaryExpression(data) ->
            transform_unary_expression(data, ctx, cont)

        AssignmentExpression(data) ->
            transform_assignment_expression(data, ctx, cont)

        CallExpression(data) ->
            transform_call_expression(data, ctx, cont)

        MemberExpression(data) ->
            transform_member_expression(data, ctx, cont)

        ConditionalExpression(data) ->
            transform_conditional_expression(data, ctx, cont)

        ArrayExpression(data) ->
            transform_array_expression(data, ctx, cont)

        ObjectExpression(data) ->
            transform_object_expression(data, ctx, cont)

        FunctionExpression(data) ->
            transform_function_expression(data, ctx, cont)

        FunctionDeclaration(data) ->
            transform_function_declaration(data, ctx, cont)

        ArrowFunctionExpression(data) ->
            transform_arrow_function(data, ctx, cont)

        ReturnStatement(data) ->
            transform_return_statement(data, ctx, cont)

        IfStatement(data) ->
            transform_if_statement(data, ctx, cont)

        BlockStatement(data) ->
            (transformed_body, new_ctx) = transform_statements(data.body, ctx)
            (apply_continuation(cont, BlockStatement({ data & body: transformed_body })), new_ctx)

        VariableDeclaration(data) ->
            transform_variable_declaration(data, ctx, cont)

        WhileStatement(data) ->
            transform_while_statement(data, ctx, cont)

        ForStatement(data) ->
            transform_for_statement(data, ctx, cont)

        TryStatement(data) ->
            transform_try_statement(data, ctx, cont)

        ThrowStatement(data) ->
            transform_throw_statement(data, ctx, cont)

        _ ->
            (apply_continuation(cont, node), ctx)

transform_binary_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_binary_expression = |data, ctx, cont|
    (left_var, ctx1) = fresh_var_name(ctx, "left")
    (right_var, ctx2) = fresh_var_name(ctx1, "right")

    right_cont = make_continuation(right_var, {
        body: apply_continuation(cont, BinaryExpression({
            data &
            left: Identifier({ name: left_var }),
            right: Identifier({ name: right_var }),
        })),
    })

    left_cont = make_continuation(left_var, {
        body: transform_node(data.right, ctx2, right_cont) |> .0,
    })

    transform_node(data.left, ctx2, left_cont)

transform_unary_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_unary_expression = |data, ctx, cont|
    (arg_var, new_ctx) = fresh_var_name(ctx, "arg")

    arg_cont = make_continuation(arg_var, {
        body: apply_continuation(cont, UnaryExpression({
            data & argument: Identifier({ name: arg_var })
        })),
    })

    transform_node(data.argument, new_ctx, arg_cont)

transform_assignment_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_assignment_expression = |data, ctx, cont|
    (val_var, new_ctx) = fresh_var_name(ctx, "val")

    val_cont = make_continuation(val_var, {
        body: apply_continuation(cont, AssignmentExpression({
            data &
            right: Identifier({ name: val_var })
        })),
    })

    transform_node(data.right, new_ctx, val_cont)

transform_call_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_call_expression = |data, ctx, cont|
    (callee_var, ctx1) = fresh_var_name(ctx, "fn")
    (args_vars, ctx2) = transform_arguments(data.arguments, ctx1)

    arg_nodes = List.map(args_vars, |var| Identifier({ name: var }))
    call_node = CallExpression({
        data &
        callee: Identifier({ name: callee_var }),
        arguments: List.concat(arg_nodes, [cont]),
    })

    callee_cont = make_continuation(callee_var, {
        body: transform_arguments_to_cps(data.arguments, args_vars, ctx2, call_node) |> .0,
    })

    transform_node(data.callee, ctx2, callee_cont)

transform_member_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_member_expression = |data, ctx, cont|
    (obj_var, ctx1) = fresh_var_name(ctx, "obj")

    if data.computed then
        (prop_var, ctx2) = fresh_var_name(ctx1, "prop")
        prop_cont = make_continuation(prop_var, {
            body: apply_continuation(cont, MemberExpression({
                data &
                object: Identifier({ name: obj_var }),
                property: Identifier({ name: prop_var }),
            })),
        })
        obj_cont = make_continuation(obj_var, {
            body: transform_node(data.property, ctx2, prop_cont) |> .0,
        })
        transform_node(data.object, ctx2, obj_cont)
    else
        obj_cont = make_continuation(obj_var, {
            body: apply_continuation(cont, MemberExpression({
                data &
                object: Identifier({ name: obj_var }),
            })),
        })
        transform_node(data.object, ctx1, obj_cont)

transform_conditional_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_conditional_expression = |data, ctx, cont|
    (test_var, ctx1) = fresh_var_name(ctx, "test")
    (cont_name, ctx2) = fresh_cont_name(ctx1)

    cont_binding = VariableDeclaration({
        declarations: [VariableDeclarator({
            id: Identifier({ name: cont_name }),
            init: Some(cont),
            typeAnnotation: None,
        })],
        kind: Const,
    })

    (consequent_cps, ctx3) = transform_node(data.consequent, ctx2, Identifier({ name: cont_name }))
    (alternate_cps, ctx4) = transform_node(data.alternate, ctx3, Identifier({ name: cont_name }))

    test_cont = make_continuation(test_var, {
        body: BlockStatement({
            body: [
                cont_binding,
                IfStatement({
                    test: Identifier({ name: test_var }),
                    consequent: consequent_cps,
                    alternate: Some(alternate_cps),
                }),
            ],
        }),
    })

    transform_node(data.test, ctx4, test_cont)

transform_array_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_array_expression = |data, ctx, cont|
    (element_vars, ctx1) = transform_arguments(data.elements, ctx)

    array_node = ArrayExpression({
        data &
        elements: List.map(element_vars, |var| Identifier({ name: var })),
    })

    transform_arguments_to_cps(data.elements, element_vars, ctx1, apply_continuation(cont, array_node))

transform_object_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_object_expression = |data, ctx, cont|
    transform_object_properties(data.properties, ctx, [], |props, final_ctx|
        (apply_continuation(cont, ObjectExpression({ data & properties: props })), final_ctx)
    )

transform_function_expression : _, CpsContext, Node -> (Node, CpsContext)
transform_function_expression = |data, ctx, cont|
    cps_params = List.append(data.params, [Identifier({ name: "k" })])
    (cps_body, _) = transform_node(data.body, { ctx & current_cont: "k" }, Identifier({ name: "k" }))

    cps_function = FunctionExpression({
        data &
        params: cps_params,
        body: cps_body,
    })

    (apply_continuation(cont, cps_function), ctx)

transform_function_declaration : _, CpsContext, Node -> (Node, CpsContext)
transform_function_declaration = |data, ctx, cont|
    cps_params = List.append(data.params, [Identifier({ name: "k" })])
    (cps_body, _) = transform_node(data.body, { ctx & current_cont: "k" }, Identifier({ name: "k" }))

    cps_function = FunctionDeclaration({
        data &
        params: cps_params,
        body: cps_body,
    })

    (BlockStatement({
        body: [cps_function, apply_continuation(cont, UndefinedLiteral({}))],
    }), ctx)

transform_arrow_function : _, CpsContext, Node -> (Node, CpsContext)
transform_arrow_function = |data, ctx, cont|
    cps_params = List.append(data.params, [Identifier({ name: "k" })])
    (cps_body, _) = transform_node(data.body, { ctx & current_cont: "k" }, Identifier({ name: "k" }))

    cps_function = ArrowFunctionExpression({
        data &
        params: cps_params,
        body: cps_body,
    })

    (apply_continuation(cont, cps_function), ctx)

transform_return_statement : _, CpsContext, Node -> (Node, CpsContext)
transform_return_statement = |data, ctx, _cont|
    when data.argument is
        Some(arg) ->
            transform_node(arg, ctx, Identifier({ name: ctx.current_cont }))
        None ->
            (apply_continuation(Identifier({ name: ctx.current_cont }), UndefinedLiteral({})), ctx)

transform_if_statement : _, CpsContext, Node -> (Node, CpsContext)
transform_if_statement = |data, ctx, cont|
    (test_var, ctx1) = fresh_var_name(ctx, "test")
    (cont_name, ctx2) = fresh_cont_name(ctx1)

    cont_binding = VariableDeclaration({
        declarations: [VariableDeclarator({
            id: Identifier({ name: cont_name }),
            init: Some(cont),
            typeAnnotation: None,
        })],
        kind: Const,
    })

    (consequent_cps, ctx3) = transform_node(data.consequent, ctx2, Identifier({ name: cont_name }))

    (alternate_cps, ctx4) = when data.alternate is
        Some(alt) -> transform_node(alt, ctx3, Identifier({ name: cont_name }))
        None -> (apply_continuation(Identifier({ name: cont_name }), UndefinedLiteral({})), ctx3)

    test_cont = make_continuation(test_var, {
        body: BlockStatement({
            body: [
                cont_binding,
                IfStatement({
                    test: Identifier({ name: test_var }),
                    consequent: consequent_cps,
                    alternate: Some(alternate_cps),
                }),
            ],
        }),
    })

    transform_node(data.test, ctx4, test_cont)

transform_variable_declaration : _, CpsContext, Node -> (Node, CpsContext)
transform_variable_declaration = |data, ctx, cont|
    transform_variable_declarators(data.declarations, data.kind, ctx, [], |decls, final_ctx|
        (BlockStatement({
            body: [
                VariableDeclaration({ data & declarations: decls }),
                apply_continuation(cont, UndefinedLiteral({})),
            ],
        }), final_ctx)
    )

transform_while_statement : _, CpsContext, Node -> (Node, CpsContext)
transform_while_statement = |data, ctx, cont|
    (loop_name, ctx1) = fresh_var_name(ctx, "loop")
    (test_var, ctx2) = fresh_var_name(ctx1, "test")

    (body_cps, ctx3) = transform_node(data.body, ctx2, CallExpression({
        callee: Identifier({ name: loop_name }),
        arguments: [],
    }))

    loop_body = BlockStatement({
        body: [
            transform_node(data.test, ctx3, make_continuation(test_var, {
                body: IfStatement({
                    test: Identifier({ name: test_var }),
                    consequent: body_cps,
                    alternate: Some(apply_continuation(cont, UndefinedLiteral({}))),
                }),
            })) |> .0,
        ],
    })

    loop_function = FunctionDeclaration({
        id: Identifier({ name: loop_name }),
        params: [],
        body: loop_body,
        generator: Bool.false,
        async: Bool.false,
        typeParameters: None,
    })

    (BlockStatement({
        body: [
            loop_function,
            CallExpression({
                callee: Identifier({ name: loop_name }),
                arguments: [],
            }),
        ],
    }), ctx3)

transform_for_statement : _, CpsContext, Node -> (Node, CpsContext)
transform_for_statement = |data, ctx, cont|
    (loop_name, ctx1) = fresh_var_name(ctx, "loop")

    loop_body_statements = []

    loop_body_statements = when data.test is
        Some(test) ->
            (test_var, ctx2) = fresh_var_name(ctx1, "test")
            (body_cps, ctx3) = transform_node(data.body, ctx2, Identifier({ name: "next" }))

            next_cont = when data.update is
                Some(update) ->
                    transform_node(update, ctx3, CallExpression({
                        callee: Identifier({ name: loop_name }),
                        arguments: [],
                    })) |> .0
                None ->
                    CallExpression({
                        callee: Identifier({ name: loop_name }),
                        arguments: [],
                    })

            List.append(loop_body_statements,
                transform_node(test, ctx3, make_continuation(test_var, {
                    body: IfStatement({
                        test: Identifier({ name: test_var }),
                        consequent: BlockStatement({
                            body: [
                                VariableDeclaration({
                                    declarations: [VariableDeclarator({
                                        id: Identifier({ name: "next" }),
                                        init: Some(make_continuation("_", { body: next_cont })),
                                        typeAnnotation: None,
                                    })],
                                    kind: Const,
                                }),
                                body_cps,
                            ],
                        }),
                        alternate: Some(apply_continuation(cont, UndefinedLiteral({}))),
                    }),
                })) |> .0
            )
        None ->
            loop_body_statements

    loop_function = FunctionDeclaration({
        id: Identifier({ name: loop_name }),
        params: [],
        body: BlockStatement({ body: loop_body_statements }),
        generator: Bool.false,
        async: Bool.false,
        typeParameters: None,
    })

    init_statements = when data.init is
        Some(init) ->
            (init_cps, _ctx_init) = transform_node(init, ctx1, CallExpression({
                callee: Identifier({ name: loop_name }),
                arguments: [],
            }))
            [loop_function, init_cps]
        None ->
            [loop_function, CallExpression({
                callee: Identifier({ name: loop_name }),
                arguments: [],
            })]

    (BlockStatement({ body: init_statements }), ctx1)

transform_try_statement : _, CpsContext, Node -> (Node, CpsContext)
transform_try_statement = |data, ctx, cont|
    (error_cont_name, ctx1) = fresh_cont_name(ctx)
    (finally_cont_name, ctx2) = fresh_cont_name(ctx1)

    finally_cont = when data.finalizer is
        Some(finalizer) ->
            (finalizer_cps, _) = transform_node(finalizer, ctx2, cont)
            make_continuation("_", { body: finalizer_cps })
        None ->
            cont

    error_cont = when data.handler is
        Some(CatchClause(catch_data)) ->
            (catch_body_cps, _) = transform_node(catch_data.body, ctx2, finally_cont)
            make_continuation(
                when catch_data.param is
                    Some(Identifier(id_data)) -> id_data.name
                    _ -> "error",
                { body: catch_body_cps }
            )
        _ ->
            make_continuation("error", {
                body: apply_continuation(finally_cont, ThrowStatement({ argument: Identifier({ name: "error" }) }))
            })

    (try_body_cps, ctx3) = transform_node(data.block, { ctx2 & current_cont: finally_cont_name }, finally_cont)

    (BlockStatement({
        body: [
            VariableDeclaration({
                declarations: [
                    VariableDeclarator({
                        id: Identifier({ name: error_cont_name }),
                        init: Some(error_cont),
                        typeAnnotation: None,
                    }),
                    VariableDeclarator({
                        id: Identifier({ name: finally_cont_name }),
                        init: Some(finally_cont),
                        typeAnnotation: None,
                    }),
                ],
                kind: Const,
            }),
            TryStatement({
                block: try_body_cps,
                handler: Some(CatchClause({
                    param: Some(Identifier({ name: "e" })),
                    body: CallExpression({
                        callee: Identifier({ name: error_cont_name }),
                        arguments: [Identifier({ name: "e" })],
                    }),
                })),
                finalizer: None,
            }),
        ],
    }), ctx3)

transform_throw_statement : _, CpsContext, Node -> (Node, CpsContext)
transform_throw_statement = |data, ctx, _cont|
    transform_node(data.argument, ctx, make_continuation("error", {
        body: ThrowStatement({ argument: Identifier({ name: "error" }) }),
    }))

transform_statements : List Node, CpsContext -> (List Node, CpsContext)
transform_statements = |statements, ctx|
    when statements is
        [] -> ([], ctx)
        [stmt] ->
            transform_node(stmt, ctx, make_final_continuation())
        [stmt, .. as rest] ->
            (rest_cps, ctx1) = transform_statements(rest, ctx)
            rest_cont = make_continuation("_", {
                body: BlockStatement({ body: rest_cps }),
            })
            transform_node(stmt, ctx1, rest_cont)

transform_arguments : List Node, CpsContext -> (List Str, CpsContext)
transform_arguments = |args, ctx|
    List.walk(args, ([], ctx), |state, _arg|
        (vars, current_ctx) = state
        (var_name, new_ctx) = fresh_var_name(current_ctx, "arg")
        (List.append(vars, var_name), new_ctx)
    )

transform_arguments_to_cps : List Node, List Str, CpsContext, Node -> (Node, CpsContext)
transform_arguments_to_cps = |args, vars, ctx, final_expr|
    when (args, vars) is
        ([], []) -> (final_expr, ctx)
        ([arg, .. as rest_args], [var, .. as rest_vars]) ->
            arg_cont = make_continuation(var, {
                body: transform_arguments_to_cps(rest_args, rest_vars, ctx, final_expr) |> .0,
            })
            transform_node(arg, ctx, arg_cont)
        _ -> (final_expr, ctx)

transform_object_properties : List Node, CpsContext, List Node, (List Node, CpsContext -> (Node, CpsContext)) -> (Node, CpsContext)
transform_object_properties = |props, ctx, acc, final_cont|
    when props is
        [] -> final_cont(List.reverse(acc), ctx)
        [Property(prop_data), .. as rest] ->
            (val_var, ctx1) = fresh_var_name(ctx, "propVal")
            val_cont = make_continuation(val_var, {
                body: transform_object_properties(rest, ctx1,
                    List.append(acc, Property({ prop_data & value: Identifier({ name: val_var }) })),
                    final_cont
                ) |> .0,
            })
            transform_node(prop_data.value, ctx1, val_cont)
        [prop, .. as rest] ->
            transform_object_properties(rest, ctx, List.append(acc, prop), final_cont)

transform_variable_declarators : List Node, _, CpsContext, List Node, (List Node, CpsContext -> (Node, CpsContext)) -> (Node, CpsContext)
transform_variable_declarators = |decls, _kind, ctx, acc, final_cont|
    when decls is
        [] -> final_cont(List.reverse(acc), ctx)
        [VariableDeclarator(decl_data), .. as rest] ->
            when decl_data.init is
                Some(init_expr) ->
                    (init_var, ctx1) = fresh_var_name(ctx, "init")
                    init_cont = make_continuation(init_var, {
                        body: transform_variable_declarators(rest, _kind, ctx1,
                            List.append(acc, VariableDeclarator({
                                decl_data & init: Some(Identifier({ name: init_var }))
                            })),
                            final_cont
                        ) |> .0,
                    })
                    transform_node(init_expr, ctx1, init_cont)
                None ->
                    transform_variable_declarators(rest, _kind, ctx, List.append(acc, VariableDeclarator(decl_data)), final_cont)
        [decl, .. as rest] ->
            transform_variable_declarators(rest, _kind, ctx, List.append(acc, decl), final_cont)

wrap_in_continuation_call : List Node, Node -> Node
wrap_in_continuation_call = |statements, cont|
    CallExpression({
        callee: FunctionExpression({
            id: None,
            params: [],
            body: BlockStatement({ body: statements }),
            generator: Bool.false,
            async: Bool.false,
            typeParameters: None,
        }),
        arguments: [cont],
    })
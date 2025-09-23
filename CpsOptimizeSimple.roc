module [
    optimize_cps,
]

import Ast exposing [Node]
import Option exposing [Option]

# Simple CPS optimizer that performs beta reduction
optimize_cps : Node -> Node
optimize_cps = |ast|
    beta_reduce(ast)

# Beta reduction: inline simple continuation calls
# ((function(x) { return body })(value)) => body[x := value]
beta_reduce : Node -> Node
beta_reduce = |node|
    when node is
        CallExpression(call_data) ->
            when call_data.callee is
                FunctionExpression(fn_data) ->
                    # Check if this is a simple beta-reducible form
                    if is_simple_function(fn_data) && List.len(call_data.arguments) == 1 then
                        # Get the parameter name and argument
                        when (fn_data.params, call_data.arguments) is
                            ([Identifier(param)], [arg]) ->
                                # Perform substitution
                                substitute_in_body(fn_data.body, param.name, arg)
                            _ ->
                                # Recursively optimize children
                                new_callee = beta_reduce(call_data.callee)
                                new_args = List.map(call_data.arguments, beta_reduce)
                                CallExpression({ call_data & callee: new_callee, arguments: new_args })
                    else
                        # Recursively optimize children
                        new_callee = beta_reduce(call_data.callee)
                        new_args = List.map(call_data.arguments, beta_reduce)
                        CallExpression({ call_data & callee: new_callee, arguments: new_args })

                _ ->
                    # Not immediately callable, recurse on children
                    new_callee = beta_reduce(call_data.callee)
                    new_args = List.map(call_data.arguments, beta_reduce)
                    CallExpression({ call_data & callee: new_callee, arguments: new_args })

        BlockStatement(data) ->
            new_body = List.map(data.body, beta_reduce)
            BlockStatement({ data & body: new_body })

        Program(data) ->
            new_body = List.map(data.body, beta_reduce)
            Program({ data & body: new_body })

        FunctionExpression(data) ->
            new_body = beta_reduce(data.body)
            FunctionExpression({ data & body: new_body })

        FunctionDeclaration(data) ->
            new_body = beta_reduce(data.body)
            FunctionDeclaration({ data & body: new_body })

        ArrowFunctionExpression(data) ->
            new_body = beta_reduce(data.body)
            ArrowFunctionExpression({ data & body: new_body })

        IfStatement(data) ->
            new_test = beta_reduce(data.test)
            new_cons = beta_reduce(data.consequent)
            new_alt = Option.map(data.alternate, beta_reduce)
            IfStatement({ data & test: new_test, consequent: new_cons, alternate: new_alt })

        VariableDeclaration(data) ->
            new_decls = List.map(data.declarations, beta_reduce)
            VariableDeclaration({ data & declarations: new_decls })

        VariableDeclarator(data) ->
            new_init = Option.map(data.init, beta_reduce)
            VariableDeclarator({ data & init: new_init })

        BinaryExpression(data) ->
            new_left = beta_reduce(data.left)
            new_right = beta_reduce(data.right)
            BinaryExpression({ data & left: new_left, right: new_right })

        UnaryExpression(data) ->
            new_arg = beta_reduce(data.argument)
            UnaryExpression({ data & argument: new_arg })

        MemberExpression(data) ->
            new_object = beta_reduce(data.object)
            new_property = beta_reduce(data.property)
            MemberExpression({ data & object: new_object, property: new_property })

        ConditionalExpression(data) ->
            new_test = beta_reduce(data.test)
            new_cons = beta_reduce(data.consequent)
            new_alt = beta_reduce(data.alternate)
            ConditionalExpression({ data & test: new_test, consequent: new_cons, alternate: new_alt })

        ArrayExpression(data) ->
            new_elements = List.map(data.elements, beta_reduce)
            ArrayExpression({ data & elements: new_elements })

        ObjectExpression(data) ->
            new_props = List.map(data.properties, beta_reduce)
            ObjectExpression({ data & properties: new_props })

        Property(data) ->
            new_key = beta_reduce(data.key)
            new_value = beta_reduce(data.value)
            Property({ data & key: new_key, value: new_value })

        ReturnStatement(data) ->
            new_arg = Option.map(data.argument, beta_reduce)
            ReturnStatement({ data & argument: new_arg })

        AssignmentExpression(data) ->
            new_left = beta_reduce(data.left)
            new_right = beta_reduce(data.right)
            AssignmentExpression({ data & left: new_left, right: new_right })

        _ ->
            # For other node types, return as-is
            node

is_simple_function : _ -> Bool
is_simple_function = |fn_data|
    # A simple function has a single return statement
    when fn_data.body is
        BlockStatement(block_data) ->
            when block_data.body is
                [ReturnStatement(_)] -> Bool.true
                _ -> Bool.false
        _ -> Bool.false

substitute_in_body : Node, Str, Node -> Node
substitute_in_body = |body, param_name, arg|
    when body is
        BlockStatement(data) ->
            when data.body is
                [ReturnStatement(ret_data)] ->
                    when ret_data.argument is
                        Some(expr) ->
                            substitute_in_node(expr, param_name, arg)
                        None ->
                            UndefinedLiteral({})
                _ ->
                    body
        _ ->
            body

substitute_in_node : Node, Str, Node -> Node
substitute_in_node = |node, param_name, replacement|
    when node is
        Identifier(data) ->
            if data.name == param_name then
                replacement
            else
                node

        BinaryExpression(data) ->
            BinaryExpression({
                data &
                left: substitute_in_node(data.left, param_name, replacement),
                right: substitute_in_node(data.right, param_name, replacement)
            })

        UnaryExpression(data) ->
            UnaryExpression({
                data &
                argument: substitute_in_node(data.argument, param_name, replacement)
            })

        MemberExpression(data) ->
            MemberExpression({
                data &
                object: substitute_in_node(data.object, param_name, replacement),
                property: if data.computed then
                    substitute_in_node(data.property, param_name, replacement)
                else
                    data.property
            })

        CallExpression(data) ->
            CallExpression({
                data &
                callee: substitute_in_node(data.callee, param_name, replacement),
                arguments: List.map(data.arguments, |arg| substitute_in_node(arg, param_name, replacement))
            })

        ConditionalExpression(data) ->
            ConditionalExpression({
                data &
                test: substitute_in_node(data.test, param_name, replacement),
                consequent: substitute_in_node(data.consequent, param_name, replacement),
                alternate: substitute_in_node(data.alternate, param_name, replacement)
            })

        ArrayExpression(data) ->
            ArrayExpression({
                data &
                elements: List.map(data.elements, |elem| substitute_in_node(elem, param_name, replacement))
            })

        ObjectExpression(data) ->
            ObjectExpression({
                data &
                properties: List.map(data.properties, |prop| substitute_in_node(prop, param_name, replacement))
            })

        Property(data) ->
            Property({
                data &
                key: substitute_in_node(data.key, param_name, replacement),
                value: substitute_in_node(data.value, param_name, replacement)
            })

        _ ->
            # For literals and other nodes, return as-is
            node
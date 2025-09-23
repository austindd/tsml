module [
    optimize_cps,
    OptimizationStats,
]

import Ast exposing [Node]
import Option exposing [Option]

OptimizationStats : {
    beta_reductions : U64,
    dead_eliminations : U64,
    constant_folds : U64,
    tail_call_optimizations : U64,
}

initial_stats : OptimizationStats
initial_stats = {
    beta_reductions: 0,
    dead_eliminations: 0,
    constant_folds: 0,
    tail_call_optimizations: 0,
}

optimize_cps : Node -> (Node, OptimizationStats)
optimize_cps = |ast|
    # Apply multiple optimization passes until fixpoint
    optimize_until_stable(ast, initial_stats, 10)

optimize_until_stable : Node, OptimizationStats, U64 -> (Node, OptimizationStats)
optimize_until_stable = |ast, stats, max_iterations|
    if max_iterations == 0 then
        (ast, stats)
    else
        (optimized, new_stats) = apply_optimization_pass(ast, stats)
        if nodes_equal(ast, optimized) then
            (optimized, new_stats)
        else
            optimize_until_stable(optimized, new_stats, max_iterations - 1)

apply_optimization_pass : Node, OptimizationStats -> (Node, OptimizationStats)
apply_optimization_pass = |ast, stats|
    # Apply optimizations in sequence
    (ast1, stats1) = beta_reduce(ast, stats)
    (ast2, stats2) = eliminate_dead_continuations(ast1, stats1)
    (ast3, stats3) = fold_constants(ast2, stats2)
    (ast4, stats4) = optimize_tail_calls(ast3, stats3)
    (ast4, stats4)

# Beta reduction: inline simple continuation calls
# ((function(x) { return body })(value)) => body[x := value]
beta_reduce : Node, OptimizationStats -> (Node, OptimizationStats)
beta_reduce = |node, stats|
    when node is
        CallExpression(call_data) ->
            when call_data.callee is
                FunctionExpression(fn_data) ->
                    # Check if this is a simple beta-reducible form
                    if is_simple_function(fn_data) && List.len(call_data.arguments) == List.len(fn_data.params) then
                        # Perform substitution
                        substitutions = build_substitution_map(fn_data.params, call_data.arguments)
                        reduced_body = substitute_in_node(fn_data.body, substitutions)

                        # If body is a BlockStatement with single return, unwrap it
                        unwrapped = unwrap_single_return(reduced_body)

                        (unwrapped, { stats & beta_reductions: stats.beta_reductions + 1 })
                    else
                        # Recursively optimize children
                        (new_callee, stats1) = beta_reduce(call_data.callee, stats)
                        (new_args, stats2) = beta_reduce_list(call_data.arguments, stats1)
                        (CallExpression({ call_data & callee: new_callee, arguments: new_args }), stats2)

                ArrowFunctionExpression(fn_data) ->
                    # Similar handling for arrow functions
                    if is_simple_arrow_function(fn_data) && List.len(call_data.arguments) == List.len(fn_data.params) then
                        substitutions = build_substitution_map(fn_data.params, call_data.arguments)
                        reduced_body = substitute_in_node(fn_data.body, substitutions)
                        unwrapped = unwrap_single_return(reduced_body)
                        (unwrapped, { stats & beta_reductions: stats.beta_reductions + 1 })
                    else
                        (new_callee, stats1) = beta_reduce(call_data.callee, stats)
                        (new_args, stats2) = beta_reduce_list(call_data.arguments, stats1)
                        (CallExpression({ call_data & callee: new_callee, arguments: new_args }), stats2)

                _ ->
                    # Not immediately callable, recurse on children
                    (new_callee, stats1) = beta_reduce(call_data.callee, stats)
                    (new_args, stats2) = beta_reduce_list(call_data.arguments, stats1)
                    (CallExpression({ call_data & callee: new_callee, arguments: new_args }), stats2)

        BlockStatement(data) ->
            (new_body, new_stats) = beta_reduce_list(data.body, stats)
            (BlockStatement({ data & body: new_body }), new_stats)

        Program(data) ->
            (new_body, new_stats) = beta_reduce_list(data.body, stats)
            (Program({ data & body: new_body }), new_stats)

        FunctionExpression(data) ->
            (new_body, new_stats) = beta_reduce(data.body, stats)
            (FunctionExpression({ data & body: new_body }), new_stats)

        FunctionDeclaration(data) ->
            (new_body, new_stats) = beta_reduce(data.body, stats)
            (FunctionDeclaration({ data & body: new_body }), new_stats)

        ArrowFunctionExpression(data) ->
            (new_body, new_stats) = beta_reduce(data.body, stats)
            (ArrowFunctionExpression({ data & body: new_body }), new_stats)

        IfStatement(data) ->
            (new_test, stats1) = beta_reduce(data.test, stats)
            (new_cons, stats2) = beta_reduce(data.consequent, stats1)
            (new_alt, stats3) = when data.alternate is
                Some(alt) ->
                    (opt_alt, s) = beta_reduce(alt, stats2)
                    (Some(opt_alt), s)
                None -> (None, stats2)
            (IfStatement({ data & test: new_test, consequent: new_cons, alternate: new_alt }), stats3)

        VariableDeclaration(data) ->
            (new_decls, new_stats) = beta_reduce_list(data.declarations, stats)
            (VariableDeclaration({ data & declarations: new_decls }), new_stats)

        VariableDeclarator(data) ->
            (new_init, new_stats) = when data.init is
                Some(init) ->
                    (opt_init, s) = beta_reduce(init, stats)
                    (Some(opt_init), s)
                None -> (None, stats)
            (VariableDeclarator({ data & init: new_init }), new_stats)

        BinaryExpression(data) ->
            (new_left, stats1) = beta_reduce(data.left, stats)
            (new_right, stats2) = beta_reduce(data.right, stats1)
            (BinaryExpression({ data & left: new_left, right: new_right }), stats2)

        UnaryExpression(data) ->
            (new_arg, new_stats) = beta_reduce(data.argument, stats)
            (UnaryExpression({ data & argument: new_arg }), new_stats)

        MemberExpression(data) ->
            (new_object, stats1) = beta_reduce(data.object, stats)
            (new_property, stats2) = beta_reduce(data.property, stats1)
            (MemberExpression({ data & object: new_object, property: new_property }), stats2)

        ConditionalExpression(data) ->
            (new_test, stats1) = beta_reduce(data.test, stats)
            (new_cons, stats2) = beta_reduce(data.consequent, stats1)
            (new_alt, stats3) = beta_reduce(data.alternate, stats2)
            (ConditionalExpression({ data & test: new_test, consequent: new_cons, alternate: new_alt }), stats3)

        ArrayExpression(data) ->
            (new_elements, new_stats) = beta_reduce_list(data.elements, stats)
            (ArrayExpression({ data & elements: new_elements }), new_stats)

        ObjectExpression(data) ->
            (new_props, new_stats) = beta_reduce_list(data.properties, stats)
            (ObjectExpression({ data & properties: new_props }), new_stats)

        Property(data) ->
            (new_key, stats1) = beta_reduce(data.key, stats)
            (new_value, stats2) = beta_reduce(data.value, stats1)
            (Property({ data & key: new_key, value: new_value }), stats2)

        ReturnStatement(data) ->
            (new_arg, new_stats) = when data.argument is
                Some(arg) ->
                    (opt_arg, s) = beta_reduce(arg, stats)
                    (Some(opt_arg), s)
                None -> (None, stats)
            (ReturnStatement({ data & argument: new_arg }), new_stats)

        AssignmentExpression(data) ->
            (new_left, stats1) = beta_reduce(data.left, stats)
            (new_right, stats2) = beta_reduce(data.right, stats1)
            (AssignmentExpression({ data & left: new_left, right: new_right }), stats2)

        _ ->
            # For other node types, return as-is
            (node, stats)

beta_reduce_list : List Node, OptimizationStats -> (List Node, OptimizationStats)
beta_reduce_list = |nodes, stats|
    List.walk(nodes, ([], stats), |state, node|
        (acc, current_stats) = state
        (optimized, new_stats) = beta_reduce(node, current_stats)
        (List.append(acc, optimized), new_stats)
    )

is_simple_function : _ -> Bool
is_simple_function = |fn_data|
    # A simple function has no side effects and just returns a value
    when fn_data.body is
        BlockStatement(block_data) ->
            when block_data.body is
                [ReturnStatement(_)] -> Bool.true
                _ -> Bool.false
        _ -> Bool.false

is_simple_arrow_function : _ -> Bool
is_simple_arrow_function = |fn_data|
    # Arrow functions with expression bodies are always simple
    when fn_data.body is
        BlockStatement(block_data) ->
            when block_data.body is
                [ReturnStatement(_)] -> Bool.true
                _ -> Bool.false
        _ -> Bool.true  # Expression body

unwrap_single_return : Node -> Node
unwrap_single_return = |node|
    when node is
        BlockStatement(data) ->
            when data.body is
                [ReturnStatement(ret_data)] ->
                    when ret_data.argument is
                        Some(arg) -> arg
                        None -> UndefinedLiteral({})
                _ -> node
        _ -> node

build_substitution_map : List Node, List Node -> List (Str, Node)
build_substitution_map = |params, args|
    List.map2(params, args, |param, arg|
        when param is
            Identifier(id_data) -> (id_data.name, arg)
            _ -> ("", arg)  # Should not happen in well-formed CPS
    )

substitute_in_node : Node, List (Str, Node) -> Node
substitute_in_node = |node, substitutions|
    when node is
        Identifier(data) ->
            # Look up in substitution map
            when find_substitution(data.name, substitutions) is
                Some(replacement) -> replacement
                None -> node

        BlockStatement(data) ->
            BlockStatement({ data & body: List.map(data.body, |n| substitute_in_node(n, substitutions)) })

        ReturnStatement(data) ->
            ReturnStatement({ data & argument: Option.map(data.argument, |n| substitute_in_node(n, substitutions)) })

        CallExpression(data) ->
            CallExpression({
                data &
                callee: substitute_in_node(data.callee, substitutions),
                arguments: List.map(data.arguments, |n| substitute_in_node(n, substitutions))
            })

        BinaryExpression(data) ->
            BinaryExpression({
                data &
                left: substitute_in_node(data.left, substitutions),
                right: substitute_in_node(data.right, substitutions)
            })

        UnaryExpression(data) ->
            UnaryExpression({ data & argument: substitute_in_node(data.argument, substitutions) })

        MemberExpression(data) ->
            MemberExpression({
                data &
                object: substitute_in_node(data.object, substitutions),
                property: substitute_in_node(data.property, substitutions)
            })

        ConditionalExpression(data) ->
            ConditionalExpression({
                data &
                test: substitute_in_node(data.test, substitutions),
                consequent: substitute_in_node(data.consequent, substitutions),
                alternate: substitute_in_node(data.alternate, substitutions)
            })

        ArrayExpression(data) ->
            ArrayExpression({ data & elements: List.map(data.elements, |n| substitute_in_node(n, substitutions)) })

        ObjectExpression(data) ->
            ObjectExpression({ data & properties: List.map(data.properties, |n| substitute_in_node(n, substitutions)) })

        Property(data) ->
            Property({
                data &
                key: substitute_in_node(data.key, substitutions),
                value: substitute_in_node(data.value, substitutions)
            })

        IfStatement(data) ->
            IfStatement({
                data &
                test: substitute_in_node(data.test, substitutions),
                consequent: substitute_in_node(data.consequent, substitutions),
                alternate: Option.map(data.alternate, |n| substitute_in_node(n, substitutions))
            })

        AssignmentExpression(data) ->
            AssignmentExpression({
                data &
                left: substitute_in_node(data.left, substitutions),
                right: substitute_in_node(data.right, substitutions)
            })

        _ ->
            # For literals and other simple nodes, return as-is
            node

find_substitution : Str, List (Str, Node) -> Option Node
find_substitution = |name, substitutions|
    when substitutions is
        [] -> None
        [(sub_name, sub_node), .. as rest] ->
            if sub_name == name then
                Some(sub_node)
            else
                find_substitution(name, rest)

# Dead continuation elimination: remove unused continuations
eliminate_dead_continuations : Node, OptimizationStats -> (Node, OptimizationStats)
eliminate_dead_continuations = |node, stats|
    # First collect all used identifiers
    used_ids = collect_used_identifiers(node, [])

    # Then eliminate unused bindings
    eliminate_unused_bindings(node, used_ids, stats)

collect_used_identifiers : Node, List Str -> List Str
collect_used_identifiers = |node, used|
    when node is
        Identifier(data) ->
            if List.contains(used, data.name) then
                used
            else
                List.append(used, data.name)

        CallExpression(data) ->
            used1 = collect_used_identifiers(data.callee, used)
            List.walk(data.arguments, used1, |acc, arg| collect_used_identifiers(arg, acc))

        BlockStatement(data) ->
            List.walk(data.body, used, |acc, stmt| collect_used_identifiers(stmt, acc))

        FunctionExpression(data) ->
            collect_used_identifiers(data.body, used)

        FunctionDeclaration(data) ->
            # Add function name to used if it exists
            used1 = when data.id is
                Identifier(id_data) -> List.append(used, id_data.name)
                _ -> used
            collect_used_identifiers(data.body, used1)

        BinaryExpression(data) ->
            used1 = collect_used_identifiers(data.left, used)
            collect_used_identifiers(data.right, used1)

        UnaryExpression(data) ->
            collect_used_identifiers(data.argument, used)

        MemberExpression(data) ->
            used1 = collect_used_identifiers(data.object, used)
            if data.computed then
                collect_used_identifiers(data.property, used1)
            else
                used1

        ConditionalExpression(data) ->
            used1 = collect_used_identifiers(data.test, used)
            used2 = collect_used_identifiers(data.consequent, used1)
            collect_used_identifiers(data.alternate, used2)

        IfStatement(data) ->
            used1 = collect_used_identifiers(data.test, used)
            used2 = collect_used_identifiers(data.consequent, used1)
            when data.alternate is
                Some(alt) -> collect_used_identifiers(alt, used2)
                None -> used2

        VariableDeclarator(data) ->
            when data.init is
                Some(init) -> collect_used_identifiers(init, used)
                None -> used

        VariableDeclaration(data) ->
            List.walk(data.declarations, used, |acc, decl| collect_used_identifiers(decl, acc))

        ReturnStatement(data) ->
            when data.argument is
                Some(arg) -> collect_used_identifiers(arg, used)
                None -> used

        AssignmentExpression(data) ->
            used1 = collect_used_identifiers(data.left, used)
            collect_used_identifiers(data.right, used1)

        ArrayExpression(data) ->
            List.walk(data.elements, used, |acc, elem| collect_used_identifiers(elem, acc))

        ObjectExpression(data) ->
            List.walk(data.properties, used, |acc, prop| collect_used_identifiers(prop, acc))

        Property(data) ->
            used1 = collect_used_identifiers(data.key, used)
            collect_used_identifiers(data.value, used1)

        _ ->
            used

eliminate_unused_bindings : Node, List Str, OptimizationStats -> (Node, OptimizationStats)
eliminate_unused_bindings = |node, used_ids, stats|
    when node is
        BlockStatement(data) ->
            (filtered_body, new_stats) = filter_unused_statements(data.body, used_ids, stats)
            (BlockStatement({ data & body: filtered_body }), new_stats)

        Program(data) ->
            (filtered_body, new_stats) = filter_unused_statements(data.body, used_ids, stats)
            (Program({ data & body: filtered_body }), new_stats)

        _ ->
            (node, stats)

filter_unused_statements : List Node, List Str, OptimizationStats -> (List Node, OptimizationStats)
filter_unused_statements = |statements, used_ids, stats|
    List.walk(statements, ([], stats), |state, stmt|
        (acc, current_stats) = state
        when stmt is
            VariableDeclaration(decl_data) ->
                # Check if any declarator is used
                is_used = List.any(decl_data.declarations, |decl|
                    when decl is
                        VariableDeclarator(v_data) ->
                            when v_data.id is
                                Identifier(id_data) -> List.contains(used_ids, id_data.name)
                                _ -> Bool.true
                        _ -> Bool.true
                )

                if is_used then
                    (List.append(acc, stmt), current_stats)
                else
                    (acc, { current_stats & dead_eliminations: current_stats.dead_eliminations + 1 })

            FunctionDeclaration(fn_data) ->
                # Check if function is used
                is_used = when fn_data.id is
                    Identifier(id_data) -> List.contains(used_ids, id_data.name)
                    _ -> Bool.true

                if is_used then
                    (List.append(acc, stmt), current_stats)
                else
                    (acc, { current_stats & dead_eliminations: current_stats.dead_eliminations + 1 })

            _ ->
                # Keep other statements
                (List.append(acc, stmt), current_stats)
    )

# Constant folding: evaluate constant expressions at compile time
fold_constants : Node, OptimizationStats -> (Node, OptimizationStats)
fold_constants = |node, stats|
    when node is
        BinaryExpression(data) ->
            (left_folded, stats1) = fold_constants(data.left, stats)
            (right_folded, stats2) = fold_constants(data.right, stats1)

            # Try to fold if both operands are constants
            when (left_folded, right_folded) is
                (NumberLiteral(left_data), NumberLiteral(right_data)) ->
                    when evaluate_binary_op(left_data.value, data.operator, right_data.value) is
                        Some(result) ->
                            (result, { stats2 & constant_folds: stats2.constant_folds + 1 })
                        None ->
                            (BinaryExpression({ data & left: left_folded, right: right_folded }), stats2)

                (StringLiteral(left_data), StringLiteral(right_data)) ->
                    when data.operator is
                        Plus ->
                            combined = Str.concat(left_data.value, right_data.value)
                            (StringLiteral({ value: combined }), { stats2 & constant_folds: stats2.constant_folds + 1 })
                        _ ->
                            (BinaryExpression({ data & left: left_folded, right: right_folded }), stats2)

                _ ->
                    (BinaryExpression({ data & left: left_folded, right: right_folded }), stats2)

        UnaryExpression(data) ->
            (arg_folded, new_stats) = fold_constants(data.argument, stats)

            # Try to fold unary operations on constants
            when arg_folded is
                NumberLiteral(num_data) ->
                    when evaluate_unary_op(data.operator, num_data.value) is
                        Some(result) ->
                            (result, { new_stats & constant_folds: new_stats.constant_folds + 1 })
                        None ->
                            (UnaryExpression({ data & argument: arg_folded }), new_stats)

                BooleanLiteral(bool_data) ->
                    when data.operator is
                        Bang ->
                            (BooleanLiteral({ value: !bool_data.value }), { new_stats & constant_folds: new_stats.constant_folds + 1 })
                        _ ->
                            (UnaryExpression({ data & argument: arg_folded }), new_stats)

                _ ->
                    (UnaryExpression({ data & argument: arg_folded }), new_stats)

        ConditionalExpression(data) ->
            (test_folded, stats1) = fold_constants(data.test, stats)

            # If test is constant, select branch at compile time
            when test_folded is
                BooleanLiteral(bool_data) ->
                    if bool_data.value then
                        fold_constants(data.consequent, { stats1 & constant_folds: stats1.constant_folds + 1 })
                    else
                        fold_constants(data.alternate, { stats1 & constant_folds: stats1.constant_folds + 1 })

                _ ->
                    (cons_folded, stats2) = fold_constants(data.consequent, stats1)
                    (alt_folded, stats3) = fold_constants(data.alternate, stats2)
                    (ConditionalExpression({ data & test: test_folded, consequent: cons_folded, alternate: alt_folded }), stats3)

        IfStatement(data) ->
            (test_folded, stats1) = fold_constants(data.test, stats)

            # If test is constant, eliminate dead branch
            when test_folded is
                BooleanLiteral(bool_data) ->
                    if bool_data.value then
                        fold_constants(data.consequent, { stats1 & constant_folds: stats1.constant_folds + 1 })
                    else
                        when data.alternate is
                            Some(alt) -> fold_constants(alt, { stats1 & constant_folds: stats1.constant_folds + 1 })
                            None -> (EmptyStatement({}), { stats1 & constant_folds: stats1.constant_folds + 1 })

                _ ->
                    (cons_folded, stats2) = fold_constants(data.consequent, stats1)
                    (alt_folded, stats3) = when data.alternate is
                        Some(alt) ->
                            (folded_alt, s) = fold_constants(alt, stats2)
                            (Some(folded_alt), s)
                        None -> (None, stats2)
                    (IfStatement({ data & test: test_folded, consequent: cons_folded, alternate: alt_folded }), stats3)

        _ ->
            # For other nodes, recurse on children
            fold_constants_generic(node, stats)

fold_constants_generic : Node, OptimizationStats -> (Node, OptimizationStats)
fold_constants_generic = |node, stats|
    # Generic recursive constant folding for other node types
    when node is
        BlockStatement(data) ->
            (new_body, new_stats) = fold_constants_list(data.body, stats)
            (BlockStatement({ data & body: new_body }), new_stats)

        Program(data) ->
            (new_body, new_stats) = fold_constants_list(data.body, stats)
            (Program({ data & body: new_body }), new_stats)

        CallExpression(data) ->
            (new_callee, stats1) = fold_constants(data.callee, stats)
            (new_args, stats2) = fold_constants_list(data.arguments, stats1)
            (CallExpression({ data & callee: new_callee, arguments: new_args }), stats2)

        _ ->
            (node, stats)

fold_constants_list : List Node, OptimizationStats -> (List Node, OptimizationStats)
fold_constants_list = |nodes, stats|
    List.walk(nodes, ([], stats), |state, node|
        (acc, current_stats) = state
        (folded, new_stats) = fold_constants(node, current_stats)
        (List.append(acc, folded), new_stats)
    )

evaluate_binary_op : Str, Ast.BinaryOperator, Str -> Option Node
evaluate_binary_op = |left_str, operator, right_str|
    # Try to parse and evaluate numeric operations
    when (Str.to_f64(left_str), Str.to_f64(right_str)) is
        (Ok(left_num), Ok(right_num)) ->
            when operator is
                Plus -> Some(NumberLiteral({ value: Num.to_str(left_num + right_num) }))
                Minus -> Some(NumberLiteral({ value: Num.to_str(left_num - right_num) }))
                Times -> Some(NumberLiteral({ value: Num.to_str(left_num * right_num) }))
                Divide ->
                    # Can't compare floats directly for equality in Roc
                    # Just perform the division and let JavaScript handle divide by zero
                    Some(NumberLiteral({ value: Num.to_str(left_num / right_num) }))
                LessThan -> Some(BooleanLiteral({ value: left_num < right_num }))
                LessThanOrEqual -> Some(BooleanLiteral({ value: left_num <= right_num }))
                GreaterThan -> Some(BooleanLiteral({ value: left_num > right_num }))
                GreaterThanOrEqual -> Some(BooleanLiteral({ value: left_num >= right_num }))
                Equal -> Some(BooleanLiteral({ value: left_num == right_num }))
                NotEqual -> Some(BooleanLiteral({ value: left_num != right_num }))
                _ -> None
        _ -> None

evaluate_unary_op : Ast.UnaryOperator, Str -> Option Node
evaluate_unary_op = |operator, value_str|
    when Str.to_f64(value_str) is
        Ok(num) ->
            when operator is
                Minus -> Some(NumberLiteral({ value: Num.to_str(-num) }))
                Plus -> Some(NumberLiteral({ value: value_str }))
                _ -> None
        Err(_) -> None

# Tail call optimization: convert tail-recursive continuations to loops
optimize_tail_calls : Node, OptimizationStats -> (Node, OptimizationStats)
optimize_tail_calls = |node, stats|
    # For now, just return the node unchanged
    # This is a placeholder for future tail call optimization
    (node, stats)

# Helper function to check if two nodes are structurally equal
nodes_equal : Node, Node -> Bool
nodes_equal = |node1, node2|
    when (node1, node2) is
        (Identifier(data1), Identifier(data2)) ->
            data1.name == data2.name

        (NumberLiteral(data1), NumberLiteral(data2)) ->
            data1.value == data2.value

        (StringLiteral(data1), StringLiteral(data2)) ->
            data1.value == data2.value

        (BooleanLiteral(data1), BooleanLiteral(data2)) ->
            data1.value == data2.value

        (NullLiteral(_), NullLiteral(_)) ->
            Bool.true

        (UndefinedLiteral(_), UndefinedLiteral(_)) ->
            Bool.true

        (BlockStatement(data1), BlockStatement(data2)) ->
            nodes_list_equal(data1.body, data2.body)

        (CallExpression(data1), CallExpression(data2)) ->
            nodes_equal(data1.callee, data2.callee) &&
            nodes_list_equal(data1.arguments, data2.arguments)

        _ ->
            # For complex comparisons, assume different
            Bool.false

nodes_list_equal : List Node, List Node -> Bool
nodes_list_equal = |list1, list2|
    if List.len(list1) != List.len(list2) then
        Bool.false
    else
        List.map2(list1, list2, nodes_equal)
        |> List.all(|x| x)
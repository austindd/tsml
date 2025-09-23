module [
    check,
    TypeResult,
]

import Ast
import SimpleComprehensiveType as Type
import Option exposing [Option]

TypeResult : {
    node_type : Type.Type,
    errors : List { message : Str },
}

check : Ast.Node -> TypeResult
check = |node|
    when node is
        Program({ body, sourceType }) ->
            # Recursively check body
            results = List.map(body, check)

            all_errors = List.walk(results, [], |acc, result|
                List.concat(acc, result.errors))

            { node_type: Type.mk_unknown, errors: all_errors }

        VariableDeclaration({ declarations, kind }) ->
            # Check all declarators
            results = List.map(declarations, check)
            all_errors = List.walk(results, [], |acc, result|
                List.concat(acc, result.errors))
            { node_type: Type.mk_unknown, errors: all_errors }

        VariableDeclarator({ id, init }) ->
            # Check initializer if present
            when init is
                Some(init_expr) ->
                    check(init_expr)
                None ->
                    { node_type: Type.mk_undefined, errors: [] }

        BinaryExpression({ left, right, operator }) ->
            left_result = check(left)
            right_result = check(right)

            # Determine result type based on operator
            result_type = when operator is
                Plus ->
                    if is_string_type(left_result.node_type) || is_string_type(right_result.node_type) then
                        Type.mk_string
                    else
                        Type.mk_number
                Minus | Star | Slash | Percent ->
                    Type.mk_number
                LessThan | LessThanEqual | GreaterThan | GreaterThanEqual |
                EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
                    Type.mk_boolean
                _ ->
                    Type.mk_unknown

            # Check for type errors
            errors = List.concat(left_result.errors, right_result.errors)

            # Add error if numeric operator on non-numbers
            final_errors = when operator is
                Minus | Star | Slash | Percent ->
                    if !is_numeric_type(left_result.node_type) then
                        List.append(errors, { message: "Left operand must be a number" })
                    else if !is_numeric_type(right_result.node_type) then
                        List.append(errors, { message: "Right operand must be a number" })
                    else
                        errors
                _ ->
                    errors

            { node_type: result_type, errors: final_errors }

        LogicalExpression({ left, right, operator }) ->
            left_result = check(left)
            right_result = check(right)
            errors = List.concat(left_result.errors, right_result.errors)

            # Logical operators return one of their operands
            { node_type: Type.mk_union([left_result.node_type, right_result.node_type]), errors }

        Identifier({ name }) ->
            # For now, identifiers are unknown type
            # In a real implementation, we'd look them up in a symbol table
            { node_type: Type.mk_unknown, errors: [] }

        NumberLiteral({ value }) ->
            { node_type: Type.mk_number, errors: [] }

        StringLiteral({ value }) ->
            { node_type: Type.mk_string, errors: [] }

        BooleanLiteral({ value }) ->
            { node_type: Type.mk_boolean, errors: [] }

        NullLiteral({}) ->
            { node_type: Type.mk_null, errors: [] }

        UndefinedLiteral({}) ->
            { node_type: Type.mk_undefined, errors: [] }

        ExpressionStatement({ expression }) ->
            check(expression)

        CallExpression({ callee, arguments }) ->
            # Check callee and arguments
            callee_result = check(callee)
            arg_results = List.map(arguments, check)

            all_errors = List.walk(arg_results, callee_result.errors, |acc, result|
                List.concat(acc, result.errors))

            { node_type: Type.mk_unknown, errors: all_errors }

        MemberExpression({ object, property, computed }) ->
            # Check object and property
            obj_result = check(object)
            prop_result = check(property)

            errors = List.concat(obj_result.errors, prop_result.errors)
            { node_type: Type.mk_unknown, errors }

        ArrayExpression({ elements }) ->
            # Check all elements
            elem_results = List.map(elements, |elem|
                when elem is
                    Some(e) -> check(e)
                    None -> { node_type: Type.mk_undefined, errors: [] })

            all_errors = List.walk(elem_results, [], |acc, result|
                List.concat(acc, result.errors))

            # For now, arrays are just arrays of unknown
            { node_type: Type.mk_array(Type.mk_unknown), errors: all_errors }

        ObjectExpression({ properties }) ->
            # Check all properties
            prop_results = List.map(properties, check)

            all_errors = List.walk(prop_results, [], |acc, result|
                List.concat(acc, result.errors))

            # For now, objects are unknown type
            { node_type: Type.mk_object([]), errors: all_errors }

        _ ->
            { node_type: Type.mk_unknown, errors: [] }

# Helper functions
is_numeric_type : Type.Type -> Bool
is_numeric_type = |t|
    Type.is_assignable_to(t, Type.mk_number)

is_string_type : Type.Type -> Bool
is_string_type = |t|
    Type.is_assignable_to(t, Type.mk_string)

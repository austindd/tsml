module [
    TypeError,
    ErrorKind,
    TypeResult,
    check_program,
    check_expression,
    format_error,
]

import Ast
import SimpleComprehensiveType as Type exposing [Type]

# Type error
TypeError : {
    kind : ErrorKind,
    message : Str,
    expected_type : Type,
    actual_type : Type,
}

# Error kinds
ErrorKind : [
    TypeMismatch,
    UndefinedVariable,
    InvalidOperation,
    ConstReassignment,
    NotCallable,
    PropertyNotFound,
]

# Result of type checking
TypeResult : {
    node_type : Type,
    errors : List TypeError,
}

# Check a complete program and return errors
check_program : Ast.Node -> TypeResult
check_program = |node|
    check_node_impl(node)

# Check node implementation
check_node_impl : Ast.Node -> TypeResult
check_node_impl = |node|
    when node is
        Program({ body, sourceType: _ }) ->
            # Check all statements
            results = List.map(body, |stmt|
                check_node_impl(stmt))

            # Collect all errors
            all_errors = List.walk(results, [], |acc, result|
                List.concat(acc, result.errors))

            {
                node_type: Type.mk_unknown,
                errors: all_errors,
            }

        # Variable declaration
        VariableDeclaration({ declarations, kind }) ->
            check_var_declaration(declarations, kind)

        # Binary expression
        BinaryExpression({ left, right, operator }) ->
            check_binary_expr(left, right, operator)

        # Logical expression
        LogicalExpression({ left, right, operator }) ->
            left_result = check_node_impl(left)
            right_result = check_node_impl(right)
            errors = List.concat(left_result.errors, right_result.errors)
            {
                node_type: Type.mk_union([left_result.node_type, right_result.node_type]),
                errors,
            }

        # Identifier
        Identifier({ name }) ->
            check_identifier(name)

        # Literals
        NumberLiteral(_) ->
            { node_type: Type.mk_number, errors: [] }

        StringLiteral(_) ->
            { node_type: Type.mk_string, errors: [] }

        BooleanLiteral(_) ->
            { node_type: Type.mk_boolean, errors: [] }

        NullLiteral(_) ->
            { node_type: Type.mk_null, errors: [] }

        # Expression statement
        Directive({ expression }) ->
            check_node_impl(expression)

        # Default - return unknown
        _ ->
            { node_type: Type.mk_unknown, errors: [] }

# Check variable declaration
check_var_declaration : List Ast.Node, Ast.VariableDeclarationKind -> TypeResult
check_var_declaration = |declarations, kind|
    is_const = when kind is
        Const -> Bool.true
        _ -> Bool.false

    # Process each declarator
    results = List.map(declarations, |decl|
        when decl is
            VariableDeclarator({ id, init }) ->
                when id is
                    Identifier({ name }) ->
                        # Get initializer type
                        init_result = when init is
                            Some(init_expr) ->
                                check_node_impl(init_expr)
                            None ->
                                { node_type: Type.mk_undefined, errors: [] }

                        # Try to add to symbol table (would fail on duplicates)
                        init_result

                    _ ->
                        { node_type: Type.mk_unknown, errors: [] }
            _ ->
                { node_type: Type.mk_unknown, errors: [] })

    # Collect errors
    all_errors = List.walk(results, [], |acc, result|
        List.concat(acc, result.errors))

    {
        node_type: Type.mk_unknown,
        errors: all_errors,
    }

# Check identifier
check_identifier : Str -> TypeResult
check_identifier = |name|
    # For now, just return unknown type for all identifiers
    # In a real implementation, we'd track variables in scope
    {
        node_type: Type.mk_unknown,
        errors: [],
    }

# Check binary expression
check_binary_expr : Ast.Node, Ast.Node, Ast.BinaryOperator -> TypeResult
check_binary_expr = |left, right, operator|
    left_result = check_node_impl(left)
    right_result = check_node_impl(right)

    # Combine errors
    errors = List.concat(left_result.errors, right_result.errors)

    # Check operator compatibility
    when operator is
        # Arithmetic (except Plus) requires numbers
        Minus | Star | Slash | Percent ->
            op_errors = []

            op_errors1 = if !(is_numeric(left_result.node_type)) then
                List.append(op_errors, {
                    kind: InvalidOperation,
                    message: "Left operand must be a number for $(operator_to_str(operator))",
                    expected_type: Type.mk_number,
                    actual_type: left_result.node_type,
                })
            else
                op_errors

            op_errors2 = if !(is_numeric(right_result.node_type)) then
                List.append(op_errors1, {
                    kind: InvalidOperation,
                    message: "Right operand must be a number for $(operator_to_str(operator))",
                    expected_type: Type.mk_number,
                    actual_type: right_result.node_type,
                })
            else
                op_errors1

            {
                node_type: Type.mk_number,
                errors: List.concat(errors, op_errors2),
            }

        # Plus can be string concat or addition
        Plus ->
            result_type = if is_string_like(left_result.node_type) || is_string_like(right_result.node_type) then
                Type.mk_string
            else
                Type.mk_number

            { node_type: result_type, errors }

        # Comparison operators return boolean
        LessThan | LessThanEqual | GreaterThan | GreaterThanEqual |
        EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
            { node_type: Type.mk_boolean, errors }

        # Default
        _ ->
            { node_type: Type.mk_unknown, errors }

# Check a standalone expression
check_expression : Ast.Node -> TypeResult
check_expression = |node|
    check_node_impl(node)

# Format error for display
format_error : TypeError -> Str
format_error = |error|
    kind_str = when error.kind is
        TypeMismatch -> "Type Mismatch"
        UndefinedVariable -> "Undefined Variable"
        InvalidOperation -> "Invalid Operation"
        ConstReassignment -> "Const Reassignment"
        NotCallable -> "Not Callable"
        PropertyNotFound -> "Property Not Found"

    "$(kind_str): $(error.message)"

# Helper functions
is_numeric : Type -> Bool
is_numeric = |t|
    Type.is_assignable_to(t, Type.mk_number)

is_string_like : Type -> Bool
is_string_like = |t|
    Type.is_assignable_to(t, Type.mk_string)

operator_to_str : Ast.BinaryOperator -> Str
operator_to_str = |op|
    when op is
        Plus -> "+"
        Minus -> "-"
        Star -> "*"
        Slash -> "/"
        Percent -> "%"
        _ -> "operator"


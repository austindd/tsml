module [
    TypeError,
    ErrorKind,
    TypeResult,
    check_program,
    check_expression,
    format_error,
]

import Ast exposing [
    Node,
    BinaryOperator,
    VariableDeclarationKind,
]
import SimpleComprehensiveType as Type exposing [Type]
import TypedSymbolTable as TST
import JSGlobals
import MinimalType

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
check_program : Node -> TypeResult
check_program = |node|
    # Initialize symbol table with JS globals
    initial_table = TST.empty_table {}
        |> JSGlobals.add_js_globals

    check_with_table node initial_table

# Check with symbol table context
check_with_table : Node, TST.SymbolTable -> TypeResult
check_with_table = |node, table|
    when node is
        Program(data) ->
            # Check all statements
            results = List.map(data.body, |stmt|
                check_with_table(stmt, table))

            # Collect all errors
            all_errors = List.walk(results, [], |acc, result|
                List.concat(acc, result.errors))

            {
                node_type: Type.mk_unknown,
                errors: all_errors,
            }

        # Variable declaration
        VariableDeclaration(data) ->
            check_var_declaration(data.declarations, data.kind, table)

        # Binary expression
        BinaryExpression(data) ->
            check_binary_expr(data.left, data.right, data.operator, table)

        # Logical expression
        LogicalExpression(data) ->
            left_result = check_with_table(data.left, table)
            right_result = check_with_table(data.right, table)
            errors = List.concat(left_result.errors, right_result.errors)
            {
                node_type: Type.mk_union([left_result.node_type, right_result.node_type]),
                errors,
            }

        # Identifier
        Identifier(data) ->
            check_identifier(data.name, table)

        # Literals
        NumericLiteral _ ->
            { node_type: Type.mk_number, errors: [] }

        StringLiteral _ ->
            { node_type: Type.mk_string, errors: [] }

        BooleanLiteral _ ->
            { node_type: Type.mk_boolean, errors: [] }

        NullLiteral _ ->
            { node_type: Type.mk_null, errors: [] }

        # Expression statement
        ExpressionStatement(data) ->
            check_with_table(data.expression, table)

        # Default - return unknown
        _ ->
            { node_type: Type.mk_unknown, errors: [] }

# Check variable declaration
check_var_declaration : List Node, VariableDeclarationKind, TST.SymbolTable -> TypeResult
check_var_declaration = |declarations, kind, table|
    is_const = when kind is
        Const -> Bool.true
        _ -> Bool.false

    # Process each declarator
    results = List.map(declarations, |decl|
        when decl is
            VariableDeclarator(decl_data) ->
                when decl_data.id is
                    Identifier(id_data) ->
                        # Get initializer type
                        init_result = when decl_data.init is
                            Some(init_expr) ->
                                check_with_table(init_expr, table)
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
check_identifier : Str, TST.SymbolTable -> TypeResult
check_identifier = |name, table|
    when TST.lookup_symbol(table, name) is
        Ok(symbol) ->
            # Convert from MinimalType to SimpleComprehensiveType
            converted_type = convert_minimal_type(symbol.sym_type)
            {
                node_type: converted_type,
                errors: [],
            }
        Err _ ->
            {
                node_type: Type.mk_unknown,
                errors: [
                    {
                        kind: UndefinedVariable,
                        message: "Undefined variable: $(name)",
                        expected_type: Type.mk_unknown,
                        actual_type: Type.mk_unknown,
                    }
                ],
            }

# Check binary expression
check_binary_expr : Node, Node, BinaryOperator, TST.SymbolTable -> TypeResult
check_binary_expr = |left, right, operator, table|
    left_result = check_with_table(left, table)
    right_result = check_with_table(right, table)

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
check_expression : Node -> TypeResult
check_expression = |node|
    table = TST.empty_table {}
        |> JSGlobals.add_js_globals
    check_with_table(node, table)

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

operator_to_str : BinaryOperator -> Str
operator_to_str = |op|
    when op is
        Plus -> "+"
        Minus -> "-"
        Star -> "*"
        Slash -> "/"
        Percent -> "%"
        _ -> "operator"

# Convert MinimalType to SimpleComprehensiveType
convert_minimal_type : MinimalType.TType -> Type
convert_minimal_type = |minimal|
    if MinimalType.is_num(minimal) then
        Type.mk_number
    else if MinimalType.is_str(minimal) then
        Type.mk_string
    else if MinimalType.is_bool(minimal) then
        Type.mk_boolean
    else
        Type.mk_unknown
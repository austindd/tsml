module [
    check_program,
    TypeInfo,
    TypeContext,
]

import Ast
import SimpleComprehensiveType as Type
import Parser
import Token
import Option exposing [Option]

# Type information result
TypeInfo : {
    type: Type.Type,
    errors: List { message: Str, location: Option U32 },
}

# Type checking context
TypeContext : {
    variables: List { name: Str, type: Type.Type },
    functions: List { name: Str, type: Type.Type },
    errors: List { message: Str, location: Option U32 },
}

# Create empty context
empty_context : TypeContext
empty_context = {
    variables: [],
    functions: [],
    errors: [],
}

# Check a complete program from source
check_program : Str -> TypeInfo
check_program = |source|
    # Tokenize
    tokens = Token.tokenize_str(source)

    # Filter trivia tokens
    filtered = List.keep_if(tokens, |tok|
        when tok is
            NewLineTrivia(_) -> Bool.false
            WhitespaceTrivia(_) -> Bool.false
            ShebangTrivia -> Bool.false
            CommentText(_) -> Bool.false
            LineCommentStart -> Bool.false
            BlockCommentStart -> Bool.false
            BlockCommentEnd -> Bool.false
            _ -> Bool.true
    )

    # Parse the filtered tokens
    parsed = Parser.parse_program(filtered)

    # Check the AST
    result = check_node(parsed, empty_context)
    {
        type: Type.mk_unknown,
        errors: result.errors,
    }

# Check an AST node
check_node : Ast.Node, TypeContext -> TypeContext
check_node = |node, ctx|
    when node is
        Program({ body, sourceType }) ->
            # Check all statements in the body
            List.walk(body, ctx, |acc, stmt|
                check_node(stmt, acc))

        VariableDeclaration({ declarations, kind }) ->
            # Check all variable declarators
            List.walk(declarations, ctx, |acc, decl|
                check_node(decl, acc))

        VariableDeclarator({ id, init }) ->
            # Get variable name
            var_name = when id is
                Identifier({ name }) -> name
                _ -> "_unknown"

            # Infer type from initializer
            var_type = when init is
                Some(expr) -> infer_type(expr, ctx)
                None -> Type.mk_undefined

            # Add to context
            { ctx &
                variables: List.append(ctx.variables, { name: var_name, type: var_type })
            }

        Directive({ expression }) ->
            # Check the expression
            _ = infer_type(expression, ctx)
            ctx

        BinaryExpression({ left, right, operator }) ->
            left_type = infer_type(left, ctx)
            right_type = infer_type(right, ctx)

            # Check for type errors with numeric operators
            new_errors = when operator is
                Minus | Star | Slash | Percent ->
                    errors = []

                    errors1 = if !Type.is_assignable_to(left_type, Type.mk_number) then
                        List.append(errors, {
                            message: "Left operand of $(operator_to_str(operator)) must be numeric",
                            location: None
                        })
                    else
                        errors

                    errors2 = if !Type.is_assignable_to(right_type, Type.mk_number) then
                        List.append(errors1, {
                            message: "Right operand of $(operator_to_str(operator)) must be numeric",
                            location: None
                        })
                    else
                        errors1

                    errors2
                _ ->
                    []

            { ctx & errors: List.concat(ctx.errors, new_errors) }

        _ ->
            ctx

# Infer type of an expression
infer_type : Ast.Node, TypeContext -> Type.Type
infer_type = |node, ctx|
    when node is
        NumberLiteral({ value }) ->
            Type.mk_number

        StringLiteral({ value }) ->
            Type.mk_string

        BooleanLiteral({ value }) ->
            Type.mk_boolean

        NullLiteral({}) ->
            Type.mk_null

        UndefinedLiteral({}) ->
            Type.mk_undefined

        Identifier({ name }) ->
            # Look up variable in context
            lookup_result = List.find_first(ctx.variables, |var| var.name == name)
            (when lookup_result is
                Ok(var) -> var.type
                Err(_) -> Type.mk_unknown)

        BinaryExpression({ left, right, operator }) ->
            left_type = infer_type(left, ctx)
            right_type = infer_type(right, ctx)

            when operator is
                Plus ->
                    # JavaScript's + operator
                    if Type.is_assignable_to(left_type, Type.mk_string) ||
                       Type.is_assignable_to(right_type, Type.mk_string) then
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

        LogicalExpression({ left, right, operator }) ->
            left_type = infer_type(left, ctx)
            right_type = infer_type(right, ctx)
            Type.mk_union([left_type, right_type])

        ArrayExpression({ elements }) ->
            # For now, array of unknown
            Type.mk_array(Type.mk_unknown)

        ObjectExpression({ properties }) ->
            # For now, empty object
            Type.mk_object([])

        CallExpression({ callee, arguments }) ->
            # For now, unknown
            Type.mk_unknown

        _ ->
            Type.mk_unknown

# Convert operator to string for error messages
operator_to_str : Ast.BinaryOperator -> Str
operator_to_str = |op|
    when op is
        Plus -> "+"
        Minus -> "-"
        Star -> "*"
        Slash -> "/"
        Percent -> "%"
        _ -> "operator"

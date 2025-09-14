module [
]

import Option exposing [
    Option,
]

import Token exposing [
    Token,
    TokenResult,
    tokenize_str,
    ts_token_debug_display,
]

import Ast exposing [
    Node,
    ProgramKind,
    VariableDeclarationKind,
    PropertyKind,
    AssignmentOperator,
    LogicalOperator,
    BinaryOperator,
    UnaryOperator,
    UpdateOperator,
]

parse_program : List Token -> Node
parse_program = |token_list|
    (body, _) = parse_program_body([], token_list)
    Program({ sourceType: Module, body: body })

parse_program_body : List Node, List Token -> (List Node, List Token)
parse_program_body = |statements, token_list|
    when token_list is
        [EndOfFileToken, .. as rest] ->
            (statements, rest)

        [] ->
            (statements, [])

        _ ->
            (stmt, remaining_tokens) = parse_statement(token_list)
            new_statements = List.append(statements, stmt)
            parse_program_body(new_statements, remaining_tokens)

parse_block : List Node, List Token -> List Node
parse_block = |node_list, token_list|
    when token_list is
        [CloseBraceToken, ..] -> node_list
        _ -> node_list

parse_ : List Token -> Option Node
parse_ = |token_list|
    when token_list is
        [EndOfFileToken, ..] -> None
        _ -> None

parse_identifier : List Token -> (Node, List Token)
parse_identifier = |token_list|
    when token_list is
        [IdentifierToken(ident), .. as rest] -> (Identifier({ name: ident }), rest)
        _ -> crash("parse_identifier() failed -- This should never happen")

parse_string_literal : List Token -> (Node, List Token)
parse_string_literal = |token_list|
    when token_list is
        [StringLiteralToken(str), .. as rest] -> (StringLiteral({ value: str }), rest)
        _ -> crash("parse_string_literal() failed -- This should never happen")

# parse_number_literal : List Token -> (Node, List Token)
# parse_number_literal = |token_list|
#     when token_list is
#         [NumberLiteralToken(str), .. as rest] -> (NumberLiteral({ value: str }), rest)
#         _ -> crash("parse_number_literal() failed -- This should never happen")

parse_big_int_literal : List Token -> (Node, List Token)
parse_big_int_literal = |token_list|
    when token_list is
        [BigIntLiteralToken(str), .. as rest] -> (BigIntLiteral({ value: str }), rest)
        _ -> crash("parse_big_int_literal() failed -- This should never happen")

# parse_regular_expression_literal : List Token -> (Node, List Token)
# parse_regular_expression_literal = |token_list|
#     when token_list is
#         [RegularExpressionLiteralToken(str), .. as rest] ->
#             (RegularExpressionLiteral({ pattern: str, flags: "" }), rest)
#         _ -> crash("parse_regular_expression_literal() failed -- This should never happen")

PrattParserMode : [Nud, Led { left_node : Node }]

parse_expression : PrattParserMode, U16, List Token -> (Node, List Token)
parse_expression = |mode, min_precedence, token_list|
    when mode is
        Nud ->
            when token_list is
                [IdentifierToken(ident), .. as rest] ->
                    (Identifier({ name: ident }), rest)

                [StringLiteralToken(str), .. as rest] ->
                    (StringLiteral({ value: str }), rest)

                [NumberLiteralToken(str), .. as rest] ->
                    (NumberLiteral({ value: str }), rest)

                [BigIntLiteralToken(str), .. as rest] ->
                    (BigIntLiteral({ value: str }), rest)

                # Template literals
                [BacktickToken, .. as rest1] ->
                    parse_template_literal(rest1)

                # Unary prefix operators
                [PlusToken as tok, .. as rest1]
                | [MinusToken as tok, .. as rest1]
                | [ExclamationToken as tok, .. as rest1]
                | [TildeToken as tok, .. as rest1]
                | [TypeofKeyword as tok, .. as rest1]
                | [VoidKeyword as tok, .. as rest1]
                | [DeleteKeyword as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Nud, tok)
                    (argument, rest2) = parse_expression(Nud, expr_precedence, rest1)
                    unary_node = UnaryExpression(
                        {
                            operator: token_to_unary_operator(tok),
                            prefix: Bool.true,
                            argument: argument,
                        },
                    )
                    (unary_node, rest2)

                # Update prefix operators
                [PlusPlusToken as tok, .. as rest1]
                | [MinusMinusToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Nud, tok)
                    (argument, rest2) = parse_expression(Nud, expr_precedence, rest1)
                    update_node = UpdateExpression(
                        {
                            operator: token_to_update_operator(tok),
                            prefix: Bool.true,
                            argument: argument,
                        },
                    )
                    (update_node, rest2)

                # Array literals
                [OpenBracketToken, .. as rest1] ->
                    parse_array_literal(rest1)

                # Object literals
                [OpenBraceToken, .. as rest1] ->
                    parse_object_literal(rest1)

                # Function expressions
                [FunctionKeyword, .. as rest1] ->
                    parse_function_expression(rest1)

                [OpenParenToken, .. as rest1] ->
                    (node, remaining_tokens) = parse_expression(Nud, 0, rest1)
                    when remaining_tokens is
                        [CloseParenToken, .. as rest2] ->
                            (node, rest2)

                        [tok, ..] ->
                            (
                                Error({ message: "parse_expression() failed -- Expected close paren, but got ${ts_token_debug_display(tok)}" }),
                                remaining_tokens,
                            )

                        [] ->
                            (
                                Error({ message: "parse_expression() failed -- Expected close paren, but got nothing" }),
                                remaining_tokens,
                            )

                [CloseParenToken, .. as rest] ->
                    (Error({ message: "Unexpected close paren" }), rest)

                _ -> crash("parse_expression() failed -- This should never happen")

        Led({ left_node }) ->
            when token_list is
                # _ -> crash("parse_expression() failed -- This should never happen")
                # Left associative
                # Assignment
                [EqualsToken as tok, .. as rest1]
                | [PlusEqualsToken as tok, .. as rest1]
                | [MinusEqualsToken as tok, .. as rest1]
                | [AsteriskEqualsToken as tok, .. as rest1]
                | [AsteriskAsteriskEqualsToken as tok, .. as rest1]
                | [SlashEqualsToken as tok, .. as rest1]
                | [PercentEqualsToken as tok, .. as rest1]
                | [LessThanLessThanEqualsToken as tok, .. as rest1]
                | [GreaterThanGreaterThanEqualsToken as tok, .. as rest1]
                | [GreaterThanGreaterThanGreaterThanEqualsToken as tok, .. as rest1]
                | [AmpersandEqualsToken as tok, .. as rest1]
                | [BarEqualsToken as tok, .. as rest1]
                | [BarBarEqualsToken as tok, .. as rest1]
                | [AmpersandAmpersandEqualsToken as tok, .. as rest1]
                | [QuestionQuestionEqualsToken as tok, .. as rest1]
                | [CaretEqualsToken as tok, .. as rest1]
                # Conditional
                | [QuestionToken as tok, .. as rest1]
                | [ColonToken as tok, .. as rest1]
                # Logical
                | [AmpersandAmpersandToken as tok, .. as rest1]
                | [BarBarToken as tok, .. as rest1]
                # Bitwise
                | [AmpersandToken as tok, .. as rest1]
                | [BarToken as tok, .. as rest1]
                | [CaretToken as tok, .. as rest1]
                | [LessThanLessThanToken as tok, .. as rest1]
                | [GreaterThanGreaterThanToken as tok, .. as rest1]
                | [GreaterThanGreaterThanGreaterThanToken as tok, .. as rest1]
                # Relational
                | [EqualsEqualsToken as tok, .. as rest1]
                | [ExclamationEqualsToken as tok, .. as rest1]
                | [EqualsEqualsEqualsToken as tok, .. as rest1]
                | [ExclamationEqualsEqualsToken as tok, .. as rest1]
                # Additive
                | [PlusToken as tok, .. as rest1]
                | [MinusToken as tok, .. as rest1]
                # Multiplicative
                | [AsteriskToken as tok, .. as rest1]
                | [SlashToken as tok, .. as rest1]
                | [PercentToken as tok, .. as rest1]
                # Postfix (update expressions)
                | [PlusPlusToken as tok, .. as rest1]
                | [MinusMinusToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Led({ left_node }), tok)
                    update_node = UpdateExpression(
                        {
                            operator: token_to_update_operator(tok),
                            prefix: Bool.false,
                            argument: left_node,
                        },
                    )
                    (update_node, rest1)

                # Arrow function
                [EqualsGreaterThanToken, .. as rest1] ->
                    parse_arrow_function_body(left_node, rest1)

                # FunctionCall
                [OpenParenToken, .. as rest1] ->
                    parse_function_call(left_node, rest1)

                # MemberAccess - dot notation
                [DotToken, .. as rest1] ->
                    parse_member_access_dot(left_node, rest1)

                # MemberAccess - bracket notation
                [OpenBracketToken, .. as rest1] ->
                    parse_member_access_bracket(left_node, rest1)

                # Right associative
                # Exponentiation
                [AsteriskAsteriskToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Led({ left_node }), tok)
                    (right_node, rest2) = parse_expression(Led({ left_node }), expr_precedence - 1, rest1)
                    current_node = BinaryExpression(
                        {
                            left: left_node,
                            operator: token_to_binary_operator(tok),
                            right: right_node,
                        },
                    )
                    (current_node, rest2)

                [] -> crash("parse_expression() failed -- Unexpected end of expression")
                _ -> crash("parse_expression() failed -- This should never happen")

OperatorPosition : [
    Prefix,
    Infix,
    Postfix,
]

Associativity : [
    Left,
    Right,
]

OperatorGroup : [
    Assignment, # "=", "+=", "-=", "*=", "/="
    ArrowFunction, # "=>"
    Conditional, # "?", ":"
    Logical, # "||", "&&"
    Bitwise, # "|", "&", ">>", "<<", ">>>"
    Relational, # "<", ">", "<=", ">=", "instanceof", "in", "of"
    Additive, # "+", "-"
    Multiplicative, # "*", "/", "%"
    Exponentiation, # "**"
    Unary, # "+", "-", "!", "~", "typeof"
    Postfix, # "++", "--"
    FunctionCall, # "func()", "method()"
    MemberAccess, # "obj.", "arr[]"
]

get_operator_group_precedence : OperatorGroup -> U16
get_operator_group_precedence = |operator_group|
    when operator_group is
        Assignment -> 100
        ArrowFunction -> 150
        Conditional -> 200
        Logical -> 300
        Bitwise -> 400
        Relational -> 500
        Additive -> 600
        Multiplicative -> 700
        Exponentiation -> 800
        Unary -> 900
        Postfix -> 1000
        FunctionCall -> 1100
        MemberAccess -> 1100

# This function accepts a token and returns the operator group it belongs to
get_expr_operator_group : PrattParserMode, Token -> OperatorGroup
get_expr_operator_group = |mode, token|
    when mode is
        Nud ->
            when token is
                # Unary
                PlusToken -> Unary
                MinusToken -> Unary
                ExclamationToken -> Unary
                TildeToken -> Unary
                TypeofKeyword -> Unary
                VoidKeyword -> Unary
                DeleteKeyword -> Unary
                # Update (prefix)
                PlusPlusToken -> Unary
                MinusMinusToken -> Unary
                _ -> crash("get_expr_operator_group() failed -- This should never happen")

        Led(_) ->
            when token is
                # Assignment
                EqualsToken -> Assignment
                PlusEqualsToken -> Assignment
                MinusEqualsToken -> Assignment
                AsteriskEqualsToken -> Assignment
                AsteriskAsteriskEqualsToken -> Assignment
                SlashEqualsToken -> Assignment
                PercentEqualsToken -> Assignment
                LessThanLessThanEqualsToken -> Assignment
                GreaterThanGreaterThanEqualsToken -> Assignment
                GreaterThanGreaterThanGreaterThanEqualsToken -> Assignment
                AmpersandEqualsToken -> Assignment
                BarEqualsToken -> Assignment
                BarBarEqualsToken -> Assignment
                AmpersandAmpersandEqualsToken -> Assignment
                QuestionQuestionEqualsToken -> Assignment
                CaretEqualsToken -> Assignment
                # Arrow function
                EqualsGreaterThanToken -> ArrowFunction
                # Conditional
                QuestionToken -> Conditional
                ColonToken -> Conditional
                # Logical
                AmpersandAmpersandToken -> Logical
                BarBarToken -> Logical
                # Bitwise
                AmpersandToken -> Bitwise
                BarToken -> Bitwise
                CaretToken -> Bitwise
                LessThanLessThanToken -> Bitwise
                GreaterThanGreaterThanToken -> Bitwise
                GreaterThanGreaterThanGreaterThanToken -> Bitwise
                # Relational
                EqualsEqualsToken -> Relational
                ExclamationEqualsToken -> Relational
                EqualsEqualsEqualsToken -> Relational
                ExclamationEqualsEqualsToken -> Relational
                EqualsGreaterThanToken -> Relational
                # Additive
                PlusToken -> Additive
                MinusToken -> Additive
                # Multiplicative
                AsteriskToken -> Multiplicative
                SlashToken -> Multiplicative
                PercentToken -> Multiplicative
                # Exponentiation
                AsteriskAsteriskToken -> Exponentiation
                # Postfix
                PlusPlusToken -> Postfix
                MinusMinusToken -> Postfix
                # FunctionCall
                OpenParenToken -> FunctionCall
                # MemberAccess
                DotToken -> MemberAccess
                OpenBracketToken -> MemberAccess
                _ -> crash("get_expr_operator_group() failed -- This should never happen")

get_expr_precedence : PrattParserMode, Token -> U16
get_expr_precedence = |mode, token|
    operator_group = get_expr_operator_group(mode, token)
    get_operator_group_precedence(operator_group)

is_expression_node : Node -> Bool
is_expression_node = |node|
    when node is
        Identifier(_)
        | StringLiteral(_)
        | NumberLiteral(_)
        | BooleanLiteral(_)
        | RegExpLiteral(_)
        | NullLiteral(_)
        | UndefinedLiteral(_)
        | BigIntLiteral(_)
        | TemplateLiteral(_)
        | LogicalExpression(_)
        | BinaryExpression(_)
        | UnaryExpression(_)
        | UpdateExpression(_)
        | ConditionalExpression(_)
        | CallExpression(_)
        | NewExpression(_)
        | SequenceExpression(_)
        | FunctionExpression(_)
        | ArrowFunctionExpression(_)
        | ObjectExpression(_)
        | ArrayExpression(_)
        | MemberExpression(_)
        | Error(_) -> Bool.true

        _ -> Bool.false

token_to_binary_operator : Token -> BinaryOperator
token_to_binary_operator = |token|
    when token is
        # Assignment
        EqualsEqualsToken -> EqualEqual
        ExclamationEqualsToken -> BangEqual
        EqualsEqualsEqualsToken -> EqualEqualEqual
        ExclamationEqualsEqualsToken -> EqualEqual
        LessThanToken -> LessThan
        LessThanEqualsToken -> LessThanEqual
        GreaterThanToken -> GreaterThan
        GreaterThanEqualsToken -> GreaterThanEqual
        LessThanLessThanToken -> LeftShift
        GreaterThanGreaterThanToken -> RightShift
        GreaterThanGreaterThanGreaterThanToken -> UnsignedRightShift
        PlusToken -> Plus
        MinusToken -> Minus
        AsteriskToken -> Star
        SlashToken -> Slash
        PercentToken -> Percent
        BarToken -> Pipe
        CaretToken -> Caret
        AmpersandToken -> Ampersand
        InKeyword -> In
        InstanceofKeyword -> Instanceof
        _ -> crash("token_to_binary_operator() failed -- This should never happen")

token_to_unary_operator : Token -> UnaryOperator
token_to_unary_operator = |token|
    when token is
        PlusToken -> Plus
        MinusToken -> Minus
        ExclamationToken -> Bang
        TildeToken -> Tilde
        TypeofKeyword -> Typeof
        VoidKeyword -> Void
        DeleteKeyword -> Delete
        _ -> crash("token_to_unary_operator() failed -- This should never happen")

token_to_update_operator : Token -> UpdateOperator
token_to_update_operator = |token|
    when token is
        PlusPlusToken -> PlusPlus
        MinusMinusToken -> MinusMinus
        _ -> crash("token_to_update_operator() failed -- This should never happen")

parse_array_literal : List Token -> (Node, List Token)
parse_array_literal = |token_list|
    parse_array_elements([], token_list)

parse_array_elements : List Node, List Token -> (Node, List Token)
parse_array_elements = |elements, token_list|
    when token_list is
        [CloseBracketToken, .. as rest] ->
            (ArrayExpression({ elements: elements }), rest)

        [CommaToken, .. as rest] ->
            # Handle sparse arrays (holes)
            hole_element = Identifier({ name: "_hole_" }) # Placeholder for array holes
            parse_array_elements(List.append(elements, hole_element), rest)

        [] ->
            (Error({ message: "Unexpected end of array literal" }), [])

        _ ->
            (element, remaining_tokens) = parse_expression(Nud, 0, token_list)
            when remaining_tokens is
                [CommaToken, .. as rest] ->
                    parse_array_elements(List.append(elements, element), rest)

                [CloseBracketToken, .. as rest] ->
                    final_elements = List.append(elements, element)
                    (ArrayExpression({ elements: final_elements }), rest)

                _ ->
                    (Error({ message: "Expected comma or close bracket in array literal" }), remaining_tokens)

parse_object_literal : List Token -> (Node, List Token)
parse_object_literal = |token_list|
    parse_object_properties([], token_list)

parse_object_properties : List Node, List Token -> (Node, List Token)
parse_object_properties = |properties, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            (ObjectExpression({ properties: properties }), rest)

        [] ->
            (Error({ message: "Unexpected end of object literal" }), [])

        _ ->
            (property, remaining_tokens) = parse_object_property(token_list)
            when remaining_tokens is
                [CommaToken, .. as rest] ->
                    parse_object_properties(List.append(properties, property), rest)

                [CloseBraceToken, .. as rest] ->
                    final_properties = List.append(properties, property)
                    (ObjectExpression({ properties: final_properties }), rest)

                _ ->
                    (Error({ message: "Expected comma or close brace in object literal" }), remaining_tokens)

parse_object_property : List Token -> (Node, List Token)
parse_object_property = |token_list|
    # Parse the key
    (key, rest1) = parse_property_key(token_list)
    when rest1 is
        [ColonToken, .. as rest2] ->
            # Parse the value
            (value, rest3) = parse_expression(Nud, 0, rest2)
            property_node = Property(
                {
                    key: key,
                    value: value,
                    kind: Init,
                },
            )
            (property_node, rest3)

        _ ->
            (Error({ message: "Expected colon after property key" }), rest1)

parse_property_key : List Token -> (Node, List Token)
parse_property_key = |token_list|
    when token_list is
        [IdentifierToken(ident), .. as rest] ->
            (Identifier({ name: ident }), rest)

        [StringLiteralToken(str), .. as rest] ->
            (StringLiteral({ value: str }), rest)

        [NumberLiteralToken(num), .. as rest] ->
            (NumberLiteral({ value: num }), rest)

        [OpenBracketToken, .. as rest1] ->
            # Computed property key [expr]
            (expr, rest2) = parse_expression(Nud, 0, rest1)
            when rest2 is
                [CloseBracketToken, .. as rest3] ->
                    (expr, rest3)

                _ ->
                    (Error({ message: "Expected close bracket for computed property key" }), rest2)

        _ ->
            (Error({ message: "Expected property key" }), token_list)

parse_function_call : Node, List Token -> (Node, List Token)
parse_function_call = |callee, token_list|
    parse_function_arguments(callee, [], token_list)

parse_function_arguments : Node, List Node, List Token -> (Node, List Token)
parse_function_arguments = |callee, arguments, token_list|
    when token_list is
        [CloseParenToken, .. as rest] ->
            (CallExpression({ callee: callee, arguments: arguments }), rest)

        [] ->
            (Error({ message: "Unexpected end of function call" }), [])

        _ ->
            (arg, remaining_tokens) = parse_expression(Nud, 0, token_list)
            when remaining_tokens is
                [CommaToken, .. as rest] ->
                    parse_function_arguments(callee, List.append(arguments, arg), rest)

                [CloseParenToken, .. as rest] ->
                    final_args = List.append(arguments, arg)
                    (CallExpression({ callee: callee, arguments: final_args }), rest)

                _ ->
                    (Error({ message: "Expected comma or close paren in function call" }), remaining_tokens)

parse_member_access_dot : Node, List Token -> (Node, List Token)
parse_member_access_dot = |object, token_list|
    when token_list is
        [IdentifierToken(prop_name), .. as rest] ->
            property = Identifier({ name: prop_name })
            member_expr = MemberExpression(
                {
                    object: object,
                    property: property,
                    computed: Bool.false,
                },
            )
            (member_expr, rest)

        _ ->
            (Error({ message: "Expected identifier after dot" }), token_list)

parse_member_access_bracket : Node, List Token -> (Node, List Token)
parse_member_access_bracket = |object, token_list|
    (property, rest1) = parse_expression(Nud, 0, token_list)
    when rest1 is
        [CloseBracketToken, .. as rest2] ->
            member_expr = MemberExpression(
                {
                    object: object,
                    property: property,
                    computed: Bool.true,
                },
            )
            (member_expr, rest2)

        _ ->
            (Error({ message: "Expected close bracket in member access" }), rest1)

parse_statement : List Token -> (Node, List Token)
parse_statement = |token_list|
    when token_list is
        # Variable declarations
        [VarKeyword, .. as rest] ->
            parse_variable_declaration(Var, rest)

        [LetKeyword, .. as rest] ->
            parse_variable_declaration(Let, rest)

        [ConstKeyword, .. as rest] ->
            parse_variable_declaration(Const, rest)

        # Block statement
        [OpenBraceToken, .. as rest] ->
            parse_block_statement(rest)

        # Control flow statements
        [IfKeyword, .. as rest] ->
            parse_if_statement(rest)

        [WhileKeyword, .. as rest] ->
            parse_while_statement(rest)

        [DoKeyword, .. as rest] ->
            parse_do_while_statement(rest)

        [ForKeyword, .. as rest] ->
            parse_for_statement(rest)

        # Function declaration
        [FunctionKeyword, .. as rest] ->
            parse_function_declaration(rest)

        # Expression statement (fallback)
        _ ->
            parse_expression_statement(token_list)

parse_variable_declaration : VariableDeclarationKind, List Token -> (Node, List Token)
parse_variable_declaration = |kind, token_list|
    parse_variable_declarators(kind, [], token_list)

parse_variable_declarators : VariableDeclarationKind, List Node, List Token -> (Node, List Token)
parse_variable_declarators = |kind, declarators, token_list|
    (declarator, rest1) = parse_variable_declarator(token_list)
    new_declarators = List.append(declarators, declarator)

    when rest1 is
        [CommaToken, .. as rest2] ->
            # More declarators
            parse_variable_declarators(kind, new_declarators, rest2)

        [SemicolonToken, .. as rest2] ->
            # End of declaration
            var_decl = VariableDeclaration(
                {
                    declarations: new_declarators,
                    kind: kind,
                },
            )
            (var_decl, rest2)

        _ ->
            # Allow without semicolon (ASI)
            var_decl = VariableDeclaration(
                {
                    declarations: new_declarators,
                    kind: kind,
                },
            )
            (var_decl, rest1)

parse_variable_declarator : List Token -> (Node, List Token)
parse_variable_declarator = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            identifier = Identifier({ name: name })
            when rest1 is
                [EqualsToken, .. as rest2] ->
                    # Has initializer
                    (init_expr, rest3) = parse_expression(Nud, 0, rest2)
                    declarator = VariableDeclarator(
                        {
                            id: identifier,
                            init: Some(init_expr),
                        },
                    )
                    (declarator, rest3)

                _ ->
                    # No initializer
                    declarator = VariableDeclarator(
                        {
                            id: identifier,
                            init: None,
                        },
                    )
                    (declarator, rest1)

        _ ->
            (Error({ message: "Expected identifier in variable declaration" }), token_list)

parse_block_statement : List Token -> (Node, List Token)
parse_block_statement = |token_list|
    parse_statement_list([], token_list)

parse_statement_list : List Node, List Token -> (Node, List Token)
parse_statement_list = |statements, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            (BlockStatement({ body: statements }), rest)

        [] ->
            (Error({ message: "Unexpected end of block statement" }), [])

        _ ->
            (stmt, remaining_tokens) = parse_statement(token_list)
            new_statements = List.append(statements, stmt)
            parse_statement_list(new_statements, remaining_tokens)

parse_expression_statement : List Token -> (Node, List Token)
parse_expression_statement = |token_list|
    (expr, rest1) = parse_expression(Nud, 0, token_list)
    when rest1 is
        [SemicolonToken, .. as rest2] ->
            # TODO: Create ExpressionStatement node type if it doesn't exist
            (expr, rest2)

        _ ->
            # Allow without semicolon (ASI)
            (expr, rest1)

parse_if_statement : List Token -> (Node, List Token)
parse_if_statement = |token_list|
    when token_list is
        [OpenParenToken, .. as rest1] ->
            (test_expr, rest2) = parse_expression(Nud, 0, rest1)
            when rest2 is
                [CloseParenToken, .. as rest3] ->
                    (consequent, rest4) = parse_statement(rest3)
                    when rest4 is
                        [ElseKeyword, .. as rest5] ->
                            (alternate, rest6) = parse_statement(rest5)
                            if_stmt = IfStatement(
                                {
                                    test: test_expr,
                                    consequent: consequent,
                                    alternate: Some(alternate),
                                },
                            )
                            (if_stmt, rest6)

                        _ ->
                            if_stmt = IfStatement(
                                {
                                    test: test_expr,
                                    consequent: consequent,
                                    alternate: None,
                                },
                            )
                            (if_stmt, rest4)

                _ ->
                    (Error({ message: "Expected close paren after if condition" }), rest2)

        _ ->
            (Error({ message: "Expected open paren after if" }), token_list)

parse_while_statement : List Token -> (Node, List Token)
parse_while_statement = |token_list|
    when token_list is
        [OpenParenToken, .. as rest1] ->
            (test_expr, rest2) = parse_expression(Nud, 0, rest1)
            when rest2 is
                [CloseParenToken, .. as rest3] ->
                    (body, rest4) = parse_statement(rest3)
                    while_stmt = WhileStatement(
                        {
                            test: test_expr,
                            body: body,
                        },
                    )
                    (while_stmt, rest4)

                _ ->
                    (Error({ message: "Expected close paren after while condition" }), rest2)

        _ ->
            (Error({ message: "Expected open paren after while" }), token_list)

parse_do_while_statement : List Token -> (Node, List Token)
parse_do_while_statement = |token_list|
    (body, rest1) = parse_statement(token_list)
    when rest1 is
        [WhileKeyword, .. as rest2] ->
            when rest2 is
                [OpenParenToken, .. as rest3] ->
                    (test_expr, rest4) = parse_expression(Nud, 0, rest3)
                    when rest4 is
                        [CloseParenToken, .. as rest5] ->
                            when rest5 is
                                [SemicolonToken, .. as rest6] ->
                                    do_while_stmt = DoWhileStatement(
                                        {
                                            body: body,
                                            test: test_expr,
                                        },
                                    )
                                    (do_while_stmt, rest6)

                                _ ->
                                    # Allow without semicolon
                                    do_while_stmt = DoWhileStatement(
                                        {
                                            body: body,
                                            test: test_expr,
                                        },
                                    )
                                    (do_while_stmt, rest5)

                        _ ->
                            (Error({ message: "Expected close paren after do-while condition" }), rest4)

                _ ->
                    (Error({ message: "Expected open paren after while in do-while" }), rest2)

        _ ->
            (Error({ message: "Expected while after do statement body" }), rest1)

parse_for_statement : List Token -> (Node, List Token)
parse_for_statement = |token_list|
    when token_list is
        [OpenParenToken, .. as rest1] ->
            # Parse init (can be variable declaration or expression)
            (init, rest2) =
                when rest1 is
                    [SemicolonToken, .. as rest] ->
                        (None, rest)

                    [VarKeyword, .. as rest] ->
                        (var_decl, remaining) = parse_variable_declaration(Var, rest)
                        (Some(var_decl), remaining)

                    [LetKeyword, .. as rest] ->
                        (var_decl, remaining) = parse_variable_declaration(Let, rest)
                        (Some(var_decl), remaining)

                    [ConstKeyword, .. as rest] ->
                        (var_decl, remaining) = parse_variable_declaration(Const, rest)
                        (Some(var_decl), remaining)

                    _ ->
                        (expr, remaining) = parse_expression(Nud, 0, rest1)
                        when remaining is
                            [SemicolonToken, .. as rest] ->
                                (Some(expr), rest)

                            _ ->
                                (Some(expr), remaining)

            # Parse test condition
            (test, rest3) =
                when rest2 is
                    [SemicolonToken, .. as rest] ->
                        (None, rest)

                    _ ->
                        (test_expr, remaining) = parse_expression(Nud, 0, rest2)
                        when remaining is
                            [SemicolonToken, .. as rest] ->
                                (Some(test_expr), rest)

                            _ ->
                                (Some(test_expr), remaining)

            # Parse update expression
            (update, rest4) =
                when rest3 is
                    [CloseParenToken, .. as rest] ->
                        (None, rest)

                    _ ->
                        (update_expr, remaining) = parse_expression(Nud, 0, rest3)
                        when remaining is
                            [CloseParenToken, .. as rest] ->
                                (Some(update_expr), rest)

                            _ ->
                                (Some(update_expr), remaining)

            # Parse body
            (body, rest5) = parse_statement(rest4)
            for_stmt = ForStatement(
                {
                    init: init,
                    test: test,
                    update: update,
                    body: body,
                },
            )
            (for_stmt, rest5)

        _ ->
            (Error({ message: "Expected open paren after for" }), token_list)

parse_function_declaration : List Token -> (Node, List Token)
parse_function_declaration = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)
            (body, rest3) = parse_function_body(rest2)
            func_decl = FunctionDeclaration(
                {
                    id: identifier,
                    params: params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.false,
                },
            )
            (func_decl, rest3)

        _ ->
            crash("parse_function_declaration() failed -- This should never happen")

parse_function_expression : List Token -> (Node, List Token)
parse_function_expression = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            # Named function expression
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: Some(identifier),
                    params: params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.false,
                },
            )
            (func_expr, rest3)

        [OpenParenToken, .. as rest1] ->
            # Anonymous function expression
            (params, rest2) = parse_function_parameters(token_list)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: None,
                    params: params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.false,
                },
            )
            (func_expr, rest3)

        _ ->
            (Error({ message: "Expected function parameters or name" }), token_list)

parse_function_parameters : List Token -> (List Node, List Token)
parse_function_parameters = |token_list|
    when token_list is
        [OpenParenToken, .. as rest1] ->
            parse_parameter_list([], rest1)

        [_, .. as rest1] ->
            ([Error({ message: "Expected open paren for function parameters" })], rest1)

        [_] ->
            ([Error({ message: "Expected open paren for function parameters" })], [])

        [] ->
            ([Error({ message: "Expected open paren for function parameters" })], [])

parse_parameter_list : List Node, List Token -> (List Node, List Token)
parse_parameter_list = |params, token_list|
    when token_list is
        [CloseParenToken, .. as rest] ->
            (params, rest)

        [IdentifierToken(param_name), .. as rest1] ->
            param = Identifier({ name: param_name })
            new_params = List.append(params, param)
            when rest1 is
                [CommaToken, .. as rest2] ->
                    parse_parameter_list(new_params, rest2)

                [CloseParenToken, .. as rest2] ->
                    (new_params, rest2)

                [_, .. as rest2] ->
                    (
                        List.append(
                            new_params,
                            Error({ message: "Expected comma or close paren in parameter list" }),
                        ),
                        rest2,
                    )

                [_] | [] ->
                    (
                        List.append(
                            new_params,
                            Error({ message: "Expected comma or close paren in parameter list" }),
                        ),
                        [],
                    )

        _ ->
            (
                List.append(
                    params,
                    Error({ message: "Expected parameter name or close paren" }),
                ),
                [],
            )

parse_function_body : List Token -> (Node, List Token)
parse_function_body = |token_list|
    when token_list is
        [OpenBraceToken, .. as rest] ->
            parse_function_body_statements([], rest)

        _ ->
            (Error({ message: "Expected open brace for function body" }), token_list)

parse_function_body_statements : List Node, List Token -> (Node, List Token)
parse_function_body_statements = |statements, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            (FunctionBody({ body: statements }), rest)

        [] ->
            (Error({ message: "Unexpected end of function body" }), [])

        _ ->
            (stmt, remaining_tokens) = parse_statement(token_list)
            new_statements = List.append(statements, stmt)
            parse_function_body_statements(new_statements, remaining_tokens)

parse_arrow_function_body : Node, List Token -> (Node, List Token)
parse_arrow_function_body = |params_node, token_list|
    # Convert the left side to parameters
    arrow_params = convert_to_arrow_params(params_node)

    when token_list is
        [OpenBraceToken, .. as rest] ->
            # Block body: () => { statements }
            (body, remaining_tokens) = parse_block_statement(rest)
            arrow_fn = ArrowFunctionExpression(
                {
                    params: arrow_params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.false,
                },
            )
            (arrow_fn, remaining_tokens)

        _ ->
            # Expression body: () => expression
            (expr, remaining_tokens) = parse_expression(Nud, 0, token_list)
            arrow_fn = ArrowFunctionExpression(
                {
                    params: arrow_params,
                    body: expr,
                    generator: Bool.false,
                    async: Bool.false,
                },
            )
            (arrow_fn, remaining_tokens)

convert_to_arrow_params : Node -> List Node
convert_to_arrow_params = |node|
    when node is
        # Single identifier: x => ...
        Identifier(_) ->
            [node]

        # Parenthesized parameters: (a, b) => ...
        # This would come from a parenthesized expression that contains identifiers
        _ ->
            # For now, treat anything else as a single parameter
            # In a full implementation, we'd need to handle:
            # - (a, b) => ... (multiple params)
            # - () => ... (no params)
            # - (a = 1) => ... (default params)
            [node]

parse_template_literal : List Token -> (Node, List Token)
parse_template_literal = |token_list|
    parse_template_elements([], [], token_list)

parse_template_elements : List Node, List Node, List Token -> (Node, List Token)
parse_template_elements = |quasis, expressions, token_list|
    when token_list is
        [BacktickToken, .. as rest] ->
            # End of template literal
            template = TemplateLiteral(
                {
                    quasis: quasis,
                    expressions: expressions,
                },
            )
            (template, rest)

        [StringLiteralToken(text), .. as rest] ->
            # Template text (quasi)
            quasi = Identifier({ name: text }) # Placeholder for TemplateElement
            new_quasis = List.append(quasis, quasi)
            parse_template_elements(new_quasis, expressions, rest)

        _ ->
            # For now, handle simple case - in a full implementation we'd need:
            # - ${expression} handling
            # - Proper TemplateHead/TemplateMiddle/TemplateTail tokens
            template = TemplateLiteral(
                {
                    quasis: quasis,
                    expressions: expressions,
                },
            )
            (template, token_list)


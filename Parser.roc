module [
    parse_program,
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
    MethodKind,
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
            # First parse the left operand
            (left_expr, rest_after_left) = parse_primary_expression(token_list)
            # Then continue parsing with Led mode
            parse_expression(Led({ left_node: left_expr }), min_precedence, rest_after_left)
        
        Led({ left_node }) ->
            parse_expression_led(left_node, min_precedence, token_list)

parse_primary_expression : List Token -> (Node, List Token)
parse_primary_expression = |token_list|
    when token_list is
        [IdentifierToken(ident), .. as rest] ->
            (Identifier({ name: ident }), rest)

        [StringLiteralToken(str), .. as rest] ->
            (StringLiteral({ value: str }), rest)

        [NumberLiteralToken(str), .. as rest] ->
            (NumberLiteral({ value: str }), rest)

        [BigIntLiteralToken(str), .. as rest] ->
            (BigIntLiteral({ value: str }), rest)

        # Boolean literals
        [TrueKeyword, .. as rest] ->
            (BooleanLiteral({ value: Bool.true }), rest)

        [FalseKeyword, .. as rest] ->
            (BooleanLiteral({ value: Bool.false }), rest)

        # Template literals
        [NoSubstitutionTemplateLiteralToken(content), .. as rest] ->
            parse_template_literal(token_list)

        [TemplateHead(content), .. as rest] ->
            parse_template_literal(token_list)

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
        [AsyncKeyword, FunctionKeyword, AsteriskToken, .. as rest1] ->
            parse_async_generator_function_expression(rest1)

        [AsyncKeyword, FunctionKeyword, .. as rest1] ->
            parse_async_function_expression(rest1)

        [FunctionKeyword, AsteriskToken, .. as rest1] ->
            parse_generator_function_expression(rest1)

        # Async arrow functions - async x => ... or async (x) => ...
        [AsyncKeyword, .. as rest1] ->
            # Look ahead manually to see if this is an arrow function pattern
            when rest1 is
                # Pattern: async identifier => ...
                [IdentifierToken(param_name), EqualsGreaterThanToken, .. as arrow_rest] ->
                    param_node = Identifier({ name: param_name })
                    parse_async_arrow_function_body(param_node, arrow_rest)

                # Pattern: async (params) => ...
                [OpenParenToken, .. as paren_rest] ->
                    # Parse the content inside parentheses, then look for arrow
                    (params_node, after_paren_content) = parse_expression(Nud, 0, paren_rest)
                    when after_paren_content is
                        [CloseParenToken, EqualsGreaterThanToken, .. as arrow_rest] ->
                            parse_async_arrow_function_body(params_node, arrow_rest)
                        _ ->
                            # Not an arrow function, treat async as regular identifier
                            async_id = Identifier({ name: "async" })
                            (async_id, rest1)

                _ ->
                    # Not an arrow function pattern, treat async as regular identifier
                    async_id = Identifier({ name: "async" })
                    (async_id, rest1)

        [FunctionKeyword, .. as rest1] ->
            parse_function_expression(rest1)

        # New expressions
        [NewKeyword, .. as rest1] ->
            parse_new_expression(rest1)

        [OpenParenToken, .. as rest1] ->
            when rest1 is
                # Empty parentheses - could be arrow function params
                [CloseParenToken, .. as rest2] ->
                    when rest2 is
                        [EqualsGreaterThanToken, ..] ->
                            # This is an arrow function with empty params: () =>
                            # Create a special marker for empty params
                            empty_params = Error({ message: "EMPTY_ARROW_PARAMS" })
                            (empty_params, rest2)
                        
                        _ ->
                            # Empty parentheses but not arrow function - this is an error
                            (Error({ message: "Empty parentheses with no expression" }), rest2)
                
                _ ->
                    # Non-empty parentheses - parse as parenthesized expression
                    (node, remaining_tokens) = parse_expression(Nud, 0, rest1)
                    when remaining_tokens is
                        [CloseParenToken, .. as rest2] ->
                            (node, rest2)

                        [tok, ..] ->
                            token_name = Token.ts_token_debug_display(tok)
                            (
                                Error({ message: "parse_expression() failed -- Expected close paren, but got $(token_name)" }),
                                remaining_tokens,
                            )

                        [] ->
                            (
                                Error({ message: "parse_expression() failed -- Expected close paren, but got nothing" }),
                                remaining_tokens,
                            )

        # Await expressions
        [AwaitKeyword, .. as rest1] ->
            (argument, rest2) = parse_expression(Nud, 1600, rest1)  # High precedence for await
            await_expr = AwaitExpression({ argument: argument })
            (await_expr, rest2)

        # Yield expressions
        [YieldKeyword, AsteriskToken, .. as rest1] ->
            # yield* expression (delegate)
            (argument, rest2) = parse_expression(Nud, 1600, rest1)  # High precedence for yield
            yield_expr = YieldExpression({ argument: argument, delegate: Bool.true })
            (yield_expr, rest2)

        [YieldKeyword, .. as rest1] ->
            # yield expression (non-delegate)
            (argument, rest2) = parse_expression(Nud, 1600, rest1)  # High precedence for yield
            yield_expr = YieldExpression({ argument: argument, delegate: Bool.false })
            (yield_expr, rest2)

        [CloseParenToken, .. as rest] ->
            (Error({ message: "Unexpected close paren" }), rest)

        [] ->
            (Error({ message: "parse_primary_expression: Empty token list" }), [])

        [tok, .. as rest] ->
            token_name = Token.ts_token_debug_display(tok)
            (Error({ message: "parse_primary_expression: Unexpected token $(token_name)" }), rest)

parse_expression_led : Node, U16, List Token -> (Node, List Token)
parse_expression_led = |left_node, min_precedence, token_list|
    when token_list is
                # Sequence operator (comma) - lowest precedence
                [CommaToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Led({ left_node }), tok)
                    when Num.compare(expr_precedence, min_precedence) is
                        LT -> (left_node, token_list)
                        _ ->
                            (right_node, rest2) = parse_expression(Nud, expr_precedence + 1, rest1)
                            sequence_node =
                                when left_node is
                                    SequenceExpression(seq_data) ->
                                        # If left is already a sequence, add to its expressions
                                        SequenceExpression({
                                            expressions: List.append(seq_data.expressions, right_node)
                                        })
                                    _ ->
                                        # Create new sequence with left and right
                                        SequenceExpression({
                                            expressions: [left_node, right_node]
                                        })
                            parse_expression_led(sequence_node, min_precedence, rest2)

                # Logical operators
                [AmpersandAmpersandToken as tok, .. as rest1]
                | [BarBarToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Led({ left_node }), tok)
                    when Num.compare(expr_precedence, min_precedence) is
                        LT -> (left_node, token_list)
                        _ ->
                            (right_node, rest2) = parse_expression(Nud, expr_precedence + 1, rest1)
                            logical_node = LogicalExpression(
                                {
                                    left: left_node,
                                    operator: token_to_logical_operator(tok),
                                    right: right_node,
                                },
                            )
                            parse_expression_led(logical_node, min_precedence, rest2)

                # Binary operators (left associative)
                # Bitwise
                [AmpersandToken as tok, .. as rest1]
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
                | [PercentToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Led({ left_node }), tok)
                    # Check precedence - if current operator has lower precedence than min, stop
                    when Num.compare(expr_precedence, min_precedence) is
                        LT -> (left_node, token_list)
                        _ ->
                            (right_node, rest2) = parse_expression(Nud, expr_precedence + 1, rest1)
                            binary_node = BinaryExpression(
                                {
                                    left: left_node,
                                    operator: token_to_binary_operator(tok),
                                    right: right_node,
                                },
                            )
                            # Continue parsing at this level
                            parse_expression_led(binary_node, min_precedence, rest2)

                # Assignment operators
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
                | [CaretEqualsToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Led({ left_node }), tok)
                    (right_node, rest2) = parse_expression(Nud, expr_precedence, rest1)
                    assignment_node = AssignmentExpression(
                        {
                            left: left_node,
                            operator: token_to_assignment_operator(tok),
                            right: right_node,
                        },
                    )
                    (assignment_node, rest2)

                # Conditional operator (ternary)
                [QuestionToken, .. as rest1] ->
                    # Parse the consequent (true case)
                    (consequent, rest2) = parse_expression(Nud, 0, rest1)
                    when rest2 is
                        [ColonToken, .. as rest3] ->
                            # Parse the alternate (false case)
                            expr_precedence = get_expr_precedence(Led({ left_node }), QuestionToken)
                            (alternate, rest4) = parse_expression(Nud, expr_precedence, rest3)
                            conditional_node = ConditionalExpression(
                                {
                                    test: left_node,
                                    consequent: consequent,
                                    alternate: alternate,
                                },
                            )
                            (conditional_node, rest4)

                        _ ->
                            (Error({ message: "Expected ':' after '?' in conditional expression" }), rest2)

                # Postfix (update expressions)
                [PlusPlusToken as tok, .. as rest1]
                | [MinusMinusToken as tok, .. as rest1] ->
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
                    (call_expr, rest2) = parse_function_call(left_node, rest1)
                    # Continue parsing for additional operators (like sequence expressions)
                    parse_expression_led(call_expr, min_precedence, rest2)

                # Tagged template literals
                [NoSubstitutionTemplateLiteralToken(_), .. as rest1]
                | [TemplateHead(_), .. as rest1] ->
                    (template, rest2) = parse_template_literal(token_list)
                    tagged_template = TaggedTemplateExpression({ tag: left_node, quasi: template })
                    parse_expression_led(tagged_template, min_precedence, rest2)

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

                [] -> (left_node, [])
                _ -> (left_node, token_list)

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
    Sequence, # ","
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
        Sequence -> 50
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
                # Sequence
                CommaToken -> Sequence
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

token_to_assignment_operator : Token -> AssignmentOperator
token_to_assignment_operator = |token|
    when token is
        EqualsToken -> Equal
        PlusEqualsToken -> PlusEqual
        MinusEqualsToken -> MinusEqual
        AsteriskEqualsToken -> StarEqual
        AsteriskAsteriskEqualsToken -> StarEqual  # Note: ** operator needs special handling
        SlashEqualsToken -> SlashEqual
        PercentEqualsToken -> PercentEqual
        LessThanLessThanEqualsToken -> LeftShiftEqual
        GreaterThanGreaterThanEqualsToken -> RightShiftEqual
        GreaterThanGreaterThanGreaterThanEqualsToken -> UnsignedRightShiftEqual
        AmpersandEqualsToken -> AmpersandEqual
        BarEqualsToken -> PipeEqual
        CaretEqualsToken -> CaretEqual
        # Note: Logical assignment operators (&&=, ||=, ??=) may need new AST nodes
        BarBarEqualsToken -> PipeEqual  # Placeholder
        AmpersandAmpersandEqualsToken -> AmpersandEqual  # Placeholder
        QuestionQuestionEqualsToken -> Equal  # Placeholder
        _ -> crash("token_to_assignment_operator() failed -- This should never happen")

token_to_logical_operator : Token -> LogicalOperator
token_to_logical_operator = |token|
    when token is
        AmpersandAmpersandToken -> LogicalAnd
        BarBarToken -> LogicalOr
        _ -> crash("token_to_logical_operator() failed -- This should never happen")

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

        [DotDotDotToken, .. as rest1] ->
            # Spread element: ...expression
            (argument, rest2) = parse_expression(Nud, 100, rest1)
            spread_element = SpreadElement({ argument: argument })
            when rest2 is
                [CommaToken, .. as rest3] ->
                    parse_array_elements(List.append(elements, spread_element), rest3)

                [CloseBracketToken, .. as rest3] ->
                    final_elements = List.append(elements, spread_element)
                    (ArrayExpression({ elements: final_elements }), rest3)

                _ ->
                    (Error({ message: "Expected comma or close bracket after spread element" }), rest2)

        _ ->
            # Parse expression with precedence higher than sequence (50) to avoid treating commas as sequence operators
            (element, remaining_tokens) = parse_expression(Nud, 100, token_list)
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
    when token_list is
        [DotDotDotToken, .. as rest1] ->
            # Spread element: ...expression
            (argument, rest2) = parse_expression(Nud, 60, rest1)  # Use same precedence as destructuring defaults
            spread_element = SpreadElement({ argument: argument })
            (spread_element, rest2)

        _ ->
            # Parse the key
            (key, rest1) = parse_property_key(token_list)
            when rest1 is
                [ColonToken, .. as rest2] ->
                    # Parse the value
                    # Use precedence 60 to stop at comma (sequence expressions have precedence 50)
                    (value, rest3) = parse_expression(Nud, 60, rest2)
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
            # Continue parsing for additional operators (like function calls)
            parse_expression_led(member_expr, 0, rest)

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
            # Continue parsing for additional operators (like function calls)
            parse_expression_led(member_expr, 0, rest2)

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

        # Check for const enum before const variable
        [ConstKeyword, EnumKeyword, ..] ->
            parse_enum_declaration(token_list)

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

        # Switch statement
        [SwitchKeyword, .. as rest] ->
            parse_switch_statement(rest)

        # Async generator function declaration
        [AsyncKeyword, FunctionKeyword, AsteriskToken, .. as rest] ->
            parse_async_generator_function_declaration(rest)

        # Async function declaration
        [AsyncKeyword, FunctionKeyword, .. as rest] ->
            parse_async_function_declaration(rest)

        # Generator function declaration
        [FunctionKeyword, AsteriskToken, .. as rest] ->
            parse_generator_function_declaration(rest)

        # Function declaration
        [FunctionKeyword, .. as rest] ->
            parse_function_declaration(rest)

        # Class declaration
        [ClassKeyword, .. as rest] ->
            parse_class_declaration(rest)

        # TypeScript interface declaration
        [InterfaceKeyword, .. as rest] ->
            parse_interface_declaration(rest)

        # TypeScript type alias declaration
        [TypeKeyword, .. as rest] ->
            parse_type_alias_declaration(rest)

        # TypeScript enum declaration
        [EnumKeyword, .. as rest] ->
            parse_enum_declaration(token_list)


        # Import declaration
        [ImportKeyword, .. as rest] ->
            parse_import_declaration(rest)

        # Export declaration
        [ExportKeyword, .. as rest] ->
            parse_export_declaration(rest)

        # Return statement
        [ReturnKeyword, .. as rest] ->
            parse_return_statement(rest)

        # Break statement
        [BreakKeyword, .. as rest] ->
            parse_break_statement(rest)

        # Continue statement
        [ContinueKeyword, .. as rest] ->
            parse_continue_statement(rest)

        # Throw statement
        [ThrowKeyword, .. as rest] ->
            parse_throw_statement(rest)

        # Try statement
        [TryKeyword, .. as rest] ->
            parse_try_statement(rest)

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

            # Check for type annotation: let name: type
            (type_annotation, remaining_after_type) = when rest1 is
                [ColonToken, .. as rest2] ->
                    (type_node, remaining) = parse_type_annotation(rest2)
                    (Some(type_node), remaining)
                _ ->
                    # No type annotation
                    (None, rest1)

            when remaining_after_type is
                [EqualsToken, .. as rest2] ->
                    # Has initializer
                    (init_expr, rest3) = parse_expression(Nud, 0, rest2)
                    declarator = VariableDeclarator(
                        {
                            id: identifier,
                            init: Some(init_expr),
                            typeAnnotation: type_annotation,
                        },
                    )
                    (declarator, rest3)

                _ ->
                    # No initializer
                    declarator = VariableDeclarator(
                        {
                            id: identifier,
                            init: None,
                            typeAnnotation: type_annotation,
                        },
                    )
                    (declarator, remaining_after_type)

        # Array destructuring: [a, b] = expr
        [OpenBracketToken, .. as rest1] ->
            (pattern, rest2) = parse_array_pattern(rest1)
            when rest2 is
                [EqualsToken, .. as rest3] ->
                    # Has initializer (required for destructuring)
                    (init_expr, rest4) = parse_expression(Nud, 0, rest3)
                    declarator = VariableDeclarator(
                        {
                            id: pattern,
                            init: Some(init_expr),
                            typeAnnotation: None,
                        },
                    )
                    (declarator, rest4)

                _ ->
                    # Destructuring requires initializer
                    (Error({ message: "Destructuring declaration must have an initializer" }), rest2)

        # Object destructuring: {a, b} = expr
        [OpenBraceToken, .. as rest1] ->
            (pattern, rest2) = parse_object_pattern(rest1)
            when rest2 is
                [EqualsToken, .. as rest3] ->
                    # Has initializer (required for destructuring)
                    (init_expr, rest4) = parse_expression(Nud, 0, rest3)
                    declarator = VariableDeclarator(
                        {
                            id: pattern,
                            init: Some(init_expr),
                            typeAnnotation: None,
                        },
                    )
                    (declarator, rest4)

                _ ->
                    # Destructuring requires initializer
                    (Error({ message: "Destructuring declaration must have an initializer" }), rest2)

        _ ->
            (Error({ message: "Expected identifier or destructuring pattern in variable declaration" }), token_list)

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
            # First, try to parse the left side and see if it's followed by 'in' or 'of'
            (left_side, rest_after_left) = parse_for_loop_left(rest1)

            when rest_after_left is
                # for...of loop
                [OfKeyword, .. as rest2] ->
                    (right_expr, rest3) = parse_expression(Nud, 0, rest2)
                    when rest3 is
                        [CloseParenToken, .. as rest4] ->
                            (body, rest5) = parse_statement(rest4)
                            for_of_stmt = ForOfStatement({
                                left: left_side,
                                right: right_expr,
                                body: body,
                            })
                            (for_of_stmt, rest5)
                        _ ->
                            error_stmt = ForOfStatement({
                                left: left_side,
                                right: Error({ message: "Expected ')' after for...of expression" }),
                                body: Error({ message: "Missing body" }),
                            })
                            (error_stmt, rest3)

                # for...in loop
                [InKeyword, .. as rest2] ->
                    (right_expr, rest3) = parse_expression(Nud, 0, rest2)
                    when rest3 is
                        [CloseParenToken, .. as rest4] ->
                            (body, rest5) = parse_statement(rest4)
                            for_in_stmt = ForInStatement({
                                left: left_side,
                                right: right_expr,
                                body: body,
                            })
                            (for_in_stmt, rest5)
                        _ ->
                            error_stmt = ForInStatement({
                                left: left_side,
                                right: Error({ message: "Expected ')' after for...in expression" }),
                                body: Error({ message: "Missing body" }),
                            })
                            (error_stmt, rest3)

                # Traditional for loop - parse semicolon-separated parts
                [SemicolonToken, .. as rest2] ->
                    parse_traditional_for_loop(Some(left_side), rest2)

                _ ->
                    # For traditional for loops, we need to find and consume the semicolon after init
                    when rest_after_left is
                        [SemicolonToken, .. as rest_after_semi] ->
                            parse_traditional_for_loop(Some(left_side), rest_after_semi)
                        _ ->
                            # Not a traditional for loop - this is an error
                            error_stmt = ForStatement({
                                init: Some(left_side),
                                test: None,
                                update: None,
                                body: Error({ message: "Expected ';', 'in', or 'of' in for loop" }),
                            })
                            (error_stmt, rest_after_left)

        _ ->
            (Error({ message: "Expected open paren after for" }), token_list)

parse_for_loop_left : List Token -> (Node, List Token)
parse_for_loop_left = |token_list|
    when token_list is
        [VarKeyword, .. as rest] ->
            parse_for_variable_declaration(Var, rest)

        [LetKeyword, .. as rest] ->
            parse_for_variable_declaration(Let, rest)

        [ConstKeyword, .. as rest] ->
            parse_for_variable_declaration(Const, rest)

        _ ->
            parse_expression(Nud, 0, token_list)

# Special version for for loops that doesn't consume semicolons
parse_for_variable_declaration : VariableDeclarationKind, List Token -> (Node, List Token)
parse_for_variable_declaration = |kind, token_list|
    (declarator, rest1) = parse_variable_declarator(token_list)
    var_decl = VariableDeclaration({
        declarations: [declarator],
        kind: kind,
    })
    (var_decl, rest1)

parse_traditional_for_loop : Option Node, List Token -> (Node, List Token)
parse_traditional_for_loop = |init, token_list|
    # Parse test condition (expression between first and second semicolon)
    (test, rest2) =
        when token_list is
            [SemicolonToken, .. as rest] ->
                # Empty test condition
                (None, rest)

            _ ->
                # Parse expression until we hit a semicolon
                (test_expr, remaining) = parse_expression_until_semicolon(token_list)
                when remaining is
                    [SemicolonToken, .. as rest] ->
                        (Some(test_expr), rest)
                    _ ->
                        # Error: missing semicolon
                        (Some(test_expr), remaining)

    # Parse update expression (expression between second semicolon and close paren)
    (update, rest3) =
        when rest2 is
            [CloseParenToken, .. as rest] ->
                # Empty update expression
                (None, rest)

            _ ->
                # Parse expression until we hit a close paren
                (update_expr, remaining) = parse_expression_until_close_paren(rest2)
                when remaining is
                    [CloseParenToken, .. as rest] ->
                        (Some(update_expr), rest)
                    _ ->
                        # Error: missing close paren
                        (Some(update_expr), remaining)

    # Parse body
    (body, rest4) = parse_statement(rest3)
    for_stmt = ForStatement({
        init: init,
        test: test,
        update: update,
        body: body,
    })
    (for_stmt, rest4)

# Parse expression until semicolon (for for-loop test condition)
parse_expression_until_semicolon : List Token -> (Node, List Token)
parse_expression_until_semicolon = |token_list|
    parse_expression_until_token(token_list, SemicolonToken)

# Parse expression until close paren (for for-loop update expression)
parse_expression_until_close_paren : List Token -> (Node, List Token)
parse_expression_until_close_paren = |token_list|
    parse_expression_until_token(token_list, CloseParenToken)

# Generic helper to parse expression until a specific token
parse_expression_until_token : List Token, Token -> (Node, List Token)
parse_expression_until_token = |token_list, end_token|
    # Collect tokens until we find the end token
    collect_until_token(token_list, end_token, [])

collect_until_token : List Token, Token, List Token -> (Node, List Token)
collect_until_token = |token_list, end_token, acc|
    when token_list is
        [] ->
            # End of tokens - parse what we have
            if List.len(acc) > 0 then
                parse_expression(Nud, 0, acc)
            else
                (Error({ message: "Empty expression" }), [])

        [token, .. as rest] ->
            if token == end_token then
                # Found the end token - parse accumulated tokens
                if List.len(acc) > 0 then
                    (expr, _) = parse_expression(Nud, 0, acc)
                    (expr, [token] |> List.concat(rest))
                else
                    (Error({ message: "Empty expression" }), [token] |> List.concat(rest))
            else
                # Continue collecting
                new_acc = List.append(acc, token)
                collect_until_token(rest, end_token, new_acc)

parse_function_declaration : List Token -> (Node, List Token)
parse_function_declaration = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)

            # Check for return type annotation: function name(): type
            (return_type, rest3) = when rest2 is
                [ColonToken, .. as rest_after_colon] ->
                    (type_node, remaining) = parse_type_annotation(rest_after_colon)
                    (Some(type_node), remaining)
                _ ->
                    # No return type annotation
                    (None, rest2)

            (body, rest4) = parse_function_body(rest3)
            func_decl = FunctionDeclaration(
                {
                    id: identifier,
                    params: params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.false,
                },
            )
            (func_decl, rest4)

        _ ->
            crash("parse_function_declaration() failed -- This should never happen")

parse_async_function_declaration : List Token -> (Node, List Token)
parse_async_function_declaration = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)

            # Check for return type annotation: async function name(): type
            (return_type, rest3) = when rest2 is
                [ColonToken, .. as rest_after_colon] ->
                    (type_node, remaining) = parse_type_annotation(rest_after_colon)
                    (Some(type_node), remaining)
                _ ->
                    # No return type annotation
                    (None, rest2)

            (body, rest4) = parse_function_body(rest3)
            func_decl = FunctionDeclaration(
                {
                    id: identifier,
                    params: params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.true,
                },
            )
            (func_decl, rest4)

        _ ->
            crash("parse_async_function_declaration() failed -- This should never happen")

parse_generator_function_declaration : List Token -> (Node, List Token)
parse_generator_function_declaration = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)

            # Check for return type annotation: function* name(): type
            (return_type, rest3) = when rest2 is
                [ColonToken, .. as rest_after_colon] ->
                    (type_node, remaining) = parse_type_annotation(rest_after_colon)
                    (Some(type_node), remaining)
                _ ->
                    # No return type annotation
                    (None, rest2)

            (body, rest4) = parse_function_body(rest3)
            func_decl = FunctionDeclaration(
                {
                    id: identifier,
                    params: params,
                    body: body,
                    generator: Bool.true,
                    async: Bool.false,
                },
            )
            (func_decl, rest4)

        _ ->
            crash("parse_generator_function_declaration() failed -- This should never happen")

parse_async_generator_function_declaration : List Token -> (Node, List Token)
parse_async_generator_function_declaration = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)

            # Check for return type annotation: async function* name(): type
            (return_type, rest3) = when rest2 is
                [ColonToken, .. as rest_after_colon] ->
                    (type_node, remaining) = parse_type_annotation(rest_after_colon)
                    (Some(type_node), remaining)
                _ ->
                    # No return type annotation
                    (None, rest2)

            (body, rest4) = parse_function_body(rest3)
            func_decl = FunctionDeclaration(
                {
                    id: identifier,
                    params: params,
                    body: body,
                    generator: Bool.true,
                    async: Bool.true,
                },
            )
            (func_decl, rest4)

        _ ->
            crash("parse_async_generator_function_declaration() failed -- This should never happen")

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

parse_async_function_expression : List Token -> (Node, List Token)
parse_async_function_expression = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            # Named async function expression
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: Some(identifier),
                    params: params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.true,
                },
            )
            (func_expr, rest3)

        [OpenParenToken, .. as rest1] ->
            # Anonymous async function expression
            (params, rest2) = parse_function_parameters(token_list)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: None,
                    params: params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.true,
                },
            )
            (func_expr, rest3)

        _ ->
            (Error({ message: "Expected async function parameters or name" }), token_list)

parse_generator_function_expression : List Token -> (Node, List Token)
parse_generator_function_expression = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            # Named generator function expression
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: Some(identifier),
                    params: params,
                    body: body,
                    generator: Bool.true,
                    async: Bool.false,
                },
            )
            (func_expr, rest3)

        [OpenParenToken, .. as rest1] ->
            # Anonymous generator function expression
            (params, rest2) = parse_function_parameters(token_list)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: None,
                    params: params,
                    body: body,
                    generator: Bool.true,
                    async: Bool.false,
                },
            )
            (func_expr, rest3)

        _ ->
            (Error({ message: "Expected generator function parameters or name" }), token_list)

parse_async_generator_function_expression : List Token -> (Node, List Token)
parse_async_generator_function_expression = |token_list|
    when token_list is
        [IdentifierToken(name), .. as rest1] ->
            # Named async generator function expression
            identifier = Identifier({ name: name })
            (params, rest2) = parse_function_parameters(rest1)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: Some(identifier),
                    params: params,
                    body: body,
                    generator: Bool.true,
                    async: Bool.true,
                },
            )
            (func_expr, rest3)

        [OpenParenToken, .. as rest1] ->
            # Anonymous async generator function expression
            (params, rest2) = parse_function_parameters(token_list)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression(
                {
                    id: None,
                    params: params,
                    body: body,
                    generator: Bool.true,
                    async: Bool.true,
                },
            )
            (func_expr, rest3)

        _ ->
            (Error({ message: "Expected async generator function parameters or name" }), token_list)

parse_new_expression : List Token -> (Node, List Token)
parse_new_expression = |token_list|
    # Parse the constructor (callee)
    (callee, rest1) = parse_primary_expression(token_list)

    # Check if there are arguments (optional for new expressions)
    when rest1 is
        [OpenParenToken, .. as rest2] ->
            # Parse arguments like a function call
            parse_new_arguments(callee, [], rest2)

        _ ->
            # New expression without arguments (like `new Date`)
            new_expr = NewExpression(
                {
                    callee: callee,
                    arguments: [],
                },
            )
            (new_expr, rest1)

parse_new_arguments : Node, List Node, List Token -> (Node, List Token)
parse_new_arguments = |callee, arguments, token_list|
    when token_list is
        [CloseParenToken, .. as rest] ->
            new_expr = NewExpression(
                {
                    callee: callee,
                    arguments: arguments,
                },
            )
            (new_expr, rest)

        [] ->
            (Error({ message: "Unexpected end of new expression" }), [])

        _ ->
            (arg, remaining_tokens) = parse_expression(Nud, 0, token_list)
            when remaining_tokens is
                [CommaToken, .. as rest] ->
                    parse_new_arguments(callee, List.append(arguments, arg), rest)

                [CloseParenToken, .. as rest] ->
                    final_args = List.append(arguments, arg)
                    new_expr = NewExpression(
                        {
                            callee: callee,
                            arguments: final_args,
                        },
                    )
                    (new_expr, rest)

                _ ->
                    (Error({ message: "Expected comma or close paren in new expression" }), remaining_tokens)

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
            # Check for type annotation: param: type
            (param_with_type, remaining_after_type) = when rest1 is
                [ColonToken, .. as rest2] ->
                    (type_annotation, remaining) = parse_type_annotation(rest2)
                    identifier_with_type = Identifier({ name: param_name })
                    # For now, we'll create a simple identifier but in a full implementation
                    # we'd need a TypeScript-specific parameter node with type annotation
                    (identifier_with_type, remaining)
                _ ->
                    # No type annotation
                    (Identifier({ name: param_name }), rest1)

            # Check for default value: param = defaultValue
            (param, remaining_tokens) = when remaining_after_type is
                [EqualsToken, .. as rest2] ->
                    # Parse default value with precedence 60 to stop at comma
                    (default_expr, remaining) = parse_expression(Nud, 60, rest2)
                    assignment_pattern = AssignmentPattern({
                        left: param_with_type,
                        right: default_expr,
                    })
                    (assignment_pattern, remaining)
                _ ->
                    # No default value
                    (param_with_type, remaining_after_type)

            new_params = List.append(params, param)
            when remaining_tokens is
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

        # Array destructuring parameter: [a, b]
        [OpenBracketToken, .. as rest1] ->
            (pattern, rest2) = parse_array_pattern(rest1)
            new_params = List.append(params, pattern)
            when rest2 is
                [CommaToken, .. as rest3] ->
                    parse_parameter_list(new_params, rest3)

                [CloseParenToken, .. as rest3] ->
                    (new_params, rest3)

                _ ->
                    (
                        List.append(
                            new_params,
                            Error({ message: "Expected comma or close paren after destructuring parameter" }),
                        ),
                        rest2,
                    )

        # Object destructuring parameter: {a, b}
        [OpenBraceToken, .. as rest1] ->
            (pattern, rest2) = parse_object_pattern(rest1)
            new_params = List.append(params, pattern)
            when rest2 is
                [CommaToken, .. as rest3] ->
                    parse_parameter_list(new_params, rest3)

                [CloseParenToken, .. as rest3] ->
                    (new_params, rest3)

                _ ->
                    (
                        List.append(
                            new_params,
                            Error({ message: "Expected comma or close paren after destructuring parameter" }),
                        ),
                        rest2,
                    )

        # Rest parameter: ...param
        [DotDotDotToken, .. as rest1] ->
            when rest1 is
                [IdentifierToken(param_name), .. as rest2] ->
                    rest_param = RestElement({ argument: Identifier({ name: param_name }) })
                    new_params = List.append(params, rest_param)
                    # Rest parameter must be last (semantically, but we allow parsing)
                    when rest2 is
                        [CloseParenToken, .. as rest3] ->
                            (new_params, rest3)
                        [CommaToken, .. as rest3] ->
                            # Continue parsing (semantically invalid but syntactically allowed)
                            parse_parameter_list(new_params, rest3)
                        _ ->
                            # Continue parsing remaining tokens
                            parse_parameter_list(new_params, rest2)
                _ ->
                    # Error: rest parameter needs identifier
                    error_param = RestElement({ argument: Error({ message: "Expected identifier after ..." }) })
                    new_params = List.append(params, error_param)
                    parse_parameter_list(new_params, rest1)

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

parse_class_declaration : List Token -> (Node, List Token)
parse_class_declaration = |token_list|
    when token_list is
        [IdentifierToken(class_name), .. as rest1] ->
            class_id = Identifier({ name: class_name })

            # Check for extends clause
            (super_class, rest2) = when rest1 is
                [ExtendsKeyword, IdentifierToken(super_name), .. as rest] ->
                    super_id = Identifier({ name: super_name })
                    (Some(super_id), rest)
                _ ->
                    (None, rest1)

            # Parse class body
            (class_body, rest3) = parse_class_body(rest2)

            class_decl = ClassDeclaration({
                id: class_id,
                superClass: super_class,
                body: class_body,
            })
            (class_decl, rest3)

        _ ->
            (Error({ message: "Expected class name after 'class' keyword" }), token_list)

parse_class_body : List Token -> (Node, List Token)
parse_class_body = |token_list|
    when token_list is
        [OpenBraceToken, .. as rest] ->
            # For now, create an empty class body
            # TODO: Parse class methods and properties
            (body, remaining_tokens) = parse_class_body_statements([], rest)
            (body, remaining_tokens)

        _ ->
            (Error({ message: "Expected '{' to start class body" }), token_list)

parse_class_body_statements : List Node, List Token -> (Node, List Token)
parse_class_body_statements = |statements, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            # Create a simple block statement for the class body
            block = BlockStatement({ body: statements })
            (block, rest)

        [] ->
            (Error({ message: "Expected '}' to close class body" }), [])

        # Parse constructor method
        [ConstructorKeyword, .. as rest] ->
            (method_def, remaining) = parse_method_definition(Constructor, rest)
            new_statements = List.append(statements, method_def)
            parse_class_body_statements(new_statements, remaining)

        # Parse regular method (identifier followed by parentheses)
        [IdentifierToken(method_name), OpenParenToken, .. as rest] ->
            method_key = Identifier({ name: method_name })
            (method_def, remaining) = parse_method_definition_with_key(Method, method_key, List.prepend(rest, OpenParenToken))
            new_statements = List.append(statements, method_def)
            parse_class_body_statements(new_statements, remaining)

        # Skip other tokens for now (getters, setters, properties, etc.)
        [_, .. as rest] ->
            parse_class_body_statements(statements, rest)

parse_method_definition : MethodKind, List Token -> (Node, List Token)
parse_method_definition = |kind, token_list|
    when token_list is
        [OpenParenToken, .. as rest] ->
            method_key = when kind is
                Constructor -> Identifier({ name: "constructor" })
                _ -> Identifier({ name: "method" })
            parse_method_definition_with_key(kind, method_key, token_list)
        _ ->
            (Error({ message: "Expected '(' after method name" }), token_list)

parse_method_definition_with_key : MethodKind, Node, List Token -> (Node, List Token)
parse_method_definition_with_key = |kind, key, token_list|
    # Parse method as an anonymous function expression
    when token_list is
        [OpenParenToken, .. as rest] ->
            (params, rest2) = parse_function_parameters(token_list)
            (body, rest3) = parse_function_body(rest2)
            func_expr = FunctionExpression({
                id: None,
                params: params,
                body: body,
                generator: Bool.false,
                async: Bool.false,
            })
            method_def = MethodDefinition({
                key: key,
                value: func_expr,
                kind: kind,
                computed: Bool.false,
                static: Bool.false,
            })
            (method_def, rest3)

        _ ->
            error_method = MethodDefinition({
                key: key,
                value: Error({ message: "Expected '(' for method parameters" }),
                kind: kind,
                computed: Bool.false,
                static: Bool.false,
            })
            (error_method, token_list)

parse_return_statement : List Token -> (Node, List Token)
parse_return_statement = |token_list|
    when token_list is
        # Return with no argument (just return;)
        [SemicolonToken, .. as rest] ->
            return_stmt = ReturnStatement({ argument: None })
            (return_stmt, rest)

        # Return with argument
        _ ->
            (argument, rest1) = parse_expression(Nud, 0, token_list)
            # Consume optional semicolon
            rest2 = when rest1 is
                [SemicolonToken, .. as rest] -> rest
                _ -> rest1
            return_stmt = ReturnStatement({ argument: Some(argument) })
            (return_stmt, rest2)

parse_throw_statement : List Token -> (Node, List Token)
parse_throw_statement = |token_list|
    # Throw always requires an argument
    (argument, rest1) = parse_expression(Nud, 0, token_list)
    # Consume optional semicolon
    rest2 = when rest1 is
        [SemicolonToken, .. as rest] -> rest
        _ -> rest1
    throw_stmt = ThrowStatement({ argument: argument })
    (throw_stmt, rest2)

parse_try_statement : List Token -> (Node, List Token)
parse_try_statement = |token_list|
    # Parse try block
    (try_block, rest1) = parse_block_statement(token_list)

    # Parse optional catch clause
    (catch_clause, rest2) = when rest1 is
        [CatchKeyword, .. as rest] ->
            parse_catch_clause(rest)
        _ ->
            (None, rest1)

    # Parse optional finally clause
    (finally_clause, rest3) = when rest2 is
        [FinallyKeyword, .. as rest] ->
            (finally_block, remaining) = parse_block_statement(rest)
            (Some(finally_block), remaining)
        _ ->
            (None, rest2)

    try_stmt = TryStatement({
        block: try_block,
        handler: catch_clause,
        finalizer: finally_clause,
    })
    (try_stmt, rest3)

parse_catch_clause : List Token -> (Option Node, List Token)
parse_catch_clause = |token_list|
    when token_list is
        # Catch with parameter: catch (e) { ... }
        [OpenParenToken, IdentifierToken(param_name), CloseParenToken, .. as rest] ->
            param = Identifier({ name: param_name })
            (catch_body, remaining) = parse_block_statement(rest)
            catch_clause = CatchClause({
                param: Some(param),
                body: catch_body,
            })
            (Some(catch_clause), remaining)

        # Catch without parameter: catch { ... }
        [OpenBraceToken, ..] ->
            (catch_body, remaining) = parse_block_statement(token_list)
            catch_clause = CatchClause({
                param: None,
                body: catch_body,
            })
            (Some(catch_clause), remaining)

        _ ->
            (None, token_list)

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

parse_async_arrow_function_body : Node, List Token -> (Node, List Token)
parse_async_arrow_function_body = |params_node, token_list|
    # Convert the left side to parameters
    arrow_params = convert_to_arrow_params(params_node)

    when token_list is
        [OpenBraceToken, .. as rest] ->
            # Block body: async () => { statements }
            (body, remaining_tokens) = parse_block_statement(rest)
            arrow_fn = ArrowFunctionExpression(
                {
                    params: arrow_params,
                    body: body,
                    generator: Bool.false,
                    async: Bool.true,
                },
            )
            (arrow_fn, remaining_tokens)

        _ ->
            # Expression body: async () => expression
            (expr, remaining_tokens) = parse_expression(Nud, 0, token_list)
            arrow_fn = ArrowFunctionExpression(
                {
                    params: arrow_params,
                    body: expr,
                    generator: Bool.false,
                    async: Bool.true,
                },
            )
            (arrow_fn, remaining_tokens)

convert_to_arrow_params : Node -> List Node
convert_to_arrow_params = |node|
    when node is
        # Single identifier: x => ...
        Identifier(_) ->
            [node]

        # Empty parameters: () => ...
        Error(data) ->
            when data.message is
                "EMPTY_ARROW_PARAMS" -> []
                _ -> [node]

        # Parenthesized parameters: (a, b) => ...
        # This would come from a parenthesized expression that contains identifiers
        _ ->
            # For now, treat anything else as a single parameter
            # In a full implementation, we'd need to handle:
            # - (a, b) => ... (multiple params)
            # - (a = 1) => ... (default params)
            [node]

parse_template_literal : List Token -> (Node, List Token)
parse_template_literal = |token_list|
    when token_list is
        # Simple template literal without interpolation
        [NoSubstitutionTemplateLiteralToken(content), .. as rest] ->
            template_element = TemplateElement({ value: content, raw: content, tail: Bool.true })
            template = TemplateLiteral(
                {
                    quasis: [template_element],
                    expressions: [],
                },
            )
            (template, rest)

        # Template literal with interpolation - starts with TemplateHead
        [TemplateHead(content), .. as rest] ->
            template_element = TemplateElement({ value: content, raw: content, tail: Bool.false })
            parse_template_parts([template_element], [], rest)

        _ ->
            # Error case - unexpected token
            (Error({ message: "Expected template literal" }), token_list)

# Parse template literal parts (for interpolated templates)
parse_template_parts : List Node, List Node, List Token -> (Node, List Token)
parse_template_parts = |quasis, expressions, token_list|
    when token_list is
        # ${expression} pattern
        [DollarBraceToken, .. as rest1] ->
            # Parse the expression inside ${}
            (expr, rest2) = parse_expression(Nud, 0, rest1)
            new_expressions = List.append(expressions, expr)

            # Expect closing brace
            when rest2 is
                [CloseBraceToken, .. as rest3] ->
                    parse_template_continuation(quasis, new_expressions, rest3)

                _ ->
                    error_node = Error({ message: "Expected '}' after template expression" })
                    (error_node, rest2)

        _ ->
            # Error case - expected ${
            error_node = Error({ message: "Expected template expression" })
            (error_node, token_list)

# Parse continuation after a template expression
parse_template_continuation : List Node, List Node, List Token -> (Node, List Token)
parse_template_continuation = |quasis, expressions, token_list|
    when token_list is
        # Middle part: ${expr}middle${
        [TemplateMiddle(content), .. as rest] ->
            template_element = TemplateElement({ value: content, raw: content, tail: Bool.false })
            new_quasis = List.append(quasis, template_element)
            parse_template_parts(new_quasis, expressions, rest)

        # End part: ${expr}end`
        [TemplateTail(content), .. as rest] ->
            template_element = TemplateElement({ value: content, raw: content, tail: Bool.true })
            final_quasis = List.append(quasis, template_element)
            template = TemplateLiteral(
                {
                    quasis: final_quasis,
                    expressions: expressions,
                },
            )
            (template, rest)

        _ ->
            # Error case - expected TemplateMiddle or TemplateTail
            error_node = Error({ message: "Expected template continuation" })
            (error_node, token_list)

# Import declaration parsing
parse_import_declaration : List Token -> (Node, List Token)
parse_import_declaration = |token_list|
    when token_list is
        # import defaultExport from "module"
        [IdentifierToken(name), FromKeyword, StringLiteralToken(source), .. as rest] ->
            default_specifier = ImportDefaultSpecifier({ local: Identifier({ name: name }) })
            source_literal = StringLiteral({ value: source })
            import_decl = ImportDeclaration({
                specifiers: [default_specifier],
                source: source_literal,
            })
            rest_after_semi = when rest is
                [SemicolonToken, .. as after_semi] -> after_semi
                _ -> rest
            (import_decl, rest_after_semi)

        # import * as namespace from "module"
        [AsteriskToken, AsKeyword, IdentifierToken(name), FromKeyword, StringLiteralToken(source), .. as rest] ->
            namespace_specifier = ImportNamespaceSpecifier({ local: Identifier({ name: name }) })
            source_literal = StringLiteral({ value: source })
            import_decl = ImportDeclaration({
                specifiers: [namespace_specifier],
                source: source_literal,
            })
            rest_after_semi = when rest is
                [SemicolonToken, .. as after_semi] -> after_semi
                _ -> rest
            (import_decl, rest_after_semi)

        # import { named } from "module"
        [OpenBraceToken, .. as rest1] ->
            (specifiers, rest2) = parse_import_specifiers([], rest1)
            when rest2 is
                [FromKeyword, StringLiteralToken(source), .. as rest3] ->
                    source_literal = StringLiteral({ value: source })
                    import_decl = ImportDeclaration({
                        specifiers: specifiers,
                        source: source_literal,
                    })
                    rest_after_semi = when rest3 is
                        [SemicolonToken, .. as after_semi] -> after_semi
                        _ -> rest3
                    (import_decl, rest_after_semi)
                _ ->
                    error_import = ImportDeclaration({
                        specifiers: [],
                        source: Error({ message: "Expected 'from' clause in import" }),
                    })
                    (error_import, rest2)

        # import "module" (side-effect import)
        [StringLiteralToken(source), .. as rest] ->
            source_literal = StringLiteral({ value: source })
            import_decl = ImportDeclaration({
                specifiers: [],
                source: source_literal,
            })
            rest_after_semi = when rest is
                [SemicolonToken, .. as after_semi] -> after_semi
                _ -> rest
            (import_decl, rest_after_semi)

        _ ->
            error_import = ImportDeclaration({
                specifiers: [],
                source: Error({ message: "Invalid import syntax" }),
            })
            (error_import, token_list)

parse_import_specifiers : List Node, List Token -> (List Node, List Token)
parse_import_specifiers = |specifiers, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            (specifiers, rest)

        [IdentifierToken(imported), AsKeyword, IdentifierToken(local), .. as rest1] ->
            # import { foo as bar }
            specifier = ImportSpecifier({
                imported: Identifier({ name: imported }),
                local: Identifier({ name: local }),
            })
            new_specifiers = List.append(specifiers, specifier)
            when rest1 is
                [CommaToken, .. as rest2] ->
                    parse_import_specifiers(new_specifiers, rest2)
                _ ->
                    parse_import_specifiers(new_specifiers, rest1)

        [IdentifierToken(name), .. as rest1] ->
            # import { foo }
            specifier = ImportSpecifier({
                imported: Identifier({ name: name }),
                local: Identifier({ name: name }),
            })
            new_specifiers = List.append(specifiers, specifier)
            when rest1 is
                [CommaToken, .. as rest2] ->
                    parse_import_specifiers(new_specifiers, rest2)
                _ ->
                    parse_import_specifiers(new_specifiers, rest1)

        _ ->
            error_specifier = ImportSpecifier({
                imported: Error({ message: "Invalid import specifier" }),
                local: Error({ message: "Invalid import specifier" }),
            })
            new_specifiers = List.append(specifiers, error_specifier)
            (new_specifiers, token_list)

# Export declaration parsing
parse_export_declaration : List Token -> (Node, List Token)
parse_export_declaration = |token_list|
    when token_list is
        # export default declaration
        [DefaultKeyword, .. as rest] ->
            (declaration, rest1) = parse_expression(Nud, 0, rest)
            export_decl = ExportDefaultDeclaration({ declaration: declaration })
            rest_after_semi = when rest1 is
                [SemicolonToken, .. as after_semi] -> after_semi
                _ -> rest1
            (export_decl, rest_after_semi)

        # export * from "module"
        [AsteriskToken, FromKeyword, StringLiteralToken(source), .. as rest] ->
            source_literal = StringLiteral({ value: source })
            export_decl = ExportAllDeclaration({ source: source_literal })
            rest_after_semi = when rest is
                [SemicolonToken, .. as after_semi] -> after_semi
                _ -> rest
            (export_decl, rest_after_semi)

        # export { named } or export { named } from "module"
        [OpenBraceToken, .. as rest1] ->
            (specifiers, rest2) = parse_export_specifiers([], rest1)
            when rest2 is
                [FromKeyword, StringLiteralToken(source), .. as rest3] ->
                    # Re-export from another module
                    source_literal = StringLiteral({ value: source })
                    export_decl = ExportNamedDeclaration({
                        declaration: None,
                        specifiers: specifiers,
                        source: Some(source_literal),
                    })
                    rest_after_semi = when rest3 is
                        [SemicolonToken, .. as after_semi] -> after_semi
                        _ -> rest3
                    (export_decl, rest_after_semi)
                _ ->
                    # Export local bindings
                    export_decl = ExportNamedDeclaration({
                        declaration: None,
                        specifiers: specifiers,
                        source: None,
                    })
                    rest_after_semi = when rest2 is
                        [SemicolonToken, .. as after_semi] -> after_semi
                        _ -> rest2
                    (export_decl, rest_after_semi)

        # export var/let/const/function/class declaration
        [VarKeyword, .. as rest] ->
            (declaration, rest1) = parse_variable_declaration(Var, rest)
            export_decl = ExportNamedDeclaration({
                declaration: Some(declaration),
                specifiers: [],
                source: None,
            })
            (export_decl, rest1)

        [LetKeyword, .. as rest] ->
            (declaration, rest1) = parse_variable_declaration(Let, rest)
            export_decl = ExportNamedDeclaration({
                declaration: Some(declaration),
                specifiers: [],
                source: None,
            })
            (export_decl, rest1)

        [ConstKeyword, .. as rest] ->
            (declaration, rest1) = parse_variable_declaration(Const, rest)
            export_decl = ExportNamedDeclaration({
                declaration: Some(declaration),
                specifiers: [],
                source: None,
            })
            (export_decl, rest1)

        [FunctionKeyword, .. as rest] ->
            (declaration, rest1) = parse_function_declaration(rest)
            export_decl = ExportNamedDeclaration({
                declaration: Some(declaration),
                specifiers: [],
                source: None,
            })
            (export_decl, rest1)

        [ClassKeyword, .. as rest] ->
            (declaration, rest1) = parse_class_declaration(rest)
            export_decl = ExportNamedDeclaration({
                declaration: Some(declaration),
                specifiers: [],
                source: None,
            })
            (export_decl, rest1)

        _ ->
            error_export = ExportNamedDeclaration({
                declaration: None,
                specifiers: [],
                source: Some(Error({ message: "Invalid export syntax" })),
            })
            (error_export, token_list)

parse_export_specifiers : List Node, List Token -> (List Node, List Token)
parse_export_specifiers = |specifiers, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            (specifiers, rest)

        [IdentifierToken(local), AsKeyword, IdentifierToken(exported), .. as rest1] ->
            # export { foo as bar }
            specifier = ExportSpecifier({
                local: Identifier({ name: local }),
                exported: Identifier({ name: exported }),
            })
            new_specifiers = List.append(specifiers, specifier)
            when rest1 is
                [CommaToken, .. as rest2] ->
                    parse_export_specifiers(new_specifiers, rest2)
                _ ->
                    parse_export_specifiers(new_specifiers, rest1)

        [IdentifierToken(name), .. as rest1] ->
            # export { foo }
            specifier = ExportSpecifier({
                local: Identifier({ name: name }),
                exported: Identifier({ name: name }),
            })
            new_specifiers = List.append(specifiers, specifier)
            when rest1 is
                [CommaToken, .. as rest2] ->
                    parse_export_specifiers(new_specifiers, rest2)
                _ ->
                    parse_export_specifiers(new_specifiers, rest1)

        _ ->
            error_specifier = ExportSpecifier({
                local: Error({ message: "Invalid export specifier" }),
                exported: Error({ message: "Invalid export specifier" }),
            })
            new_specifiers = List.append(specifiers, error_specifier)
            (new_specifiers, token_list)

# Switch statement parsing
parse_switch_statement : List Token -> (Node, List Token)
parse_switch_statement = |token_list|
    when token_list is
        [OpenParenToken, .. as rest1] ->
            # Parse the discriminant expression
            (discriminant, rest2) = parse_expression(Nud, 0, rest1)
            when rest2 is
                [CloseParenToken, OpenBraceToken, .. as rest3] ->
                    # Parse switch cases
                    (cases, rest4) = parse_switch_cases([], rest3)
                    when rest4 is
                        [CloseBraceToken, .. as rest5] ->
                            switch_stmt = SwitchStatement({
                                discriminant: discriminant,
                                cases: cases,
                            })
                            (switch_stmt, rest5)
                        _ ->
                            error_switch = SwitchStatement({
                                discriminant: discriminant,
                                cases: cases,
                            })
                            (error_switch, rest4)
                _ ->
                    error_switch = SwitchStatement({
                        discriminant: Error({ message: "Expected ')' and '{' after switch discriminant" }),
                        cases: [],
                    })
                    (error_switch, rest2)
        _ ->
            error_switch = SwitchStatement({
                discriminant: Error({ message: "Expected '(' after switch keyword" }),
                cases: [],
            })
            (error_switch, token_list)

parse_switch_cases : List Node, List Token -> (List Node, List Token)
parse_switch_cases = |cases, token_list|
    when token_list is
        # End of switch body
        [CloseBraceToken, .. as rest] ->
            (cases, [CloseBraceToken] |> List.concat(rest))

        # Case clause
        [CaseKeyword, .. as rest1] ->
            (test_expr, rest2) = parse_expression(Nud, 0, rest1)
            when rest2 is
                [ColonToken, .. as rest3] ->
                    (consequent, rest4) = parse_case_consequent([], rest3)
                    case_node = SwitchCase({
                        test: Some(test_expr),
                        consequent: consequent,
                    })
                    new_cases = List.append(cases, case_node)
                    parse_switch_cases(new_cases, rest4)
                _ ->
                    error_case = SwitchCase({
                        test: Some(Error({ message: "Expected ':' after case test" })),
                        consequent: [],
                    })
                    new_cases = List.append(cases, error_case)
                    parse_switch_cases(new_cases, rest2)

        # Default clause
        [DefaultKeyword, ColonToken, .. as rest1] ->
            (consequent, rest2) = parse_case_consequent([], rest1)
            default_case = SwitchCase({
                test: None,
                consequent: consequent,
            })
            new_cases = List.append(cases, default_case)
            parse_switch_cases(new_cases, rest2)

        # Error case - unexpected token
        [_, .. as rest] ->
            error_case = SwitchCase({
                test: Some(Error({ message: "Expected 'case' or 'default' in switch body" })),
                consequent: [],
            })
            new_cases = List.append(cases, error_case)
            (new_cases, rest)

        # Empty case
        [] ->
            (cases, [])

parse_case_consequent : List Node, List Token -> (List Node, List Token)
parse_case_consequent = |statements, token_list|
    when token_list is
        # End of case - next case or default
        [CaseKeyword, .. as rest] ->
            (statements, [CaseKeyword] |> List.concat(rest))

        [DefaultKeyword, .. as rest] ->
            (statements, [DefaultKeyword] |> List.concat(rest))

        # End of switch body
        [CloseBraceToken, .. as rest] ->
            (statements, [CloseBraceToken] |> List.concat(rest))

        # Parse a statement
        _ ->
            (stmt, rest) = parse_statement(token_list)
            new_statements = List.append(statements, stmt)
            parse_case_consequent(new_statements, rest)

# Array pattern parsing for destructuring: [a, b, ...rest]
parse_array_pattern : List Token -> (Node, List Token)
parse_array_pattern = |token_list|
    parse_array_pattern_elements([], token_list)

parse_array_pattern_elements : List Node, List Token -> (Node, List Token)
parse_array_pattern_elements = |elements, token_list|
    when token_list is
        [CloseBracketToken, .. as rest] ->
            # End of array pattern
            array_pattern = ArrayPattern({ elements: elements })
            (array_pattern, rest)

        [CommaToken, .. as rest] ->
            # Empty element (hole)
            new_elements = List.append(elements, NullLiteral({}))
            parse_array_pattern_elements(new_elements, rest)

        [DotDotDotToken, .. as rest1] ->
            # Rest element: ...name
            when rest1 is
                [IdentifierToken(name), .. as rest2] ->
                    rest_element = RestElement({ argument: Identifier({ name: name }) })
                    new_elements = List.append(elements, rest_element)
                    # Continue parsing after rest element (semantically invalid but syntactically valid)
                    when rest2 is
                        [CloseBracketToken, .. as rest3] ->
                            array_pattern = ArrayPattern({ elements: new_elements })
                            (array_pattern, rest3)
                        [CommaToken, .. as rest3] ->
                            # Continue parsing more elements after rest (semantically invalid but allowed)
                            parse_array_pattern_elements(new_elements, rest3)
                        _ ->
                            # No comma or close bracket - continue with remaining tokens
                            parse_array_pattern_elements(new_elements, rest2)
                _ ->
                    # Error: rest element needs identifier
                    error_element = RestElement({ argument: Error({ message: "Expected identifier after ..." }) })
                    new_elements = List.append(elements, error_element)
                    array_pattern = ArrayPattern({ elements: new_elements })
                    (array_pattern, rest1)

        [IdentifierToken(name), .. as rest1] ->
            # Simple identifier
            identifier = Identifier({ name: name })
            element = when rest1 is
                [EqualsToken, .. as rest2] ->
                    # Default value: a = defaultValue
                    # Use precedence 60 to stop at comma (sequence expressions have precedence 50)
                    (default_expr, remaining) = parse_expression(Nud, 60, rest2)
                    (AssignmentPattern({ left: identifier, right: default_expr }), remaining)
                _ ->
                    (identifier, rest1)

            (element_node, rest_after_element) = element
            new_elements = List.append(elements, element_node)

            when rest_after_element is
                [CommaToken, .. as rest2] ->
                    parse_array_pattern_elements(new_elements, rest2)
                _ ->
                    parse_array_pattern_elements(new_elements, rest_after_element)

        [OpenBracketToken, .. as rest1] ->
            # Nested array pattern
            (nested_pattern, rest2) = parse_array_pattern(rest1)
            new_elements = List.append(elements, nested_pattern)
            when rest2 is
                [CommaToken, .. as rest3] ->
                    parse_array_pattern_elements(new_elements, rest3)
                _ ->
                    parse_array_pattern_elements(new_elements, rest2)

        [OpenBraceToken, .. as rest1] ->
            # Nested object pattern
            (nested_pattern, rest2) = parse_object_pattern(rest1)
            new_elements = List.append(elements, nested_pattern)
            when rest2 is
                [CommaToken, .. as rest3] ->
                    parse_array_pattern_elements(new_elements, rest3)
                _ ->
                    parse_array_pattern_elements(new_elements, rest2)

        _ ->
            # Error case
            error_element = Error({ message: "Unexpected token in array pattern" })
            new_elements = List.append(elements, error_element)
            array_pattern = ArrayPattern({ elements: new_elements })
            (array_pattern, token_list)

# Object pattern parsing for destructuring: {a, b: alias, ...rest}
parse_object_pattern : List Token -> (Node, List Token)
parse_object_pattern = |token_list|
    parse_object_pattern_properties([], token_list)

parse_object_pattern_properties : List Node, List Token -> (Node, List Token)
parse_object_pattern_properties = |properties, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            # End of object pattern
            object_pattern = ObjectPattern({ properties: properties })
            (object_pattern, rest)

        [DotDotDotToken, .. as rest1] ->
            # Rest element: ...rest
            when rest1 is
                [IdentifierToken(name), .. as rest2] ->
                    rest_element = RestElement({ argument: Identifier({ name: name }) })
                    new_properties = List.append(properties, rest_element)
                    # Rest element must be last
                    when rest2 is
                        [CloseBraceToken, .. as rest3] ->
                            object_pattern = ObjectPattern({ properties: new_properties })
                            (object_pattern, rest3)
                        [CommaToken, CloseBraceToken, .. as rest3] ->
                            object_pattern = ObjectPattern({ properties: new_properties })
                            (object_pattern, rest3)
                        _ ->
                            error_pattern = ObjectPattern({ properties: new_properties })
                            (error_pattern, rest2)
                _ ->
                    # Error: rest element needs identifier
                    error_element = RestElement({ argument: Error({ message: "Expected identifier after ..." }) })
                    new_properties = List.append(properties, error_element)
                    object_pattern = ObjectPattern({ properties: new_properties })
                    (object_pattern, rest1)

        [IdentifierToken(name), .. as rest1] ->
            # Property: could be shorthand {a} or {a: b} or {a = default}
            key = Identifier({ name: name })
            (property, rest_after_property) = when rest1 is
                [ColonToken, .. as rest2] ->
                    # {a: pattern}
                    (value_pattern, rest3) = parse_destructuring_pattern(rest2)
                    prop = Property({
                        key: key,
                        value: value_pattern,
                        kind: Init,
                    })
                    (prop, rest3)

                [EqualsToken, .. as rest2] ->
                    # {a = default} - shorthand with default
                    # Use precedence 60 to stop at comma (sequence expressions have precedence 50)
                    (default_expr, rest3) = parse_expression(Nud, 60, rest2)
                    assignment_pattern = AssignmentPattern({ left: key, right: default_expr })
                    prop = Property({
                        key: key,
                        value: assignment_pattern,
                        kind: Init,
                    })
                    (prop, rest3)

                _ ->
                    # {a} - shorthand
                    prop = Property({
                        key: key,
                        value: key,
                        kind: Init,
                    })
                    (prop, rest1)

            new_properties = List.append(properties, property)

            when rest_after_property is
                [CommaToken, .. as rest2] ->
                    parse_object_pattern_properties(new_properties, rest2)
                _ ->
                    parse_object_pattern_properties(new_properties, rest_after_property)

        _ ->
            # Error case
            error_property = Error({ message: "Unexpected token in object pattern" })
            new_properties = List.append(properties, error_property)
            object_pattern = ObjectPattern({ properties: new_properties })
            (object_pattern, token_list)

# Parse a destructuring pattern (array or object)
parse_destructuring_pattern : List Token -> (Node, List Token)
parse_destructuring_pattern = |token_list|
    when token_list is
        [OpenBracketToken, .. as rest] ->
            parse_array_pattern(rest)
        [OpenBraceToken, .. as rest] ->
            parse_object_pattern(rest)
        [IdentifierToken(name), .. as rest] ->
            (Identifier({ name: name }), rest)
        _ ->
            (Error({ message: "Expected destructuring pattern" }), token_list)

# Break statement parsing
parse_break_statement : List Token -> (Node, List Token)
parse_break_statement = |token_list|
    when token_list is
        # break label;
        [IdentifierToken(label), SemicolonToken, .. as rest] ->
            label_node = Identifier({ name: label })
            break_stmt = BreakStatement({ label: Some(label_node) })
            (break_stmt, rest)

        # break label (without semicolon)
        [IdentifierToken(label), .. as rest] ->
            label_node = Identifier({ name: label })
            break_stmt = BreakStatement({ label: Some(label_node) })
            (break_stmt, rest)

        # break;
        [SemicolonToken, .. as rest] ->
            break_stmt = BreakStatement({ label: None })
            (break_stmt, rest)

        # break (without semicolon - ASI)
        _ ->
            break_stmt = BreakStatement({ label: None })
            (break_stmt, token_list)

# Continue statement parsing
parse_continue_statement : List Token -> (Node, List Token)
parse_continue_statement = |token_list|
    when token_list is
        # continue label;
        [IdentifierToken(label), SemicolonToken, .. as rest] ->
            label_node = Identifier({ name: label })
            continue_stmt = ContinueStatement({ label: Some(label_node) })
            (continue_stmt, rest)

        # continue label (without semicolon)
        [IdentifierToken(label), .. as rest] ->
            label_node = Identifier({ name: label })
            continue_stmt = ContinueStatement({ label: Some(label_node) })
            (continue_stmt, rest)

        # continue;
        [SemicolonToken, .. as rest] ->
            continue_stmt = ContinueStatement({ label: None })
            (continue_stmt, rest)

        # continue (without semicolon - ASI)
        _ ->
            continue_stmt = ContinueStatement({ label: None })
            (continue_stmt, token_list)

# TypeScript interface declaration parsing
parse_interface_declaration : List Token -> (Node, List Token)
parse_interface_declaration = |token_list|
    when token_list is
        [IdentifierToken(interface_name), .. as rest1] ->
            interface_id = Identifier({ name: interface_name })

            # Check for type parameters
            (type_params, rest2) = parse_type_parameters(rest1)

            # Check for extends clause
            (extends_clause, rest3) = when rest2 is
                [ExtendsKeyword, .. as rest] ->
                    (extends_list, remaining) = parse_interface_extends(rest)
                    (Some(extends_list), remaining)
                _ ->
                    (None, rest2)

            # Parse interface body
            (interface_body, rest4) = parse_interface_body(rest3)

            interface_decl = TSInterfaceDeclaration({
                id: interface_id,
                body: interface_body,
                extends: extends_clause,
                typeParameters: type_params,
            })
            (interface_decl, rest4)

        _ ->
            (Error({ message: "Expected interface name after 'interface' keyword" }), token_list)

parse_interface_extends : List Token -> (List Node, List Token)
parse_interface_extends = |token_list|
    when token_list is
        [IdentifierToken(type_name), .. as rest1] ->
            type_ref = TSTypeReference({ typeName: Identifier({ name: type_name }), typeParameters: None })

            # Check for more extends (comma-separated)
            when rest1 is
                [CommaToken, .. as rest2] ->
                    (more_extends, rest3) = parse_interface_extends(rest2)
                    (List.prepend(more_extends, type_ref), rest3)
                _ ->
                    ([type_ref], rest1)

        _ ->
            ([], token_list)

parse_interface_body : List Token -> (Node, List Token)
parse_interface_body = |token_list|
    when token_list is
        [OpenBraceToken, .. as rest1] ->
            parse_interface_body_statements([], rest1)

        _ ->
            (Error({ message: "Expected '{' for interface body" }), token_list)

parse_interface_body_statements : List Node, List Token -> (Node, List Token)
parse_interface_body_statements = |statements, token_list|
    when token_list is
        [CloseBraceToken, .. as rest] ->
            interface_body = TSInterfaceBody({ body: statements })
            (interface_body, rest)

        [IdentifierToken(prop_name), QuestionToken, ColonToken, .. as rest1] ->
            # Optional property: name?: type
            (type_annotation, rest2) = parse_type_annotation(rest1)
            prop_key = Identifier({ name: prop_name })
            prop_sig = TSPropertySignature({
                key: prop_key,
                typeAnnotation: Some(type_annotation),
                optional: Bool.true,
            })
            new_statements = List.append(statements, prop_sig)
            parse_interface_body_statements(new_statements, rest2)

        [IdentifierToken(prop_name), ColonToken, .. as rest1] ->
            # Required property: name: type
            (type_annotation, rest2) = parse_type_annotation(rest1)
            prop_key = Identifier({ name: prop_name })
            prop_sig = TSPropertySignature({
                key: prop_key,
                typeAnnotation: Some(type_annotation),
                optional: Bool.false,
            })
            new_statements = List.append(statements, prop_sig)
            parse_interface_body_statements(new_statements, rest2)

        [IdentifierToken(method_name), OpenParenToken, .. as rest1] ->
            # Method signature: name(params): returnType
            method_key = Identifier({ name: method_name })
            (params, rest2) = parse_method_signature_params([], rest1)
            (return_type, rest3) = when rest2 is
                [ColonToken, .. as rest] ->
                    (type_node, remaining) = parse_type_annotation(rest)
                    (Some(type_node), remaining)
                _ ->
                    (None, rest2)

            method_sig = TSMethodSignature({
                key: method_key,
                params: params,
                returnType: return_type,
            })
            new_statements = List.append(statements, method_sig)
            parse_interface_body_statements(new_statements, rest3)

        [SemicolonToken, .. as rest] ->
            # Skip semicolons
            parse_interface_body_statements(statements, rest)

        [CommaToken, .. as rest] ->
            # Skip commas
            parse_interface_body_statements(statements, rest)

        _ ->
            # End of statements or error
            interface_body = TSInterfaceBody({ body: statements })
            (interface_body, token_list)

parse_method_signature_params : List Node, List Token -> (List Node, List Token)
parse_method_signature_params = |params, token_list|
    when token_list is
        [CloseParenToken, .. as rest] ->
            (params, rest)

        [IdentifierToken(param_name), ColonToken, .. as rest1] ->
            # Parameter with type: name: type
            (type_annotation, rest2) = parse_type_annotation(rest1)
            param = Identifier({ name: param_name })  # Simplified - in real TS this would be a parameter with type
            new_params = List.append(params, param)

            when rest2 is
                [CommaToken, .. as rest3] ->
                    parse_method_signature_params(new_params, rest3)
                _ ->
                    parse_method_signature_params(new_params, rest2)

        _ ->
            (params, token_list)

# TypeScript type alias declaration parsing
parse_type_alias_declaration : List Token -> (Node, List Token)
parse_type_alias_declaration = |token_list|
    when token_list is
        [IdentifierToken(type_name), .. as rest1] ->
            type_id = Identifier({ name: type_name })

            # Check for type parameters
            (type_params, rest2) = parse_type_parameters(rest1)

            # Expect equals sign
            rest3 = when rest2 is
                [EqualsToken, .. as rest] -> rest
                _ -> rest2

            (type_annotation, rest4) = parse_type_annotation(rest3)

            # Consume optional semicolon
            rest5 = when rest4 is
                [SemicolonToken, .. as rest] -> rest
                _ -> rest4

            type_alias = TSTypeAliasDeclaration({
                id: type_id,
                typeAnnotation: type_annotation,
                typeParameters: type_params,
            })
            (type_alias, rest5)

        _ ->
            (Error({ message: "Expected type alias syntax: type Name = Type" }), token_list)

# Parse TypeScript type annotations
parse_type_annotation : List Token -> (Node, List Token)
parse_type_annotation = |token_list|
    # Parse union types (Type1 | Type2 | Type3)
    parse_union_type(token_list)

parse_union_type : List Token -> (Node, List Token)
parse_union_type = |token_list|
    # Parse the first type (with potential array suffix)
    (first_type, rest1) = parse_array_type(token_list)

    # Look for union operator |
    collect_union_types([first_type], rest1)

collect_union_types : List Node, List Token -> (Node, List Token)
collect_union_types = |types, token_list|
    when token_list is
        [BarToken, .. as rest] ->
            # Parse the next type in the union
            (next_type, remaining) = parse_array_type(rest)
            new_types = List.append(types, next_type)
            collect_union_types(new_types, remaining)

        _ ->
            # No more union types
            when List.len(types) is
                1 ->
                    # Single type, return it directly
                    when List.first(types) is
                        Ok(single_type) -> (single_type, token_list)
                        Err(_) -> (Error({ message: "Failed to get single type" }), token_list)
                _ ->
                    # Multiple types, create union
                    union_type = TSUnionType({ types: types })
                    (union_type, token_list)

parse_array_type : List Token -> (Node, List Token)
parse_array_type = |token_list|
    # Parse the base type first
    (base_type, rest1) = parse_primary_type(token_list)

    # Look for array suffix []
    parse_array_suffix(base_type, rest1)

parse_array_suffix : Node, List Token -> (Node, List Token)
parse_array_suffix = |base_type, token_list|
    when token_list is
        [OpenBracketToken, CloseBracketToken, .. as rest] ->
            # Found array syntax Type[]
            array_type = TSArrayType({ elementType: base_type })
            # Check for nested arrays like Type[][]
            parse_array_suffix(array_type, rest)

        _ ->
            # No array suffix
            (base_type, token_list)

parse_primary_type : List Token -> (Node, List Token)
parse_primary_type = |token_list|
    when token_list is
        # Generic function type: <T>(param: T) => ReturnType
        [LessThanToken, .. as rest] ->
            parse_generic_function_type(token_list)

        # Built-in type keywords (from tokenizer)
        [StringKeyword, .. as rest] ->
            (TSStringKeyword({}), rest)

        [NumberKeyword, .. as rest] ->
            (TSNumberKeyword({}), rest)

        [BooleanKeyword, .. as rest] ->
            (TSBooleanKeyword({}), rest)

        [VoidKeyword, .. as rest] ->
            (TSVoidKeyword({}), rest)

        # Other TypeScript keywords
        [IdentifierToken("void"), .. as rest] ->
            (TSVoidKeyword({}), rest)

        [IdentifierToken("any"), .. as rest] ->
            (TSAnyKeyword({}), rest)

        [IdentifierToken("unknown"), .. as rest] ->
            (TSUnknownKeyword({}), rest)

        # null and undefined types
        [NullKeyword, .. as rest] ->
            (TSNullKeyword({}), rest)

        [UndefinedKeyword, .. as rest] ->
            (TSUndefinedKeyword({}), rest)

        # Typeof type: typeof expression
        [TypeofKeyword, .. as rest] ->
            parse_typeof_type(rest)

        # Function type OR parenthesized type: (param1: type1) => returnType OR (string | number)
        [OpenParenToken, .. as rest] ->
            parse_parenthesized_or_function_type(rest)

        # Object type literal: { prop1: type1; prop2: type2 }
        [OpenBraceToken, .. as rest] ->
            parse_object_type_literal(rest)

        # Tuple type: [type1, type2, type3]
        [OpenBracketToken, .. as rest] ->
            parse_tuple_type(rest)

        # Type reference (custom types)
        [IdentifierToken(type_name), .. as rest] ->
            # Check for type arguments
            (type_args, rest1) = parse_type_arguments(rest)
            type_params = when type_args is
                Some(args) ->
                    when args is
                        TSTypeParameterInstantiation(data) -> Some(data.params)
                        _ -> None
                None -> None
            type_ref = TSTypeReference({
                typeName: Identifier({ name: type_name }),
                typeParameters: type_params,
            })
            (type_ref, rest1)

        _ ->
            (Error({ message: "Expected type annotation" }), token_list)

parse_typeof_type : List Token -> (Node, List Token)
parse_typeof_type = |token_list|
    when token_list is
        [IdentifierToken(expr_name), .. as rest] ->
            # Create an identifier node for the expression
            expr_node = Identifier({ name: expr_name })
            typeof_type = TSTypeofType({
                exprName: expr_node,
            })
            (typeof_type, rest)

        _ ->
            (Error({ message: "Expected identifier after 'typeof'" }), token_list)

parse_parenthesized_or_function_type : List Token -> (Node, List Token)
parse_parenthesized_or_function_type = |token_list|
    # Try to parse as a parenthesized type first by looking ahead
    # If we see a type followed by ')' (not ': type'), it's parenthesized
    # If we see 'identifier:' or ')', it's likely a function type
    detect_parenthesized_vs_function(token_list)

detect_parenthesized_vs_function : List Token -> (Node, List Token)
detect_parenthesized_vs_function = |token_list|
    when token_list is
        # Empty parens () => definitely function type
        [CloseParenToken, ..] ->
            parse_function_type(token_list)

        # Identifier followed by colon => function parameter
        [IdentifierToken(_), ColonToken, ..] ->
            parse_function_type(token_list)

        # Identifier followed by comma => function parameter
        [IdentifierToken(_), CommaToken, ..] ->
            parse_function_type(token_list)

        # Identifier followed by close paren and arrow => function with single param
        [IdentifierToken(_), CloseParenToken, EqualsGreaterThanToken, ..] ->
            parse_function_type(token_list)

        _ ->
            # Try to parse as parenthesized type
            parse_parenthesized_type(token_list)

parse_parenthesized_type : List Token -> (Node, List Token)
parse_parenthesized_type = |token_list|
    # Parse the inner type (which might be a union)
    (inner_type, rest1) = parse_union_type(token_list)

    when rest1 is
        [CloseParenToken, .. as rest2] ->
            # Successfully parsed parenthesized type
            (inner_type, rest2)

        _ ->
            # Failed to find closing paren, fall back to function type parsing
            parse_function_type(token_list)

parse_tuple_type : List Token -> (Node, List Token)
parse_tuple_type = |token_list|
    # Parse tuple elements: [type1, type2, type3]
    (element_types, rest1) = parse_tuple_elements([], token_list)

    when rest1 is
        [CloseBracketToken, .. as rest2] ->
            tuple_type = TSTupleType({
                elementTypes: element_types,
            })
            (tuple_type, rest2)

        _ ->
            (Error({ message: "Expected ']' in tuple type" }), rest1)

parse_tuple_elements : List Node, List Token -> (List Node, List Token)
parse_tuple_elements = |elements, token_list|
    when token_list is
        # Empty tuple or end of elements
        [CloseBracketToken, ..] ->
            (elements, token_list)

        [] ->
            # End of tokens
            (elements, [])

        _ ->
            # Parse a type element (could be a literal, identifier, etc.)
            (element_type, rest1) = parse_tuple_element(token_list)
            new_elements = List.append(elements, element_type)

            when rest1 is
                [CommaToken, .. as rest2] ->
                    # More elements
                    parse_tuple_elements(new_elements, rest2)

                _ ->
                    # No more elements
                    (new_elements, rest1)

parse_tuple_element : List Token -> (Node, List Token)
parse_tuple_element = |token_list|
    when token_list is
        # Literal types
        [NumberLiteralToken(value), .. as rest] ->
            # Create a literal type node for numbers
            literal_type = TSLiteralType({ literal: NumberLiteral({ value: value }) })
            (literal_type, rest)

        [StringLiteralToken(value), .. as rest] ->
            # Create a literal type node for strings
            literal_type = TSLiteralType({ literal: StringLiteral({ value: value }) })
            (literal_type, rest)

        [TrueKeyword, .. as rest] ->
            # Boolean literal true
            literal_type = TSLiteralType({ literal: BooleanLiteral({ value: Bool.true }) })
            (literal_type, rest)

        [FalseKeyword, .. as rest] ->
            # Boolean literal false
            literal_type = TSLiteralType({ literal: BooleanLiteral({ value: Bool.false }) })
            (literal_type, rest)

        _ ->
            # Fall back to regular type parsing (for non-literals)
            parse_union_type(token_list)

parse_enum_declaration : List Token -> (Node, List Token)
parse_enum_declaration = |token_list|
    # Check for optional const modifier
    (is_const, rest1) = when token_list is
        [ConstKeyword, .. as rest] -> (Bool.true, rest)
        _ -> (Bool.false, token_list)

    # Expect enum keyword
    rest2 = when rest1 is
        [EnumKeyword, .. as rest] -> rest
        _ -> crash("Expected enum keyword")

    # Parse the enum identifier
    (id, rest3) = when rest2 is
        [IdentifierToken(name), .. as rest] ->
            (Identifier({ name }), rest)
        _ ->
            (Error({ message: "Expected enum identifier" }), rest2)

    # Expect opening brace
    rest4 = when rest3 is
        [OpenBraceToken, .. as rest] -> rest
        _ -> rest3

    # Parse enum members
    (members, rest5) = parse_enum_members([], rest4)

    # Expect closing brace
    rest6 = when rest5 is
        [CloseBraceToken, .. as rest] -> rest
        _ -> rest5

    enum_decl = TSEnumDeclaration({
        id,
        members,
        const: is_const,
    })
    (enum_decl, rest6)

parse_enum_members : List Node, List Token -> (List Node, List Token)
parse_enum_members = |members, token_list|
    when token_list is
        [CloseBraceToken, ..] | [] ->
            (members, token_list)

        _ ->
            # Parse a single enum member
            (member, rest1) = parse_enum_member(token_list)
            new_members = List.append(members, member)

            # Check for comma and continue or end
            rest2 = when rest1 is
                [CommaToken, .. as rest] -> rest
                _ -> rest1

            # Continue parsing or stop at close brace
            when rest2 is
                [CloseBraceToken, ..] ->
                    (new_members, rest2)
                _ ->
                    parse_enum_members(new_members, rest2)

parse_enum_member : List Token -> (Node, List Token)
parse_enum_member = |token_list|
    # Parse the member identifier (can be identifier or string literal)
    (id, rest1) = when token_list is
        [IdentifierToken(name), .. as rest] ->
            (Identifier({ name }), rest)
        [StringLiteralToken(value), .. as rest] ->
            (StringLiteral({ value }), rest)
        _ ->
            (Error({ message: "Expected enum member identifier" }), token_list)

    # Check for optional initializer
    (initializer, rest2) = when rest1 is
        [EqualsToken, .. as rest] ->
            # Parse the initializer expression with higher precedence to stop at commas
            (expr, remaining) = parse_expression(Nud, 60, rest)
            (Some(expr), remaining)
        _ ->
            (None, rest1)

    member = TSEnumMember({
        id,
        initializer,
    })
    (member, rest2)

parse_type_parameters : List Token -> (Option Node, List Token)
parse_type_parameters = |token_list|
    when token_list is
        [LessThanToken, .. as rest] ->
            (params, rest1) = parse_type_parameter_list([], rest)
            when rest1 is
                [GreaterThanToken, .. as rest2] ->
                    type_params = TSTypeParameterDeclaration({ params })
                    (Some(type_params), rest2)
                _ ->
                    (None, token_list)
        _ ->
            (None, token_list)

parse_type_parameter_list : List Node, List Token -> (List Node, List Token)
parse_type_parameter_list = |params, token_list|
    when token_list is
        [GreaterThanToken, ..] | [] ->
            (params, token_list)
        _ ->
            (param, rest1) = parse_type_parameter(token_list)
            new_params = List.append(params, param)

            when rest1 is
                [CommaToken, .. as rest2] ->
                    parse_type_parameter_list(new_params, rest2)
                _ ->
                    (new_params, rest1)

parse_type_parameter : List Token -> (Node, List Token)
parse_type_parameter = |token_list|
    # Parse the parameter name
    (name, rest1) = when token_list is
        [IdentifierToken(id), .. as rest] ->
            (Identifier({ name: id }), rest)
        _ ->
            (Error({ message: "Expected type parameter name" }), token_list)

    # Check for constraint
    (constraint, rest2) = when rest1 is
        [ExtendsKeyword, .. as rest] ->
            (type_node, remaining) = parse_type_annotation(rest)
            (Some(type_node), remaining)
        _ ->
            (None, rest1)

    # Check for default
    (default, rest3) = when rest2 is
        [EqualsToken, .. as rest] ->
            (type_node, remaining) = parse_type_annotation(rest)
            (Some(type_node), remaining)
        _ ->
            (None, rest2)

    param = TSTypeParameter({
        name,
        constraint,
        default,
    })
    (param, rest3)

parse_type_arguments : List Token -> (Option Node, List Token)
parse_type_arguments = |token_list|
    when token_list is
        [LessThanToken, .. as rest] ->
            (args, rest1) = parse_type_argument_list([], rest)
            when rest1 is
                [GreaterThanToken, .. as rest2] ->
                    type_args = TSTypeParameterInstantiation({ params: args })
                    (Some(type_args), rest2)
                _ ->
                    (None, token_list)
        _ ->
            (None, token_list)

parse_type_argument_list : List Node, List Token -> (List Node, List Token)
parse_type_argument_list = |args, token_list|
    when token_list is
        [GreaterThanToken, ..] | [] ->
            (args, token_list)
        _ ->
            (arg, rest1) = parse_type_annotation(token_list)
            new_args = List.append(args, arg)

            when rest1 is
                [CommaToken, .. as rest2] ->
                    parse_type_argument_list(new_args, rest2)
                _ ->
                    (new_args, rest1)

parse_function_type : List Token -> (Node, List Token)
parse_function_type = |token_list|
    # For now, just parse a simple function type without complex recursion
    # This will collect tokens until we see ) => and then parse the return type
    collect_function_type_tokens([], token_list)

collect_function_type_tokens : List Token, List Token -> (Node, List Token)
collect_function_type_tokens = |collected, token_list|
    when token_list is
        [CloseParenToken, EqualsGreaterThanToken, .. as rest] ->
            # Found the end pattern ") =>"
            # Parse return type
            (return_type, remaining) = parse_simple_type_annotation(rest)

            # Create a simple function type with empty parameters for now
            function_type = TSFunctionType({
                parameters: [],
                returnType: return_type,
                typeParameters: None,
            })
            (function_type, remaining)

        [token, .. as rest] ->
            # Continue collecting
            new_collected = List.append(collected, token)
            collect_function_type_tokens(new_collected, rest)

        [] ->
            # End of tokens, return error
            (Error({ message: "Incomplete function type" }), [])

# Simpler type annotation parser that doesn't handle function types (to avoid infinite recursion)
parse_simple_type_annotation : List Token -> (Node, List Token)
parse_simple_type_annotation = |token_list|
    when token_list is
        [StringKeyword, .. as rest] ->
            (TSStringKeyword({}), rest)

        [NumberKeyword, .. as rest] ->
            (TSNumberKeyword({}), rest)

        [BooleanKeyword, .. as rest] ->
            (TSBooleanKeyword({}), rest)

        [VoidKeyword, .. as rest] ->
            (TSVoidKeyword({}), rest)

        [IdentifierToken("any"), .. as rest] ->
            (TSAnyKeyword({}), rest)

        [IdentifierToken("unknown"), .. as rest] ->
            (TSUnknownKeyword({}), rest)

        [IdentifierToken(type_name), .. as rest] ->
            type_ref = TSTypeReference({
                typeName: Identifier({ name: type_name }),
                typeParameters: None,
            })
            (type_ref, rest)

        _ ->
            (Error({ message: "Expected simple type annotation" }), token_list)

parse_object_type_literal : List Token -> (Node, List Token)
parse_object_type_literal = |token_list|
    # Parse object type members: { prop1: type1; prop2: type2; }
    (members, rest1) = parse_object_type_members([], token_list)

    when rest1 is
        [CloseBraceToken, .. as rest2] ->
            object_type = TSTypeLiteral({
                members: members,
            })
            (object_type, rest2)

        _ ->
            (Error({ message: "Expected '}' in object type literal" }), rest1)

parse_object_type_members : List Node, List Token -> (List Node, List Token)
parse_object_type_members = |members, token_list|
    when token_list is
        # Empty object type or end of members
        [CloseBraceToken, ..] ->
            (members, token_list)

        [] ->
            # End of tokens
            (members, [])

        # Property type: propName: type
        [IdentifierToken(prop_name), ColonToken, .. as rest1] ->
            # Parse the property type
            (prop_type, rest2) = parse_simple_type_annotation(rest1)

            # Create a property signature (reusing TSPropertySignature)
            prop_signature = TSPropertySignature({
                key: Identifier({ name: prop_name }),
                typeAnnotation: Some(prop_type),
                optional: Bool.false,
            })
            new_members = List.append(members, prop_signature)

            when rest2 is
                [SemicolonToken, .. as rest3] ->
                    # More properties (semicolon separated)
                    parse_object_type_members(new_members, rest3)

                [CommaToken, .. as rest3] ->
                    # More properties (comma separated)
                    parse_object_type_members(new_members, rest3)

                _ ->
                    # No more properties
                    (new_members, rest2)

        # Optional property: propName?: type
        [IdentifierToken(prop_name), QuestionToken, ColonToken, .. as rest1] ->
            # Parse the property type
            (prop_type, rest2) = parse_simple_type_annotation(rest1)

            # Create an optional property signature
            prop_signature = TSPropertySignature({
                key: Identifier({ name: prop_name }),
                typeAnnotation: Some(prop_type),
                optional: Bool.true,
            })
            new_members = List.append(members, prop_signature)

            when rest2 is
                [SemicolonToken, .. as rest3] ->
                    # More properties (semicolon separated)
                    parse_object_type_members(new_members, rest3)

                [CommaToken, .. as rest3] ->
                    # More properties (comma separated)
                    parse_object_type_members(new_members, rest3)

                _ ->
                    # No more properties
                    (new_members, rest2)

        _ ->
            # Skip unexpected tokens and return what we have
            (members, token_list)

# Parse generic function type: <T>(param: T) => ReturnType
parse_generic_function_type : List Token -> (Node, List Token)
parse_generic_function_type = |token_list|
    # Parse type parameters <T, U, ...>
    (type_params, rest1) = parse_type_parameters(token_list)

    # Parse function type starting with parentheses
    when rest1 is
        [OpenParenToken, .. as rest2] ->
            (function_type, rest3) = parse_parenthesized_or_function_type(rest2)

            # Enhance the function type with type parameters
            enhanced_type = when function_type is
                TSFunctionType(data) ->
                    TSFunctionType({
                        parameters: data.parameters,
                        returnType: data.returnType,
                        typeParameters: type_params,
                    })
                _ ->
                    # If not a function type, return as-is
                    function_type

            (enhanced_type, rest3)

        _ ->
            # Not a valid generic function type
            (Error({ message: "Expected function parameters after generic type parameters" }), token_list)


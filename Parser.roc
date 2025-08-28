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
    top_level_node = Program({ sourceType: Module, body: [] })
    top_level_node

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
                | [EqualsGreaterThanToken as tok, .. as rest1]
                # Additive
                | [PlusToken as tok, .. as rest1]
                | [MinusToken as tok, .. as rest1]
                # Multiplicative
                | [AsteriskToken as tok, .. as rest1]
                | [SlashToken as tok, .. as rest1]
                | [PercentToken as tok, .. as rest1]
                # Postfix
                | [PlusPlusToken as tok, .. as rest1]
                | [MinusMinusToken as tok, .. as rest1]
                # FunctionCall
                | [OpenParenToken as tok, .. as rest1]
                # MemberAccess
                | [DotToken as tok, .. as rest1]
                | [OpenBracketToken as tok, .. as rest1] ->
                    expr_precedence = get_expr_precedence(Led({ left_node }), tok)
                    (right_node, rest2) = parse_expression(Led({ left_node }), expr_precedence, rest1)
                    current_node = BinaryExpression(
                        {
                            left: left_node,
                            operator: token_to_binary_operator(tok),
                            right: right_node,
                        },
                    )
                    (current_node, rest2)

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

operator_group_precedence : OperatorGroup -> U16
operator_group_precedence = |operator_group|
    when operator_group is
        Assignment -> 100
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
expr_operator_group : PrattParserMode, Token -> OperatorGroup
expr_operator_group = |mode, token|
    when mode is
        Nud ->
            when token is
                # Unary
                PlusToken -> Unary
                MinusToken -> Unary
                ExclamationToken -> Unary
                TildeToken -> Unary
                ExclamationToken -> Unary
                TypeofKeyword -> Unary
                _ -> crash("expr_operator_group() failed -- This should never happen")

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
                _ -> crash("expr_operator_group() failed -- This should never happen")

get_expr_precedence : PrattParserMode, Token -> U16
get_expr_precedence = |mode, token|
    operator_group = expr_operator_group(mode, token)
    operator_group_precedence(operator_group)

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


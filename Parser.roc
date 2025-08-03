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
                [PlusToken, .. as rest] ->
                    crash("parse_expression() failed -- This should never happen")

                [] -> crash("parse_expression() failed -- This should never happen")
# _ -> crash("parse_expression() failed -- This should never happen")

OperatorPosition : [
    Prefix,
    Infix,
    Postfix,
]

Associativity : [
    Left,
    Right,
]

expr_precedence : OperatorPosition, Associativity, Token -> U16
expr_precedence = |operator_position, associativity, token|
    when (operator_position, associativity, token) is
        (_, _, EndOfFileToken) -> 0
        # Assignment operators
        (Infix, Right, EqualsToken) -> 10
        (Infix, Right, PlusEqualsToken) -> 10
        (Infix, Right, MinusEqualsToken) -> 10
        (Infix, Right, AsteriskEqualsToken) -> 10
        (Infix, Right, AsteriskAsteriskEqualsToken) -> 10
        (Infix, Right, SlashEqualsToken) -> 10
        (Infix, Right, PercentEqualsToken) -> 10
        (Infix, Right, LessThanLessThanEqualsToken) -> 10
        (Infix, Right, GreaterThanGreaterThanEqualsToken) -> 10
        (Infix, Right, GreaterThanGreaterThanGreaterThanEqualsToken) -> 10
        (Infix, Right, AmpersandEqualsToken) -> 10
        (Infix, Right, BarEqualsToken) -> 10
        (Infix, Right, BarBarEqualsToken) -> 10
        (Infix, Right, AmpersandAmpersandEqualsToken) -> 10
        (Infix, Right, QuestionQuestionEqualsToken) -> 10
        (Infix, Right, CaretEqualsToken) -> 10
        # Ternary operator
        (Infix, Right, QuestionToken) -> 20
        (Infix, Right, ColonToken) -> 20
        # Logical OR
        (Infix, Left, BarBarToken) -> 30
        # Logical AND
        (Infix, Left, AmpersandAmpersandToken) -> 40
        # Equality
        (Infix, Left, EqualsEqualsToken) -> 50
        (Infix, Left, EqualsEqualsEqualsToken) -> 50
        (Infix, Left, BangEqualsToken) -> 50
        (Infix, Left, BangEqualsEqualsToken) -> 50
        # Relational
        (Infix, Left, LessThanToken) -> 60
        (Infix, Left, GreaterThanToken) -> 60
        (Infix, Left, LessThanEqualsToken) -> 60
        (Infix, Left, GreaterThanEqualsToken) -> 60
        (Infix, Left, InstanceOfKeyword) -> 60
        (Infix, Left, InKeyword) -> 60
        # Additive
        (Infix, Left, PlusToken) -> 70
        (Infix, Left, MinusToken) -> 70
        # Multiplicative
        (Infix, Left, AsteriskToken) -> 80
        (Infix, Left, SlashToken) -> 80
        # Exponentiative
        (Infix, Right, AsteriskAsteriskToken) -> 90
        # Unary
        (Prefix, Right, PlusToken) -> 100
        (Prefix, Right, MinusToken) -> 100
        (Prefix, Right, ExclamationToken) -> 100
        (Prefix, Right, TildeToken) -> 100
        # Binary
        (Unary, AmpersandToken) -> 110
        (Unary, BarToken) -> 110
        (Unary, CaretToken) -> 110
        # Other
        _ -> 0

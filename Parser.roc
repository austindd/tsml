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

OperatorGroup : [
    Assignment, # "=", "+=", "-=", "*=", "/="
    Conditional, # "?", ":"
    Logical, # "||", "&&"
    Bitwise, # "|", "&", ">>", "<<", ">>>"
    Relational, # "<", ">", "<=", ">="
    Additive, # "+", "-"
    Multiplicative, # "*", "/", "%"
    Exponentiation, # "**"
    Unary, # "+", "-", "!", "~", "typeof"
    Postfix, # "++", "--"
    FunctionCall, # "func()", "method()"
    MemberAccess, # "obj.", "arr[]"
]

expr_precedence : OperatorGroup -> U16
expr_precedence = |operator_group|
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

# "ASSIGN": 1,         # =
# "PLUS_ASSIGN": 1,    # +=
# "MINUS_ASSIGN": 1,   # -=
# "MULTIPLY_ASSIGN": 1, # *=
# "DIVIDE_ASSIGN": 1,  # /=
#
# # Ternary conditional (right associative)
# "QUESTION": 2,       # condition ? true : false
#
# # Logical OR
# "LOGICAL_OR": 3,     # ||
#
# # Logical AND
# "LOGICAL_AND": 4,    # &&
#
# # Equality
# "EQUALITY": 5,       # ==, !=, ===, !==
#
# # Relational
# "RELATIONAL": 6,     # <, >, <=, >=
#
# # Additive
# "ADDITIVE": 7,       # +, -
#
# # Multiplicative
# "MULTIPLICATIVE": 8, # *, /, %
#
# # Exponentiation (right associative)
# "EXPONENTIATION": 9, # **
#
# # Unary (handled in nud, not led)
# "UNARY": 10,         # +, -, !, ~, typeof
#
# # Postfix
# "POSTFIX": 11,       # ++, --
#
# # Call and member access
# "CALL": 12,          # func(), obj.prop, obj[prop]

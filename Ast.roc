module [
    Node,
    # parse,
]

import Token exposing [Token, TokenResult, tokenize_str]
import Option exposing [Option, Some, None, is_some, is_none, map, map2, map3, join_map, unsafe_get, combine2, combine3, gather, compact]

Location : {
    start : U32,
    end : U32,
}

# Literals
IdentifierNode : [Identifier { loc : Location, name : Str }]
StringLiteralNode : [StringLiteral { loc : Location, value : Str }]
BooleanLiteralNode : [BooleanLiteral { loc : Location, value : Bool }]
NullLiteralNode : [NullLiteral { loc : Location }]
NumberLiteralNode : [NumberLiteral { loc : Location, value : Str }]
UndefinedLiteralNode : [UndefinedLiteral { loc : Location }]
RegExpLiteralNode : [RegExpLiteral { loc : Location, value : Str }]
BigIntLiteralNode : [BigIntLiteral { loc : Location, value : Str }]

# Functions
FunctionNode : [Function { loc : Location, name : Option Str, params : List Node, body : Node }]

FunctionDeclarationNode : [FunctionDeclaration { loc : Location, name : Str, params : List Node, body : Node, id : IdentifierNode }]

LiteralNode : [
    StringLiteralNode,
    BooleanLiteralNode,
    NullLiteralNode,
    NumberLiteralNode,
    UndefinedLiteralNode,
    RegExpLiteralNode,
    BigIntLiteralNode,
]

BinaryOperator : [
    EqualEqual,
    BangEqual,
    EqualEqualEqual,
    BangEqualEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Pipe,
    Caret,
    Ampersand,
    In,
    Instanceof,
]

#    "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | ">>>=" | "|=" | "^=" | "&="
AssignmentOperator : [
    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    LeftShiftEqual,
    RightShiftEqual,
    UnsignedRightShiftEqual,
    PipeEqual,
    CaretEqual,
    AmpersandEqual,
]

ThisExpressionNode : [ThisExpression { loc : Location }]

ArrayExpressionNode : [ArrayExpression { loc : Location, elements : List ExpressionNode }]

ObjectExpressionNode : [ObjectExpression { loc : Location, properties : List PropertyNode }]

AssignmentExpressionNode : [
    AssignmentExpression { operator : AssignmentOperator, left : [PatternNode, ExpressionNode], right : [ExpressionNode] },
]

LogicalExpressionNode : [
    LogicalExpression { operator : LogicalOperator, left : [ExpressionNode], right : [ExpressionNode] },
]

LogicalOperator : [
    LogicalAnd,
    LogicalOr,
]

BinaryExpressionNode : [BinaryExpression { loc : Location, operator : BinaryOperator, left : ExpressionNode, right : ExpressionNode }]

NormalMemberExpressionNode : [NormalMemberExpression { loc : Location, object : ExpressionNode, property : IdentifierNode }]

ComputedMemberExpressionNode : [ComputedMemberExpression { loc : Location, object : ExpressionNode, property : ExpressionNode }]

ConditionalExpression : [ConditionalExpression { test : [ExpressionNode], alternate : ExpressionNode, consequent : ExpressionNode }]

PatternNode : [IdentifierNode]

PropertyKeyNode : {
    loc : Location,
    key : [Literal LiteralNode, Identifier IdentifierNode],
    value : ExpressionNode,
    kind : [Init, Get, Set],
}

PropertyNode : []

ExpressionNode : [
    ThisExpression ThisExpressionNode,
    ArrayExpression ArrayExpressionNode,
    ObjectExpression ObjectExpressionNode,
    Literal LiteralNode,
    Identifier IdentifierNode,
    BinaryExpression, # BinaryExpressionNode ,
    # AssignmentExpression AssignmentExpressionNode ,
    # LogicalExpression LogicalExpressionNode ,
    # NormalMemberExpression NormalMemberExpressionNode ,
    # ComputedMemberExpression ComputedMemberExpressionNode ,
]


# fn : ExpressionNode -> BinaryExpressionNode
# fn = |expression|
#     when expression is
#         BinaryExpression(x) -> x
#         _ -> crash("fn: not a function")

# func : ExpressionNode -> Location
# func = |expression|
#     when expression is
#         AssignmentExpressionNode({ loc }) -> loc
#         _ -> crash("func: not a function")

Program : {
    modules : Dict Str Node,
    entry_point : Str,
}

StatementNode : []

# Basic AST node types
Node : [
    # Program structure
    Program (List []),
    ModuleDeclaration { path : List Str, body : Node },
    # Statements
    ExpressionStatement Node,
    BlockStatement (List Node),
    IfStatement { condition : Node, thenBranch : Node, elseBranch : Node },
    ForStatement { init : Node, condition : Node, update : Node, body : Node },
    WhileStatement { condition : Node, body : Node },
    ReturnStatement { expression : Node },
    FunctionDeclaration { name : Str, params : List Node, body : Node, returnType : Node },
    VariableDeclaration { kind : DeclarationKind, declarations : List Node },
    #
    # # Expressions
    # BinaryExpression { left : Node, operator : BinaryOperator, right : Node },
    # UnaryExpression { operator : UnaryOperator, operand : Node },
    # CallExpression { callee : Node, arguments : List Node },
    # MemberExpression { object : Node, property : Node, computed : Bool },
    # ConditionalExpression { condition : Node, consequent : Node, alternate : Node },
    #
    # # Literals
    # Identifier Str,
    # NumericLiteral Str,
    # StringLiteral Str,
    # BooleanLiteral Bool,
    # NullLiteral,
    #
    # # TypeScript specific
    # TypeAnnotation Node,
    # InterfaceDeclaration { name : Str, members : List Node },
    #
    # # Other
    # Error Str,
]

DeclarationKind : [Var, Let, Const]

# BinaryOperator : [
#     Add, # +
#     Subtract, # -
#     Multiply, # *
#     Divide, # /
#     Modulo, # %
#     Exponent, # **
#     BitwiseAnd, # &
#     BitwiseOr, # |
#     BitwiseXor, # ^
#     BitwiseLeftShift, # <<
#     BitwiseRightShift, # >>
#     BitwiseUnsignedRightShift, # >>>
#     Equal, # ==
#     StrictEqual, # ===
#     NotEqual, # !=
#     StrictNotEqual, # !==
#     LessThan, # <
#     LessThanOrEqual, # <=
#     GreaterThan, # >
#     GreaterThanOrEqual, # >=
#     LogicalAnd, # &&
#     LogicalOr, # ||
#     NullishCoalescing, # ??
#     Assign, # =
#     AddAssign, # +=
#     SubtractAssign, # -=
#     MultiplyAssign, # *=
#     DivideAssign, # /=
#     ModuloAssign, # %=
#     ExponentAssign, # **=
#     BitwiseAndAssign, # &=
#     BitwiseOrAssign, # |=
#     BitwiseXorAssign, # ^=
#     BitwiseLeftShiftAssign, # <<=
#     BitwiseRightShiftAssign, # >>=
#     BitwiseUnsignedRightShiftAssign, # >>>=
#     LogicalAndAssign, # &&=
#     LogicalOrAssign, # ||=
#     NullishCoalescingAssign, # ??=
# ]

UnaryOperator : [
    Negate, # -
    Plus, # +
    LogicalNot, # !
    BitwiseNot, # ~
    Typeof, # typeof
    Void, # void
    Delete, # delete
]

# # Main parsing function to convert tokens into an AST
# parse : List TokenResult -> Node
# parse = |tokens|
#     # Create a parser state with the token list
#     initialState = {
#         tokens,
#         index: 0,
#         errors: [],
#     }
#
#     # Start parsing from the program level
#     parseProgram(initialState)
#
# # Helper function to parse a complete program
# parseProgram : { tokens : List TokenResult, index : U64, errors : List Str } -> { node : Node, state : { tokens : List TokenResult, index : U64, errors : List Str } }
# parseProgram = |state|
#     # TODO: Implement parseProgram logic
#     # This will be the entry point for parsing, collecting statements until end of file
#
#     # For now, just return an empty program
#     {
#         node: Program([]),
#         state: state,
#     }

# TODO: Add more parsing functions for different node types:
# - parseStatement
# - parseExpression
# - parseFunctionDeclaration
# - parseVariableDeclaration
# - etc.

# Compose a : []a
#
# TypeAB a : [A, B]a
#
# TypeCD a : TypeAB [C, D]a
#
# TypeEF a : TypeCD [E, F]a
#
# TypeGH : TypeAB (TypeCD [G, H])
#
# get_location : TypeGH -> U8
# get_location = |test1|
#     when test1 is
#         A -> 1
#         B -> 2
#

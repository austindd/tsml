module []

import Option exposing [
    Option,
]

WithPosition x : { line : U32, column : U32 }x

Position : WithPosition {}

WithSourceLocation x : { source : Str, start : Position, end : Position, byte_index : U32 }x

SourceLocation : WithSourceLocation {}

WithBaseNodeData x : { loc : SourceLocation }x

BaseNodeData : WithBaseNodeData {}

Node : [
    Program (WithBaseNodeData {
                body : List Node,
                sourceType : ProgramKind,
            }),

    Identifier (WithBaseNodeData {
                name : Str,
            }),
    BooleanLiteral (WithBaseNodeData {
                value : Bool,
            }),
    NumberLiteral (WithBaseNodeData {
                value : U32,
            }),
    StringLiteral (WithBaseNodeData {
                value : Str,
            }),
    NullLiteral (WithBaseNodeData {}),
    UndefinedLiteral (WithBaseNodeData {}),
    RegExpLiteral (WithBaseNodeData {
                pattern : Str,
                flags : Str,
            }),
    BigIntLiteral (WithBaseNodeData {
                value : Str,
            }),
    TemplateLiteral (WithBaseNodeData {
                quasis : List Node,
                expressions : List Node,
            }),
    ThisExpression (WithBaseNodeData {}),
    ArrayExpression (WithBaseNodeData {
                elements : List Node,
            }),
    ObjectExpression (WithBaseNodeData {
                properties : List Node,
            }),
    AssignmentExpression (WithBaseNodeData {
                left : Node,
                right : Node,
                operator : AssignmentOperator,
            }),
    LogicalExpression (WithBaseNodeData {
                left : Node,
                right : Node,
                operator : LogicalOperator,
            }),
    Property (WithBaseNodeData {
                key : Node,
                value : Node,
                kind : PropertyKind,
            }),
    Pattern (WithBaseNodeData {}),
    FunctionExpression (WithBaseNodeData {
                id : Option Node,
                params : List Node,
                body : Node,
                generator : Bool,
                async : Bool,
            }),
    FunctionDeclaration (WithBaseNodeData {
                id : Node,
                params : List Node,
                body : Node,
                generator : Bool,
                async : Bool,
            }),
    ArrowFunctionExpression (WithBaseNodeData {
                params : List Node,
                body : Node,
                generator : Bool,
                async : Bool,
            }),
    Directive (WithBaseNodeData {
                expression : Node,
                directive : Str,
            }),
    BlockStatement (WithBaseNodeData {
                body : List Node,
            }),
    FunctionBody (WithBaseNodeData {
                body : List Node,
            }),
    EmptyStatement (WithBaseNodeData {}),
    DebuggerStatement (WithBaseNodeData {}),
    WithStatement (WithBaseNodeData {
                object : Node,
                body : Node,
            }),
    ReturnStatement (WithBaseNodeData {
                argument : Option Node,
            }),
    LabeledStatement (WithBaseNodeData {
                label : Node,
                body : Node,
            }),
    BreakStatement (WithBaseNodeData {
                label : Option Node,
            }),
    ContinueStatement (WithBaseNodeData {
                label : Option Node,
            }),
    IfStatement (WithBaseNodeData {
                test : Node,
                consequent : Node,
                alternate : Option Node,
            }),
    SwitchStatement (WithBaseNodeData {
                discriminant : Node,
                cases : List Node,
            }),
    SwitchCase (WithBaseNodeData {
                test : Option Node,
                consequent : List Node,
            }),
    ThrowStatement (WithBaseNodeData {
                argument : Node,
            }),
    TryStatement (WithBaseNodeData {
                block : Node,
                handler : Option Node,
                finalizer : Option Node,
            }),
    WhileStatement (WithBaseNodeData {
                test : Node,
                body : Node,
            }),
    DoWhileStatement (WithBaseNodeData {
                body : Node,
                test : Node,
            }),
    ForStatement (WithBaseNodeData {
                init : Option Node,
                test : Option Node,
                update : Option Node,
                body : Node,
            }),
    ForInStatement (WithBaseNodeData {
                left : Node,
                right : Node,
                body : Node,
            }),
    ForOfStatement (WithBaseNodeData {
                left : Node,
                right : Node,
                body : Node,
            }),
    VariableDeclaration (WithBaseNodeData {
                declarations : List Node,
                kind : VariableDeclarationKind,
            }),
    VariableDeclarator (WithBaseNodeData {
                id : Node,
                init : Option Node,
            }),
    UnaryExpression (WithBaseNodeData {
                operator : UnaryOperator,
                prefix : Bool,
                argument : Node,
            }),
    UpdateExpression (WithBaseNodeData {
                operator : UpdateOperator,
                argument : Node,
                prefix : Bool,
            }),
    BinaryExpression (WithBaseNodeData {
                left : Node,
                right : Node,
                operator : BinaryOperator,
            }),
    MemberExpression (WithBaseNodeData {
                object : Node,
                property : Node,
                computed : Bool,
            }),
    ConditionalExpression (WithBaseNodeData {
                test : Node,
                consequent : Node,
                alternate : Node,
            }),
    CallExpression (WithBaseNodeData {
                callee : Node,
                arguments : List Node,
            }),
    NewExpression (WithBaseNodeData {
                callee : Node,
                arguments : List Node,
            }),
    SequenceExpression (WithBaseNodeData {
                expressions : List Node,
            }),
]

ProgramKind : [
    Script,
    Module,
]

VariableDeclarationKind : [
    Var,
    Let,
    Const,
]

PropertyKind : [
    Init,
    Get,
    Set,
]

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

LogicalOperator : [
    LogicalAnd,
    LogicalOr,
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

UnaryOperator : [
    Minus,
    Plus,
    Bang,
    Tilde,
    Typeof,
    Void,
    Delete,
]

UpdateOperator : [
    PlusPlus,
    MinusMinus,
]

WithLiteralNode x : [
    BooleanLiteral (WithBaseNodeData {
                value : Bool,
            }),
    NumberLiteral (WithBaseNodeData {
                value : U32,
            }),
    StringLiteral (WithBaseNodeData {
                value : Str,
            }),
    NullLiteral (WithBaseNodeData {}),
    UndefinedLiteral (WithBaseNodeData {}),
    RegExpLiteral (WithBaseNodeData {
                pattern : Str,
                flags : Str,
            }),
    BigIntLiteral (WithBaseNodeData {
                value : Str,
            }),
]x
LiteralNode : WithLiteralNode []

as_literal_node : Node -> Result LiteralNode _
as_literal_node = |node|
    when node is
        BooleanLiteral(x) -> Ok (BooleanLiteral(x))
        NumberLiteral(x) -> Ok (NumberLiteral(x))
        StringLiteral(x) -> Ok (StringLiteral(x))
        NullLiteral(x) -> Ok (NullLiteral(x))
        UndefinedLiteral(x) -> Ok (UndefinedLiteral(x))
        RegExpLiteral(x) -> Ok (RegExpLiteral(x))
        BigIntLiteral(x) -> Ok (BigIntLiteral(x))
        _ -> Err (node)

as_literal_node_opt : Node -> Option LiteralNode
as_literal_node_opt = |node|
    when node is
        BooleanLiteral(x) -> Some (BooleanLiteral(x))
        NumberLiteral(x) -> Some (NumberLiteral(x))
        StringLiteral(x) -> Some (StringLiteral(x))
        NullLiteral(x) -> Some (NullLiteral(x))
        UndefinedLiteral(x) -> Some (UndefinedLiteral(x))
        RegExpLiteral(x) -> Some (RegExpLiteral(x))
        BigIntLiteral(x) -> Some (BigIntLiteral(x))
        _ -> None

unsafe_as_literal_node : Node -> LiteralNode
unsafe_as_literal_node = |node|
    when node is
        BooleanLiteral(x) -> BooleanLiteral(x)
        NumberLiteral(x) -> NumberLiteral(x)
        StringLiteral(x) -> StringLiteral(x)
        NullLiteral(x) -> NullLiteral(x)
        UndefinedLiteral(x) -> UndefinedLiteral(x)
        RegExpLiteral(x) -> RegExpLiteral(x)
        BigIntLiteral(x) -> BigIntLiteral(x)
        _ -> crash ("unsafe_as_literal_node")


module []
import Token exposing [
    Token,
    TokenResult,
    # tokenize_str,
]
import Option exposing [
    Option,
    # Some,
    # None,
    # is_some,
    # is_none,
    # map,
    # map2,
    # map3,
    # join_map,
    # unsafe_get,
    # combine2,
    # combine3,
    # gather,
    # compact,
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
    FunctionBody (WithBaseNodeData {
                body : List Node,
                sourceType : ProgramKind,
            }),
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
]

ProgramKind : [
    Script,
    Module,
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

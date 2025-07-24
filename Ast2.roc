module []

Rec x := x

WithLocationData x : { start : U32, end : U32 }x

LocationData : WithLocationData {}

WithBaseNodeData x : { loc : LocationData }x

BaseNodeData : WithBaseNodeData {}

#################################################################
#### Literals ###################################################
#################################################################
WithIdentifierData x :
    WithBaseNodeData {
        name : Str,
    }x
IdentifierData : WithIdentifierData {}
WithIdentifierNode x : [
    Identifier IdentifierData,
]x
IdentifierNode : WithIdentifierNode []

# BooleanLiteralNode
WithBooleanLiteralData x :
    WithBaseNodeData {
        value : Bool,
    }x
BooleanLiteralData : WithBooleanLiteralData {}
WithBooleanLiteralNode x : [
    BooleanLiteral BooleanLiteralData,
]x
BooleanLiteralNode : WithBooleanLiteralNode []

# NumberLiteralNode
WithNumberLiteralData x :
    WithBaseNodeData {
        value : F64,
    }x
NumberLiteralData : WithNumberLiteralData {}
WithNumberLiteralNode x : [
    NumberLiteral NumberLiteralData,
]x
NumberLiteralNode : WithNumberLiteralNode []

# LiteralNode
WithLiteralNode x : [
    BooleanLiteral BooleanLiteralData,
    NumberLiteral NumberLiteralData,
]x
LiteralNode : WithLiteralNode []

#################################################################
#### Expressions ################################################
#################################################################

# ThisExpressionNode
WithThisExpressionData x :
    WithBaseNodeData {}
ThisExpressionData : WithThisExpressionData {}
WithThisExpressionNode x : [
    ThisExpression ThisExpressionData,
]x
ThisExpressionNode : WithThisExpressionNode []

# ArrayExpressionNode
WithArrayExpressionData x :
    WithBaseNodeData {
        elements : List ExpressionNode,
    }x
ArrayExpressionData : WithArrayExpressionData {}
WithArrayExpressionNode x : [
    ArrayExpression ArrayExpressionData,
]x
ArrayExpressionNode : WithArrayExpressionNode []

# ObjectExpressionNode
WithObjectExpressionData x :
    WithBaseNodeData {
        properties : List PropertyNode,
    }x
ObjectExpressionData : WithObjectExpressionData {}
WithObjectExpressionNode x : [
    ObjectExpression ObjectExpressionData,
]x
ObjectExpressionNode : WithObjectExpressionNode []

# AssignmentExpressionNode
WithAssignmentExpressionData x :
    WithBaseNodeData {
        operator : AssignmentOperator,
        left : [Pattern PatternNode, Expression ExpressionNode],
        right : ExpressionNode,
    }x
AssignmentExpressionData : WithAssignmentExpressionData {}
WithAssignmentExpressionNode : [
    AssignmentExpression AssignmentExpressionData,
]
AssignmentExpressionNode : WithAssignmentExpressionNode []

# LogicalExpressionNode
WithLogicalExpressionData x :
    WithBaseNodeData {
        operator : LogicalOperator,
        left : ExpressionNode,
        right : ExpressionNode,
    }x
LogicalExpressionData : WithLogicalExpressionData {}
WithLogicalExpressionNode x : [
    LogicalExpression LogicalExpressionData,
]x
LogicalExpressionNode : WithLogicalExpressionNode []

# ExpressionNode
WithExpressionNode x : [
    AssignmentExpression (WithBaseNodeData {
                operator : AssignmentOperator,
                left : [Pattern PatternNode, Expression (WithExpressionNode x)],
                right : WithExpressionNode x,
            }),
    LogicalExpression (WithBaseNodeData {
                operator : LogicalOperator,
                left : WithExpressionNode x,
                right : WithExpressionNode x,
            }),
]x
ExpressionNode : WithExpressionNode []

#################################################################
#### Properties #################################################
#################################################################

# PropertyNode
WithPropertyData x :
    WithBaseNodeData {
        key : [Literal LiteralNode, Identifier IdentifierNode],
        value : ExpressionNode,
        kind : [Init, Get, Set],
    }x
PropertyData : WithPropertyData {}
WithPropertyNode x : [
    Property PropertyData,
]x
PropertyNode : WithPropertyNode []

#################################################################
#### Patterns ###################################################
#################################################################
# PatternNode
WithPatternNode x : []x
PatternNode : WithPatternNode []

#################################################################
#### Misc #######################################################
#################################################################
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

#################################################################
#### Tests ######################################################
#################################################################

take_expr_node : ExpressionNode -> U8
take_expr_node = |expr|
    when expr is
        AssignmentExpression({ loc, operator, left, right }) -> 1
        LogicalExpression({ loc, operator, left, right }) -> 2
        _ -> crash("take_expr_node: not an expression node")


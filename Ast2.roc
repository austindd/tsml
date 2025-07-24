module []

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
    AssignmentExpression AssignmentExpressionData,
    LogicalExpression LogicalExpressionData,
]
ExpressionNode : WithExpressionNode []


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

# AssignmentExpressionNode : [
#     AssignmentExpression { loc : Location, operator : AssignmentOperator, left : [Pattern PatternNode, Expression ExpressionNode], right : ExpressionNode },
# ]
#
# LogicalExpressionNode : [
#     LogicalExpression { loc : Location, operator : LogicalOperator, left : ExpressionNode, right : ExpressionNode },
# ]

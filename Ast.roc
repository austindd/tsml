module [
    EsVersion,
    Node,
    # SourceLocation,
    # Position,
    ProgramKind,
    VariableDeclarationKind,
    MethodKind,
    PropertyKind,
    AssignmentOperator,
    LogicalOperator,
    BinaryOperator,
    UnaryOperator,
    UpdateOperator,
    LiteralNode,
    node_to_str,
]

import Token exposing [
    Token,
]

import Option exposing [
    Option,
]

WithEsVersion x : [
    Es5,
    Es2015,
    Es2016,
    Es2017,
    Es2018,
    Es2019,
    Es2020,
    Es2021,
    Es2022,
    Es2025,
    Es2026,
]x

EsVersion : WithEsVersion []

get_es_version_rank : EsVersion -> U8
get_es_version_rank = |version|
    when version is
        Es5 -> 1
        Es2015 -> 2
        Es2016 -> 3
        Es2017 -> 4
        Es2018 -> 5
        Es2019 -> 6
        Es2020 -> 7
        Es2021 -> 8
        Es2022 -> 9
        Es2025 -> 10
        Es2026 -> 11

es_version_cmp : EsVersion, EsVersion -> [EQ, LT, GT]
es_version_cmp = |a, b|
    rank_a = get_es_version_rank(a)
    rank_b = get_es_version_rank(b)
    Num.compare(rank_a, rank_b)

# WithPosition x : { line : U32, column : U32 }x
#
# Position : WithPosition {}
#
# WithSourceLocation x : { source : Str, start : Position, end : Position, byte_index : U32 }x
#
# SourceLocation : WithSourceLocation {}

WithBaseNodeData x : {
    # esVersion : EsVersion,
    # loc : SourceLocation,
    # tokens : List Token,
}x

BaseNodeData : WithBaseNodeData {}

Node : [
    Error (WithBaseNodeData {
                message : Str,
            }),
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
                value : Str,
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
    TemplateElement (WithBaseNodeData {
                value : Str,
                raw : Str,
                tail : Bool,
            }),
    TaggedTemplateExpression (WithBaseNodeData {
                tag : Node,
                quasi : Node,
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
                typeParameters : Option Node,
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
    CatchClause (WithBaseNodeData {
                param : Option Node,
                body : Node,
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
                typeAnnotation : Option Node,
            }),
    ClassDeclaration (WithBaseNodeData {
                id : Node,
                superClass : Option Node,
                body : Node,
            }),
    MethodDefinition (WithBaseNodeData {
                key : Node,
                value : Node,
                kind : MethodKind,
                computed : Bool,
                static : Bool,
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
                operator : BinaryOperator,
                right : Node,
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
    ImportDeclaration (WithBaseNodeData {
                specifiers : List Node,
                source : Node,
            }),
    ImportSpecifier (WithBaseNodeData {
                imported : Node,
                local : Node,
            }),
    ImportDefaultSpecifier (WithBaseNodeData {
                local : Node,
            }),
    ImportNamespaceSpecifier (WithBaseNodeData {
                local : Node,
            }),
    ExportNamedDeclaration (WithBaseNodeData {
                declaration : Option Node,
                specifiers : List Node,
                source : Option Node,
            }),
    ExportDefaultDeclaration (WithBaseNodeData {
                declaration : Node,
            }),
    ExportAllDeclaration (WithBaseNodeData {
                source : Node,
            }),
    ExportSpecifier (WithBaseNodeData {
                exported : Node,
                local : Node,
            }),
    ArrayPattern (WithBaseNodeData {
                elements : List Node,
            }),
    ObjectPattern (WithBaseNodeData {
                properties : List Node,
            }),
    RestElement (WithBaseNodeData {
                argument : Node,
            }),
    AssignmentPattern (WithBaseNodeData {
                left : Node,
                right : Node,
            }),
    SpreadElement (WithBaseNodeData {
                argument : Node,
            }),
    AwaitExpression (WithBaseNodeData {
                argument : Node,
            }),
    YieldExpression (WithBaseNodeData {
                argument : Node,
                delegate : Bool,
            }),
    # TypeScript-specific nodes
    TSInterfaceDeclaration (WithBaseNodeData {
                id : Node,
                body : Node,
                extends : Option (List Node),
                typeParameters : Option Node,
            }),
    TSInterfaceBody (WithBaseNodeData {
                body : List Node,
            }),
    TSMethodSignature (WithBaseNodeData {
                key : Node,
                params : List Node,
                returnType : Option Node,
            }),
    TSPropertySignature (WithBaseNodeData {
                key : Node,
                typeAnnotation : Option Node,
                optional : Bool,
            }),
    TSTypeAliasDeclaration (WithBaseNodeData {
                id : Node,
                typeAnnotation : Node,
                typeParameters : Option Node,
            }),
    TSTypeAnnotation (WithBaseNodeData {
                typeAnnotation : Node,
            }),
    TSTypeReference (WithBaseNodeData {
                typeName : Node,
                typeParameters : Option (List Node),
            }),
    TSStringKeyword (WithBaseNodeData {}),
    TSNumberKeyword (WithBaseNodeData {}),
    TSBooleanKeyword (WithBaseNodeData {}),
    TSVoidKeyword (WithBaseNodeData {}),
    TSAnyKeyword (WithBaseNodeData {}),
    TSUnknownKeyword (WithBaseNodeData {}),
    TSNullKeyword (WithBaseNodeData {}),
    TSUndefinedKeyword (WithBaseNodeData {}),
    TSFunctionType (WithBaseNodeData {
                parameters : List Node,
                returnType : Node,
                typeParameters : Option Node,
            }),
    TSTypeofType (WithBaseNodeData {
                exprName : Node,
            }),
    TSTypeLiteral (WithBaseNodeData {
                members : List Node,
            }),
    TSArrayType (WithBaseNodeData {
                elementType : Node,
            }),
    TSUnionType (WithBaseNodeData {
                types : List Node,
            }),
    TSTupleType (WithBaseNodeData {
                elementTypes : List Node,
            }),
    TSLiteralType (WithBaseNodeData {
                literal : Node,
            }),
    TSEnumDeclaration (WithBaseNodeData {
                id : Node,
                members : List Node,
                const : Bool,
            }),
    TSEnumMember (WithBaseNodeData {
                id : Node,
                initializer : Option Node,
            }),
    TSTypeParameter (WithBaseNodeData {
                name : Node,
                constraint : Option Node,
                default : Option Node,
            }),
    TSTypeParameterInstantiation (WithBaseNodeData {
                params : List Node,
            }),
    TSTypeParameterDeclaration (WithBaseNodeData {
                params : List Node,
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

MethodKind : [
    Constructor,
    Method,
    Get,
    Set,
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

# as_literal_node : Node -> Result LiteralNode _
# as_literal_node = |node|
#     when node is
#         BooleanLiteral(x) -> Ok (BooleanLiteral(x))
#         NumberLiteral(x) -> Ok (NumberLiteral(x))
#         StringLiteral(x) -> Ok (StringLiteral(x))
#         NullLiteral(x) -> Ok (NullLiteral(x))
#         UndefinedLiteral(x) -> Ok (UndefinedLiteral(x))
#         RegExpLiteral(x) -> Ok (RegExpLiteral(x))
#         BigIntLiteral(x) -> Ok (BigIntLiteral(x))
#         _ -> Err (node)
#
# as_literal_node_opt : Node -> Option LiteralNode
# as_literal_node_opt = |node|
#     when node is
#         BooleanLiteral(x) -> Some (BooleanLiteral(x))
#         NumberLiteral(x) -> Some (NumberLiteral(x))
#         StringLiteral(x) -> Some (StringLiteral(x))
#         NullLiteral(x) -> Some (NullLiteral(x))
#         UndefinedLiteral(x) -> Some (UndefinedLiteral(x))
#         RegExpLiteral(x) -> Some (RegExpLiteral(x))
#         BigIntLiteral(x) -> Some (BigIntLiteral(x))
#         _ -> None
#
# unsafe_as_literal_node : Node -> LiteralNode
# unsafe_as_literal_node = |node|
#     when node is
#         BooleanLiteral(x) -> BooleanLiteral(x)
#         NumberLiteral(x) -> NumberLiteral(x)
#         StringLiteral(x) -> StringLiteral(x)
#         NullLiteral(x) -> NullLiteral(x)
#         UndefinedLiteral(x) -> UndefinedLiteral(x)
#         RegExpLiteral(x) -> RegExpLiteral(x)
#         BigIntLiteral(x) -> BigIntLiteral(x)
#         _ -> crash ("unsafe_as_literal_node")
#

################################################################################
# Node to string
################################################################################
node_to_str : Node -> Str
node_to_str = |node|
    node_to_str_with_indent(node, 0)

node_to_str_with_indent : Node, U32 -> Str
node_to_str_with_indent = |node, indent_level|
    indent = Str.repeat("  ", Num.to_u64(indent_level))
    when node is
        Error(data) ->
            Str.concat(indent, "Error { message: \"")
            |> Str.concat(data.message)
            |> Str.concat("\" }")

        Program(data) ->
            body_count = List.len(data.body) |> Num.to_str
            source_type = program_kind_to_str(data.sourceType)
            body_str = list_to_str_with_indent(data.body, indent_level + 2)
            Str.concat(indent, "Program {\n")
            |> Str.concat(indent)
            |> Str.concat("  sourceType: ")
            |> Str.concat(source_type)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: [\n")
            |> Str.concat(body_str)
            |> Str.concat(indent)
            |> Str.concat("  ]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        Identifier(data) ->
            Str.concat(indent, "Identifier { name: \"")
            |> Str.concat(data.name)
            |> Str.concat("\" }")

        BooleanLiteral(data) ->
            value_str = Inspect.to_str(data.value)
            Str.concat(indent, "BooleanLiteral { value: ")
            |> Str.concat(value_str)
            |> Str.concat(" }")

        NumberLiteral(data) ->
            Str.concat(indent, "NumberLiteral { value: \"")
            |> Str.concat(data.value)
            |> Str.concat("\" }")

        StringLiteral(data) ->
            Str.concat(indent, "StringLiteral { value: \"")
            |> Str.concat(data.value)
            |> Str.concat("\" }")

        NullLiteral(_) ->
            Str.concat(indent, "NullLiteral")

        UndefinedLiteral(_) ->
            Str.concat(indent, "UndefinedLiteral")

        RegExpLiteral(data) ->
            Str.concat(indent, "RegExpLiteral { pattern: \"")
            |> Str.concat(data.pattern)
            |> Str.concat("\", flags: \"")
            |> Str.concat(data.flags)
            |> Str.concat("\" }")

        BigIntLiteral(data) ->
            Str.concat(indent, "BigIntLiteral { value: \"")
            |> Str.concat(data.value)
            |> Str.concat("\" }")

        TemplateLiteral(data) ->
            quasis_count = List.len(data.quasis) |> Num.to_str
            expr_count = List.len(data.expressions) |> Num.to_str
            Str.concat(indent, "TemplateLiteral { quasis: [")
            |> Str.concat(quasis_count)
            |> Str.concat(" items], expressions: [")
            |> Str.concat(expr_count)
            |> Str.concat(" items] }")

        TemplateElement(data) ->
            tail_str = if data.tail then "Bool.true" else "Bool.false"
            Str.concat(indent, "TemplateElement {\n")
            |> Str.concat(indent)
            |> Str.concat("  value: \"")
            |> Str.concat(data.value)
            |> Str.concat("\",\n")
            |> Str.concat(indent)
            |> Str.concat("  raw: \"")
            |> Str.concat(data.raw)
            |> Str.concat("\",\n")
            |> Str.concat(indent)
            |> Str.concat("  tail: ")
            |> Str.concat(tail_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        TaggedTemplateExpression(data) ->
            tag_str = node_to_str_inline(data.tag, indent_level + 1)
            quasi_str = node_to_str_inline(data.quasi, indent_level + 1)
            Str.concat(indent, "TaggedTemplateExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  tag: ")
            |> Str.concat(tag_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  quasi: ")
            |> Str.concat(quasi_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ThisExpression(_) ->
            Str.concat(indent, "ThisExpression")

        ArrayExpression(data) ->
            elements_count = List.len(data.elements) |> Num.to_str
            elements_str = list_to_str_with_indent(data.elements, indent_level + 2)
            Str.concat(indent, "ArrayExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  elements: [\n")
            |> Str.concat(elements_str)
            |> Str.concat(indent)
            |> Str.concat("  ]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ObjectExpression(data) ->
            props_count = List.len(data.properties) |> Num.to_str
            props_str = list_to_str_with_indent(data.properties, indent_level + 2)
            Str.concat(indent, "ObjectExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  properties: [\n")
            |> Str.concat(props_str)
            |> Str.concat(indent)
            |> Str.concat("  ]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        Property(data) ->
            kind_str = property_kind_to_str(data.kind)
            Str.concat(indent, "Property {\n")
            |> Str.concat(indent)
            |> Str.concat("  kind: ")
            |> Str.concat(kind_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  key: ")
            |> Str.concat(node_to_str_inline(data.key, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  value: ")
            |> Str.concat(node_to_str_inline(data.value, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        BinaryExpression(data) ->
            op_str = binary_operator_to_str(data.operator)
            Str.concat(indent, "BinaryExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  operator: ")
            |> Str.concat(op_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(node_to_str_inline(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_inline(data.right, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        UnaryExpression(data) ->
            op_str = unary_operator_to_str(data.operator)
            prefix_str = Inspect.to_str(data.prefix)
            Str.concat(indent, "UnaryExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  operator: ")
            |> Str.concat(op_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  prefix: ")
            |> Str.concat(prefix_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  argument: ")
            |> Str.concat(node_to_str_inline(data.argument, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        UpdateExpression(data) ->
            op_str = update_operator_to_str(data.operator)
            prefix_str = Inspect.to_str(data.prefix)
            Str.concat(indent, "UpdateExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  operator: ")
            |> Str.concat(op_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  prefix: ")
            |> Str.concat(prefix_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  argument: ")
            |> Str.concat(node_to_str_inline(data.argument, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        CallExpression(data) ->
            args_count = List.len(data.arguments) |> Num.to_str
            Str.concat(indent, "CallExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  callee: ")
            |> Str.concat(node_to_str_inline(data.callee, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  arguments: [")
            |> Str.concat(args_count)
            |> Str.concat(" items]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        NewExpression(data) ->
            args_count = List.len(data.arguments) |> Num.to_str
            Str.concat(indent, "NewExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  callee: ")
            |> Str.concat(node_to_str_inline(data.callee, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  arguments: [")
            |> Str.concat(args_count)
            |> Str.concat(" items]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        MemberExpression(data) ->
            computed_str = Inspect.to_str(data.computed)
            Str.concat(indent, "MemberExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  object: ")
            |> Str.concat(node_to_str_inline(data.object, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  property: ")
            |> Str.concat(node_to_str_inline(data.property, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  computed: ")
            |> Str.concat(computed_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        VariableDeclaration(data) ->
            kind_str = variable_declaration_kind_to_str(data.kind)
            decl_count = List.len(data.declarations) |> Num.to_str
            decl_str = list_to_str_with_indent(data.declarations, indent_level + 2)
            Str.concat(indent, "VariableDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  kind: ")
            |> Str.concat(kind_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  declarations: [\n")
            |> Str.concat(decl_str)
            |> Str.concat(indent)
            |> Str.concat("  ]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        VariableDeclarator(data) ->
            init_str = option_to_str_inline(data.init, indent_level + 1)
            type_str = option_to_str_inline(data.typeAnnotation, indent_level + 1)

            base_str =
                Str.concat(indent, "VariableDeclarator {\n")
                |> Str.concat(indent)
                |> Str.concat("  id: ")
                |> Str.concat(node_to_str_inline(data.id, indent_level + 1))
                |> Str.concat(",\n")

            with_type =
                when data.typeAnnotation is
                    Some(_) ->
                        base_str
                        |> Str.concat(indent)
                        |> Str.concat("  typeAnnotation: ")
                        |> Str.concat(type_str)
                        |> Str.concat(",\n")
                    None ->
                        base_str

            with_type
            |> Str.concat(indent)
            |> Str.concat("  init: ")
            |> Str.concat(init_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        FunctionDeclaration(data) ->
            async_str = Inspect.to_str(data.async)
            generator_str = Inspect.to_str(data.generator)
            params_count = List.len(data.params) |> Num.to_str
            Str.concat(indent, "FunctionDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  id: ")
            |> Str.concat(node_to_str_inline(data.id, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  params: [")
            |> Str.concat(params_count)
            |> Str.concat(" items],\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  async: ")
            |> Str.concat(async_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  generator: ")
            |> Str.concat(generator_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        FunctionExpression(data) ->
            id_str =
                when data.id is
                    Some(identifier) ->
                        " id: "
                        |> Str.concat(node_to_str_inline(identifier, indent_level + 1))
                        |> Str.concat(",\n")
                        |> Str.concat(indent)

                    None -> ""

            type_params_str = when data.typeParameters is
                Some(type_params) ->
                    "  typeParameters: "
                    |> Str.concat(node_to_str_inline(type_params, indent_level + 1))
                    |> Str.concat(",\n")
                    |> Str.concat(indent)
                None -> ""

            async_str = Inspect.to_str(data.async)
            generator_str = Inspect.to_str(data.generator)
            params_count = List.len(data.params) |> Num.to_str

            Str.concat(indent, "FunctionExpression {\n")
            |> Str.concat(indent)
            |> Str.concat(id_str)
            |> Str.concat(type_params_str)
            |> Str.concat("  params: [")
            |> Str.concat(params_count)
            |> Str.concat(" items],\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  async: ")
            |> Str.concat(async_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  generator: ")
            |> Str.concat(generator_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ArrowFunctionExpression(data) ->
            async_str = Inspect.to_str(data.async)
            generator_str = Inspect.to_str(data.generator)
            params_count = List.len(data.params) |> Num.to_str
            Str.concat(indent, "ArrowFunctionExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  params: [")
            |> Str.concat(params_count)
            |> Str.concat(" items],\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  async: ")
            |> Str.concat(async_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  generator: ")
            |> Str.concat(generator_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        BlockStatement(data) ->
            body_count = List.len(data.body) |> Num.to_str
            body_str = list_to_str_with_indent(data.body, indent_level + 2)
            Str.concat(indent, "BlockStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  body: [\n")
            |> Str.concat(body_str)
            |> Str.concat(indent)
            |> Str.concat("  ]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        FunctionBody(data) ->
            body_count = List.len(data.body) |> Num.to_str
            body_str = list_to_str_with_indent(data.body, indent_level + 2)
            Str.concat(indent, "FunctionBody {\n")
            |> Str.concat(indent)
            |> Str.concat("  body: [\n")
            |> Str.concat(body_str)
            |> Str.concat(indent)
            |> Str.concat("  ]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        IfStatement(data) ->
            alternate_str = option_to_str_with_indent(data.alternate, indent_level + 1)
            Str.concat(indent, "IfStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  test: ")
            |> Str.concat(node_to_str_inline(data.test, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  consequent: ")
            |> Str.concat(node_to_str_inline(data.consequent, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  alternate: ")
            |> Str.concat(alternate_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        WhileStatement(data) ->
            Str.concat(indent, "WhileStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  test: ")
            |> Str.concat(node_to_str_inline(data.test, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ForStatement(data) ->
            init_str = option_to_str_with_indent(data.init, indent_level + 1)
            test_str = option_to_str_with_indent(data.test, indent_level + 1)
            update_str = option_to_str_with_indent(data.update, indent_level + 1)
            Str.concat(indent, "ForStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  init: ")
            |> Str.concat(init_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  test: ")
            |> Str.concat(test_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  update: ")
            |> Str.concat(update_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ForOfStatement(data) ->
            Str.concat(indent, "ForOfStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(node_to_str_inline(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_inline(data.right, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ForInStatement(data) ->
            Str.concat(indent, "ForInStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(node_to_str_inline(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_inline(data.right, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        SwitchStatement(data) ->
            cases_count = List.len(data.cases) |> Num.to_str
            cases_str = list_to_str_with_indent(data.cases, indent_level + 2)
            Str.concat(indent, "SwitchStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  discriminant: ")
            |> Str.concat(node_to_str_inline(data.discriminant, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  cases: [")
            |> Str.concat(cases_count)
            |> Str.concat(" items]\n")
            |> Str.concat(cases_str)
            |> Str.concat(indent)
            |> Str.concat("}")

        SwitchCase(data) ->
            test_str =
                when data.test is
                    Some(test) ->
                        "\n"
                        |> Str.concat(indent)
                        |> Str.concat("  test: ")
                        |> Str.concat(node_to_str_inline(test, indent_level + 1))
                        |> Str.concat(",\n")

                    None ->
                        "\n"
                        |> Str.concat(indent)
                        |> Str.concat("  test: null,\n")

            consequent_count = List.len(data.consequent) |> Num.to_str
            consequent_str = list_to_str_with_indent(data.consequent, indent_level + 2)

            Str.concat(indent, "SwitchCase {")
            |> Str.concat(test_str)
            |> Str.concat(indent)
            |> Str.concat("  consequent: [")
            |> Str.concat(consequent_count)
            |> Str.concat(" items]\n")
            |> Str.concat(consequent_str)
            |> Str.concat(indent)
            |> Str.concat("}")

        AssignmentExpression(data) ->
            op_str = assignment_operator_to_str(data.operator)
            Str.concat(indent, "AssignmentExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  operator: ")
            |> Str.concat(op_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(node_to_str_inline(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_inline(data.right, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        LogicalExpression(data) ->
            op_str = logical_operator_to_str(data.operator)
            Str.concat(indent, "LogicalExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  operator: ")
            |> Str.concat(op_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(node_to_str_inline(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_inline(data.right, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ConditionalExpression(data) ->
            Str.concat(indent, "ConditionalExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  test: ")
            |> Str.concat(node_to_str_inline(data.test, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  consequent: ")
            |> Str.concat(node_to_str_inline(data.consequent, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  alternate: ")
            |> Str.concat(node_to_str_inline(data.alternate, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        SequenceExpression(data) ->
            expressions_count = List.len(data.expressions) |> Num.to_str
            expressions_str = list_to_str_with_indent(data.expressions, indent_level + 2)
            Str.concat(indent, "SequenceExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  expressions: [")
            |> Str.concat(expressions_count)
            |> Str.concat(" items]\n")
            |> Str.concat(expressions_str)
            |> Str.concat(indent)
            |> Str.concat("}")

        ImportDeclaration(data) ->
            specifiers_count = List.len(data.specifiers) |> Num.to_str
            specifiers_str = list_to_str_with_indent(data.specifiers, indent_level + 2)
            source_str = node_to_str_with_indent(data.source, indent_level + 1)
            Str.concat(indent, "ImportDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  specifiers: [")
            |> Str.concat(specifiers_count)
            |> Str.concat(" items]\n")
            |> Str.concat(specifiers_str)
            |> Str.concat(indent)
            |> Str.concat("  source: ")
            |> Str.concat(source_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ImportSpecifier(data) ->
            imported_str = node_to_str_with_indent(data.imported, indent_level + 1)
            local_str = node_to_str_with_indent(data.local, indent_level + 1)
            Str.concat(indent, "ImportSpecifier {\n")
            |> Str.concat(indent)
            |> Str.concat("  imported: ")
            |> Str.concat(imported_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  local: ")
            |> Str.concat(local_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ImportDefaultSpecifier(data) ->
            local_str = node_to_str_with_indent(data.local, indent_level + 1)
            Str.concat(indent, "ImportDefaultSpecifier {\n")
            |> Str.concat(indent)
            |> Str.concat("  local: ")
            |> Str.concat(local_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ImportNamespaceSpecifier(data) ->
            local_str = node_to_str_with_indent(data.local, indent_level + 1)
            Str.concat(indent, "ImportNamespaceSpecifier {\n")
            |> Str.concat(indent)
            |> Str.concat("  local: ")
            |> Str.concat(local_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ExportNamedDeclaration(data) ->
            declaration_str =
                when data.declaration is
                    Some(decl) ->
                        "\n"
                        |> Str.concat(indent)
                        |> Str.concat("  declaration: ")
                        |> Str.concat(node_to_str_inline(decl, indent_level + 1))
                        |> Str.concat(",\n")

                    None -> ""

            specifiers_count = List.len(data.specifiers) |> Num.to_str
            specifiers_str = list_to_str_with_indent(data.specifiers, indent_level + 2)

            source_str =
                when data.source is
                    Some(src) ->
                        "\n"
                        |> Str.concat(indent)
                        |> Str.concat("  source: ")
                        |> Str.concat(node_to_str_inline(src, indent_level + 1))
                        |> Str.concat(",\n")

                    None -> ""

            Str.concat(indent, "ExportNamedDeclaration {")
            |> Str.concat(declaration_str)
            |> Str.concat(indent)
            |> Str.concat("  specifiers: [")
            |> Str.concat(specifiers_count)
            |> Str.concat(" items]\n")
            |> Str.concat(specifiers_str)
            |> Str.concat(source_str)
            |> Str.concat(indent)
            |> Str.concat("}")

        ExportDefaultDeclaration(data) ->
            declaration_str = node_to_str_inline(data.declaration, indent_level + 1)
            Str.concat(indent, "ExportDefaultDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  declaration: ")
            |> Str.concat(declaration_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ExportAllDeclaration(data) ->
            source_str = node_to_str_inline(data.source, indent_level + 1)
            Str.concat(indent, "ExportAllDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  source: ")
            |> Str.concat(source_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ExportSpecifier(data) ->
            exported_str = node_to_str_inline(data.exported, indent_level + 1)
            local_str = node_to_str_inline(data.local, indent_level + 1)
            Str.concat(indent, "ExportSpecifier {\n")
            |> Str.concat(indent)
            |> Str.concat("  exported: ")
            |> Str.concat(exported_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  local: ")
            |> Str.concat(local_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ArrayPattern(data) ->
            elements_count = List.len(data.elements) |> Num.to_str
            elements_str = list_to_str_with_indent(data.elements, indent_level + 2)
            Str.concat(indent, "ArrayPattern {\n")
            |> Str.concat(indent)
            |> Str.concat("  elements: [")
            |> Str.concat(elements_count)
            |> Str.concat(" items]\n")
            |> Str.concat(elements_str)
            |> Str.concat(indent)
            |> Str.concat("}")

        ObjectPattern(data) ->
            properties_count = List.len(data.properties) |> Num.to_str
            properties_str = list_to_str_with_indent(data.properties, indent_level + 2)
            Str.concat(indent, "ObjectPattern {\n")
            |> Str.concat(indent)
            |> Str.concat("  properties: [")
            |> Str.concat(properties_count)
            |> Str.concat(" items]\n")
            |> Str.concat(properties_str)
            |> Str.concat(indent)
            |> Str.concat("}")

        RestElement(data) ->
            argument_str = node_to_str_inline(data.argument, indent_level + 1)
            Str.concat(indent, "RestElement {\n")
            |> Str.concat(indent)
            |> Str.concat("  argument: ")
            |> Str.concat(argument_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        AssignmentPattern(data) ->
            left_str = node_to_str_inline(data.left, indent_level + 1)
            right_str = node_to_str_inline(data.right, indent_level + 1)
            Str.concat(indent, "AssignmentPattern {\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(left_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(right_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        SpreadElement(data) ->
            argument_str = node_to_str_inline(data.argument, indent_level + 1)
            Str.concat(indent, "SpreadElement {\n")
            |> Str.concat(indent)
            |> Str.concat("  argument: ")
            |> Str.concat(argument_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        AwaitExpression(data) ->
            argument_str = node_to_str_inline(data.argument, indent_level + 1)
            Str.concat(indent, "AwaitExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  argument: ")
            |> Str.concat(argument_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        YieldExpression(data) ->
            argument_str = node_to_str_inline(data.argument, indent_level + 1)
            delegate_str = if data.delegate then "Bool.true" else "Bool.false"
            Str.concat(indent, "YieldExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  argument: ")
            |> Str.concat(argument_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  delegate: ")
            |> Str.concat(delegate_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        # TypeScript-specific nodes
        TSInterfaceDeclaration(data) ->
            id_str = node_to_str_inline(data.id, indent_level + 1)
            body_str = node_to_str_inline(data.body, indent_level + 1)
            type_params_str = when data.typeParameters is
                Some(type_params) ->
                    type_params_val = node_to_str_inline(type_params, indent_level + 1)
                    ",\n$(indent)  typeParameters: $(type_params_val)"
                None -> ""
            extends_str = when data.extends is
                Some(extends_list) ->
                    count = List.len(extends_list) |> Num.to_str
                    ",\n$(indent)  extends: [$(count) items]"
                None -> ""
            Str.concat(indent, "TSInterfaceDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  id: ")
            |> Str.concat(id_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(body_str)
            |> Str.concat(type_params_str)
            |> Str.concat(extends_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        TSInterfaceBody(data) ->
            body_count = List.len(data.body) |> Num.to_str
            Str.concat(indent, "TSInterfaceBody { body: [$(body_count) items] }")

        TSMethodSignature(data) ->
            key_str = node_to_str_inline(data.key, indent_level + 1)
            params_count = List.len(data.params) |> Num.to_str
            return_type_str = when data.returnType is
                Some(ret_type) ->
                    ret_str = node_to_str_inline(ret_type, indent_level + 1)
                    ",\n$(indent)  returnType: $(ret_str)"
                None -> ""
            Str.concat(indent, "TSMethodSignature {\n")
            |> Str.concat(indent)
            |> Str.concat("  key: ")
            |> Str.concat(key_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  params: [$(params_count) items]")
            |> Str.concat(return_type_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        TSPropertySignature(data) ->
            key_str = node_to_str_inline(data.key, indent_level + 1)
            optional_str = if data.optional then "Bool.true" else "Bool.false"
            type_annotation_str = when data.typeAnnotation is
                Some(type_ann) ->
                    ann_str = node_to_str_inline(type_ann, indent_level + 1)
                    ",\n$(indent)  typeAnnotation: $(ann_str)"
                None -> ""
            Str.concat(indent, "TSPropertySignature {\n")
            |> Str.concat(indent)
            |> Str.concat("  key: ")
            |> Str.concat(key_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  optional: ")
            |> Str.concat(optional_str)
            |> Str.concat(type_annotation_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        TSTypeAliasDeclaration(data) ->
            id_str = node_to_str_inline(data.id, indent_level + 1)
            type_annotation_str = node_to_str_inline(data.typeAnnotation, indent_level + 1)
            type_params_str = when data.typeParameters is
                Some(type_params) ->
                    type_params_val = node_to_str_inline(type_params, indent_level + 1)
                    ",\n$(indent)  typeParameters: $(type_params_val)"
                None -> ""
            Str.concat(indent, "TSTypeAliasDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  id: ")
            |> Str.concat(id_str)
            |> Str.concat(type_params_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  typeAnnotation: ")
            |> Str.concat(type_annotation_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        TSTypeAnnotation(data) ->
            type_annotation_str = node_to_str_inline(data.typeAnnotation, indent_level + 1)
            Str.concat(indent, "TSTypeAnnotation { typeAnnotation: $(type_annotation_str) }")

        TSTypeReference(data) ->
            type_name_str = node_to_str_inline(data.typeName, indent_level + 1)
            type_params_str = when data.typeParameters is
                Some(params) ->
                    count = List.len(params) |> Num.to_str
                    ", typeParameters: [$(count) items]"
                None -> ""
            Str.concat(indent, "TSTypeReference { typeName: $(type_name_str)$(type_params_str) }")

        TSStringKeyword(_) ->
            Str.concat(indent, "TSStringKeyword")

        TSNumberKeyword(_) ->
            Str.concat(indent, "TSNumberKeyword")

        TSBooleanKeyword(_) ->
            Str.concat(indent, "TSBooleanKeyword")

        TSVoidKeyword(_) ->
            Str.concat(indent, "TSVoidKeyword")

        TSAnyKeyword(_) ->
            Str.concat(indent, "TSAnyKeyword")

        TSUnknownKeyword(_) ->
            Str.concat(indent, "TSUnknownKeyword")

        TSNullKeyword(_) ->
            Str.concat(indent, "TSNullKeyword")

        TSUndefinedKeyword(_) ->
            Str.concat(indent, "TSUndefinedKeyword")

        TSFunctionType(data) ->
            params_str = List.map(data.parameters, |param| node_to_str_inline(param, indent_level + 1))
                        |> Str.join_with(", ")
            return_type_str = node_to_str_inline(data.returnType, indent_level + 1)

            type_params_str = when data.typeParameters is
                Some(type_params) ->
                    type_params_inline = node_to_str_inline(type_params, indent_level + 1)
                    Str.concat("typeParameters: ", type_params_inline)
                    |> Str.concat(", ")
                None -> ""

            Str.concat(indent, "TSFunctionType { ")
            |> Str.concat(type_params_str)
            |> Str.concat("params: [")
            |> Str.concat(params_str)
            |> Str.concat("] => ")
            |> Str.concat(return_type_str)
            |> Str.concat(" }")

        TSTypeofType(data) ->
            expr_str = node_to_str_inline(data.exprName, indent_level + 1)
            Str.concat(indent, "TSTypeofType { typeof ")
            |> Str.concat(expr_str)
            |> Str.concat(" }")

        TSTypeLiteral(data) ->
            members_count = List.len(data.members) |> Num.to_str
            Str.concat(indent, "TSTypeLiteral { members: [")
            |> Str.concat(members_count)
            |> Str.concat(" items] }")

        TSArrayType(data) ->
            element_str = node_to_str_inline(data.elementType, indent_level + 1)
            Str.concat(indent, "TSArrayType { elementType: ")
            |> Str.concat(element_str)
            |> Str.concat(" }")

        TSUnionType(data) ->
            types_count = List.len(data.types) |> Num.to_str
            Str.concat(indent, "TSUnionType { types: [")
            |> Str.concat(types_count)
            |> Str.concat(" items] }")

        TSTupleType(data) ->
            elements_count = List.len(data.elementTypes) |> Num.to_str
            Str.concat(indent, "TSTupleType { elementTypes: [")
            |> Str.concat(elements_count)
            |> Str.concat(" items] }")

        TSLiteralType(data) ->
            literal_str = node_to_str_inline(data.literal, indent_level + 1)
            Str.concat(indent, "TSLiteralType { literal: ")
            |> Str.concat(literal_str)
            |> Str.concat(" }")

        TSEnumDeclaration(data) ->
            id_str = node_to_str_inline(data.id, indent_level + 1)
            members_count = List.len(data.members) |> Num.to_str
            const_str = if data.const then "const " else ""
            Str.concat(indent, "TSEnumDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  ")
            |> Str.concat(const_str)
            |> Str.concat("id: ")
            |> Str.concat(id_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  members: [")
            |> Str.concat(members_count)
            |> Str.concat(" items]\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        TSEnumMember(data) ->
            id_str = node_to_str_inline(data.id, indent_level + 1)
            init_str = when data.initializer is
                Some(init) ->
                    init_val = node_to_str_inline(init, indent_level + 1)
                    Str.concat(" = ", init_val)
                None -> ""
            Str.concat(indent, "TSEnumMember { id: ")
            |> Str.concat(id_str)
            |> Str.concat(init_str)
            |> Str.concat(" }")

        TSTypeParameter(data) ->
            name_str = node_to_str_inline(data.name, indent_level + 1)
            constraint_str = when data.constraint is
                Some(constraint) ->
                    constraint_val = node_to_str_inline(constraint, indent_level + 1)
                    Str.concat(" extends ", constraint_val)
                None -> ""
            default_str = when data.default is
                Some(default) ->
                    default_val = node_to_str_inline(default, indent_level + 1)
                    Str.concat(" = ", default_val)
                None -> ""
            Str.concat(indent, "TSTypeParameter { name: ")
            |> Str.concat(name_str)
            |> Str.concat(constraint_str)
            |> Str.concat(default_str)
            |> Str.concat(" }")

        TSTypeParameterDeclaration(data) ->
            params_count = List.len(data.params) |> Num.to_str
            Str.concat(indent, "TSTypeParameterDeclaration { params: [")
            |> Str.concat(params_count)
            |> Str.concat(" items] }")

        TSTypeParameterInstantiation(data) ->
            params_count = List.len(data.params) |> Num.to_str
            Str.concat(indent, "TSTypeParameterInstantiation { params: [")
            |> Str.concat(params_count)
            |> Str.concat(" items] }")

        ClassDeclaration(data) ->
            super_class_str =
                when data.superClass is
                    Some(super) ->
                        "\n"
                        |> Str.concat(indent)
                        |> Str.concat("  superClass: ")
                        |> Str.concat(node_to_str_inline(super, indent_level + 1))
                        |> Str.concat(",")

                    None -> ""

            Str.concat(indent, "ClassDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  id: ")
            |> Str.concat(node_to_str_inline(data.id, indent_level + 1))
            |> Str.concat(",")
            |> Str.concat(super_class_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ReturnStatement(data) ->
            argument_str =
                when data.argument is
                    Some(arg) ->
                        " "
                        |> Str.concat(node_to_str_inline(arg, indent_level + 1))

                    None -> ""

            Str.concat(indent, "ReturnStatement {")
            |> Str.concat(argument_str)
            |> Str.concat(" }")

        BreakStatement(data) ->
            label_str =
                when data.label is
                    Some(label) ->
                        " label: "
                        |> Str.concat(node_to_str_inline(label, indent_level + 1))

                    None -> ""

            Str.concat(indent, "BreakStatement {")
            |> Str.concat(label_str)
            |> Str.concat(" }")

        ContinueStatement(data) ->
            label_str =
                when data.label is
                    Some(label) ->
                        " label: "
                        |> Str.concat(node_to_str_inline(label, indent_level + 1))

                    None -> ""

            Str.concat(indent, "ContinueStatement {")
            |> Str.concat(label_str)
            |> Str.concat(" }")

        ThrowStatement(data) ->
            Str.concat(indent, "ThrowStatement { ")
            |> Str.concat(node_to_str_with_indent(data.argument, indent_level + 1))
            |> Str.concat(" }")

        TryStatement(data) ->
            handler_str =
                when data.handler is
                    Some(handler) ->
                        "\n"
                        |> Str.concat(indent)
                        |> Str.concat("  handler: ")
                        |> Str.concat(node_to_str_inline(handler, indent_level + 1))
                        |> Str.concat(",")

                    None -> ""

            finalizer_str =
                when data.finalizer is
                    Some(finalizer) ->
                        "\n"
                        |> Str.concat(indent)
                        |> Str.concat("  finalizer: ")
                        |> Str.concat(node_to_str_inline(finalizer, indent_level + 1))

                    None -> ""

            Str.concat(indent, "TryStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  block: ")
            |> Str.concat(node_to_str_inline(data.block, indent_level + 1))
            |> Str.concat(",")
            |> Str.concat(handler_str)
            |> Str.concat(finalizer_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        CatchClause(data) ->
            param_str =
                when data.param is
                    Some(param) ->
                        " param: "
                        |> Str.concat(node_to_str_inline(param, indent_level + 1))
                        |> Str.concat(",")

                    None -> ""

            Str.concat(indent, "CatchClause {")
            |> Str.concat(param_str)
            |> Str.concat(" body: ")
            |> Str.concat(node_to_str_inline(data.body, indent_level + 1))
            |> Str.concat(" }")

        MethodDefinition(data) ->
            kind_str = method_kind_to_str(data.kind)
            static_str = if data.static then " (static)" else ""
            computed_str = if data.computed then " (computed)" else ""

            Str.concat(indent, "MethodDefinition {\n")
            |> Str.concat(indent)
            |> Str.concat("  kind: ")
            |> Str.concat(kind_str)
            |> Str.concat(static_str)
            |> Str.concat(computed_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  key: ")
            |> Str.concat(node_to_str_inline(data.key, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  value: ")
            |> Str.concat(node_to_str_inline(data.value, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        _ ->
            Str.concat(indent, "UnsupportedNode")

list_to_str_with_indent : List Node, U32 -> Str
list_to_str_with_indent = |nodes, indent_level|
    List.walk(
        nodes,
        "",
        |acc, node|
            node_str = node_to_str_with_indent(node, indent_level)
            Str.concat(acc, node_str) |> Str.concat(",\n"),
    )

node_to_str_inline : Node, U32 -> Str
node_to_str_inline = |node, base_indent_level|
    # Render node inline (without leading indent) but preserve internal structure
    result = node_to_str_with_indent(node, base_indent_level)
    # Remove leading whitespace
    Str.trim_start(result)

option_to_str_with_indent : Option Node, U32 -> Str
option_to_str_with_indent = |opt, indent_level|
    when opt is
        Some(node) -> node_to_str_with_indent(node, indent_level)
        None -> "None"

option_to_str_inline : Option Node, U32 -> Str
option_to_str_inline = |opt, base_indent_level|
    when opt is
        Some(node) -> node_to_str_inline(node, base_indent_level)
        None -> "None"

program_kind_to_str : ProgramKind -> Str
program_kind_to_str = |kind|
    when kind is
        Script -> "Script"
        Module -> "Module"

variable_declaration_kind_to_str : VariableDeclarationKind -> Str
variable_declaration_kind_to_str = |kind|
    when kind is
        Var -> "Var"
        Let -> "Let"
        Const -> "Const"

method_kind_to_str : MethodKind -> Str
method_kind_to_str = |kind|
    when kind is
        Constructor -> "constructor"
        Method -> "method"
        Get -> "get"
        Set -> "set"

property_kind_to_str : PropertyKind -> Str
property_kind_to_str = |kind|
    when kind is
        Init -> "Init"
        Get -> "Get"
        Set -> "Set"

binary_operator_to_str : BinaryOperator -> Str
binary_operator_to_str = |op|
    when op is
        EqualEqual -> "=="
        BangEqual -> "!="
        EqualEqualEqual -> "==="
        BangEqualEqual -> "!=="
        LessThan -> "<"
        LessThanEqual -> "<="
        GreaterThan -> ">"
        GreaterThanEqual -> ">="
        LeftShift -> "<<"
        RightShift -> ">>"
        UnsignedRightShift -> ">>>"
        Plus -> "+"
        Minus -> "-"
        Star -> "*"
        Slash -> "/"
        Percent -> "%"
        Pipe -> "|"
        Caret -> "^"
        Ampersand -> "&"
        In -> "in"
        Instanceof -> "instanceof"

unary_operator_to_str : UnaryOperator -> Str
unary_operator_to_str = |op|
    when op is
        Plus -> "+"
        Minus -> "-"
        Bang -> "!"
        Tilde -> "~"
        Typeof -> "typeof"
        Void -> "void"
        Delete -> "delete"

update_operator_to_str : UpdateOperator -> Str
update_operator_to_str = |op|
    when op is
        PlusPlus -> "++"
        MinusMinus -> "--"

assignment_operator_to_str : AssignmentOperator -> Str
assignment_operator_to_str = |op|
    when op is
        Equal -> "="
        PlusEqual -> "+="
        MinusEqual -> "-="
        StarEqual -> "*="
        SlashEqual -> "/="
        PercentEqual -> "%="
        LeftShiftEqual -> "<<="
        RightShiftEqual -> ">>="
        UnsignedRightShiftEqual -> ">>>="
        PipeEqual -> "|="
        CaretEqual -> "^="
        AmpersandEqual -> "&="

logical_operator_to_str : LogicalOperator -> Str
logical_operator_to_str = |op|
    when op is
        LogicalAnd -> "&&"
        LogicalOr -> "||"

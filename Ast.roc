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
            |> Str.concat(node_to_str_with_indent(data.key, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  value: ")
            |> Str.concat(node_to_str_with_indent(data.value, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_with_indent(data.right, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.argument, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.argument, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.object, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  property: ")
            |> Str.concat(node_to_str_with_indent(data.property, indent_level + 1))
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
            Str.concat(indent, "VariableDeclarator {\n")
            |> Str.concat(indent)
            |> Str.concat("  id: ")
            |> Str.concat(node_to_str_inline(data.id, indent_level + 1))
            |> Str.concat(",\n")
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
            |> Str.concat(node_to_str_with_indent(data.id, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  params: [")
            |> Str.concat(params_count)
            |> Str.concat(" items],\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_with_indent(data.body, indent_level + 1))
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
            id_str = when data.id is
                Some(identifier) ->
                    " id: "
                    |> Str.concat(node_to_str_with_indent(identifier, indent_level + 1))
                    |> Str.concat(",\n")
                    |> Str.concat(indent)
                None -> ""

            async_str = Inspect.to_str(data.async)
            generator_str = Inspect.to_str(data.generator)
            params_count = List.len(data.params) |> Num.to_str

            Str.concat(indent, "FunctionExpression {\n")
            |> Str.concat(indent)
            |> Str.concat(id_str)
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
            |> Str.concat(node_to_str_with_indent(data.test, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  consequent: ")
            |> Str.concat(node_to_str_with_indent(data.consequent, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.test, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_with_indent(data.body, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ForOfStatement(data) ->
            Str.concat(indent, "ForOfStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(node_to_str_with_indent(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_with_indent(data.right, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_with_indent(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ForInStatement(data) ->
            Str.concat(indent, "ForInStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  left: ")
            |> Str.concat(node_to_str_with_indent(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_with_indent(data.right, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  body: ")
            |> Str.concat(node_to_str_with_indent(data.body, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        SwitchStatement(data) ->
            cases_count = List.len(data.cases) |> Num.to_str
            cases_str = list_to_str_with_indent(data.cases, indent_level + 2)
            Str.concat(indent, "SwitchStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  discriminant: ")
            |> Str.concat(node_to_str_with_indent(data.discriminant, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  cases: [")
            |> Str.concat(cases_count)
            |> Str.concat(" items]\n")
            |> Str.concat(cases_str)
            |> Str.concat(indent)
            |> Str.concat("}")

        SwitchCase(data) ->
            test_str = when data.test is
                Some(test) ->
                    "\n"
                    |> Str.concat(indent)
                    |> Str.concat("  test: ")
                    |> Str.concat(node_to_str_with_indent(test, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_with_indent(data.right, indent_level + 1))
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
            |> Str.concat(node_to_str_with_indent(data.left, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  right: ")
            |> Str.concat(node_to_str_with_indent(data.right, indent_level + 1))
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ConditionalExpression(data) ->
            Str.concat(indent, "ConditionalExpression {\n")
            |> Str.concat(indent)
            |> Str.concat("  test: ")
            |> Str.concat(node_to_str_with_indent(data.test, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  consequent: ")
            |> Str.concat(node_to_str_with_indent(data.consequent, indent_level + 1))
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("  alternate: ")
            |> Str.concat(node_to_str_with_indent(data.alternate, indent_level + 1))
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
            declaration_str = when data.declaration is
                Some(decl) ->
                    "\n"
                    |> Str.concat(indent)
                    |> Str.concat("  declaration: ")
                    |> Str.concat(node_to_str_with_indent(decl, indent_level + 1))
                    |> Str.concat(",\n")
                None -> ""

            specifiers_count = List.len(data.specifiers) |> Num.to_str
            specifiers_str = list_to_str_with_indent(data.specifiers, indent_level + 2)

            source_str = when data.source is
                Some(src) ->
                    "\n"
                    |> Str.concat(indent)
                    |> Str.concat("  source: ")
                    |> Str.concat(node_to_str_with_indent(src, indent_level + 1))
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
            declaration_str = node_to_str_with_indent(data.declaration, indent_level + 1)
            Str.concat(indent, "ExportDefaultDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  declaration: ")
            |> Str.concat(declaration_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ExportAllDeclaration(data) ->
            source_str = node_to_str_with_indent(data.source, indent_level + 1)
            Str.concat(indent, "ExportAllDeclaration {\n")
            |> Str.concat(indent)
            |> Str.concat("  source: ")
            |> Str.concat(source_str)
            |> Str.concat(",\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        ExportSpecifier(data) ->
            exported_str = node_to_str_with_indent(data.exported, indent_level + 1)
            local_str = node_to_str_with_indent(data.local, indent_level + 1)
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

        ClassDeclaration(data) ->
            super_class_str = when data.superClass is
                Some(super) ->
                    "\n"
                    |> Str.concat(indent)
                    |> Str.concat("  superClass: ")
                    |> Str.concat(node_to_str_with_indent(super, indent_level + 1))
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
            argument_str = when data.argument is
                Some(arg) ->
                    " "
                    |> Str.concat(node_to_str_inline(arg, indent_level + 1))
                None -> ""

            Str.concat(indent, "ReturnStatement {")
            |> Str.concat(argument_str)
            |> Str.concat(" }")

        BreakStatement(data) ->
            label_str = when data.label is
                Some(label) ->
                    " label: "
                    |> Str.concat(node_to_str_inline(label, indent_level + 1))
                None -> ""

            Str.concat(indent, "BreakStatement {")
            |> Str.concat(label_str)
            |> Str.concat(" }")

        ContinueStatement(data) ->
            label_str = when data.label is
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
            handler_str = when data.handler is
                Some(handler) ->
                    "\n"
                    |> Str.concat(indent)
                    |> Str.concat("  handler: ")
                    |> Str.concat(node_to_str_with_indent(handler, indent_level + 1))
                    |> Str.concat(",")
                None -> ""

            finalizer_str = when data.finalizer is
                Some(finalizer) ->
                    "\n"
                    |> Str.concat(indent)
                    |> Str.concat("  finalizer: ")
                    |> Str.concat(node_to_str_with_indent(finalizer, indent_level + 1))
                None -> ""

            Str.concat(indent, "TryStatement {\n")
            |> Str.concat(indent)
            |> Str.concat("  block: ")
            |> Str.concat(node_to_str_with_indent(data.block, indent_level + 1))
            |> Str.concat(",")
            |> Str.concat(handler_str)
            |> Str.concat(finalizer_str)
            |> Str.concat("\n")
            |> Str.concat(indent)
            |> Str.concat("}")

        CatchClause(data) ->
            param_str = when data.param is
                Some(param) ->
                    " param: "
                    |> Str.concat(node_to_str_with_indent(param, indent_level + 1))
                    |> Str.concat(",")
                None -> ""

            Str.concat(indent, "CatchClause {")
            |> Str.concat(param_str)
            |> Str.concat(" body: ")
            |> Str.concat(node_to_str_with_indent(data.body, indent_level + 1))
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

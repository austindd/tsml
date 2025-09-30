module [
    check_program,
    TypeCheckResult,
    TypeError,
]

import Ast
import Option exposing [Option, Some, None]
import ComprehensiveTypeIndexed as T

# Result of type checking
TypeCheckResult : {
    type : T.TypeId,
    store : T.TypeStore,
    errors : List TypeError,
    warnings : List Str,
}

# Type errors
TypeError : Str

# Simple type checker that just returns unknown for now
check_program : Ast.Node -> TypeCheckResult
check_program = \ast ->
    store0 = T.empty_store
    (store1, unknown) = T.make_unknown(store0)

    # For now, just analyze the structure and return unknown type
    type_str = analyze_ast_structure(ast)

    {
        type: unknown,
        store: store1,
        errors: [],
        warnings: [type_str],
    }

# Analyze the AST structure for debugging
analyze_ast_structure : Ast.Node -> Str
analyze_ast_structure = \node ->
    when node is
        # Program(details) ->
        #     num_statements = List.len(details.value.body)
        #     "Program with $(Num.to_str(num_statements)) statements"
        #
        # VariableDeclaration(details) ->
        #     kind_str = when details.value.kind is
        #         Let -> "let"
        #         Const -> "const"
        #         Var -> "var"
        #     num_decls = List.len(details.value.declarations)
        #     "$(kind_str) declaration with $(Num.to_str(num_decls)) variables"
        #
        # FunctionDeclaration(details) ->
        #     name = when details.value.id is
        #         Some(Identifier(id_details)) -> id_details.value.name
        #         _ -> "anonymous"
        #     "Function declaration: $(name)"
        #
        # BinaryExpression(details) ->
        #     op = details.value.operator
        #     "Binary expression: $(op)"
        #
        # Literal(details) ->
        #     when details.value is
        #         NumericLiteral(n) -> "Number literal: $(Num.to_str(n))"
        #         StringLiteral(s) -> "String literal: \"$(s)\""
        #         BooleanLiteral(b) ->
        #             bool_str = if b then "true" else "false"
        #             "Boolean literal: $(bool_str)"
        #         NullLiteral -> "Null literal"
        #         _ -> "Other literal"
        #
        # Identifier(details) ->
        #     "Identifier: $(details.value.name)"
        #
        # CallExpression(details) ->
        #     num_args = List.len(details.value.arguments)
        #     "Call expression with $(Num.to_str(num_args)) arguments"
        #
        # ArrowFunctionExpression(details) ->
        #     num_params = List.len(details.value.params)
        #     "Arrow function with $(Num.to_str(num_params)) parameters"
        #
        # ObjectExpression(details) ->
        #     num_props = List.len(details.value.properties)
        #     "Object expression with $(Num.to_str(num_props)) properties"
        #
        # ArrayExpression(details) ->
        #     num_elems = List.len(details.value.elements)
        #     "Array expression with $(Num.to_str(num_elems)) elements"
        #
        # MemberExpression(details) ->
        #     "Member expression"
        #
        # AssignmentExpression(details) ->
        #     op = details.value.operator
        #     "Assignment: $(op)"
        #
        # ConditionalExpression(details) ->
        #     "Conditional expression (? :)"
        #
        # IfStatement(details) ->
        #     has_else = when details.value.alternate is
        #         Some(_) -> " with else"
        #         None -> " without else"
        #     "If statement$(has_else)"
        #
        # ReturnStatement(details) ->
        #     has_value = when details.value.argument is
        #         Some(_) -> " with value"
        #         None -> " without value"
        #     "Return statement$(has_value)"
        #
        # BlockStatement(details) ->
        #     num_stmts = List.len(details.value.body)
        #     "Block with $(Num.to_str(num_stmts)) statements"
        #
        # ForStatement(_) -> "For loop"
        # WhileStatement(_) -> "While loop"
        # Directive(_) -> "Directive"
        # UpdateExpression(_) -> "Update expression"
        # UnaryExpression(_) -> "Unary expression"
        # LogicalExpression(_) -> "Logical expression"
        # NewExpression(_) -> "New expression"
        # ThisExpression(_) -> "This expression"
        # TemplateLiteral(_) -> "Template literal"
        # TaggedTemplateExpression(_) -> "Tagged template"
        # SpreadElement(_) -> "Spread element"
        # YieldExpression(_) -> "Yield expression"
        # SequenceExpression(_) -> "Sequence expression"

        _ -> "Other AST node"

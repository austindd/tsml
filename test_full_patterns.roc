module [check]

import Ast
import SimpleComprehensiveType as Type

check : Ast.Node -> Type.Type
check = |node|
    when node is
        Program({ body, sourceType }) ->
            _ = List.map(body, check)
            Type.mk_unknown
        VariableDeclaration({ declarations, kind }) ->
            Type.mk_unknown
        BinaryExpression({ left, right, operator }) ->
            Type.mk_unknown
        Identifier({ name }) ->
            Type.mk_unknown
        NumberLiteral({ value }) ->
            Type.mk_number
        StringLiteral({ value }) ->
            Type.mk_string
        BooleanLiteral({ value }) ->
            Type.mk_boolean
        NullLiteral({}) ->
            Type.mk_null
        _ ->
            Type.mk_unknown


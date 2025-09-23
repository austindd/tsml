module [check]

import Ast
import SimpleComprehensiveType as Type

check : Ast.Node -> Type.Type
check = |node|
    when node is
        Program({ body, sourceType }) ->
            # Check all statements
            _ = List.map(body, check)
            Type.mk_unknown

        VariableDeclaration({ declarations, kind }) ->
            # Check all declarators
            _ = List.map(declarations, check)
            Type.mk_unknown

        NumberLiteral({ value }) ->
            Type.mk_number

        StringLiteral({ value }) ->
            Type.mk_string

        BooleanLiteral({ value }) ->
            Type.mk_boolean

        _ ->
            Type.mk_unknown
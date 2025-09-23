module [
    test_match,
]

import Ast exposing [Node]
import SimpleComprehensiveType as Type exposing [Type]

test_match : Node -> Type
test_match = |node|
    when node is
        Program(data) ->
            Type.mk_unknown
        VariableDeclaration(data) ->
            Type.mk_unknown
        BinaryExpression(data) ->
            Type.mk_number
        Identifier(data) ->
            Type.mk_string
        NumberLiteral(_) ->
            Type.mk_number
        _ ->
            Type.mk_unknown


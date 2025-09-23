module [check]

import Ast exposing [Node]
import SimpleComprehensiveType as Type exposing [Type]

TypeResult : {
    node_type : Type,
    errors : List {},
}

check : Node -> TypeResult  
check = |node|
    when node is
        Program(data) ->
            results = List.map(data.body, |stmt|
                check(stmt))
            { node_type: Type.mk_unknown, errors: [] }
        VariableDeclaration(data) ->
            { node_type: Type.mk_unknown, errors: [] }
        BinaryExpression(data) ->
            { node_type: Type.mk_number, errors: [] }
        Identifier(data) ->
            { node_type: Type.mk_string, errors: [] }
        _ ->
            { node_type: Type.mk_unknown, errors: [] }


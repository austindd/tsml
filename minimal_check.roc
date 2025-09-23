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
        Program(_) ->
            { node_type: Type.mk_unknown, errors: [] }
        _ ->
            { node_type: Type.mk_unknown, errors: [] }


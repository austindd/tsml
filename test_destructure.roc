module [check]

import Ast exposing [Node]
import SimpleComprehensiveType as Type

check : Node -> Type.Type
check = |node|
    when node is
        Program({ body, sourceType }) ->
            Type.mk_unknown
        _ ->
            Type.mk_unknown


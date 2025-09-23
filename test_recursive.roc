module [check]

import Ast
import SimpleComprehensiveType as Type

check : Ast.Node -> Type.Type  
check = |node|
    when node is
        Program({ body, sourceType }) ->
            # Recursively check body
            _ = List.map(body, check)
            Type.mk_unknown
            
        _ ->
            Type.mk_unknown

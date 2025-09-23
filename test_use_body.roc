module [check]

import Ast
import SimpleComprehensiveType as Type

check : Ast.Node -> Type.Type  
check = |node|
    when node is
        Program({ body, sourceType }) ->
            # Use body
            _ = List.len(body)
            Type.mk_unknown
            
        _ ->
            Type.mk_unknown

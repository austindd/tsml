module [check]

import Ast
import SimpleComprehensiveType as Type
import Option exposing [Option]

check : Ast.Node -> Type.Type
check = |node|
    when node is
        Program({ body, sourceType }) ->
            _ = List.map(body, check)
            Type.mk_unknown
        
        VariableDeclarator({ id, init }) ->
            when init is
                Some(expr) -> check(expr)
                None -> Type.mk_undefined
                
        _ -> Type.mk_unknown

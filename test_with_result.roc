module [check]

import Ast
import SimpleComprehensiveType as Type

TypeResult : {
    node_type : Type.Type,
    errors : List { message : Str },
}

check : Ast.Node -> TypeResult
check = |node|
    when node is
        Program({ body, sourceType }) ->
            # Recursively check body
            results = List.map(body, check)
            
            all_errors = List.walk(results, [], |acc, result|
                List.concat(acc, result.errors))
                
            { node_type: Type.mk_unknown, errors: all_errors }
            
        _ ->
            { node_type: Type.mk_unknown, errors: [] }

module [
    check,
]

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
            # Check all statements
            results = List.map(body, check)
            
            # Collect all errors
            all_errors = List.walk(results, [], |acc, result|
                List.concat(acc, result.errors))
            
            {
                node_type: Type.mk_unknown,
                errors: all_errors,
            }
            
        VariableDeclaration({ declarations, kind }) ->
            { node_type: Type.mk_unknown, errors: [] }
            
        BinaryExpression({ left, right, operator }) ->
            left_result = check(left)
            right_result = check(right)
            
            { 
                node_type: Type.mk_number,
                errors: List.concat(left_result.errors, right_result.errors),
            }
            
        Identifier({ name }) ->
            { node_type: Type.mk_unknown, errors: [] }
            
        NumberLiteral({ value }) ->
            { node_type: Type.mk_number, errors: [] }
            
        StringLiteral({ value }) ->
            { node_type: Type.mk_string, errors: [] }
            
        BooleanLiteral({ value }) ->
            { node_type: Type.mk_boolean, errors: [] }
            
        NullLiteral ->
            { node_type: Type.mk_null, errors: [] }
            
        UndefinedLiteral ->
            { node_type: Type.mk_undefined, errors: [] }
            
        ExpressionStatement({ expression }) ->
            check(expression)
            
        _ ->
            { node_type: Type.mk_unknown, errors: [] }

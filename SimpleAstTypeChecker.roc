module [
    SimpleNode,
    node_to_simple,
    check_simple,
]

import Ast
import SimpleComprehensiveType as Type exposing [Type]

# Simplified AST for type checking
SimpleNode : [
    SimpleProgram (List SimpleNode),
    SimpleVarDecl { declarations: List SimpleNode, is_const: Bool },
    SimpleBinaryExpr { left: SimpleNode, right: SimpleNode, op: Str },
    SimpleIdentifier Str,
    SimpleNumberLit F64,
    SimpleStringLit Str,
    SimpleBoolLit Bool,
    SimpleNullLit,
    SimpleExprStmt SimpleNode,
    SimpleOther,
]

# Convert Ast.Node to SimpleNode
node_to_simple : Ast.Node -> SimpleNode
node_to_simple = |node|
    # For now just return SimpleOther
    # Would need to use Ast accessor functions when they work
    SimpleOther

# Type check a SimpleNode
check_simple : SimpleNode -> { node_type: Type, errors: List {} }
check_simple = |node|
    when node is
        SimpleProgram(body) ->
            # Check all statements
            _ = List.map(body, check_simple)
            { node_type: Type.mk_unknown, errors: [] }
            
        SimpleVarDecl(_) ->
            { node_type: Type.mk_unknown, errors: [] }
            
        SimpleBinaryExpr(data) ->
            left_result = check_simple(data.left)
            right_result = check_simple(data.right)
            
            result_type = when data.op is
                "+" -> Type.mk_number
                "-" | "*" | "/" -> Type.mk_number
                _ -> Type.mk_unknown
                
            { node_type: result_type, errors: [] }
            
        SimpleIdentifier(_) ->
            { node_type: Type.mk_unknown, errors: [] }
            
        SimpleNumberLit(_) ->
            { node_type: Type.mk_number, errors: [] }
            
        SimpleStringLit(_) ->
            { node_type: Type.mk_string, errors: [] }
            
        SimpleBoolLit(_) ->
            { node_type: Type.mk_boolean, errors: [] }
            
        SimpleNullLit ->
            { node_type: Type.mk_null, errors: [] }
            
        SimpleExprStmt(expr) ->
            check_simple(expr)
            
        SimpleOther ->
            { node_type: Type.mk_unknown, errors: [] }


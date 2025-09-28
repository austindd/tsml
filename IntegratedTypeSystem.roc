module [
    TypeInfo,
    Context,
    check_program,
    check_expression,
    infer_type,
    coerce_type,
    narrow_type,
]

import Ast
import Option exposing [Option]
import Parser
import Token exposing [Token]

# Core type representation
TypeInfo : [
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,
    TObject (List { key: Str, value: TypeInfo }),
    TArray TypeInfo,
    TFunction { params: List TypeInfo, returns: TypeInfo },
    TUnion (List TypeInfo),
    TIntersection (List TypeInfo),
    TAny,
    TNever,
    TUnknown,
    TLiteral LiteralValue,
]

LiteralValue : [
    NumLit F64,
    StrLit Str,
    BoolLit Bool,
    NullLit,
]

# Type checking context
Context : {
    # Variable types in scope
    variables: List { name: Str, type: TypeInfo, is_const: Bool },
    # Current function return type
    return_type: Option TypeInfo,
    # Whether we're in a loop
    in_loop: Bool,
    # Type narrowing information from conditionals
    narrowed: List { name: Str, type: TypeInfo },
    # Collected errors
    errors: List { message: Str, location: Option U64 },
}

# Create empty context
empty_context : Context
empty_context = {
    variables: [],
    return_type: None,
    in_loop: Bool.false,
    narrowed: [],
    errors: [],
}

filter_trivia : List Token -> List Token
filter_trivia = |tokens|
    List.keep_if(tokens, |tok|
        when tok is
            WhitespaceTrivia(_) -> Bool.false
            NewLineTrivia(_) -> Bool.false
            LineCommentStart -> Bool.false
            BlockCommentStart -> Bool.false
            BlockCommentEnd -> Bool.false
            CommentText(_) -> Bool.false
            ShebangTrivia -> Bool.false
            ConflictMarkerTrivia -> Bool.false
            NonTextFileMarkerTrivia -> Bool.false
            _ -> Bool.true
    )

# Check a complete program
check_program : Str -> { result: TypeInfo, errors: List { message: Str, location: Option U64 } }
check_program = |source|
    tokens = Token.tokenize_str(source)
    
    # Filter trivia tokens
    filtered = filter_trivia(tokens)
    
    parsed = Parser.parse_program(filtered)
    ctx = check_node(parsed, empty_context)
    { result: TUnknown, errors: ctx.errors }

# Check a single expression
check_expression : Str -> TypeInfo
check_expression = |source|
    tokens = Token.tokenize_str(source)
    filtered = filter_trivia(tokens)
    
    (parsed, _remaining_tokens) = Parser.parse_primary_expression(filtered)
    ctx = check_node(parsed, empty_context)
    infer_type(parsed, ctx)

# Check an AST node
check_node : Ast.Node, Context -> Context
check_node = |node, ctx|
    when node is
        Program({ body, sourceType }) ->
            List.walk(body, ctx, |acc, stmt|
                check_node(stmt, acc))
        
        VariableDeclaration({ declarations, kind }) ->
            is_const = when kind is
                Const -> Bool.true
                _ -> Bool.false
            
            List.walk(declarations, ctx, |acc, decl|
                check_var_declarator(decl, is_const, acc))
        
        BinaryExpression({ left, right, operator }) ->
            left_type = infer_type(left, ctx)
            right_type = infer_type(right, ctx)
            
            # Check for type errors in numeric operations
            when operator is
                Minus | Star | Slash | Percent ->
                    new_errors = []
                    
                    errors1 = if !is_numeric(left_type) then
                        List.append(new_errors, { message: "Left operand must be numeric", location: None })
                    else
                        new_errors
                    
                    errors2 = if !is_numeric(right_type) then
                        List.append(errors1, { message: "Right operand must be numeric", location: None })
                    else
                        errors1
                    
                    { ctx & errors: List.concat(ctx.errors, errors2) }
                _ ->
                    ctx
        
        _ ->
            ctx

# Check variable declarator
check_var_declarator : Ast.Node, Bool, Context -> Context
check_var_declarator = |node, is_const, ctx|
    when node is
        VariableDeclarator({ id, init }) ->
            when id is
                Identifier({ name }) ->
                    init_type = when init is
                        Some(expr) -> infer_type(expr, ctx)
                        None -> TUndefined
                    
                    # Add variable to context
                    new_var = { name, type: init_type, is_const }
                    { ctx & variables: List.append(ctx.variables, new_var) }
                _ ->
                    ctx
        _ ->
            ctx

# Infer type of an expression
infer_type : Ast.Node, Context -> TypeInfo
infer_type = |node, ctx|
    when node is
        NumberLiteral({ value }) ->
            TNumber
        
        StringLiteral({ value }) ->
            TString
        
        BooleanLiteral({ value }) ->
            TBoolean
        
        NullLiteral({}) ->
            TNull
        
        UndefinedLiteral({}) ->
            TUndefined
        
        Identifier({ name }) ->
            # Look up in context
            when List.find_first(ctx.variables, |var| var.name == name) is
                Ok(var) -> var.type
                Err(_) -> TUnknown
        
        BinaryExpression({ left, right, operator }) ->
            left_type = infer_type(left, ctx)
            right_type = infer_type(right, ctx)
            
            when operator is
                Plus ->
                    # JavaScript's + operator coercion
                    if is_string_like(left_type) || is_string_like(right_type) then
                        TString
                    else
                        TNumber
                
                Minus | Star | Slash | Percent ->
                    TNumber
                
                LessThan | LessThanEqual | GreaterThan | GreaterThanEqual |
                EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
                    TBoolean
                
                _ ->
                    TUnknown
        
        LogicalExpression({ left, right, operator }) ->
            left_type = infer_type(left, ctx)
            right_type = infer_type(right, ctx)
            TUnion([left_type, right_type])
        
        ArrayExpression({ elements }) ->
            # Infer array element type from first element
            when List.first(elements) is
                Ok(elem) ->
                    elem_type = infer_type(elem, ctx)
                    TArray(elem_type)
                Err(_) ->
                    # Empty array
                    TArray(TUnknown)
        
        ObjectExpression({ properties }) ->
            # For now, return empty object type
            TObject([])
        
        CallExpression({ callee, arguments }) ->
            # For now, function calls return unknown
            TUnknown
        
        _ ->
            TUnknown

# Type coercion following JavaScript rules
coerce_type : TypeInfo, TypeInfo -> TypeInfo
coerce_type = |from_type, to_type|
    when to_type is
        TNumber ->
            when from_type is
                TString | TBoolean | TNull -> TNumber
                TUndefined -> TNever  # NaN
                _ -> from_type
        
        TString ->
            # Everything can be coerced to string
            TString
        
        TBoolean ->
            # Everything can be coerced to boolean
            TBoolean
        
        _ ->
            from_type

# Type narrowing for conditional checks
narrow_type : TypeInfo, Ast.Node -> TypeInfo
narrow_type = |original_type, condition|
    when condition is
        BinaryExpression({ left, operator, right }) ->
            when operator is
                EqualEqualEqual ->
                    # typeof x === "string" narrows to string
                    when left is
                        CallExpression({ callee, arguments }) ->
                            when right is
                                StringLiteral({ value }) ->
                                    when value is
                                        "number" -> TNumber
                                        "string" -> TString
                                        "boolean" -> TBoolean
                                        _ -> original_type
                                _ -> original_type
                        _ -> original_type
                _ -> original_type
        _ -> original_type

# Helper functions
is_numeric : TypeInfo -> Bool
is_numeric = |t|
    when t is
        TNumber -> Bool.true
        TLiteral(NumLit(_)) -> Bool.true
        _ -> Bool.false

is_string_like : TypeInfo -> Bool
is_string_like = |t|
    when t is
        TString -> Bool.true
        TLiteral(StrLit(_)) -> Bool.true
        _ -> Bool.false

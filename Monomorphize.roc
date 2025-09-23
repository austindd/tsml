module [
    monomorphize,
    TypeInfo,
    CallSite,
]

import Ast exposing [Node]
import Option exposing [Option]

# Type information for monomorphization
# Note: Simplified to avoid recursive type issues
TypeInfo : [
    NumberType,
    StringType,
    BooleanType,
    ObjectType,
    ArrayType,
    FunctionType,
    UnionType,
    AnyType,
    UndefinedType,
    NullType,
    UnknownType,
]

# Call site information
CallSite : {
    function_name : Str,
    argument_types : List TypeInfo,
    location : Str, # For debugging/tracking
}

# Monomorphization context
MonomorphizationContext : {
    function_types : List (Str, List CallSite),
    specialized_functions : List (Str, Node),
    type_environment : List (Str, TypeInfo),
    next_specialized_id : U64,
}

initial_context : MonomorphizationContext
initial_context = {
    function_types: [],
    specialized_functions: [],
    type_environment: [],
    next_specialized_id: 0,
}

monomorphize : Node -> Node
monomorphize = |ast|
    # Phase 1: Collect all function definitions and call sites
    functions = collect_functions(ast)
    call_sites = collect_call_sites(ast)

    # Phase 2: Infer types at each call site
    typed_call_sites = infer_call_site_types(call_sites, initial_context)

    # Phase 3: Generate specialized versions
    ctx_with_specializations = generate_specializations(functions, typed_call_sites, initial_context)

    # Phase 4: Rewrite the AST to use specialized functions
    rewrite_with_specializations(ast, ctx_with_specializations)

# Collect all function declarations and expressions
collect_functions : Node -> List (Str, Node)
collect_functions = |node|
    when node is
        Program(data) ->
            List.walk(data.body, [], |acc, stmt| List.concat(acc, collect_functions(stmt)))

        FunctionDeclaration(data) ->
            when data.id is
                Identifier(id_data) ->
                    [(id_data.name, node)]
                _ -> []

        VariableDeclaration(data) ->
            List.walk(data.declarations, [], |acc, decl|
                when decl is
                    VariableDeclarator(v_data) ->
                        when (v_data.id, v_data.init) is
                            (Identifier(id_data), Some(FunctionExpression(fn))) ->
                                List.append(acc, (id_data.name, FunctionExpression(fn)))
                            (Identifier(id_data), Some(ArrowFunctionExpression(fn))) ->
                                List.append(acc, (id_data.name, ArrowFunctionExpression(fn)))
                            _ -> acc
                    _ -> acc
            )

        BlockStatement(data) ->
            List.walk(data.body, [], |acc, stmt| List.concat(acc, collect_functions(stmt)))

        IfStatement(data) ->
            cons_fns = collect_functions(data.consequent)
            alt_fns = when data.alternate is
                Some(alt) -> collect_functions(alt)
                None -> []
            List.concat(cons_fns, alt_fns)

        _ -> []

# Collect all call sites with their contexts
collect_call_sites : Node -> List CallSite
collect_call_sites = |node|
    collect_call_sites_with_path(node, "")

collect_call_sites_with_path : Node, Str -> List CallSite
collect_call_sites_with_path = |node, path|
    when node is
        CallExpression(data) ->
            # Get the function name if it's a direct call
            call_site = when data.callee is
                Identifier(id_data) ->
                    [{
                        function_name: id_data.name,
                        argument_types: List.map(data.arguments, infer_type),
                        location: path,
                    }]
                _ -> []

            # Recursively collect from arguments
            arg_sites = walk_with_index(data.arguments, [], |acc, arg, idx|
                new_path = Str.concat(path, ".arg$(Num.to_str(idx))")
                List.concat(acc, collect_call_sites_with_path(arg, new_path))
            )

            List.concat(call_site, arg_sites)

        Program(data) ->
            walk_with_index(data.body, [], |acc, stmt, idx|
                new_path = Str.concat(path, ".stmt$(Num.to_str(idx))")
                List.concat(acc, collect_call_sites_with_path(stmt, new_path))
            )

        BlockStatement(data) ->
            walk_with_index(data.body, [], |acc, stmt, idx|
                new_path = Str.concat(path, ".block$(Num.to_str(idx))")
                List.concat(acc, collect_call_sites_with_path(stmt, new_path))
            )

        BinaryExpression(data) ->
            left_sites = collect_call_sites_with_path(data.left, Str.concat(path, ".left"))
            right_sites = collect_call_sites_with_path(data.right, Str.concat(path, ".right"))
            List.concat(left_sites, right_sites)

        UnaryExpression(data) ->
            collect_call_sites_with_path(data.argument, Str.concat(path, ".arg"))

        ConditionalExpression(data) ->
            test_sites = collect_call_sites_with_path(data.test, Str.concat(path, ".test"))
            cons_sites = collect_call_sites_with_path(data.consequent, Str.concat(path, ".cons"))
            alt_sites = collect_call_sites_with_path(data.alternate, Str.concat(path, ".alt"))
            List.concat(test_sites, List.concat(cons_sites, alt_sites))

        ArrayExpression(data) ->
            walk_with_index(data.elements, [], |acc, elem, idx|
                new_path = Str.concat(path, ".elem$(Num.to_str(idx))")
                List.concat(acc, collect_call_sites_with_path(elem, new_path))
            )

        ObjectExpression(data) ->
            walk_with_index(data.properties, [], |acc, prop, idx|
                new_path = Str.concat(path, ".prop$(Num.to_str(idx))")
                List.concat(acc, collect_call_sites_with_path(prop, new_path))
            )

        Property(data) ->
            key_sites = collect_call_sites_with_path(data.key, Str.concat(path, ".key"))
            val_sites = collect_call_sites_with_path(data.value, Str.concat(path, ".val"))
            List.concat(key_sites, val_sites)

        ReturnStatement(data) ->
            when data.argument is
                Some(arg) -> collect_call_sites_with_path(arg, Str.concat(path, ".return"))
                None -> []

        IfStatement(data) ->
            test_sites = collect_call_sites_with_path(data.test, Str.concat(path, ".test"))
            cons_sites = collect_call_sites_with_path(data.consequent, Str.concat(path, ".cons"))
            alt_sites = when data.alternate is
                Some(alt) -> collect_call_sites_with_path(alt, Str.concat(path, ".alt"))
                None -> []
            List.concat(test_sites, List.concat(cons_sites, alt_sites))

        VariableDeclaration(data) ->
            walk_with_index(data.declarations, [], |acc, decl, idx|
                new_path = Str.concat(path, ".decl$(Num.to_str(idx))")
                List.concat(acc, collect_call_sites_with_path(decl, new_path))
            )

        VariableDeclarator(data) ->
            when data.init is
                Some(init) -> collect_call_sites_with_path(init, Str.concat(path, ".init"))
                None -> []

        AssignmentExpression(data) ->
            left_sites = collect_call_sites_with_path(data.left, Str.concat(path, ".left"))
            right_sites = collect_call_sites_with_path(data.right, Str.concat(path, ".right"))
            List.concat(left_sites, right_sites)

        MemberExpression(data) ->
            obj_sites = collect_call_sites_with_path(data.object, Str.concat(path, ".obj"))
            prop_sites = if data.computed then
                collect_call_sites_with_path(data.property, Str.concat(path, ".prop"))
            else
                []
            List.concat(obj_sites, prop_sites)

        _ -> []

# Simple type inference based on literal values and operations
infer_type : Node -> TypeInfo
infer_type = |node|
    when node is
        NumberLiteral(_) -> NumberType
        StringLiteral(_) -> StringType
        BooleanLiteral(_) -> BooleanType
        NullLiteral(_) -> NullType
        UndefinedLiteral(_) -> UndefinedType

        ArrayExpression(_data) ->
            ArrayType

        ObjectExpression(_data) ->
            ObjectType

        BinaryExpression(data) ->
            # Simplified type inference for binary operators
            NumberType

        UnaryExpression(data) ->
            # Simplified type inference for unary operators
            UnknownType

        ConditionalExpression(data) ->
            # Could be either type from branches
            UnionType

        Identifier(_) ->
            # Would need context to resolve variable types
            UnknownType

        CallExpression(_) ->
            # Would need to analyze the function to know return type
            UnknownType

        FunctionExpression(_) | ArrowFunctionExpression(_) ->
            FunctionType

        _ -> UnknownType

types_equal : TypeInfo, TypeInfo -> Bool
types_equal = |t1, t2|
    when (t1, t2) is
        (NumberType, NumberType) -> Bool.true
        (StringType, StringType) -> Bool.true
        (BooleanType, BooleanType) -> Bool.true
        (NullType, NullType) -> Bool.true
        (UndefinedType, UndefinedType) -> Bool.true
        (UnknownType, UnknownType) -> Bool.true
        (AnyType, _) | (_, AnyType) -> Bool.true
        (ArrayType, ArrayType) -> Bool.true
        (ObjectType, ObjectType) -> Bool.true
        (FunctionType, FunctionType) -> Bool.true
        (UnionType, UnionType) -> Bool.true
        _ -> Bool.false

# Infer types for all call sites
infer_call_site_types : List CallSite, MonomorphizationContext -> List CallSite
infer_call_site_types = |call_sites, _ctx|
    # For now, just return the call sites as-is
    # In a full implementation, we'd use the context to refine types
    call_sites

# Generate specialized versions of functions
generate_specializations : List (Str, Node), List CallSite, MonomorphizationContext -> MonomorphizationContext
generate_specializations = |functions, call_sites, ctx|
    # Group call sites by function name
    call_sites_by_function = group_call_sites_by_function(call_sites)

    # For each function, create specializations for unique type signatures
    List.walk(functions, ctx, |current_ctx, (fn_name, fn_node)|
        when find_first(call_sites_by_function, |entry| entry.0 == fn_name) is
            Ok((_, sites)) ->
                # Get unique type signatures
                unique_signatures = get_unique_type_signatures(sites)

                # Create a specialization for each unique signature
                List.walk(unique_signatures, current_ctx, |ctx2, signature|
                    specialize_function(fn_name, fn_node, signature, ctx2)
                )
            Err(_) ->
                # Function is never called, keep original
                current_ctx
    )

group_call_sites_by_function : List CallSite -> List (Str, List CallSite)
group_call_sites_by_function = |call_sites|
    List.walk(call_sites, [], |acc, site|
        # Check if function already exists in accumulator
        existing = List.keep_if(acc, |entry| entry.0 == site.function_name)
        if List.is_empty(existing) then
            List.append(acc, (site.function_name, [site]))
        else
            # Update existing entry
            List.map(acc, |entry|
                if entry.0 == site.function_name then
                    (entry.0, List.append(entry.1, site))
                else
                    entry
            )
    )

get_unique_type_signatures : List CallSite -> List (List TypeInfo)
get_unique_type_signatures = |sites|
    List.walk(sites, [], |acc, site|
        if List.any(acc, |sig| type_signatures_equal(sig, site.argument_types)) then
            acc
        else
            List.append(acc, site.argument_types)
    )

type_signatures_equal : List TypeInfo, List TypeInfo -> Bool
type_signatures_equal = |sig1, sig2|
    if List.len(sig1) != List.len(sig2) then
        Bool.false
    else
        List.map2(sig1, sig2, types_equal)
        |> List.all(|x| x)

specialize_function : Str, Node, List TypeInfo, MonomorphizationContext -> MonomorphizationContext
specialize_function = |fn_name, fn_node, arg_types, ctx|
    # Generate specialized function name
    specialized_name = generate_specialized_name(fn_name, arg_types, ctx.next_specialized_id)

    # Create specialized version with type annotations (as comments for now)
    specialized_node = when fn_node is
        FunctionDeclaration(data) ->
            FunctionDeclaration({
                data &
                id: Identifier({ name: specialized_name }),
                # In a full implementation, we'd also specialize the body based on types
            })

        FunctionExpression(data) ->
            FunctionExpression({
                data &
                id: Some(Identifier({ name: specialized_name })),
            })

        ArrowFunctionExpression(data) ->
            # Arrow functions can't have names, so wrap in a variable
            fn_node

        _ -> fn_node

    # Add to context
    {
        ctx &
        specialized_functions: List.append(ctx.specialized_functions, (specialized_name, specialized_node)),
        next_specialized_id: ctx.next_specialized_id + 1,
    }

generate_specialized_name : Str, List TypeInfo, U64 -> Str
generate_specialized_name = |base_name, arg_types, id|
    type_suffix = List.map(arg_types, type_to_suffix)
        |> Str.join_with("_")

    if Str.is_empty(type_suffix) then
        Str.concat(base_name, "_mono$(Num.to_str(id))")
    else
        Str.concat(base_name, "_$(type_suffix)_$(Num.to_str(id))")

type_to_suffix : TypeInfo -> Str
type_to_suffix = |type_info|
    when type_info is
        NumberType -> "num"
        StringType -> "str"
        BooleanType -> "bool"
        NullType -> "null"
        UndefinedType -> "undef"
        ArrayType -> "arr"
        ObjectType -> "obj"
        FunctionType -> "fn"
        UnionType -> "union"
        AnyType -> "any"
        UnknownType -> "unk"

# Rewrite the AST to use specialized functions
rewrite_with_specializations : Node, MonomorphizationContext -> Node
rewrite_with_specializations = |ast, ctx|
    # Add specialized functions to the program
    when ast is
        Program(data) ->
            # Add all specialized functions at the beginning
            specialized_nodes = List.map(ctx.specialized_functions, |entry| entry.1)
            Program({ data & body: List.concat(specialized_nodes, data.body) })
        _ -> ast

# Helper functions
walk_with_index : List a, b, (b, a, U64 -> b) -> b
walk_with_index = |list, init, fn|
    List.walk(list, (init, 0u64), |(acc, idx), elem|
        (fn(acc, elem, idx), idx + 1)
    ) |> .0

find_first : List a, (a -> Bool) -> Result a [NotFound]
find_first = |list, predicate|
    List.walk(list, Err(NotFound), |result, elem|
        when result is
            Err(NotFound) ->
                if predicate(elem) then
                    Ok(elem)
                else
                    Err(NotFound)
            Ok(_) -> result
    )
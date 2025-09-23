module [
    handle_js_features,
    CoercionRule,
    TruthinessValue,
    PrototypeChain,
]

import Ast
import SimpleComprehensiveType as Type
import Option exposing [Option]

# JavaScript coercion rules
CoercionRule : [
    # Automatic type coercion in operators
    NumericCoercion Type.Type Type.Type,  # to number
    StringCoercion Type.Type,              # to string
    BooleanCoercion Type.Type,             # to boolean
]

# Truthiness values in JavaScript
TruthinessValue : [
    Truthy Type.Type,
    Falsy Type.Type,
    Unknown,
]

# Prototype chain for objects
PrototypeChain : {
    object_type: Type.Type,
    prototype: Option Type.Type,
    properties: List { key: Str, value: Type.Type, writable: Bool },
}

# JavaScript-specific context
JSContext : {
    strict_mode: Bool,
    async_context: Bool,
    generator_context: Bool,
    module_type: [CommonJS, ESModule, Script],
    coercion_rules: List CoercionRule,
    hoisted_vars: List { name: Str, type: Type.Type },
    this_type: Type.Type,
}

# Create default JS context
default_js_context : JSContext
default_js_context = {
    strict_mode: Bool.false,
    async_context: Bool.false,
    generator_context: Bool.false,
    module_type: Script,
    coercion_rules: default_coercion_rules,
    hoisted_vars: [],
    this_type: Type.mk_unknown,
}

# Default coercion rules
default_coercion_rules : List CoercionRule
default_coercion_rules = [
    NumericCoercion Type.mk_string Type.mk_number,
    NumericCoercion Type.mk_boolean Type.mk_number,
    NumericCoercion Type.mk_null Type.mk_number,
    StringCoercion Type.mk_number,
    StringCoercion Type.mk_boolean,
    StringCoercion Type.mk_null,
    StringCoercion Type.mk_undefined,
    BooleanCoercion Type.mk_number,
    BooleanCoercion Type.mk_string,
    BooleanCoercion Type.mk_null,
    BooleanCoercion Type.mk_undefined,
]

# Handle JavaScript-specific features
handle_js_features : Ast.Node, JSContext -> (Type.Type, JSContext)
handle_js_features = |node, ctx|
    when node is
        # Handle 'use strict' directive
        Directive({ directive, expression }) ->
            if directive == "use strict" then
                (Type.mk_undefined, { ctx & strict_mode: Bool.true })
            else
                (Type.mk_undefined, ctx)

        # Handle async functions
        ArrowFunctionExpression({ async, body, generator, params }) ->
            new_ctx = { ctx &
                async_context: async,
                generator_context: generator
            }

            return_type = if async then
                # Async functions return promises
                Type.mk_unknown  # Would be Promise<T> if we had generics
            else if generator then
                # Generators return iterators
                Type.mk_unknown  # Would be Iterator<T>
            else
                Type.mk_unknown

            (return_type, new_ctx)

        # Handle this expressions
        ThisExpression({}) ->
            (ctx.this_type, ctx)

        # Handle typeof operator
        UnaryExpression({ operator, argument, prefix }) ->
            when operator is
                Typeof ->
                    # typeof always returns a string
                    (Type.mk_string, ctx)
                Void ->
                    # void always returns undefined
                    (Type.mk_undefined, ctx)
                Delete ->
                    # delete returns boolean
                    (Type.mk_boolean, ctx)
                _ ->
                    handle_unary_coercion(operator, argument, ctx)

        # Handle in operator
        BinaryExpression({ left, right, operator }) ->
            when operator is
                In ->
                    # 'in' operator returns boolean
                    (Type.mk_boolean, ctx)
                Instanceof ->
                    # 'instanceof' returns boolean
                    (Type.mk_boolean, ctx)
                _ ->
                    handle_binary_coercion(left, right, operator, ctx)

        # Handle template literals
        TemplateLiteral({ quasis, expressions }) ->
            # Template literals always produce strings
            (Type.mk_string, ctx)

        # Handle spread operator
        SpreadElement({ argument }) ->
            arg_type = infer_type(argument, ctx)
            # Spread unpacks arrays/iterables
            when arg_type is
                _ -> (Type.mk_unknown, ctx)

        # Handle destructuring
        ArrayPattern({ elements }) ->
            # Array destructuring
            (Type.mk_unknown, ctx)

        ObjectPattern({ properties }) ->
            # Object destructuring
            (Type.mk_unknown, ctx)

        # Handle optional chaining
        ChainExpression({ expression }) ->
            expr_type = infer_type(expression, ctx)
            # Optional chaining can produce undefined
            (Type.mk_union([expr_type, Type.mk_undefined]), ctx)

        # Handle logical expressions
        LogicalExpression({ left, right, operator }) ->
            left_type = infer_type(left, ctx)
            right_type = infer_type(right, ctx)
            # Logical operators return one of their operands
            (Type.mk_union([left_type, right_type]), ctx)

        # Handle class declarations
        ClassDeclaration({ id, superClass, body, decorators }) ->
            # Classes create constructor functions
            (Type.mk_unknown, ctx)

        # Handle import/export
        ImportDeclaration({ specifiers, source }) ->
            new_ctx = { ctx & module_type: ESModule }
            (Type.mk_undefined, new_ctx)

        ExportNamedDeclaration({ declaration, specifiers, source }) ->
            new_ctx = { ctx & module_type: ESModule }
            (Type.mk_undefined, new_ctx)

        _ ->
            (Type.mk_unknown, ctx)

# Handle unary operator coercion
handle_unary_coercion : Ast.UnaryOperator, Ast.Node, JSContext -> (Type.Type, JSContext)
handle_unary_coercion = |op, arg, ctx|
    arg_type = infer_type(arg, ctx)

    when op is
        Plus ->
            # Unary + coerces to number
            (Type.mk_number, ctx)
        Minus ->
            # Unary - coerces to number
            (Type.mk_number, ctx)
        Bang ->
            # ! coerces to boolean then negates
            (Type.mk_boolean, ctx)
        Tilde ->
            # ~ coerces to number for bitwise NOT
            (Type.mk_number, ctx)
        _ ->
            (Type.mk_unknown, ctx)

# Handle binary operator coercion
handle_binary_coercion : Ast.Node, Ast.Node, Ast.BinaryOperator, JSContext -> (Type.Type, JSContext)
handle_binary_coercion = |left, right, op, ctx|
    left_type = infer_type(left, ctx)
    right_type = infer_type(right, ctx)

    when op is
        Plus ->
            # + operator: string if either operand is string, else number
            if is_string_like(left_type) || is_string_like(right_type) then
                (Type.mk_string, ctx)
            else
                (Type.mk_number, ctx)

        Minus | Star | Slash | Percent |
        LeftShift | RightShift | UnsignedRightShift |
        Ampersand | Pipe | Caret ->
            # These operators always coerce to number
            (Type.mk_number, ctx)

        LessThan | LessThanEqual | GreaterThan | GreaterThanEqual |
        EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
            # Comparison operators return boolean
            (Type.mk_boolean, ctx)

        _ ->
            (Type.mk_unknown, ctx)

# Check if type is string-like
is_string_like : Type.Type -> Bool
is_string_like = |t|
    Type.is_assignable_to(t, Type.mk_string)

# Infer type from node (simplified)
infer_type : Ast.Node, JSContext -> Type.Type
infer_type = |node, ctx|
    when node is
        NumberLiteral(_) -> Type.mk_number
        StringLiteral(_) -> Type.mk_string
        BooleanLiteral(_) -> Type.mk_boolean
        NullLiteral({}) -> Type.mk_null
        UndefinedLiteral({}) -> Type.mk_undefined
        ArrayExpression(_) -> Type.mk_array(Type.mk_unknown)
        ObjectExpression(_) -> Type.mk_object([])
        Identifier({ name }) ->
            # Check hoisted variables
            when List.find_first(ctx.hoisted_vars, |v| v.name == name) is
                Ok(var) -> var.type
                Err(_) -> Type.mk_unknown
        _ -> Type.mk_unknown

# Determine truthiness of a value
get_truthiness : Type.Type -> TruthinessValue
get_truthiness = |type|
    if Type.is_assignable_to(type, Type.mk_boolean) then
        Unknown  # Boolean can be true or false
    else if Type.is_assignable_to(type, Type.mk_null) ||
            Type.is_assignable_to(type, Type.mk_undefined) then
        Falsy Type.mk_null  # null and undefined are falsy
    else if Type.is_assignable_to(type, Type.mk_number) then
        Unknown  # 0 and NaN are falsy, others truthy
    else if Type.is_assignable_to(type, Type.mk_string) then
        Unknown  # Empty string is falsy, others truthy
    else
        Truthy type  # Objects, arrays, functions are truthy

# Handle hoisting in JavaScript
handle_hoisting : List Ast.Node -> List { name: Str, type: Type.Type }
handle_hoisting = |nodes|
    List.walk(nodes, [], |acc, node|
        when node is
            FunctionDeclaration({ id, params, body, async, generator }) ->
                when id is
                    Some(Identifier({ name })) ->
                        List.append(acc, { name, type: Type.mk_unknown })
                    _ -> acc

            VariableDeclaration({ declarations, kind }) ->
                when kind is
                    Var ->
                        # var declarations are hoisted
                        List.concat(acc, List.keep_oks(declarations, |decl|
                            when decl is
                                VariableDeclarator({ id, init }) ->
                                    when id is
                                        Identifier({ name }) ->
                                            Ok({ name, type: Type.mk_undefined })
                                        _ -> Err(NoName)
                                _ -> Err(NotDeclarator)))
                    _ -> acc

            _ -> acc)

# Check if value can be coerced to target type
can_coerce : Type.Type, Type.Type, JSContext -> Bool
can_coerce = |from, to, ctx|
    if Type.is_assignable_to(from, to) then
        Bool.true
    else
        # Check coercion rules
        List.any(ctx.coercion_rules, |rule|
            when rule is
                NumericCoercion source_type target_type ->
                    Type.is_assignable_to(from, source_type) &&
                    Type.is_assignable_to(to, target_type)
                StringCoercion source_type ->
                    Type.is_assignable_to(from, source_type) &&
                    Type.is_assignable_to(to, Type.mk_string)
                BooleanCoercion source_type ->
                    Type.is_assignable_to(from, source_type) &&
                    Type.is_assignable_to(to, Type.mk_boolean))
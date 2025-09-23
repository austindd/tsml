module [
    Constraint,
    TypeEnv,
    generate_constraints,
    initial_env,
    extend_env,
    lookup_env,
]

import Ast exposing [Node, BinaryOperator, UnaryOperator]
import Type exposing [Type, TypeVar, TypeScheme]
import Option exposing [Option]
import Dict
import Set

Constraint : [
    Equal Type Type,
    Subtype Type Type,
    HasField Type Str Type,
    IsCallable Type (List Type) Type,
]

TypeEnv : Dict.Dict Str TypeScheme

initial_env : TypeEnv
initial_env = Dict.empty {}

extend_env : TypeEnv, Str, TypeScheme -> TypeEnv
extend_env = |env, name, scheme|
    Dict.insert env name scheme

lookup_env : TypeEnv, Str -> Option TypeScheme
lookup_env = |env, name|
    when Dict.get env name is
        Ok scheme -> Some scheme
        Err _ -> None

generate_constraints : Node, TypeEnv -> (Type, List Constraint, U32)
generate_constraints = |node, env|
    generate_constraints_helper node env 0

generate_constraints_helper : Node, TypeEnv, U32 -> (Type, List Constraint, U32)
generate_constraints_helper = |node, env, next_var|
    when node is
        Identifier { name } ->
            when lookup_env env name is
                Some scheme ->
                    (instantiated, new_next) = instantiate_scheme scheme next_var
                    (instantiated, [], new_next)

                None ->
                    var_type = Type.mk_type_var next_var
                    (var_type, [], next_var + 1)

        NumberLiteral { value } ->
            num_val = Str.to_f64 value |> Result.with_default 0.0
            (Type.mk_literal (NumLit num_val), [], next_var)

        StringLiteral { value } ->
            (Type.mk_literal (StrLit value), [], next_var)

        BooleanLiteral { value } ->
            (Type.mk_literal (BoolLit value), [], next_var)

        NullLiteral _ ->
            (Type.mk_literal NullLit, [], next_var)

        UndefinedLiteral _ ->
            (Type.mk_literal UndefinedLit, [], next_var)

        ArrayExpression { elements } ->
            elem_var = Type.mk_type_var next_var
            (elem_types, elem_constraints, final_var) =
                List.walk elements ([], [], next_var + 1) |(types, constraints, var), elem|
                    (elem_type, elem_cs, new_var) = generate_constraints_helper elem env var
                    (
                        List.append types elem_type,
                        List.concat constraints elem_cs,
                        new_var,
                    )

            unify_constraints = List.map elem_types |et| Equal elem_var et
            all_constraints = List.concat elem_constraints unify_constraints
            (Type.mk_array elem_var, all_constraints, final_var)

        ObjectExpression { properties } ->
            (fields, all_constraints, final_var) =
                List.walk properties ([], [], next_var) |(flds, cs, var), prop|
                    when prop is
                        Property { key, value } ->
                            key_name = extract_property_key key
                            (val_type, val_cs, new_var) = generate_constraints_helper value env var
                            field = { key: key_name, value: val_type, optional: Bool.false }
                            (
                                List.append flds field,
                                List.concat cs val_cs,
                                new_var,
                            )

                        _ -> (flds, cs, var)

            (Type.mk_record(fields), all_constraints, final_var)

        FunctionExpression({ params, body }) ->
            (param_types, param_env, param_constraints, var_after_params) =
                List.walk params ([], env, [], next_var) |(types, e, cs, v), param|
                    param_name = extract_param_name param
                    param_type = Type.mk_type_var v
                    param_scheme = { forall: Set.empty {}, body: param_type }
                    new_env = extend_env e param_name param_scheme
                    (
                        List.append types param_type,
                        new_env,
                        cs,
                        v + 1,
                    )

            (body_type, body_constraints, final_var) = generate_constraints_helper body param_env var_after_params
            all_constraints = List.concat param_constraints body_constraints
            (Type.mk_function(param_types, body_type), all_constraints, final_var)

        ArrowFunctionExpression({ params, body }) ->
            (param_types, param_env, param_constraints, var_after_params) =
                List.walk params ([], env, [], next_var) |(types, e, cs, v), param|
                    param_name = extract_param_name param
                    param_type = Type.mk_type_var v
                    param_scheme = { forall: Set.empty {}, body: param_type }
                    new_env = extend_env e param_name param_scheme
                    (
                        List.append types param_type,
                        new_env,
                        cs,
                        v + 1,
                    )

            (body_type, body_constraints, final_var) = generate_constraints_helper body param_env var_after_params
            all_constraints = List.concat param_constraints body_constraints
            (Type.mk_function(param_types, body_type), all_constraints, final_var)

        CallExpression({ callee, arguments }) ->
            (callee_type, callee_cs, var_after_callee) = generate_constraints_helper callee env next_var

            (arg_types, arg_cs, var_after_args) =
                List.walk arguments ([], [], var_after_callee) |(types, cs, v), arg|
                    (arg_type, arg_constraints, new_var) = generate_constraints_helper arg env v
                    (
                        List.append types arg_type,
                        List.concat cs arg_constraints,
                        new_var,
                    )

            result_type = Type.mk_type_var var_after_args
            call_constraint = IsCallable callee_type arg_types result_type
            all_constraints = List.concat callee_cs arg_cs |> List.append call_constraint
            (result_type, all_constraints, var_after_args + 1)

        MemberExpression { object, property, computed } ->
            (obj_type, obj_cs, var_after_obj) = generate_constraints_helper object env next_var

            if computed then
                (prop_type, prop_cs, var_after_prop) = generate_constraints_helper property env var_after_obj
                result_type = Type.mk_type_var var_after_prop
                all_constraints = List.concat obj_cs prop_cs
                (result_type, all_constraints, var_after_prop + 1)
            else
                prop_name = extract_property_key property
                result_type = Type.mk_type_var var_after_obj
                field_constraint = HasField obj_type prop_name result_type
                all_constraints = List.append obj_cs field_constraint
                (result_type, all_constraints, var_after_obj + 1)

        BinaryExpression { left, operator, right } ->
            (left_type, left_cs, var_after_left) = generate_constraints_helper left env next_var
            (right_type, right_cs, var_after_right) = generate_constraints_helper right env var_after_left

            result_type = infer_binary_op_type operator left_type right_type var_after_right
            all_constraints = List.concat left_cs right_cs
            (result_type, all_constraints, var_after_right + 1)

        UnaryExpression { operator, argument } ->
            (arg_type, arg_cs, var_after_arg) = generate_constraints_helper argument env next_var
            result_type = infer_unary_op_type operator arg_type
            (result_type, arg_cs, var_after_arg)

        ConditionalExpression { test, consequent, alternate } ->
            (test_type, test_cs, var_after_test) = generate_constraints_helper test env next_var
            (cons_type, cons_cs, var_after_cons) = generate_constraints_helper consequent env var_after_test
            (alt_type, alt_cs, var_after_alt) = generate_constraints_helper alternate env var_after_cons

            result_type = Type.mk_union [cons_type, alt_type]
            all_constraints = List.concat test_cs cons_cs |> List.concat alt_cs
            (result_type, all_constraints, var_after_alt)

        BlockStatement { body } -> process_statements(body, env, next_var)
        FunctionBody { body } -> process_statements(body, env, next_var)
        Program { body } -> process_statements(body, env, next_var)
        VariableDeclaration { declarations } ->
            process_var_declarations(declarations, env, next_var)

        ReturnStatement { argument } ->
            when argument is
                Some arg ->
                    generate_constraints_helper(arg, env, next_var)

                None ->
                    (Type.mk_literal(UndefinedLit), [], next_var)

        Directive { expression } ->
            generate_constraints_helper(expression, env, next_var)

        IfStatement { test, consequent, alternate } ->
            (test_type, test_cs, var_after_test) = generate_constraints_helper test env next_var
            (cons_type, cons_cs, var_after_cons) = generate_constraints_helper consequent env var_after_test

            when alternate is
                Some alt ->
                    (alt_type, alt_cs, var_after_alt) = generate_constraints_helper alt env var_after_cons
                    all_constraints = List.concat test_cs cons_cs |> List.concat alt_cs
                    (Type.mk_literal UndefinedLit, all_constraints, var_after_alt)

                None ->
                    all_constraints = List.concat test_cs cons_cs
                    (Type.mk_literal UndefinedLit, all_constraints, var_after_cons)

        _ ->
            (Type.mk_type_var next_var, [], next_var + 1)

extract_property_key : Node -> Str
extract_property_key = |node|
    when node is
        Identifier { name } -> name
        StringLiteral { value } -> value
        NumberLiteral { value } -> value
        _ -> "_unknown_"

extract_param_name : Node -> Str
extract_param_name = |node|
    when node is
        Identifier { name } -> name
        _ -> "_param_"

instantiate_scheme : TypeScheme, U32 -> (Type, U32)
instantiate_scheme = |scheme, next_var|
    if Set.is_empty scheme.forall then
        (scheme.body, next_var)
    else
        (substitutions, new_next) = Set.walk scheme.forall (Dict.empty {}, next_var) |(subs, nv), tv|
            (Dict.insert subs tv (Type.mk_type_var nv), nv + 1)

        instantiated = apply_substitutions scheme.body substitutions
        (instantiated, new_next)

apply_substitutions : Type, Dict.Dict TypeVar Type -> Type
apply_substitutions = |type, subs|
    when type is
        Var v ->
            Dict.get(subs, v)
            |> Result.with_default(type)

        Array elem ->
            Type.mk_array (apply_substitutions elem subs)

        Tuple elems ->
            Type.mk_tuple (List.map elems |e| apply_substitutions e subs)

        Function { params, ret } ->
            Type.mk_function
                (List.map params |p| apply_substitutions p subs)
                (apply_substitutions ret subs)

        Record fields ->
            Type.mk_record
                (
                    List.map fields |f|
                        { f & value: apply_substitutions f.value subs }
                )

        Union types ->
            Type.mk_union (List.map types |t| apply_substitutions t subs)

        Intersection types ->
            Type.mk_intersection (List.map types |t| apply_substitutions t subs)

        Negation inner ->
            Type.mk_negation (apply_substitutions inner subs)

        _ -> type

infer_binary_op_type : BinaryOperator, Type, Type, U32 -> Type
infer_binary_op_type = |op, left, right, next_var|
    when op is
        EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
            Type.mk_primitive "boolean"

        LessThan | LessThanEqual | GreaterThan | GreaterThanEqual ->
            Type.mk_primitive "boolean"

        Plus ->
            Type.mk_union [Type.mk_primitive "number", Type.mk_primitive "string"]

        Minus | Star | Slash | Percent ->
            Type.mk_primitive "number"

        Pipe | Caret | Ampersand | LeftShift | RightShift | UnsignedRightShift ->
            Type.mk_primitive "number"

        In | Instanceof ->
            Type.mk_primitive "boolean"

infer_unary_op_type : UnaryOperator, Type -> Type
infer_unary_op_type = |op, arg|
    when op is
        Bang -> Type.mk_primitive "boolean"
        Plus | Minus | Tilde -> Type.mk_primitive "number"
        Typeof -> Type.mk_primitive "string"
        Void -> Type.mk_literal UndefinedLit
        Delete -> Type.mk_primitive "boolean"

process_statements : List Node, TypeEnv, U32 -> (Type, List Constraint, U32)
process_statements = |stmts, env, next_var|
    when stmts is
        [] -> (Type.mk_literal UndefinedLit, [], next_var)
        [single] -> generate_constraints_helper single env next_var
        [first, .. as rest] ->
            (first_type, first_cs, var_after_first) = generate_constraints_helper first env next_var
            (rest_type, rest_cs, final_var) = process_statements rest env var_after_first
            (rest_type, List.concat first_cs rest_cs, final_var)

process_var_declarations : List Node, TypeEnv, U32 -> (Type, List Constraint, U32)
process_var_declarations = |decls, env, next_var|
    List.walk decls (Type.mk_literal UndefinedLit, [], next_var) |(_, cs, v), decl|
        when decl is
            VariableDeclarator { id, init } ->
                var_name = extract_param_name id
                when init is
                    Some init_expr ->
                        (init_type, init_cs, new_var) = generate_constraints_helper init_expr env v
                        (Type.mk_literal UndefinedLit, List.concat cs init_cs, new_var)

                    None ->
                        (Type.mk_literal UndefinedLit, cs, v)

            _ -> (Type.mk_literal UndefinedLit, cs, v)

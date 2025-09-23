module [
    InferenceResult,
    TypedNode,
    infer_program,
    infer_expression,
    generalize,
    get_principal_type,
]

import Ast exposing [Node]
import Type exposing [Type, TypeVar, TypeScheme]
import TypeConstraint exposing [Constraint, TypeEnv, generate_constraints]
import TypeUnify exposing [Substitution, UnificationError, solve_constraints, apply_substitution_to_type]
import TypeAlgebra
import Result exposing [Result]
import Option exposing [Option]

InferenceResult : {
    type : Type,
    substitution : Substitution,
    type_env : TypeEnv,
}

TypedNode : {
    node : Node,
    type : Type,
}

infer_program : Node -> Result InferenceResult UnificationError
infer_program = \ast ->
    env = build_initial_env {}
    (prog_type, constraints, _) = generate_constraints ast env

    when solve_constraints constraints is
        Ok substitution ->
            final_type = apply_substitution_to_type prog_type substitution
            simplified_type = TypeAlgebra.simplify final_type
            final_env = apply_substitution_to_env env substitution

            Ok {
                type: simplified_type,
                substitution,
                type_env: final_env,
            }
        Err error -> Err error

infer_expression : Node, TypeEnv -> Result Type UnificationError
infer_expression = \expr, env ->
    (expr_type, constraints, _) = generate_constraints expr env

    when solve_constraints constraints is
        Ok substitution ->
            final_type = apply_substitution_to_type expr_type substitution
            Ok (TypeAlgebra.simplify final_type)
        Err error -> Err error

generalize : Type, TypeEnv -> TypeScheme
generalize = \type, env ->
    free_in_type = Type.free_type_vars type
    free_in_env = env_free_vars env
    generalizable = Set.difference free_in_type free_in_env

    {
        forall: generalizable,
        body: type,
    }

get_principal_type : Node -> Result Type UnificationError
get_principal_type = \node ->
    when infer_program node is
        Ok result -> Ok result.type
        Err e -> Err e

build_initial_env : {} -> TypeEnv
build_initial_env = \{} ->
    Dict.empty {}
    |> add_builtin "console" (console_type {})
    |> add_builtin "Math" (math_type {})
    |> add_builtin "Array" (array_constructor_type {})
    |> add_builtin "Object" (object_constructor_type {})
    |> add_builtin "String" (string_constructor_type {})
    |> add_builtin "Number" (number_constructor_type {})
    |> add_builtin "Boolean" (boolean_constructor_type {})
    |> add_builtin "JSON" (json_type {})
    |> add_builtin "Promise" (promise_constructor_type {})
    |> add_builtin "undefined" (undefined_type {})
    |> add_builtin "null" (null_type {})
    |> add_builtin "true" (true_type {})
    |> add_builtin "false" (false_type {})
    |> add_builtin "parseInt" (parse_int_type {})
    |> add_builtin "parseFloat" (parse_float_type {})
    |> add_builtin "isNaN" (is_nan_type {})
    |> add_builtin "isFinite" (is_finite_type {})

add_builtin : TypeEnv, Str, Type -> TypeEnv
add_builtin = \env, name, type ->
    scheme = { forall: Set.empty {}, body: type }
    TypeConstraint.extend_env env name scheme

console_type : {} -> Type
console_type = \{} ->
    Type.mk_record [
        { key: "log", value: Type.mk_function [Type.mk_top] (Type.mk_literal UndefinedLit), optional: Bool.false },
        { key: "error", value: Type.mk_function [Type.mk_top] (Type.mk_literal UndefinedLit), optional: Bool.false },
        { key: "warn", value: Type.mk_function [Type.mk_top] (Type.mk_literal UndefinedLit), optional: Bool.false },
        { key: "info", value: Type.mk_function [Type.mk_top] (Type.mk_literal UndefinedLit), optional: Bool.false },
    ]

math_type : {} -> Type
math_type = \{} ->
    Type.mk_record [
        { key: "PI", value: Type.mk_primitive "number", optional: Bool.false },
        { key: "E", value: Type.mk_primitive "number", optional: Bool.false },
        { key: "abs", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "sin", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "cos", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "tan", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "sqrt", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "pow", value: Type.mk_function [Type.mk_primitive "number", Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "floor", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "ceil", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "round", value: Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "random", value: Type.mk_function [] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "max", value: Type.mk_function [Type.mk_primitive "number", Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
        { key: "min", value: Type.mk_function [Type.mk_primitive "number", Type.mk_primitive "number"] (Type.mk_primitive "number"), optional: Bool.false },
    ]

array_constructor_type : {} -> Type
array_constructor_type = \{} ->
    elem_var = Type.mk_type_var 1000
    Type.mk_record [
        { key: "isArray", value: Type.mk_function [Type.mk_top] (Type.mk_primitive "boolean"), optional: Bool.false },
        { key: "from", value: Type.mk_function [Type.mk_top] (Type.mk_array elem_var), optional: Bool.false },
        { key: "of", value: Type.mk_function [elem_var] (Type.mk_array elem_var), optional: Bool.false },
    ]

object_constructor_type : {} -> Type
object_constructor_type = \{} ->
    Type.mk_record [
        { key: "keys", value: Type.mk_function [Type.mk_top] (Type.mk_array (Type.mk_primitive "string")), optional: Bool.false },
        { key: "values", value: Type.mk_function [Type.mk_top] (Type.mk_array Type.mk_top), optional: Bool.false },
        { key: "entries", value: Type.mk_function [Type.mk_top] (Type.mk_array (Type.mk_tuple [Type.mk_primitive "string", Type.mk_top])), optional: Bool.false },
        { key: "assign", value: Type.mk_function [Type.mk_top, Type.mk_top] Type.mk_top, optional: Bool.false },
        { key: "create", value: Type.mk_function [Type.mk_top] Type.mk_top, optional: Bool.false },
    ]

string_constructor_type : {} -> Type
string_constructor_type = \{} ->
    Type.mk_function [Type.mk_top] (Type.mk_primitive "string")

number_constructor_type : {} -> Type
number_constructor_type = \{} ->
    Type.mk_function [Type.mk_top] (Type.mk_primitive "number")

boolean_constructor_type : {} -> Type
boolean_constructor_type = \{} ->
    Type.mk_function [Type.mk_top] (Type.mk_primitive "boolean")

json_type : {} -> Type
json_type = \{} ->
    Type.mk_record [
        { key: "parse", value: Type.mk_function [Type.mk_primitive "string"] Type.mk_top, optional: Bool.false },
        { key: "stringify", value: Type.mk_function [Type.mk_top] (Type.mk_primitive "string"), optional: Bool.false },
    ]

promise_constructor_type : {} -> Type
promise_constructor_type = \{} ->
    value_var = Type.mk_type_var 2000
    promise_type = Type.mk_record [
        { key: "then", value: Type.mk_function [Type.mk_function [value_var] Type.mk_top] Type.mk_top, optional: Bool.false },
        { key: "catch", value: Type.mk_function [Type.mk_function [Type.mk_top] Type.mk_top] Type.mk_top, optional: Bool.false },
    ]
    Type.mk_function [Type.mk_function [Type.mk_function [value_var] (Type.mk_literal UndefinedLit), Type.mk_function [Type.mk_top] (Type.mk_literal UndefinedLit)] (Type.mk_literal UndefinedLit)] promise_type

undefined_type : {} -> Type
undefined_type = \{} ->
    Type.mk_literal UndefinedLit

null_type : {} -> Type
null_type = \{} ->
    Type.mk_literal NullLit

true_type : {} -> Type
true_type = \{} ->
    Type.mk_literal (BoolLit(Bool.true))

false_type : {} -> Type
false_type = \{} ->
    Type.mk_literal (BoolLit(Bool.false))

parse_int_type : {} -> Type
parse_int_type = \{} ->
    Type.mk_function [Type.mk_primitive "string", Type.mk_primitive "number"] (Type.mk_primitive "number")

parse_float_type : {} -> Type
parse_float_type = \{} ->
    Type.mk_function [Type.mk_primitive "string"] (Type.mk_primitive "number")

is_nan_type : {} -> Type
is_nan_type = \{} ->
    Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "boolean")

is_finite_type : {} -> Type
is_finite_type = \{} ->
    Type.mk_function [Type.mk_primitive "number"] (Type.mk_primitive "boolean")

apply_substitution_to_env : TypeEnv, Substitution -> TypeEnv
apply_substitution_to_env = \env, subst ->
    Dict.map env \_, scheme ->
        {
            forall: scheme.forall,
            body: apply_substitution_to_type scheme.body subst,
        }

env_free_vars : TypeEnv -> Set.Set TypeVar
env_free_vars = \env ->
    Dict.walk env (Set.empty {}) \acc, _, scheme ->
        scheme_free = Type.free_type_vars scheme.body
        scheme_bound = scheme.forall
        Set.union acc (Set.difference scheme_free scheme_bound)

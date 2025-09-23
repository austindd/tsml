module [
    TypeError,
    format_type_error,
    format_type,
    explain_type_mismatch,
    suggest_fix,
]

import Type exposing [Type, TypeVar]
import TypeUnify exposing [UnificationError]
import Ast exposing [Node]

all2 : List a, List b, (a, b -> Bool) -> Bool
all2 = \list1, list2, predicate ->
    when (list1, list2) is
        ([], []) -> Bool.true
        ([h1, .. as t1], [h2, .. as t2]) ->
            if predicate h1 h2 then
                all2 t1 t2 predicate
            else
                Bool.false
        _ -> Bool.false

TypeError : {
    error : UnificationError,
    location : Option { line : U32, column : U32 },
    context : Option Node,
}

format_type_error : TypeError -> Str
format_type_error = \error ->
    location_str = when error.location is
        Some loc -> "at line $(Num.toStr loc.line), column $(Num.toStr loc.column)"
        None -> "at unknown location"

    error_msg = when error.error is
        InfiniteType var type ->
            """
            Type error $(location_str): Infinite type detected
            Cannot create the infinite type: T$(Num.toStr var) = $(format_type type)
            This usually occurs with recursive data structures without a base case.
            """

        TypeMismatch expected actual ->
            """
            Type error $(location_str): Type mismatch
            Expected: $(format_type expected)
            Actual:   $(format_type actual)

            $(explain_type_mismatch expected actual)

            $(suggest_fix expected actual)
            """

        FieldMissing obj_type field_name ->
            """
            Type error $(location_str): Missing field
            Object type: $(format_type obj_type)
            Missing field: $(field_name)

            The object does not have the required field '$(field_name)'.
            """

        NotCallable type ->
            """
            Type error $(location_str): Not callable
            Type: $(format_type type)

            The expression is not a function and cannot be called.
            Expected a function type but got: $(format_type type)
            """

        ArityMismatch expected actual ->
            """
            Type error $(location_str): Arity mismatch
            Expected $(Num.toStr expected) arguments
            Actual $(Num.toStr actual) arguments

            The function is called with the wrong number of arguments.
            """

    error_msg

format_type : Type -> Str
format_type = \type ->
    format_type_with_depth type 0

format_type_with_depth : Type, U32 -> Str
format_type_with_depth = \type, depth ->
    if depth > 5 then
        "..."
    else
        when type is
            Var id -> "T$(Num.toStr id)"
            Top -> "any"
            Bottom -> "never"
            Primitive name -> name

            Literal lit ->
                when lit is
                    StrLit s -> "\"$(s)\""
                    NumLit n -> Num.toStr n
                    BoolLit b -> if b then "true" else "false"
                    NullLit -> "null"
                    UndefinedLit -> "undefined"

            Record fields ->
                if List.is_empty fields then
                    "{}"
                else if List.len fields > 3 && depth > 2 then
                    "{ ... }"
                else
                    field_strs = List.map fields \{ key, value, optional } ->
                        opt_mark = if optional then "?" else ""
                        "$(key)$(opt_mark): $(format_type_with_depth value (depth + 1))"
                    "{ $(Str.join_with field_strs ", ") }"

            Function { params, ret } ->
                if List.is_empty params then
                    "() => $(format_type_with_depth ret (depth + 1))"
                else
                    param_strs = List.map params \p -> format_type_with_depth p (depth + 1)
                    "($(Str.join_with param_strs ", ")) => $(format_type_with_depth ret (depth + 1))"

            Array elem ->
                "$(format_type_with_depth elem (depth + 1))[]"

            Tuple elems ->
                elem_strs = List.map elems \e -> format_type_with_depth e (depth + 1)
                "[$(Str.join_with elem_strs ", ")]"

            Union types ->
                if List.len types > 3 && depth > 2 then
                    "(... union ...)"
                else
                    type_strs = List.map types \t -> format_type_with_depth t (depth + 1)
                    when type_strs is
                        [] -> "never"
                        [single] -> single
                        many -> "($(Str.join_with many " | "))"

            Intersection types ->
                if List.len types > 3 && depth > 2 then
                    "(... intersection ...)"
                else
                    type_strs = List.map types \t -> format_type_with_depth t (depth + 1)
                    when type_strs is
                        [] -> "any"
                        [single] -> single
                        many -> "($(Str.join_with many " & "))"

            Negation inner ->
                "not $(format_type_with_depth inner (depth + 1))"

            Recursive { var, body } ->
                "μT$(Num.toStr var).$(format_type_with_depth body (depth + 1))"

explain_type_mismatch : Type, Type -> Str
explain_type_mismatch = \expected, actual ->
    when (expected, actual) is
        (Function _, _) if !is_function actual ->
            "A function was expected but a non-function value was provided."

        (_, Function _) if !is_function expected ->
            "A non-function value was expected but a function was provided."

        (Array _, _) if !is_array actual ->
            "An array was expected but a different type was provided."

        (Record expected_fields, Record actual_fields) ->
            missing = find_missing_fields expected_fields actual_fields
            extra = find_extra_fields expected_fields actual_fields
            incompatible = find_incompatible_fields expected_fields actual_fields

            parts = []
            parts = if List.is_empty missing then parts else List.append parts "Missing fields: $(Str.join_with missing ", ")"
            parts = if List.is_empty extra then parts else List.append parts "Extra fields: $(Str.join_with extra ", ")"
            parts = if List.is_empty incompatible then parts else List.append parts "Incompatible fields: $(Str.join_with incompatible ", ")"

            if List.is_empty parts then
                "The record types are incompatible."
            else
                Str.join_with parts "\n"

        (Primitive p1, Primitive p2) ->
            "Cannot use '$(p2)' where '$(p1)' is expected."

        (Union _, _) ->
            "None of the union type alternatives match the provided type."

        (_, Union _) ->
            "The value must match one specific type, not a union of types."

        _ ->
            "The types are incompatible."

suggest_fix : Type, Type -> Str
suggest_fix = \expected, actual ->
    when (expected, actual) is
        (Primitive "number", Primitive "string") ->
            "Try: Use Number() or parseFloat() to convert the string to a number."

        (Primitive "string", Primitive "number") ->
            "Try: Use String() or .toString() to convert the number to a string."

        (Primitive "boolean", _) ->
            "Try: Use Boolean() or !! to convert to a boolean value."

        (Array _, _) if !is_array actual ->
            "Try: Wrap the value in an array: [value]"

        (Function { params }, _) if !is_function actual ->
            param_count = List.len params
            "Try: Provide a function that takes $(Num.toStr param_count) argument(s)."

        (Record fields, _) if !is_record actual ->
            "Try: Provide an object with the required fields."

        _ ->
            ""

is_function : Type -> Bool
is_function = \type ->
    when type is
        Function _ -> True
        _ -> Bool.false

is_array : Type -> Bool
is_array = \type ->
    when type is
        Array _ -> True
        _ -> Bool.false

is_record : Type -> Bool
is_record = \type ->
    when type is
        Record _ -> True
        _ -> Bool.false

find_missing_fields : List { key : Str, value : Type, optional : Bool }, List { key : Str, value : Type, optional : Bool } -> List Str
find_missing_fields = \expected, actual ->
    List.keep_if expected \e_field ->
        !e_field.optional &&
        !List.any actual \a_field -> a_field.key == e_field.key
    |> List.map .key

find_extra_fields : List { key : Str, value : Type, optional : Bool }, List { key : Str, value : Type, optional : Bool } -> List Str
find_extra_fields = \expected, actual ->
    List.keep_if actual \a_field ->
        !List.any expected \e_field -> e_field.key == a_field.key
    |> List.map .key

find_incompatible_fields : List { key : Str, value : Type, optional : Bool }, List { key : Str, value : Type, optional : Bool } -> List Str
find_incompatible_fields = \expected, actual ->
    List.keep_if expected \e_field ->
        List.any actual \a_field ->
            a_field.key == e_field.key && !types_compatible e_field.value a_field.value
    |> List.map .key

types_compatible : Type, Type -> Bool
types_compatible = \t1, t2 ->
    when (t1, t2) is
        (Var _, _) | (_, Var _) -> True
        (Top, _) | (_, Top) -> True
        (Bottom, _) | (_, Bottom) -> True
        (Primitive p1, Primitive p2) -> p1 == p2
        (Literal l1, Literal l2) -> literals_equal l1 l2
        (Array e1, Array e2) -> types_compatible e1 e2
        (Tuple es1, Tuple es2) ->
            List.len es1 == List.len es2 &&
            all2 es1 es2 types_compatible
        (Function f1, Function f2) ->
            List.len f1.params == List.len f2.params &&
            all2 f1.params f2.params types_compatible &&
            types_compatible f1.ret f2.ret
        (Record fs1, Record fs2) ->
            List.all fs1 \f1 ->
                List.any fs2 \f2 ->
                    f1.key == f2.key &&
                    (f1.optional || !f2.optional) &&
                    types_compatible f1.value f2.value
        _ -> Bool.false

literals_equal = \l1, l2 ->
    when (l1, l2) is
        (StrLit s1, StrLit s2) -> s1 == s2
        (NumLit n1, NumLit n2) -> Num.is_approx_eq n1 n2 {}
        (BoolLit b1, BoolLit b2) -> b1 == b2
        (NullLit, NullLit) -> Bool.true
        (UndefinedLit, UndefinedLit) -> Bool.true
        _ -> Bool.false
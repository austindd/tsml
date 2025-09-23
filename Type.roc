module [
    Type,
    TypeVar,
    TypeScheme,
    mk_type_var,
    mk_primitive,
    mk_record,
    mk_function,
    mk_union,
    mk_intersection,
    mk_array,
    mk_tuple,
    mk_literal,
    mk_top,
    mk_bottom,
    mk_negation,
    type_to_str,
    normalize_type,
    substitute,
    free_type_vars,
]

import Option exposing [Option]
import Set

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

TypeVar : U32

Type : [
    Var TypeVar,
    Top,
    Bottom,
    Primitive Str,
    Literal [StrLit Str, NumLit F64, BoolLit Bool, NullLit, UndefinedLit],
    Record (List { key : Str, value : Type, optional : Bool }),
    Function { params : List Type, ret : Type },
    Array Type,
    Tuple (List Type),
    Union (List Type),
    Intersection (List Type),
    Negation Type,
    Recursive { var : TypeVar, body : Type },
]

TypeScheme : {
    forall : Set.Set TypeVar,
    body : Type,
}

mk_type_var : TypeVar -> Type
mk_type_var = \var -> Var var

mk_primitive : Str -> Type
mk_primitive = \name -> Primitive name

mk_record : List { key : Str, value : Type, optional : Bool } -> Type
mk_record = \fields -> Record fields

mk_function : List Type, Type -> Type
mk_function = \params, ret -> Function { params, ret }

mk_union : List Type -> Type
mk_union = \types ->
    when types is
        [] -> Bottom
        [single] -> single
        many -> Union many

mk_intersection : List Type -> Type
mk_intersection = \types ->
    when types is
        [] -> Top
        [single] -> single
        many -> Intersection many

mk_array : Type -> Type
mk_array = \elem -> Array elem

mk_tuple : List Type -> Type
mk_tuple = \elems -> Tuple elems

mk_literal : [StrLit Str, NumLit F64, BoolLit Bool, NullLit, UndefinedLit] -> Type
mk_literal = \lit -> Literal lit

mk_top : Type
mk_top = Top

mk_bottom : Type
mk_bottom = Bottom

mk_negation : Type -> Type
mk_negation = \inner -> Negation inner

type_to_str : Type -> Str
type_to_str = \type ->
    when type is
        Var id -> "T$(Num.toStr id)"
        Top -> "⊤"
        Bottom -> "⊥"
        Primitive name -> name
        Literal lit ->
            when lit is
                StrLit s -> "\"$(s)\""
                NumLit n -> Num.toStr n
                BoolLit b -> if b then "true" else "false"
                NullLit -> "null"
                UndefinedLit -> "undefined"

        Record fields ->
            field_strs = List.map fields \{ key, value, optional } ->
                opt_mark = if optional then "?" else ""
                "$(key)$(opt_mark): $(type_to_str value)"
            "{ $(Str.join_with field_strs ", ") }"

        Function { params, ret } ->
            param_strs = List.map params type_to_str
            "($(Str.join_with param_strs ", ")) => $(type_to_str ret)"

        Array elem -> "$(type_to_str elem)[]"

        Tuple elems ->
            elem_strs = List.map elems type_to_str
            "[$(Str.join_with elem_strs ", ")]"

        Union types ->
            type_strs = List.map types type_to_str
            "($(Str.join_with type_strs " | "))"

        Intersection types ->
            type_strs = List.map types type_to_str
            "($(Str.join_with type_strs " & "))"

        Negation inner -> "¬$(type_to_str inner)"

        Recursive { var, body } -> "μT$(Num.toStr var).$(type_to_str body)"

normalize_type : Type -> Type
normalize_type = \type ->
    when type is
        Union types ->
            flattened = flatten_unions types []
            deduped = dedupe_types flattened
            when deduped is
                [] -> Bottom
                [single] -> single
                many -> Union many

        Intersection types ->
            flattened = flatten_intersections types []
            deduped = dedupe_types flattened
            when deduped is
                [] -> Top
                [single] -> single
                many -> Intersection many

        Negation (Negation inner) -> normalize_type inner
        Negation Top -> Bottom
        Negation Bottom -> Top

        other -> other

flatten_unions : List Type, List Type -> List Type
flatten_unions = \types, acc ->
    when types is
        [] -> acc
        [Union nested, .. as rest] -> flatten_unions (List.concat nested rest) acc
        [Bottom, .. as rest] -> flatten_unions rest acc
        [other, .. as rest] -> flatten_unions rest (List.append acc other)

flatten_intersections : List Type, List Type -> List Type
flatten_intersections = \types, acc ->
    when types is
        [] -> acc
        [Intersection nested, .. as rest] -> flatten_intersections (List.concat nested rest) acc
        [Top, .. as rest] -> flatten_intersections rest acc
        [other, .. as rest] -> flatten_intersections rest (List.append acc other)

dedupe_types : List Type -> List Type
dedupe_types = \types ->
    List.walk types [] \acc, type ->
        if List.any acc (\t -> types_equal t type) then
            acc
        else
            List.append acc type

types_equal : Type, Type -> Bool
types_equal = \t1, t2 ->
    when (t1, t2) is
        (Var v1, Var v2) -> v1 == v2
        (Top, Top) -> Bool.true
        (Bottom, Bottom) -> Bool.true
        (Primitive p1, Primitive p2) -> p1 == p2
        (Literal l1, Literal l2) -> literals_equal l1 l2
        (Array e1, Array e2) -> types_equal e1 e2
        (Tuple es1, Tuple es2) ->
            List.len es1 == List.len es2 &&
            all2 es1 es2 types_equal
        _ -> Bool.false

literals_equal : [StrLit Str, NumLit F64, BoolLit Bool, NullLit, UndefinedLit], [StrLit Str, NumLit F64, BoolLit Bool, NullLit, UndefinedLit] -> Bool
literals_equal = \l1, l2 ->
    when (l1, l2) is
        (StrLit s1, StrLit s2) -> s1 == s2
        (NumLit n1, NumLit n2) -> Num.is_approx_eq n1 n2 {}
        (BoolLit b1, BoolLit b2) -> b1 == b2
        (NullLit, NullLit) -> Bool.true
        (UndefinedLit, UndefinedLit) -> Bool.true
        _ -> Bool.false

substitute : Type, TypeVar, Type -> Type
substitute = \type, var, replacement ->
    when type is
        Var v if v == var -> replacement
        Var _ -> type
        Top -> Top
        Bottom -> Bottom
        Primitive _ -> type
        Literal _ -> type
        Record fields ->
            Record (List.map fields \field ->
                { field & value: substitute field.value var replacement })
        Function { params, ret } ->
            Function {
                params: List.map params \p -> substitute p var replacement,
                ret: substitute ret var replacement,
            }
        Array elem -> Array (substitute elem var replacement)
        Tuple elems -> Tuple (List.map elems \e -> substitute e var replacement)
        Union types -> Union (List.map types \t -> substitute t var replacement)
        Intersection types -> Intersection (List.map types \t -> substitute t var replacement)
        Negation inner -> Negation (substitute inner var replacement)
        Recursive { var: v, body } if v == var -> type
        Recursive { var: v, body } -> Recursive { var: v, body: substitute body var replacement }

free_type_vars : Type -> Set.Set TypeVar
free_type_vars = \type ->
    when type is
        Var v -> Set.single v
        Top | Bottom | Primitive _ | Literal _ -> Set.empty {}
        Record fields ->
            List.walk fields (Set.empty {}) \acc, field ->
                Set.union acc (free_type_vars field.value)
        Function { params, ret } ->
            param_vars = List.walk params (Set.empty {}) \acc, p ->
                Set.union acc (free_type_vars p)
            Set.union param_vars (free_type_vars ret)
        Array elem -> free_type_vars elem
        Tuple elems ->
            List.walk elems (Set.empty {}) \acc, e ->
                Set.union acc (free_type_vars e)
        Union types | Intersection types ->
            List.walk types (Set.empty {}) \acc, t ->
                Set.union acc (free_type_vars t)
        Negation inner -> free_type_vars inner
        Recursive { var, body } ->
            Set.remove (free_type_vars body) var
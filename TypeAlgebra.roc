module [
    subtype,
    join,
    meet,
    negate,
    disjoint,
    equivalent,
    simplify,
]

import Type exposing [Type, TypeVar]
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

subtype : Type, Type -> Bool
subtype = \sub, super ->
    when (sub, super) is
        (_, Top) -> Bool.true
        (Bottom, _) -> Bool.true
        (t1, t2) if types_structurally_equal t1 t2 -> Bool.true

        (Literal l, Primitive "string") -> is_string_literal l
        (Literal l, Primitive "number") -> is_number_literal l
        (Literal l, Primitive "boolean") -> is_boolean_literal l

        (Union subs, _) -> List.all subs \s -> subtype s super
        (_, Union supers) -> List.any supers \s -> subtype sub s

        (Intersection subs, _) -> List.any subs \s -> subtype s super
        (_, Intersection supers) -> List.all supers \s -> subtype sub s

        (Array sub_elem, Array super_elem) -> subtype sub_elem super_elem

        (Tuple sub_elems, Tuple super_elems) ->
            List.len sub_elems == List.len super_elems &&
            all2 sub_elems super_elems subtype

        (Record sub_fields, Record super_fields) ->
            List.all super_fields \super_field ->
                List.any sub_fields \sub_field ->
                    sub_field.key == super_field.key &&
                    (super_field.optional || !sub_field.optional) &&
                    subtype sub_field.value super_field.value

        (Function sub_fn, Function super_fn) ->
            List.len sub_fn.params == List.len super_fn.params &&
            all2 super_fn.params sub_fn.params subtype &&
            subtype sub_fn.ret super_fn.ret

        (Negation inner, _) -> disjoint inner super
        (_, Negation inner) -> disjoint sub inner

        _ -> Bool.false

join : Type, Type -> Type
join = \t1, t2 ->
    when (t1, t2) is
        (Top, _) | (_, Top) -> Top
        (Bottom, t) | (t, Bottom) -> t

        (s1, s2) if types_structurally_equal s1 s2 -> s1

        (Union ts1, Union ts2) -> Type.mk_union (List.concat ts1 ts2)
        (Union ts, t) | (t, Union ts) -> Type.mk_union (List.append ts t)

        _ -> Type.mk_union [t1, t2]

meet : Type, Type -> Type
meet = \t1, t2 ->
    when (t1, t2) is
        (Bottom, _) | (_, Bottom) -> Bottom
        (Top, t) | (t, Top) -> t

        (s1, s2) if types_structurally_equal s1 s2 -> s1

        (Intersection ts1, Intersection ts2) -> Type.mk_intersection (List.concat ts1 ts2)
        (Intersection ts, t) | (t, Intersection ts) -> Type.mk_intersection (List.append ts t)

        (Record fields1, Record fields2) ->
            merged = merge_record_fields fields1 fields2
            if List.is_empty merged then Bottom else Record merged

        (Array e1, Array e2) -> Array (meet e1 e2)

        (Tuple es1, Tuple es2) ->
            if List.len es1 == List.len es2 then
                Tuple (List.map2 es1 es2 meet)
            else
                Bottom

        _ -> Type.mk_intersection [t1, t2]

negate : Type -> Type
negate = \type ->
    when type is
        Top -> Bottom
        Bottom -> Top
        Negation inner -> inner
        Union types -> Type.mk_intersection (List.map types negate)
        Intersection types -> Type.mk_union (List.map types negate)
        _ -> Negation type

disjoint : Type, Type -> Bool
disjoint = \t1, t2 ->
    when (meet t1 t2) is
        Bottom -> Bool.true
        _ -> Bool.false

equivalent : Type, Type -> Bool
equivalent = \t1, t2 ->
    subtype t1 t2 && subtype t2 t1

simplify : Type -> Type
simplify = \type ->
    when type is
        Union types ->
            simplified = List.map types simplify
            flattened = flatten_unions simplified
            absorbed = absorb_subtypes flattened
            when absorbed is
                [] -> Bottom
                [single] -> single
                many -> Union many

        Intersection types ->
            simplified = List.map types simplify
            flattened = flatten_intersections simplified
            absorbed = absorb_supertypes flattened
            when absorbed is
                [] -> Top
                [single] -> single
                many -> Intersection many

        Negation (Negation inner) -> simplify inner
        Negation Top -> Bottom
        Negation Bottom -> Top
        Negation (Union types) -> simplify (Type.mk_intersection (List.map types negate))
        Negation (Intersection types) -> simplify (Type.mk_union (List.map types negate))

        Array elem -> Array (simplify elem)
        Tuple elems -> Tuple (List.map elems simplify)
        Function { params, ret } ->
            Function {
                params: List.map params simplify,
                ret: simplify ret,
            }
        Record fields ->
            Record (List.map fields \field ->
                { field & value: simplify field.value })

        other -> other

types_structurally_equal : Type, Type -> Bool
types_structurally_equal = \t1, t2 ->
    when (t1, t2) is
        (Var v1, Var v2) -> v1 == v2
        (Top, Top) | (Bottom, Bottom) -> Bool.true
        (Primitive p1, Primitive p2) -> p1 == p2
        (Literal l1, Literal l2) -> literals_equal l1 l2
        (Array e1, Array e2) -> types_structurally_equal e1 e2
        (Tuple es1, Tuple es2) ->
            List.len es1 == List.len es2 &&
            all2 es1 es2 types_structurally_equal
        (Record fs1, Record fs2) ->
            List.len fs1 == List.len fs2 &&
            List.all fs1 \f1 ->
                List.any fs2 \f2 ->
                    f1.key == f2.key &&
                    f1.optional == f2.optional &&
                    types_structurally_equal f1.value f2.value
        (Function fn1, Function fn2) ->
            List.len fn1.params == List.len fn2.params &&
            all2 fn1.params fn2.params types_structurally_equal &&
            types_structurally_equal fn1.ret fn2.ret
        _ -> Bool.false

literals_equal = \l1, l2 ->
    when (l1, l2) is
        (StrLit s1, StrLit s2) -> s1 == s2
        (NumLit n1, NumLit n2) -> Num.is_approx_eq n1 n2 {}
        (BoolLit b1, BoolLit b2) -> b1 == b2
        (NullLit, NullLit) -> Bool.true
        (UndefinedLit, UndefinedLit) -> Bool.true
        _ -> Bool.false

is_string_literal = \lit ->
    when lit is
        StrLit _ -> Bool.true
        _ -> Bool.false

is_number_literal = \lit ->
    when lit is
        NumLit _ -> Bool.true
        _ -> Bool.false

is_boolean_literal = \lit ->
    when lit is
        BoolLit _ -> Bool.true
        _ -> Bool.false

flatten_unions : List Type -> List Type
flatten_unions = \types ->
    List.walk types [] \acc, type ->
        when type is
            Union nested -> List.concat acc (flatten_unions nested)
            Bottom -> acc
            other -> List.append acc other

flatten_intersections : List Type -> List Type
flatten_intersections = \types ->
    List.walk types [] \acc, type ->
        when type is
            Intersection nested -> List.concat acc (flatten_intersections nested)
            Top -> acc
            other -> List.append acc other

absorb_subtypes : List Type -> List Type
absorb_subtypes = \types ->
    List.walk types [] \acc, type ->
        if List.any acc (\t -> subtype type t) then
            acc
        else
            filtered = List.keep_if acc \t -> !(subtype t type)
            List.append filtered type

absorb_supertypes : List Type -> List Type
absorb_supertypes = \types ->
    List.walk types [] \acc, type ->
        if List.any acc (\t -> subtype t type) then
            acc
        else
            filtered = List.keep_if acc \t -> !(subtype type t)
            List.append filtered type

merge_record_fields : List { key : Str, value : Type, optional : Bool }, List { key : Str, value : Type, optional : Bool } -> List { key : Str, value : Type, optional : Bool }
merge_record_fields = \fields1, fields2 ->
    all_keys = Set.from_list (List.concat (List.map fields1 .key) (List.map fields2 .key))
    Set.to_list all_keys |> List.map \key ->
        field1 = List.find_first fields1 \f -> f.key == key
        field2 = List.find_first fields2 \f -> f.key == key
        when (field1, field2) is
            (Ok f1, Ok f2) ->
                {
                    key,
                    value: meet f1.value f2.value,
                    optional: f1.optional && f2.optional,
                }
            (Ok f1, Err _) -> { f1 & optional: Bool.true }
            (Err _, Ok f2) -> { f2 & optional: Bool.true }
            _ -> crash "Impossible: key came from set of all keys"
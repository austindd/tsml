module [
    TypeScope,
    TypeEnv,
    TsType,
    TsBoolean,
    TsString,
    TsNumber,
    TsStruct,
    TsFunc,
    TsUnion,
    TsIntersection,
    satisfies_ts_type,
    satisfies_bool,
    satisfies_bool_lit,
    satisfies_ts_bool,
    satisfies_string,
    satisfies_number,
    satisfies_func,
    satisfies_union,
    satisfies_intersection,
]

import Stack exposing [Stack]

TsType : [
    TVar {
            name : Str,
            type : TsType,
        },
    TAlias {
            name : Str,
            path : List Str,
        },
    Struct (Dict Str [
                Prop {
                        name : Str,
                        type : TsType,
                        modifiers : Set TsPropModifer,
                    },
            ]),
    NeverStruct (Dict Str [
                Prop {
                        name : Str,
                        type : TsType,
                        modifiers : Set TsPropModifer,
                    },
            ]),
    Func {
            args : List TsType,
            returnType : TsType,
        },
    String,
    StringLit Str,
    Number,
    NumberLit Str,
    Boolean,
    BooleanLit Bool,
    Union (List TsType),
    Intersection (List TsType),
    Never,
]

TVar := U64
    implements [
        Eq,
        Hash,
    ]

TConstraint : [
    SupertypeOf TsType,
    SubtypeOf TsType,
]

ConstrainedTVar : {
    name : Str,
    constraints : List TConstraint,
}

TypeScope : Dict TVar TsType

TypeEnv : Stack TypeScope

TsPropModifer : [
    Optional,
    Readonly,
    Public,
    Private,
    Protected,
]

TsProp : [
    Prop {
            name : Str,
            type : TsType,
            modifiers : Set TsPropModifer,
        },
]

TsBoolean : [Boolean, BooleanLit Bool]

TsString : [String, StringLit Str]

TsNumber : [Number, NumberLit Str]

TsStruct : [
    Struct (Dict Str [
                Prop {
                        name : Str,
                        type : TsType,
                        modifiers : Set TsPropModifer,
                    },
            ]),
    NeverStruct (Dict Str [
                Prop {
                        name : Str,
                        type : TsType,
                        modifiers : Set TsPropModifer,
                    },
            ]),
]

TsNever : [Never]

TsFunc : [
    Func {
            args : List TsType,
            returnType : TsType,
        },
]

TsUnion : [Union (List TsType)]

TsIntersection : [Intersection (List TsType)]

# simplifyIntersection : TsIntersection -> TsIntersection
# simplifyIntersection = \Intersection intersection ->
#   List.walk intersection (Dict Str TsType, List TsType) \(totalProps, typeList) type ->
#     when type is
#       Struct struct ->
#         newTotalPropsResult = Dict.walkUntil struct totalProps \props, key, valueType ->

satisfies_ts_type : TsType, TsType -> Bool
satisfies_ts_type = \type_a, type_b ->
    when (type_a, type_b) is
        # Structs
        # (Struct structA, Struct structB) ->
        #   satisfiesStruct (Struct structA) (Struct structB)
        # Functions
        (Func func_a, Func func_b) ->
            satisfies_func (Func func_a) (Func func_b)

        # Strings
        (String, String) ->
            satisfies_string String String

        (String, StringLit str_lit_b) ->
            satisfies_string (String) (StringLit str_lit_b)

        (StringLit str_lit_a, String) ->
            satisfies_string (StringLit str_lit_a) (String)

        (StringLit str_lit_a, StringLit str_lit_b) ->
            satisfies_string (StringLit str_lit_a) (StringLit str_lit_b)

        # Numbers
        (Number, Number) ->
            satisfies_number Number Number

        (Number, NumberLit num_lit_b) ->
            satisfies_number (Number) (NumberLit num_lit_b)

        (NumberLit num_lit_a, Number) ->
            satisfies_number (NumberLit num_lit_a) (Number)

        (NumberLit num_lit_a, NumberLit num_lit_b) ->
            satisfies_number (NumberLit num_lit_a) (NumberLit num_lit_b)

        # Booleans
        (Boolean, Boolean) ->
            satisfies_bool Boolean Boolean

        (Boolean, BooleanLit bool_lit_b) ->
            satisfies_bool_lit (Boolean) (BooleanLit bool_lit_b)

        (BooleanLit bool_lit_a, Boolean) ->
            satisfies_bool (BooleanLit bool_lit_a) (Boolean)

        (BooleanLit bool_lit_a, BooleanLit bool_lit_b) ->
            satisfies_bool_lit (BooleanLit bool_lit_a) (BooleanLit bool_lit_b)

        (Union union_a, _) ->
            if List.all union_a \t_a1 -> satisfies_ts_type t_a1 type_b then
                Bool.true
            else
                Bool.false

        (_, Union union_b) ->
            satisfies_union type_a (Union union_b)

        (Intersection intersection_a, _) ->
            if List.any intersection_a \t_a1 -> satisfies_ts_type t_a1 type_b then
                Bool.true
            else
                Bool.false

        (_, Intersection intersection_b) ->
            satisfies_intersection type_a (Intersection intersection_b)

        (_, NeverStruct never_struct_b) -> Bool.false
        (NeverStruct never_struct_a, _) -> Bool.false
        # Default
        (_, _) -> Bool.false

satisfies_bool : TsType, [Boolean] -> Bool
satisfies_bool = \ts_type_a, bool_b ->
    when (ts_type_a, bool_b) is
        (Boolean, Boolean) -> Bool.true
        (BooleanLit a, Boolean) -> Bool.true
        (_, _) -> Bool.false

satisfies_bool_lit : TsType, [BooleanLit Bool] -> Bool
satisfies_bool_lit = \ts_type_a, ts_bool_b ->
    when (ts_type_a, ts_bool_b) is
        (BooleanLit a, BooleanLit b) -> a == b
        (Boolean, BooleanLit _b) -> Bool.false
        _ -> Bool.false

satisfies_ts_bool : TsType, TsBoolean -> Bool
satisfies_ts_bool = \type_a, bool_b ->
    when bool_b is
        Boolean -> satisfies_bool type_a Boolean
        BooleanLit b -> satisfies_bool_lit type_a (BooleanLit b)

satisfies_string : TsType, TsString -> Bool
satisfies_string = \type_a, string_b ->
    when (type_a, string_b) is
        (String, String) -> Bool.true
        (String, StringLit str_b_lit) -> Bool.false
        (StringLit str_a_lit, String) -> Bool.true
        (StringLit str_a_lit, StringLit str_b_lit) -> str_a_lit == str_b_lit
        _ -> Bool.false

satisfies_number : TsType, TsNumber -> Bool
satisfies_number = \type_a, number_b ->
    when (type_a, number_b) is
        (Number, Number) -> Bool.true
        (Number, NumberLit num_b_lit) -> Bool.false
        (NumberLit num_a_lit, Number) -> Bool.true
        (NumberLit num_a_lit, NumberLit num_b_lit) -> num_a_lit == num_b_lit
        _ -> Bool.false

satisfies_prop : TsProp, TsProp -> Bool
satisfies_prop = \ts_prop_a, ts_prop_b ->
    when (ts_prop_a, ts_prop_b) is
        (Prop { name: name_a, type: type_a, modifiers: modifiers_a }, Prop { name: name_b, type: type_b, modifiers: modifiers_b }) ->
            if name_a == name_b then
                if satisfies_ts_type type_a type_b then
                    Bool.true
                else if Set.contains modifiers_b Optional then
                    Bool.true
                else
                    Bool.false
            else
                Bool.false

satisfies_struct : TsType, TsStruct -> Bool
satisfies_struct = \type_a, ts_struct_b ->
    when (type_a, ts_struct_b) is
        (Struct struct_a, Struct struct_b) ->
            Dict.walk struct_b Bool.true \state, key_b, Prop prop_b ->
                when Dict.get struct_a key_b is
                    Ok ts_prop_a -> satisfies_prop ts_prop_a (Prop prop_b)
                    Err _ ->
                        if Set.contains prop_b.modifiers Optional then
                            Bool.true
                        else
                            Bool.false

        _ -> Bool.false

satisfies_never : TsType, TsNever -> Bool
satisfies_never = \type_a, Never ->
    when type_a is
        Never -> Bool.true
        _ -> Bool.false

intersect_types : TsType, TsType -> TsType
intersect_types = \type_a, type_b ->
    when (type_a, type_b) is
        (Boolean, Boolean) -> Boolean
        (Boolean, BooleanLit b) -> BooleanLit b
        (BooleanLit a, Boolean) -> BooleanLit a
        (BooleanLit a, BooleanLit b) ->
            if a == b then
                BooleanLit a
            else
                Never

        (String, String) -> String
        (String, StringLit b) -> StringLit b
        (StringLit a, String) -> StringLit a
        (StringLit a, StringLit b) ->
            if a == b then
                StringLit a
            else
                Never

        (Number, Number) -> Number
        (Number, NumberLit b) -> NumberLit b
        (NumberLit a, Number) -> NumberLit a
        (NumberLit a, NumberLit b) ->
            if a == b then
                NumberLit a
            else
                Never

        # (Struct propsA, Struct propsB) -> intersectStructs (Struct propsA) (Struct propsB)
        (_, _) -> Never

# intersectStructs : TsStruct, TsStruct -> TsStruct
# intersectStructs = \Struct propsA, Struct propsB ->
#   intersectedStruct = Dict.walk propsA propsB \combinedProps, keyA, propA ->
#     Dict.update combinedProps keyA \propBResult ->
#       when propBResult is
#         Err Missing -> Ok propA
#         Ok propB -> Ok (intersectTypes propA propB)
#   intersectedStruct

satisfies_func : TsFunc, TsFunc -> Bool
satisfies_func = \Func func_a, Func func_b ->
    if List.len func_a.args == List.len func_b.args then
        (List.map2 func_a.args func_b.args satisfies_ts_type) |> List.all \x -> x
    else
        Bool.false

# TODO: Check if this is correct
satisfies_union : TsType, TsUnion -> Bool
satisfies_union = \type_a, Union union_b ->
    List.walkUntil union_b Bool.false \_, type_b ->
        when type_a is
            Union union_a ->
                if List.all union_a \t_a1 -> satisfies_ts_type t_a1 type_b then
                    Break Bool.true
                else
                    Continue Bool.false

            Intersection intersection_a ->
                if List.any intersection_a \t_a1 -> satisfies_ts_type t_a1 type_b then
                    Break Bool.true
                else
                    Continue Bool.false

            _ ->
                if satisfies_ts_type type_a type_b then
                    Break Bool.true
                else
                    Continue Bool.false

# TODO: Check if this is correct
satisfies_intersection : TsType, TsIntersection -> Bool
satisfies_intersection = \type_a, Intersection intersection_b ->
    List.walkUntil intersection_b Bool.true \_, type_b ->
        when type_a is
            Union union_a ->
                if List.all union_a \t_a1 -> satisfies_ts_type t_a1 type_b then
                    Continue Bool.true
                else
                    Break Bool.false

            Intersection intersection_a ->
                if List.any intersection_a \t_a1 -> satisfies_ts_type t_a1 type_b then
                    Continue Bool.true
                else
                    Break Bool.false

            _ ->
                if satisfies_ts_type type_a type_b then
                    Continue Bool.true
                else
                    Break Bool.false

get_prop_by_name : List TsProp, Str -> Result TsProp [NotFound]
get_prop_by_name = \prop_list, name ->
    List.findFirst prop_list \Prop prop -> prop.name == name

resolve_type_from_env : TypeEnv, TVar -> Result TsType [TypeNotFound]
resolve_type_from_env = \env, type_var ->
    Stack.walkFromTopUntil env (Err TypeNotFound) \_, scope ->
        when Dict.get scope type_var is
            Ok type -> Break (Ok type)
            Err _ -> Continue (Err TypeNotFound)

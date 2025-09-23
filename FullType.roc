module [
    Type,
    Literal,
    FunctionSig,
    ClassInfo,
    InterfaceInfo,
    TypeParam,
    # Constructors
    mk_number,
    mk_string,
    mk_boolean,
    mk_null,
    mk_undefined,
    mk_bigint,
    mk_symbol,
    mk_any,
    mk_never,
    mk_unknown,
    mk_void,
    mk_array,
    mk_tuple,
    mk_object,
    mk_union,
    mk_intersection,
    mk_function,
    mk_literal,
    mk_generic,
    mk_class,
    mk_interface,
    mk_type_param,
    # Operations
    type_to_string,
    is_assignable_to,
]

# Literal values
Literal : [
    NumLit F64,
    StrLit Str,
    BoolLit Bool,
    BigIntLit Str,
]

# Function signature
FunctionSig : {
    params : List { name : Str, param_type : Type, optional : Bool },
    return_type : Type,
    type_params : List TypeParam,
    is_async : Bool,
    is_generator : Bool,
}

# Type parameter for generics
TypeParam : {
    name : Str,
    constraint : Result Type [NoConstraint],
    default_type : Result Type [NoDefault],
}

# Class information
ClassInfo : {
    name : Str,
    type_params : List TypeParam,
    extends : Result Type [NoExtends],
    implements : List Type,
    constructor : Result FunctionSig [NoConstructor],
    properties : List { name : Str, prop_type : Type, is_static : Bool, is_private : Bool },
    methods : List { name : Str, sig : FunctionSig, is_static : Bool, is_private : Bool },
}

# Interface information
InterfaceInfo : {
    name : Str,
    type_params : List TypeParam,
    extends : List Type,
    properties : List { name : Str, prop_type : Type, optional : Bool, readonly : Bool },
    methods : List { name : Str, sig : FunctionSig, optional : Bool },
}

# Complete type system
Type : [
    # Primitives
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,
    TBigInt,
    TSymbol,

    # Literal types
    TLiteral Literal,

    # Objects and arrays
    TObject (List { key : Str, value_type : Type, optional : Bool, readonly : Bool }),
    TArray Type,
    TTuple (List Type),

    # Function types
    TFunction FunctionSig,

    # Compound types
    TUnion (List Type),
    TIntersection (List Type),

    # Generic types
    TGeneric { base : Str, type_args : List Type },
    TTypeParam TypeParam,

    # Class and interface types
    TClass ClassInfo,
    TInterface InterfaceInfo,

    # Special types
    TAny,
    TNever,
    TUnknown,
    TVoid,
]

# === CONSTRUCTORS ===

mk_number : Type
mk_number = TNumber

mk_string : Type
mk_string = TString

mk_boolean : Type
mk_boolean = TBoolean

mk_null : Type
mk_null = TNull

mk_undefined : Type
mk_undefined = TUndefined

mk_bigint : Type
mk_bigint = TBigInt

mk_symbol : Type
mk_symbol = TSymbol

mk_any : Type
mk_any = TAny

mk_never : Type
mk_never = TNever

mk_unknown : Type
mk_unknown = TUnknown

mk_void : Type
mk_void = TVoid

mk_array : Type -> Type
mk_array = \elem_type ->
    TArray elem_type

mk_tuple : List Type -> Type
mk_tuple = \types ->
    TTuple types

mk_object : List { key : Str, value_type : Type, optional : Bool, readonly : Bool } -> Type
mk_object = \props ->
    TObject props

mk_union : List Type -> Type
mk_union = \types ->
    # Flatten nested unions
    flattened = List.walk types [] \acc, t ->
        when t is
            TUnion inner -> List.concat acc inner
            TNever -> acc  # Never disappears in union
            _ -> List.append acc t

    when flattened is
        [] -> TNever
        [single] -> single
        many -> TUnion many

mk_intersection : List Type -> Type
mk_intersection = \types ->
    # Flatten nested intersections
    flattened = List.walk types [] \acc, t ->
        when t is
            TIntersection inner -> List.concat acc inner
            TUnknown -> acc  # Unknown disappears in intersection
            _ -> List.append acc t

    # Check for never
    if List.any flattened \t ->
        when t is
            TNever -> Bool.true
            _ -> Bool.false
    then
        TNever
    else
        when flattened is
            [] -> TUnknown
            [single] -> single
            many -> TIntersection many

mk_function : List { name : Str, param_type : Type, optional : Bool }, Type -> Type
mk_function = \params, return_type ->
    TFunction {
        params,
        return_type,
        type_params: [],
        is_async: Bool.false,
        is_generator: Bool.false,
    }

mk_literal : Literal -> Type
mk_literal = \lit ->
    TLiteral lit

mk_generic : Str, List Type -> Type
mk_generic = \base, type_args ->
    TGeneric { base, type_args }

mk_type_param : Str -> Type
mk_type_param = \name ->
    TTypeParam {
        name,
        constraint: Err NoConstraint,
        default_type: Err NoDefault,
    }

mk_class : Str, List { name : Str, prop_type : Type, is_static : Bool, is_private : Bool } -> Type
mk_class = \name, properties ->
    TClass {
        name,
        type_params: [],
        extends: Err NoExtends,
        implements: [],
        constructor: Err NoConstructor,
        properties,
        methods: [],
    }

mk_interface : Str, List { name : Str, prop_type : Type, optional : Bool, readonly : Bool } -> Type
mk_interface = \name, properties ->
    TInterface {
        name,
        type_params: [],
        extends: [],
        properties,
        methods: [],
    }

# === OPERATIONS ===

type_to_string : Type -> Str
type_to_string = \t ->
    when t is
        TNumber -> "number"
        TString -> "string"
        TBoolean -> "boolean"
        TNull -> "null"
        TUndefined -> "undefined"
        TBigInt -> "bigint"
        TSymbol -> "symbol"

        TLiteral lit ->
            when lit is
                NumLit n -> Num.to_str n
                StrLit s -> "\"$(s)\""
                BoolLit b -> if b then "true" else "false"
                BigIntLit s -> "$(s)n"

        TObject props ->
            prop_strs = List.map props \p ->
                opt = if p.optional then "?" else ""
                ro = if p.readonly then "readonly " else ""
                "$(ro)$(p.key)$(opt): $(type_to_string p.value_type)"
            "{ $(Str.join_with prop_strs ", ") }"

        TArray elem ->
            "$(type_to_string elem)[]"

        TTuple types ->
            elem_strs = List.map types type_to_string
            "[$(Str.join_with elem_strs ", ")]"

        TFunction sig ->
            param_strs = List.map sig.params \p ->
                opt = if p.optional then "?" else ""
                "$(p.name)$(opt): $(type_to_string p.param_type)"
            async_str = if sig.is_async then "async " else ""
            gen_str = if sig.is_generator then "* " else ""
            type_params_str = if List.is_empty sig.type_params then
                ""
            else
                params = List.map sig.type_params \tp -> tp.name
                "<$(Str.join_with params ", ")>"
            "$(async_str)$(gen_str)$(type_params_str)($(Str.join_with param_strs ", ")) => $(type_to_string sig.return_type)"

        TUnion types ->
            parts = List.map types type_to_string
            Str.join_with parts " | "

        TIntersection types ->
            parts = List.map types type_to_string
            Str.join_with parts " & "

        TGeneric { base, type_args } ->
            if List.is_empty type_args then
                base
            else
                args = List.map type_args type_to_string
                "$(base)<$(Str.join_with args ", ")>"

        TTypeParam param ->
            constraint_str = when param.constraint is
                Ok c -> " extends $(type_to_string c)"
                Err _ -> ""
            "$(param.name)$(constraint_str)"

        TClass info ->
            type_params_str = if List.is_empty info.type_params then
                ""
            else
                params = List.map info.type_params \tp -> tp.name
                "<$(Str.join_with params ", ")>"
            "$(info.name)$(type_params_str)"

        TInterface info ->
            type_params_str = if List.is_empty info.type_params then
                ""
            else
                params = List.map info.type_params \tp -> tp.name
                "<$(Str.join_with params ", ")>"
            "$(info.name)$(type_params_str)"

        TAny -> "any"
        TNever -> "never"
        TUnknown -> "unknown"
        TVoid -> "void"

is_assignable_to : Type, Type -> Bool
is_assignable_to = \from, to ->
    when (from, to) is
        # Any is assignable to and from everything
        (_, TAny) -> Bool.true
        (TAny, _) -> Bool.true

        # Unknown - everything assignable to unknown, but not from
        (_, TUnknown) -> Bool.true
        (TUnknown, TUnknown) -> Bool.true
        (TUnknown, _) -> Bool.false

        # Never is assignable to everything
        (TNever, _) -> Bool.true
        (_, TNever) -> Bool.false

        # Void
        (TVoid, TVoid) -> Bool.true
        (TVoid, TUndefined) -> Bool.true  # void can be used as undefined
        (_, TVoid) -> Bool.false

        # Same primitive types
        (TNumber, TNumber) -> Bool.true
        (TString, TString) -> Bool.true
        (TBoolean, TBoolean) -> Bool.true
        (TNull, TNull) -> Bool.true
        (TUndefined, TUndefined) -> Bool.true
        (TBigInt, TBigInt) -> Bool.true
        (TSymbol, TSymbol) -> Bool.true

        # Literals are subtypes of their primitives
        (TLiteral (NumLit _), TNumber) -> Bool.true
        (TLiteral (StrLit _), TString) -> Bool.true
        (TLiteral (BoolLit _), TBoolean) -> Bool.true
        (TLiteral (BigIntLit _), TBigInt) -> Bool.true

        # Literal to literal
        (TLiteral l1, TLiteral l2) -> literals_equal l1 l2

        # Arrays (covariant)
        (TArray from_elem, TArray to_elem) ->
            is_assignable_to from_elem to_elem

        # Tuples
        (TTuple from_types, TTuple to_types) ->
            if List.len from_types == List.len to_types then
                check_tuple_assignability from_types to_types
            else
                Bool.false

        # Union types - from assignable to union if assignable to any member
        (_, TUnion union_types) ->
            List.any union_types \member ->
                is_assignable_to from member

        # Union source - all members must be assignable
        (TUnion union_types, target) ->
            when target is
                TUnion _ -> Bool.false  # Avoid complex recursion
                _ -> List.all union_types \member ->
                    is_assignable_to member target

        # Intersection types - must be assignable to all members
        (_, TIntersection intersection_types) ->
            List.all intersection_types \member ->
                is_assignable_to from member

        # Intersection source - at least one member assignable
        (TIntersection intersection_types, target) ->
            when target is
                TIntersection _ -> Bool.false  # Avoid complex recursion
                _ -> List.any intersection_types \member ->
                    is_assignable_to member target

        # Object structural typing
        (TObject from_props, TObject to_props) ->
            List.all to_props \to_prop ->
                if to_prop.optional then
                    # Optional property - if present, must be compatible
                    when List.find_first from_props \fp -> fp.key == to_prop.key is
                        Ok from_prop ->
                            is_assignable_to from_prop.value_type to_prop.value_type
                        Err _ -> Bool.true  # Optional can be missing
                else
                    # Required property must exist and be compatible
                    when List.find_first from_props \fp -> fp.key == to_prop.key is
                        Ok from_prop ->
                            is_assignable_to from_prop.value_type to_prop.value_type
                        Err _ -> Bool.false

        # Function types (contravariant params, covariant return)
        (TFunction from_sig, TFunction to_sig) ->
            # Check param count (from can have fewer)
            List.len from_sig.params <= List.len to_sig.params &&
            # Check params (contravariant)
            check_function_params to_sig.params from_sig.params &&
            # Check return (covariant)
            is_assignable_to from_sig.return_type to_sig.return_type

        # Class to class
        (TClass from_class, TClass to_class) ->
            # Simple name check for now (would need inheritance chain)
            from_class.name == to_class.name

        # Interface to interface
        (TInterface from_interface, TInterface to_interface) ->
            # Check structural compatibility
            check_interface_assignability from_interface to_interface

        # Class to interface (check if class implements interface)
        (TClass class_info, TInterface interface_info) ->
            # Check if class implements all interface members
            check_class_implements_interface class_info interface_info

        # Generic types
        (TGeneric from_g, TGeneric to_g) ->
            from_g.base == to_g.base &&
            List.len from_g.type_args == List.len to_g.type_args &&
            check_type_args_assignability from_g.type_args to_g.type_args

        # Type parameters
        (TTypeParam from_param, TTypeParam to_param) ->
            from_param.name == to_param.name

        # Default case
        _ -> Bool.false

# === HELPER FUNCTIONS ===

literals_equal : Literal, Literal -> Bool
literals_equal = \l1, l2 ->
    when (l1, l2) is
        (NumLit n1, NumLit n2) -> n1 == n2
        (StrLit s1, StrLit s2) -> s1 == s2
        (BoolLit b1, BoolLit b2) -> b1 == b2
        (BigIntLit s1, BigIntLit s2) -> s1 == s2
        _ -> Bool.false

check_tuple_assignability : List Type, List Type -> Bool
check_tuple_assignability = \from_types, to_types ->
    when (from_types, to_types) is
        ([], []) -> Bool.true
        ([f, .. as f_rest], [t, .. as t_rest]) ->
            if is_assignable_to f t then
                check_tuple_assignability f_rest t_rest
            else
                Bool.false
        _ -> Bool.false

check_function_params : List { name : Str, param_type : Type, optional : Bool }, List { name : Str, param_type : Type, optional : Bool } -> Bool
check_function_params = \to_params, from_params ->
    when (to_params, from_params) is
        ([], _) -> Bool.true  # Extra from_params ok
        ([to_p, .. as to_rest], [from_p, .. as from_rest]) ->
            # Contravariant - to's param type assignable to from's
            if is_assignable_to to_p.param_type from_p.param_type then
                check_function_params to_rest from_rest
            else
                Bool.false
        ([to_p, ..], []) ->
            # Missing param ok if optional
            to_p.optional

check_interface_assignability : InterfaceInfo, InterfaceInfo -> Bool
check_interface_assignability = \from_interface, to_interface ->
    # Check all required properties exist
    List.all to_interface.properties \to_prop ->
        when List.find_first from_interface.properties \fp -> fp.name == to_prop.name is
            Ok from_prop ->
                is_assignable_to from_prop.prop_type to_prop.prop_type
            Err _ ->
                to_prop.optional  # Optional properties can be missing

check_class_implements_interface : ClassInfo, InterfaceInfo -> Bool
check_class_implements_interface = \class_info, interface_info ->
    # Check all interface properties are in class
    List.all interface_info.properties \i_prop ->
        when List.find_first class_info.properties \cp -> cp.name == i_prop.name is
            Ok class_prop ->
                is_assignable_to class_prop.prop_type i_prop.prop_type
            Err _ ->
                i_prop.optional

check_type_args_assignability : List Type, List Type -> Bool
check_type_args_assignability = \from_args, to_args ->
    when (from_args, to_args) is
        ([], []) -> Bool.true
        ([f, .. as f_rest], [t, .. as t_rest]) ->
            if is_assignable_to f t then
                check_type_args_assignability f_rest t_rest
            else
                Bool.false
        _ -> Bool.false
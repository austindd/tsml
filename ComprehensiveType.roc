module [
    Type,
    Primitive,
    LiteralValue,
    FunctionSig,
    ObjectShape,
    TypeParam,
    # Constructors
    mk_number,
    mk_string,
    mk_boolean,
    mk_null,
    mk_undefined,
    mk_bigint,
    mk_symbol,
    mk_object,
    mk_array,
    mk_function,
    mk_union,
    mk_intersection,
    mk_nullable,
    mk_literal,
    mk_tuple,
    mk_any,
    mk_never,
    mk_void,
    mk_unknown,
    # Operations
    type_to_string,
    is_assignable_to,
]

# Primitive types in JavaScript/TypeScript
Primitive : [
    PNumber,
    PString,
    PBoolean,
    PNull,
    PUndefined,
    PBigInt,
    PSymbol,
]

# Literal values for literal types
LiteralValue : [
    NumLit F64,
    StrLit Str,
    BoolLit Bool,
    NullLit,
    UndefinedLit,
    BigIntLit Str,
]

# Function signature
FunctionSig : {
    params : List { name : Str, param_type : Type, optional : Bool },
    return_type : Type,
    is_async : Bool,
    is_generator : Bool,
}

# Object shape for structural typing
ObjectShape : {
    properties : List { key : Str, value_type : Type, optional : Bool, readonly : Bool },
    index_sig : Result { key_type : Type, value_type : Type } [NoIndexSig],
}

# Type parameter for generics
TypeParam : {
    name : Str,
    constraint : Result Type [NoConstraint],
}

# Complete type representation for JavaScript/TypeScript
Type : [
    # Primitive types
    TPrimitive Primitive,

    # Object and structural types
    TObject ObjectShape,
    TArray Type,
    TTuple (List Type),

    # Function type
    TFunction FunctionSig,

    # Compound types
    TUnion (List Type),
    TIntersection (List Type),

    # Nullable types
    TNullable Type,  # T | null | undefined

    # Literal types
    TLiteral LiteralValue,

    # Generic types
    TGeneric { base : Str, args : List Type },
    TParam TypeParam,

    # Special types
    TAny,
    TNever,
    TVoid,
    TUnknown,
]

# === CONSTRUCTORS ===

mk_number : Type
mk_number = TPrimitive PNumber

mk_string : Type
mk_string = TPrimitive PString

mk_boolean : Type
mk_boolean = TPrimitive PBoolean

mk_null : Type
mk_null = TPrimitive PNull

mk_undefined : Type
mk_undefined = TPrimitive PUndefined

mk_bigint : Type
mk_bigint = TPrimitive PBigInt

mk_symbol : Type
mk_symbol = TPrimitive PSymbol

mk_object : List { key : Str, value_type : Type, optional : Bool, readonly : Bool } -> Type
mk_object = |properties|
    TObject {
        properties,
        index_sig: Err NoIndexSig,
    }

mk_array : Type -> Type
mk_array = |element_type|
    TArray element_type

mk_function : List { name : Str, param_type : Type, optional : Bool }, Type -> Type
mk_function = |params, return_type|
    TFunction {
        params,
        return_type,
        is_async: Bool.false,
        is_generator: Bool.false,
    }

mk_union : List Type -> Type
mk_union = |types|
    # Flatten nested unions and remove duplicates
    flattened = flatten_unions(types)
    unique = dedupe_types(flattened)

    when unique is
        [] -> TNever
        [single] -> single
        many -> TUnion(many)

mk_intersection : List Type -> Type
mk_intersection = |types|
    # Flatten nested intersections
    flattened = flatten_intersections types

    # Check for never type
    if List.any flattened is_never_type then
        TNever
    else
        when flattened is
            [] -> TUnknown
            [single] -> single
            many -> TIntersection many

mk_nullable : Type -> Type
mk_nullable = |t|
    TNullable t

mk_literal : LiteralValue -> Type
mk_literal = |value|
    TLiteral value

mk_tuple : List Type -> Type
mk_tuple = |types|
    TTuple types

mk_any : Type
mk_any = TAny

mk_never : Type
mk_never = TNever

mk_void : Type
mk_void = TVoid

mk_unknown : Type
mk_unknown = TUnknown

# === OPERATIONS ===

type_to_string : Type -> Str
type_to_string = |t|
    when t is
        TPrimitive prim ->
            when prim is
                PNumber -> "number"
                PString -> "string"
                PBoolean -> "boolean"
                PNull -> "null"
                PUndefined -> "undefined"
                PBigInt -> "bigint"
                PSymbol -> "symbol"

        TObject obj ->
            props = List.map obj.properties \p ->
                opt = if p.optional then "?" else ""
                ro = if p.readonly then "readonly " else ""
                "$(ro)$(p.key)$(opt): $(type_to_string p.value_type)"
            "{ $(Str.join_with props ", ") }"

        TArray elem ->
            "$(type_to_string elem)[]"

        TTuple types ->
            elems = List.map types type_to_string
            "[$(Str.join_with elems ", ")]"

        TFunction func ->
            params = List.map func.params \p ->
                opt = if p.optional then "?" else ""
                "$(p.name)$(opt): $(type_to_string p.param_type)"
            async_str = if func.is_async then "async " else ""
            gen_str = if func.is_generator then "function* " else ""
            "$(async_str)$(gen_str)($(Str.join_with params ", ")) => $(type_to_string func.return_type)"

        TUnion types ->
            parts = List.map types type_to_string
            when parts is
                [] -> "never"
                [single] -> single
                many -> Str.join_with many " | "

        TIntersection types ->
            parts = List.map types type_to_string
            when parts is
                [] -> "unknown"
                [single] -> single
                many -> Str.join_with many " & "

        TNullable inner ->
            "$(type_to_string inner) | null | undefined"

        TLiteral lit ->
            when lit is
                NumLit n -> Num.to_str n
                StrLit s -> "\"$(s)\""
                BoolLit b -> if b then "true" else "false"
                NullLit -> "null"
                UndefinedLit -> "undefined"
                BigIntLit s -> "$(s)n"

        TGeneric { base, args } ->
            if List.is_empty args then
                base
            else
                arg_strs = List.map args type_to_string
                "$(base)<$(Str.join_with arg_strs ", ")>"

        TParam param ->
            param.name

        TAny -> "any"
        TNever -> "never"
        TVoid -> "void"
        TUnknown -> "unknown"

# Check if a type is assignable to another
is_assignable_to : Type, Type -> Bool
is_assignable_to = |from, to|
    when (from, to) is
        # Any is assignable to and from everything
        (_, TAny) -> Bool.true
        (TAny, _) -> Bool.true

        # Unknown - everything is assignable to unknown, but unknown is only assignable to any/unknown
        (_, TUnknown) -> Bool.true
        (TUnknown, TUnknown) -> Bool.true
        (TUnknown, _) -> Bool.false

        # Never is assignable to everything, but nothing is assignable to never
        (TNever, _) -> Bool.true
        (_, TNever) -> Bool.false

        # Void
        (TVoid, TVoid) -> Bool.true
        (TVoid, _) -> Bool.false
        (_, TVoid) -> Bool.false

        # Primitives
        (TPrimitive p1, TPrimitive p2) -> primitives_equal p1 p2

        # Literals are subtypes of their primitive types
        (TLiteral (NumLit _), TPrimitive PNumber) -> Bool.true
        (TLiteral (StrLit _), TPrimitive PString) -> Bool.true
        (TLiteral (BoolLit _), TPrimitive PBoolean) -> Bool.true
        (TLiteral NullLit, TPrimitive PNull) -> Bool.true
        (TLiteral UndefinedLit, TPrimitive PUndefined) -> Bool.true
        (TLiteral (BigIntLit _), TPrimitive PBigInt) -> Bool.true

        # Literal to literal
        (TLiteral l1, TLiteral l2) -> literals_equal l1 l2

        # Nullable types
        (TPrimitive PNull, TNullable _) -> Bool.true
        (TPrimitive PUndefined, TNullable _) -> Bool.true
        (inner, TNullable target) -> is_assignable_to inner target
        (TNullable _, _) -> Bool.false  # Nullable can't be assigned to non-nullable

        # Union types - from is assignable if it's assignable to at least one member
        (_, TUnion union_types) ->
            List.any union_types \member ->
                is_assignable_to from member

        # Union source - all members must be assignable to target
        (TUnion union_types, target) ->
            when target is
                TUnion _ -> Bool.false  # Avoid recursion, handle separately if needed
                _ -> List.all union_types \member ->
                    is_assignable_to member target

        # Intersection types - from must be assignable to all members
        (_, TIntersection intersection_types) ->
            List.all intersection_types \member ->
                is_assignable_to from member

        # Intersection source - at least one member must be assignable
        (TIntersection intersection_types, target) ->
            when target is
                TIntersection _ -> Bool.false  # Avoid recursion
                _ -> List.any intersection_types \member ->
                    is_assignable_to member target

        # Array types (covariant)
        (TArray from_elem, TArray to_elem) ->
            is_assignable_to from_elem to_elem

        # Tuple types
        (TTuple from_types, TTuple to_types) ->
            if List.len from_types == List.len to_types then
                check_tuples from_types to_types
            else
                Bool.false

        # Object types (structural typing)
        (TObject from_obj, TObject to_obj) ->
            # All required properties in 'to' must exist in 'from'
            List.all to_obj.properties \to_prop ->
                if to_prop.optional then
                    # Optional property - if it exists, must be assignable
                    when find_property from_obj.properties to_prop.key is
                        Ok from_prop -> is_assignable_to from_prop.value_type to_prop.value_type
                        Err _ -> Bool.true  # Optional property can be missing
                else
                    # Required property must exist and be assignable
                    when find_property from_obj.properties to_prop.key is
                        Ok from_prop -> is_assignable_to from_prop.value_type to_prop.value_type
                        Err _ -> Bool.false

        # Function types (contravariant in params, covariant in return)
        (TFunction from_func, TFunction to_func) ->
            # Check parameter count (from can have fewer params due to JS semantics)
            List.len from_func.params <= List.len to_func.params &&
            # Check each parameter (contravariant)
            check_function_params to_func.params from_func.params &&
            # Check return type (covariant)
            is_assignable_to from_func.return_type to_func.return_type

        # Default case
        _ -> Bool.false

# === HELPER FUNCTIONS ===

flatten_unions : List Type -> List Type
flatten_unions = |types|
    List.walk types [] \acc, t ->
        when t is
            TUnion inner -> List.concat acc (flatten_unions inner)
            TNever -> acc  # Never in union disappears
            _ -> List.append acc t

flatten_intersections : List Type -> List Type
flatten_intersections = |types|
    List.walk types [] \acc, t ->
        when t is
            TIntersection inner -> List.concat acc (flatten_intersections inner)
            TUnknown -> acc  # Unknown in intersection disappears
            _ -> List.append acc t

dedupe_types : List Type -> List Type
dedupe_types = |types|
    List.walk types [] \acc, t ->
        if List.any acc (\existing -> types_equal existing t) then
            acc
        else
            List.append acc t

types_equal : Type, Type -> Bool
types_equal = |t1, t2|
    when (t1, t2) is
        (TAny, TAny) -> Bool.true
        (TNever, TNever) -> Bool.true
        (TVoid, TVoid) -> Bool.true
        (TUnknown, TUnknown) -> Bool.true
        (TPrimitive p1, TPrimitive p2) -> primitives_equal p1 p2
        (TLiteral l1, TLiteral l2) -> literals_equal l1 l2
        (TArray e1, TArray e2) -> types_equal e1 e2
        (TTuple ts1, TTuple ts2) ->
            List.len ts1 == List.len ts2 &&
            check_tuple_equal ts1 ts2
        _ -> Bool.false

primitives_equal : Primitive, Primitive -> Bool
primitives_equal = |p1, p2|
    when (p1, p2) is
        (PNumber, PNumber) -> Bool.true
        (PString, PString) -> Bool.true
        (PBoolean, PBoolean) -> Bool.true
        (PNull, PNull) -> Bool.true
        (PUndefined, PUndefined) -> Bool.true
        (PBigInt, PBigInt) -> Bool.true
        (PSymbol, PSymbol) -> Bool.true
        _ -> Bool.false

literals_equal : LiteralValue, LiteralValue -> Bool
literals_equal = |l1, l2|
    when (l1, l2) is
        (NumLit n1, NumLit n2) -> n1 == n2
        (StrLit s1, StrLit s2) -> s1 == s2
        (BoolLit b1, BoolLit b2) -> b1 == b2
        (NullLit, NullLit) -> Bool.true
        (UndefinedLit, UndefinedLit) -> Bool.true
        (BigIntLit s1, BigIntLit s2) -> s1 == s2
        _ -> Bool.false

is_never_type : Type -> Bool
is_never_type = |t|
    when t is
        TNever -> Bool.true
        _ -> Bool.false

find_property : List { key : Str, value_type : Type, optional : Bool, readonly : Bool }, Str -> Result { key : Str, value_type : Type, optional : Bool, readonly : Bool } [NotFound]
find_property = |properties, key|
    List.find_first properties \prop -> prop.key == key
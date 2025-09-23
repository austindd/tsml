module [
    Type,
    # Constructors
    mk_number,
    mk_string,
    mk_boolean,
    mk_null,
    mk_undefined,
    mk_any,
    mk_never,
    mk_unknown,
    mk_array,
    mk_object,
    mk_union,
    # Operations
    type_to_string,
    is_assignable_to,
]

# Simplified but comprehensive type system
Type : [
    # Primitives
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,

    # Objects and arrays
    TObject (List { key : Str, value_type : Type }),
    TArray Type,

    # Compound types
    TUnion (List Type),

    # Special types
    TAny,
    TNever,
    TUnknown,
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

mk_any : Type
mk_any = TAny

mk_never : Type
mk_never = TNever

mk_unknown : Type
mk_unknown = TUnknown

mk_array : Type -> Type
mk_array = \elem_type ->
    TArray elem_type

mk_object : List { key : Str, value_type : Type } -> Type
mk_object = \props ->
    TObject props

mk_union : List Type -> Type
mk_union = \types ->
    when types is
        [] -> TNever
        [single] -> single
        many -> TUnion many

# === OPERATIONS ===

type_to_string : Type -> Str
type_to_string = \t ->
    when t is
        TNumber -> "number"
        TString -> "string"
        TBoolean -> "boolean"
        TNull -> "null"
        TUndefined -> "undefined"

        TObject props ->
            prop_strs = List.map props \p ->
                "$(p.key): $(type_to_string p.value_type)"
            "{ $(Str.join_with prop_strs ", ") }"

        TArray elem ->
            "$(type_to_string elem)[]"

        TUnion types ->
            parts = List.map types type_to_string
            Str.join_with parts " | "

        TAny -> "any"
        TNever -> "never"
        TUnknown -> "unknown"

is_assignable_to : Type, Type -> Bool
is_assignable_to = \from, to ->
    when (from, to) is
        # Any is assignable to and from everything
        (_, TAny) -> Bool.true
        (TAny, _) -> Bool.true

        # Unknown - everything is assignable to unknown
        (_, TUnknown) -> Bool.true

        # Never is assignable to everything
        (TNever, _) -> Bool.true
        (_, TNever) -> Bool.false

        # Same primitive types
        (TNumber, TNumber) -> Bool.true
        (TString, TString) -> Bool.true
        (TBoolean, TBoolean) -> Bool.true
        (TNull, TNull) -> Bool.true
        (TUndefined, TUndefined) -> Bool.true

        # Arrays
        (TArray from_elem, TArray to_elem) ->
            is_assignable_to from_elem to_elem

        # Union types - from is assignable if it's assignable to at least one member
        (_, TUnion union_types) ->
            List.any union_types \member ->
                is_assignable_to from member

        # Object structural typing (simplified)
        (TObject from_props, TObject to_props) ->
            # Check that all properties in 'to' exist in 'from' with compatible types
            List.all to_props \to_prop ->
                when List.find_first from_props \fp -> fp.key == to_prop.key is
                    Ok from_prop -> is_assignable_to from_prop.value_type to_prop.value_type
                    Err _ -> Bool.false

        # Default case
        _ -> Bool.false
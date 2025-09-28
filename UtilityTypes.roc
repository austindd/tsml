module [
    UtilityType,
    make_partial,
    make_required,
    make_readonly,
    make_pick,
    make_omit,
    make_record,
    make_exclude,
    make_extract,
    make_nonnullable,
    make_parameters,
    make_return_type,
]

# TypeScript Utility Types Implementation

TypeId : U64

# Base type representation
BaseType : [
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,
    TObject (List { key: Str, type: TypeId, optional: Bool, readonly: Bool }),
    TArray TypeId,
    TFunction (List TypeId) TypeId,
    TUnion (List BaseType),
    TIntersection (List BaseType),
    TLiteral LiteralValue,
    TTypeVar TypeId,
    TTuple (List TypeId),
    TUnknown,
    TAny,
    TNever,
]

LiteralValue : [
    LNum F64,
    LStr Str,
    LBool Bool,
]

# Utility type representations
UtilityType : [
    # Makes all properties optional
    Partial BaseType,

    # Makes all properties required
    Required BaseType,

    # Makes all properties readonly
    Readonly BaseType,

    # Pick properties K from T
    Pick BaseType (List Str),

    # Omit properties K from T
    Omit BaseType (List Str),

    # Construct object type with keys K and values T
    Record (List Str) BaseType,

    # Exclude from union
    Exclude BaseType BaseType,

    # Extract from union
    Extract BaseType BaseType,

    # Remove null and undefined
    NonNullable BaseType,

    # Get function parameters
    Parameters BaseType,

    # Get function return type
    ReturnType BaseType,

    # Get constructor parameters
    ConstructorParameters BaseType,

    # Get instance type
    InstanceType BaseType,

    # Make properties at keys K optional
    PartialBy BaseType (List Str),

    # Make properties at keys K required
    RequiredBy BaseType (List Str),
]

# === Core Utility Type Operations ===

# Partial<T> - Make all properties optional
make_partial : BaseType -> BaseType
make_partial = \typ ->
    when typ is
        TObject props ->
            TObject (List.map props \p -> { p & optional: Bool.true })

        TIntersection types ->
            TIntersection (List.map types make_partial)

        TUnion types ->
            TUnion (List.map types make_partial)

        _ -> typ

# Required<T> - Make all properties required
make_required : BaseType -> BaseType
make_required = \typ ->
    when typ is
        TObject props ->
            TObject (List.map props \p -> { p & optional: Bool.false })

        TIntersection types ->
            TIntersection (List.map types make_required)

        TUnion types ->
            TUnion (List.map types make_required)

        _ -> typ

# Readonly<T> - Make all properties readonly
make_readonly : BaseType -> BaseType
make_readonly = \typ ->
    when typ is
        TObject props ->
            TObject (List.map props \p -> { p & readonly: Bool.true })

        TArray elem ->
            # ReadonlyArray<T>
            TArray elem  # Simplified: would mark as readonly

        TIntersection types ->
            TIntersection (List.map types make_readonly)

        TUnion types ->
            TUnion (List.map types make_readonly)

        _ -> typ

# Pick<T, K> - Pick properties K from T
make_pick : BaseType, List Str -> BaseType
make_pick = \typ, keys ->
    when typ is
        TObject props ->
            picked = List.keep_if props \p ->
                List.contains keys p.key
            TObject picked

        TIntersection types ->
            # Pick from intersection: merge first then pick
            merged = merge_intersection_objects types
            make_pick merged keys

        TUnion types ->
            # Pick from each union member
            TUnion (List.map types \t -> make_pick t keys)

        _ -> TNever

# Omit<T, K> - Omit properties K from T
make_omit : BaseType, List Str -> BaseType
make_omit = \typ, keys ->
    when typ is
        TObject props ->
            kept = List.keep_if props \p ->
                Bool.not (List.contains keys p.key)
            TObject kept

        TIntersection types ->
            # Omit from intersection: merge first then omit
            merged = merge_intersection_objects types
            make_omit merged keys

        TUnion types ->
            # Omit from each union member
            TUnion (List.map types \t -> make_omit t keys)

        _ -> typ

# Record<K, T> - Create object with keys K and values T
make_record : List Str, BaseType -> BaseType
make_record = \keys, value_type ->
    props = List.map keys \key -> {
        key,
        type: type_to_id value_type,
        optional: Bool.false,
        readonly: Bool.false,
    }
    TObject props

# Exclude<T, U> - Exclude U from union T
make_exclude : BaseType, BaseType -> BaseType
make_exclude = \typ, excluded ->
    when typ is
        TUnion members ->
            filtered = List.keep_if members \m ->
                Bool.not (is_assignable_to m excluded)
            when filtered is
                [] -> TNever
                [single] -> single
                multiple -> TUnion multiple

        _ ->
            if is_assignable_to typ excluded then
                TNever
            else
                typ

# Extract<T, U> - Extract U from union T
make_extract : BaseType, BaseType -> BaseType
make_extract = \typ, extracted ->
    when typ is
        TUnion members ->
            filtered = List.keep_if members \m ->
                is_assignable_to m extracted
            when filtered is
                [] -> TNever
                [single] -> single
                multiple -> TUnion multiple

        _ ->
            if is_assignable_to typ extracted then
                typ
            else
                TNever

# NonNullable<T> - Remove null and undefined
make_nonnullable : BaseType -> BaseType
make_nonnullable = \typ ->
    when typ is
        TUnion members ->
            filtered = List.keep_if members \m ->
                when m is
                    TNull -> Bool.false
                    TUndefined -> Bool.false
                    _ -> Bool.true
            when filtered is
                [] -> TNever
                [single] -> single
                multiple -> TUnion multiple

        TNull -> TNever
        TUndefined -> TNever
        _ -> typ

# Parameters<T> - Get function parameters
make_parameters : BaseType -> BaseType
make_parameters = \typ ->
    when typ is
        TFunction params _ ->
            TTuple params

        TUnion members ->
            # Get parameters from each function in union
            param_tuples = List.keep_oks members \m ->
                when m is
                    TFunction params _ -> Ok (TTuple params)
                    _ -> Err NotFunction

            when param_tuples is
                [] -> TNever
                [single] -> single
                multiple -> TUnion multiple

        _ -> TNever

# ReturnType<T> - Get function return type
make_return_type : BaseType -> BaseType
make_return_type = \typ ->
    when typ is
        TFunction _ ret ->
            type_from_id ret

        TUnion members ->
            # Get return type from each function in union
            return_types = List.keep_oks members \m ->
                when m is
                    TFunction _ ret -> Ok (type_from_id ret)
                    _ -> Err NotFunction

            when return_types is
                [] -> TNever
                [single] -> single
                multiple -> TUnion multiple

        _ -> TNever

# === Advanced Utility Types ===

# Awaited<T> - Unwrap Promise recursively
make_awaited : BaseType -> BaseType
make_awaited = \typ ->
    when typ is
        # Promise<T> -> T
        TObject props ->
            # Check if it's a Promise-like object
            if is_promise_like props then
                # Extract the resolved type
                get_promise_resolved_type props
            else
                typ

        TUnion members ->
            TUnion (List.map members make_awaited)

        _ -> typ

# PartialBy<T, K> - Make properties K optional
make_partial_by : BaseType, List Str -> BaseType
make_partial_by = \typ, keys ->
    when typ is
        TObject props ->
            TObject (List.map props \p ->
                if List.contains keys p.key then
                    { p & optional: Bool.true }
                else
                    p)

        _ -> typ

# RequiredBy<T, K> - Make properties K required
make_required_by : BaseType, List Str -> BaseType
make_required_by = \typ, keys ->
    when typ is
        TObject props ->
            TObject (List.map props \p ->
                if List.contains keys p.key then
                    { p & optional: Bool.false }
                else
                    p)

        _ -> typ

# ReadonlyBy<T, K> - Make properties K readonly
make_readonly_by : BaseType, List Str -> BaseType
make_readonly_by = \typ, keys ->
    when typ is
        TObject props ->
            TObject (List.map props \p ->
                if List.contains keys p.key then
                    { p & readonly: Bool.true }
                else
                    p)

        _ -> typ

# Mutable<T> - Remove readonly from all properties
make_mutable : BaseType -> BaseType
make_mutable = \typ ->
    when typ is
        TObject props ->
            TObject (List.map props \p -> { p & readonly: Bool.false })

        _ -> typ

# === String Manipulation Utility Types ===

# Uppercase<S> - Convert string literal to uppercase
make_uppercase : BaseType -> BaseType
make_uppercase = \typ ->
    when typ is
        TLiteral (LStr s) ->
            TLiteral (LStr (to_uppercase s))

        TUnion members ->
            TUnion (List.map members make_uppercase)

        _ -> typ

# Lowercase<S> - Convert string literal to lowercase
make_lowercase : BaseType -> BaseType
make_lowercase = \typ ->
    when typ is
        TLiteral (LStr s) ->
            TLiteral (LStr (to_lowercase s))

        TUnion members ->
            TUnion (List.map members make_lowercase)

        _ -> typ

# Capitalize<S> - Capitalize first letter
make_capitalize : BaseType -> BaseType
make_capitalize = \typ ->
    when typ is
        TLiteral (LStr s) ->
            TLiteral (LStr (capitalize s))

        TUnion members ->
            TUnion (List.map members make_capitalize)

        _ -> typ

# === Helper Functions ===

# Check if type is assignable to another
is_assignable_to : BaseType, BaseType -> Bool
is_assignable_to = \from, to ->
    when (from, to) is
        (_, TAny) -> Bool.true
        (TNever, _) -> Bool.true
        (TNumber, TNumber) -> Bool.true
        (TString, TString) -> Bool.true
        (TBoolean, TBoolean) -> Bool.true
        (TNull, TNull) -> Bool.true
        (TUndefined, TUndefined) -> Bool.true
        _ -> Bool.false

# Merge intersection objects
merge_intersection_objects : List BaseType -> BaseType
merge_intersection_objects = \types ->
    all_props = List.walk types [] \acc, typ ->
        when typ is
            TObject props -> List.concat acc props
            _ -> acc

    TObject all_props

# Convert type to ID (simplified)
type_to_id : BaseType -> TypeId
type_to_id = \_ -> 0

# Convert ID to type (simplified)
type_from_id : TypeId -> BaseType
type_from_id = \_ -> TUnknown

# Check if object is Promise-like
is_promise_like : List { key: Str, type: TypeId, optional: Bool, readonly: Bool } -> Bool
is_promise_like = \props ->
    List.any props \p -> p.key == "then"

# Get Promise resolved type
get_promise_resolved_type : List { key: Str, type: TypeId, optional: Bool, readonly: Bool } -> BaseType
get_promise_resolved_type = \_ -> TUnknown

# String manipulation helpers
to_uppercase : Str -> Str
to_uppercase = \s -> s  # Would convert to uppercase

to_lowercase : Str -> Str
to_lowercase = \s -> s  # Would convert to lowercase

capitalize : Str -> Str
capitalize = \s -> s  # Would capitalize first letter

# === Examples ===

# type PartialUser = Partial<User>
example_partial : BaseType -> BaseType
example_partial = \user_type ->
    make_partial user_type

# type RequiredUser = Required<User>
example_required : BaseType -> BaseType
example_required = \user_type ->
    make_required user_type

# type UserName = Pick<User, "name" | "firstName" | "lastName">
example_pick : BaseType -> BaseType
example_pick = \user_type ->
    make_pick user_type ["name", "firstName", "lastName"]

# type UserWithoutPassword = Omit<User, "password">
example_omit : BaseType -> BaseType
example_omit = \user_type ->
    make_omit user_type ["password"]

# type StringRecord = Record<string, string>
example_record : BaseType
example_record =
    make_record ["key1", "key2", "key3"] TString

# type NotNull<T> = Exclude<T, null | undefined>
example_nonnullable : BaseType -> BaseType
example_nonnullable = \typ ->
    make_nonnullable typ

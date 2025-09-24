module [
    UnionType,
    IntersectionType,
    normalize_union,
    normalize_intersection,
    check_discriminated_union,
    narrow_union_type,
]

import Option exposing [Option]

# Union and Intersection Type Operations for TypeScript

TypeId : U32

# Base type for union/intersection operations
BaseType : [
    BNum,
    BStr,
    BBool,
    BNull,
    BUndefined,
    BObject (List { key: Str, type: TypeId }),
    BArray TypeId,
    BFunction (List TypeId) TypeId,
    BLiteral LiteralValue,
    BTypeVar TypeId,
    BUnknown,
    BAny,
    BNever,
]

# Literal types for discriminated unions
LiteralValue : [
    LNum F64,
    LStr Str,
    LBool Bool,
]

# Union type representation
UnionType : {
    members: List BaseType,
    discriminator: Option.Option Str,  # Field name for discriminated union
}

# Intersection type representation
IntersectionType : {
    members: List BaseType,
}

# === Core Union Operations ===

# Create a union type
make_union : List BaseType -> UnionType
make_union = \types ->
    { members: normalize_union_members types, discriminator: None }

# Normalize union members (remove duplicates, flatten nested unions)
normalize_union_members : List BaseType -> List BaseType
normalize_union_members = \types ->
    flattened = List.walk types [] \acc, typ ->
        when typ is
            # Flatten nested unions (not implemented due to mutual recursion limit)
            _ -> List.append acc typ

    # Remove duplicates
    deduplicated = List.walk flattened [] \acc, typ ->
        if List.any acc \t -> types_equal t typ then
            acc
        else
            List.append acc typ

    # Simplify special cases
    simplify_union_members deduplicated

# Simplify union members
simplify_union_members : List BaseType -> List BaseType
simplify_union_members = \members ->
    # never | T = T (never is identity for union)
    without_never = List.keep_if members \m ->
        when m is
            BNever -> Bool.false
            _ -> Bool.true

    # any | T = any (any absorbs everything in union)
    has_any = List.any without_never \m ->
        when m is
            BAny -> Bool.true
            _ -> Bool.false

    if has_any then
        [BAny]
    else if List.is_empty without_never then
        [BNever]  # Empty union is never
    else
        without_never

# Normalize a union type
normalize_union : UnionType -> UnionType
normalize_union = \union ->
    { union & members: normalize_union_members union.members }

# === Core Intersection Operations ===

# Create an intersection type
make_intersection : List BaseType -> IntersectionType
make_intersection = \types ->
    { members: normalize_intersection_members types }

# Normalize intersection members
normalize_intersection_members : List BaseType -> List BaseType
normalize_intersection_members = \types ->
    # Remove duplicates
    deduplicated = List.walk types [] \acc, typ ->
        if List.any acc \t -> types_equal t typ then
            acc
        else
            List.append acc typ

    # Simplify special cases
    simplify_intersection_members deduplicated

# Simplify intersection members
simplify_intersection_members : List BaseType -> List BaseType
simplify_intersection_members = \members ->
    # any & T = T (any is identity for intersection)
    without_any = List.keep_if members \m ->
        when m is
            BAny -> Bool.false
            _ -> Bool.true

    # never & T = never (never absorbs everything in intersection)
    has_never = List.any without_any \m ->
        when m is
            BNever -> Bool.true
            _ -> Bool.false

    if has_never then
        [BNever]
    else if List.is_empty without_any then
        [BAny]  # Empty intersection is any
    else
        # Merge object types in intersection
        merge_intersection_objects without_any

# Merge object types in an intersection
merge_intersection_objects : List BaseType -> List BaseType
merge_intersection_objects = \members ->
    objects = List.keep_oks members \m ->
        when m is
            BObject props -> Ok props
            _ -> Err NotObject

    non_objects = List.keep_if members \m ->
        when m is
            BObject _ -> Bool.false
            _ -> Bool.true

    if List.is_empty objects then
        members
    else
        # Merge all object properties
        merged_props = List.walk objects [] \acc, obj_props ->
            List.concat acc obj_props

        List.append non_objects (BObject merged_props)

# Normalize an intersection type
normalize_intersection : IntersectionType -> IntersectionType
normalize_intersection = \intersection ->
    { members: normalize_intersection_members intersection.members }

# === Discriminated Unions ===

# Check if a union is discriminated (has a common discriminator field)
check_discriminated_union : UnionType -> Option Str
check_discriminated_union = \union ->
    # Find common literal field across all object members
    object_members = List.keep_oks union.members \m ->
        when m is
            BObject props -> Ok props
            _ -> Err NotObject

    if List.len object_members < 2 then
        None
    else
        # Find fields that exist in all objects with literal values
        find_discriminator_field object_members

# Find a discriminator field
find_discriminator_field : List (List { key: Str, type: TypeId }) -> Option Str
find_discriminator_field = \objects ->
    when List.first objects is
        Ok first_obj ->
            # Check each field in the first object
            potential_discriminators = List.keep_oks first_obj \field ->
                if is_literal_field field.type && all_have_different_literals objects field.key then
                    Ok field.key
                else
                    Err NotDiscriminator

            when List.first potential_discriminators is
                Ok discriminator -> Some discriminator
                Err _ -> None
        Err _ -> None

# Check if a type is a literal
is_literal_field : TypeId -> Bool
is_literal_field = \_ ->
    # Simplified: would check if type is literal
    Bool.true

# Check if all objects have different literal values for a field
all_have_different_literals : List (List { key: Str, type: TypeId }), Str -> Bool
all_have_different_literals = \objects, field_name ->
    # Simplified: would check literal uniqueness
    Bool.true

# === Type Narrowing ===

# Narrow a union type based on a type guard
narrow_union_type : UnionType, BaseType -> Result BaseType [NarrowError Str]
narrow_union_type = \union, guard ->
    matching_members = List.keep_if union.members \member ->
        is_assignable_to member guard

    when matching_members is
        [] -> Err (NarrowError "No matching union members")
        [single] -> Ok single
        multiple -> Ok(make_union(multiple).members |> List.first |> Result.with_default(BNever))

# === Type Operations ===

# Check if two types are equal
types_equal : BaseType, BaseType -> Bool
types_equal = \t1, t2 ->
    when (t1, t2) is
        (BNum, BNum) -> Bool.true
        (BStr, BStr) -> Bool.true
        (BBool, BBool) -> Bool.true
        (BNull, BNull) -> Bool.true
        (BUndefined, BUndefined) -> Bool.true
        (BAny, BAny) -> Bool.true
        (BNever, BNever) -> Bool.true
        (BUnknown, BUnknown) -> Bool.true
        (BTypeVar v1, BTypeVar v2) -> v1 == v2
        (BLiteral l1, BLiteral l2) -> literals_equal l1 l2
        (BArray e1, BArray e2) -> e1 == e2
        _ -> Bool.false

# Check if literals are equal
literals_equal : LiteralValue, LiteralValue -> Bool
literals_equal = \l1, l2 ->
    when (l1, l2) is
        (LNum n1, LNum n2) -> Num.compare(n1, n2) == EQ
        (LStr s1, LStr s2) -> s1 == s2
        (LBool b1, LBool b2) -> b1 == b2
        _ -> Bool.false

# Check if type is assignable to another
is_assignable_to : BaseType, BaseType -> Bool
is_assignable_to = \from, to ->
    when (from, to) is
        # Any is assignable to anything
        (BAny, _) -> Bool.true
        (_, BAny) -> Bool.true

        # Never is assignable to everything
        (BNever, _) -> Bool.true

        # Unknown requires explicit narrowing
        (_, BUnknown) -> Bool.true

        # Same types
        (BNum, BNum) -> Bool.true
        (BStr, BStr) -> Bool.true
        (BBool, BBool) -> Bool.true
        (BNull, BNull) -> Bool.true
        (BUndefined, BUndefined) -> Bool.true

        # Literals are assignable to their base types
        (BLiteral (LNum _), BNum) -> Bool.true
        (BLiteral (LStr _), BStr) -> Bool.true
        (BLiteral (LBool _), BBool) -> Bool.true

        _ -> Bool.false

# === Distribution Laws ===

# Distribute union over intersection: (A | B) & C = (A & C) | (B & C)
distribute_union_over_intersection : UnionType, IntersectionType -> UnionType
distribute_union_over_intersection = \union, intersection ->
    distributed = List.map union.members \union_member ->
        make_intersection (List.append intersection.members union_member)
        |> normalize_intersection
        |> .members
        |> List.first
        |> Result.with_default BNever

    make_union distributed

# === Conditional Types ===

# Evaluate a conditional type: T extends U ? X : Y
evaluate_conditional : BaseType, BaseType, BaseType, BaseType -> BaseType
evaluate_conditional = \check_type, extends_type, true_branch, false_branch ->
    if is_assignable_to check_type extends_type then
        true_branch
    else
        false_branch

# === Examples ===

# type Status = "loading" | "success" | "error"
example_status : UnionType
example_status =
    make_union [
        BLiteral (LStr "loading"),
        BLiteral (LStr "success"),
        BLiteral (LStr "error"),
    ]

# type Result<T> = { status: "success", data: T } | { status: "error", error: string }
example_discriminated_result : TypeId -> UnionType
example_discriminated_result = \data_type ->
    union = make_union [
        BObject [
            { key: "status", type: 100 },  # "success" literal
            { key: "data", type: data_type },
        ],
        BObject [
            { key: "status", type: 101 },  # "error" literal
            { key: "error", type: 102 },   # string
        ],
    ]
    { union & discriminator: Some "status" }

# type Intersection = { a: number } & { b: string } & { c: boolean }
example_intersection : IntersectionType
example_intersection =
    make_intersection [
        BObject [{ key: "a", type: 1 }],
        BObject [{ key: "b", type: 2 }],
        BObject [{ key: "c", type: 3 }],
    ]
    # Result: { a: number, b: string, c: boolean }

# type StringOrNumber = string | number
example_string_or_number : UnionType
example_string_or_number =
    make_union [BStr, BNum]

# === Utility Functions ===

# Extract union members
get_union_members : UnionType -> List BaseType
get_union_members = \union -> union.members

# Extract intersection members
get_intersection_members : IntersectionType -> List BaseType
get_intersection_members = \intersection -> intersection.members

# Check if type is a union
is_union : BaseType -> Bool
is_union = \_ ->
    # Would check if type is union
    Bool.false

# Check if type is an intersection
is_intersection : BaseType -> Bool
is_intersection = \_ ->
    # Would check if type is intersection
    Bool.false


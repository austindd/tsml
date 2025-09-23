module [
    TType,
    type_eq,
    type_str,
    is_assignable,
    type_union,
    type_intersect,
    optimize_type,
]

# Optimized type representation using tags for fast comparison
TType : [
    TNum,
    TStr,
    TBool,
    TUnknown,
]

# Fast equality check using pattern matching
type_eq : TType, TType -> Bool
type_eq = \t1, t2 ->
    when (t1, t2) is
        (TNum, TNum) -> Bool.true
        (TStr, TStr) -> Bool.true
        (TBool, TBool) -> Bool.true
        (TUnknown, TUnknown) -> Bool.true
        _ -> Bool.false

# Convert type to string
type_str : TType -> Str
type_str = \t ->
    when t is
        TNum -> "number"
        TStr -> "string"
        TBool -> "boolean"
        TUnknown -> "unknown"

# Check if t1 is assignable to t2
is_assignable : TType, TType -> Bool
is_assignable = \from, to ->
    when (from, to) is
        # Unknown can be assigned to anything
        (TUnknown, _) -> Bool.true
        # Anything can be assigned to unknown
        (_, TUnknown) -> Bool.true
        # Same types are assignable
        (TNum, TNum) -> Bool.true
        (TStr, TStr) -> Bool.true
        (TBool, TBool) -> Bool.true
        # Different types are not assignable
        _ -> Bool.false

# Type union - returns the least upper bound
type_union : TType, TType -> TType
type_union = \t1, t2 ->
    if type_eq t1 t2 then
        t1
    else
        # In our simplified system, union of different types is unknown
        TUnknown

# Type intersection - returns the greatest lower bound
type_intersect : TType, TType -> TType
type_intersect = \t1, t2 ->
    when (t1, t2) is
        # Unknown intersected with anything is that thing
        (TUnknown, t) -> t
        (t, TUnknown) -> t
        # Same types intersect to themselves
        (TNum, TNum) -> TNum
        (TStr, TStr) -> TStr
        (TBool, TBool) -> TBool
        # Different types have no intersection (bottom type)
        # We represent this as Unknown for simplicity
        _ -> TUnknown

# Optimize a type (remove redundancies, simplify)
optimize_type : TType -> TType
optimize_type = \t ->
    # In our simple system, types are already optimal
    t
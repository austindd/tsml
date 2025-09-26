module [
    type_eq,
    type_str,
    is_assignable,
    type_union,
    type_intersect,
    optimize_type,
]

# Optimized type representation using tags for fast comparison
import SimpleComprehensiveType as Type exposing [Type]

# Fast equality check using pattern matching
type_eq : Type, Type -> Bool
type_eq = \t1, t2 ->
    when (t1, t2) is
        (TNumber, TNumber) -> Bool.true
        (TString, TString) -> Bool.true
        (TBoolean, TBoolean) -> Bool.true
        (TUnknown, TUnknown) -> Bool.true
        _ -> Bool.false

# Convert type to string
type_str : Type -> Str
type_str = \t ->
    when t is
        TNumber -> "number"
        TString -> "string"
        TBoolean -> "boolean"
        TUnknown -> "unknown"

# Check if t1 is assignable to t2
is_assignable : Type, Type -> Bool
is_assignable = \from, to ->
    when (from, to) is
        # Unknown can be assigned to anything
        (TUnknown, _) -> Bool.true
        # Anything can be assigned to unknown
        (_, TUnknown) -> Bool.true
        # Same types are assignable
        (TNumber, TNumber) -> Bool.true
        (TString, TString) -> Bool.true
        (TBoolean, TBoolean) -> Bool.true
        # Different types are not assignable
        _ -> Bool.false

# Type union - returns the least upper bound
type_union : Type, Type -> Type
type_union = \t1, t2 ->
    if type_eq t1 t2 then
        t1
    else
        # In our simplified system, union of different types is unknown
        TUnknown

# Type intersection - returns the greatest lower bound
type_intersect : Type, Type -> Type
type_intersect = \t1, t2 ->
    when (t1, t2) is
        # Unknown intersected with anything is that thing
        (TUnknown, t) -> t
        (t, TUnknown) -> t
        # Same types intersect to themselves
        (TNumber, TNumber) -> TNumber
        (TString, TString) -> TString
        (TBoolean, TBoolean) -> TBoolean
        # Different types have no intersection (bottom type)
        # We represent this as Unknown for simplicity
        _ -> TUnknown

# Optimize a type (remove redundancies, simplify)
optimize_type : Type -> Type
optimize_type = \t ->
    # In our simple system, types are already optimal
    t

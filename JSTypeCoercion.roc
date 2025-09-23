module [
    coerce_to_boolean,
    coerce_to_number,
    coerce_to_string,
    is_truthy,
    is_falsy,
    infer_binary_op,
    infer_equality,
]

import MinimalType exposing [TType]

# JavaScript type coercion rules

# Convert any type to boolean (for conditions, logical operators, etc.)
coerce_to_boolean : TType -> TType
coerce_to_boolean = \t ->
    when t is
        TBool -> TBool
        TNum -> TBool  # 0, NaN are false, others are true
        TStr -> TBool  # "" is false, others are true
        TUnknown -> TBool  # null, undefined are false
        _ -> TBool

# Convert any type to number (for arithmetic operations)
coerce_to_number : TType -> TType
coerce_to_number = \t ->
    when t is
        TNum -> TNum
        TBool -> TNum  # true -> 1, false -> 0
        TStr -> TNum  # Parsed or NaN
        TUnknown -> TNum  # null -> 0, undefined -> NaN
        _ -> TNum

# Convert any type to string (for concatenation)
coerce_to_string : TType -> TType
coerce_to_string = \t ->
    when t is
        TStr -> TStr
        TNum -> TStr  # toString()
        TBool -> TStr  # "true" or "false"
        TUnknown -> TStr  # "null" or "undefined"
        _ -> TStr

# Check if a type is always truthy
is_truthy : TType -> Bool
is_truthy = \t ->
    when t is
        TBool -> Bool.false  # Could be false
        TNum -> Bool.false  # Could be 0 or NaN
        TStr -> Bool.false  # Could be ""
        TUnknown -> Bool.false  # null/undefined are falsy
        _ -> Bool.false  # Conservative

# Check if a type is always falsy
is_falsy : TType -> Bool
is_falsy = \t ->
    when t is
        TUnknown -> Bool.true  # null/undefined always falsy (simplified)
        _ -> Bool.false  # Others could be truthy

# Infer type for binary operators with coercion
infer_binary_op : TType, TType, Str -> TType
infer_binary_op = \left, right, op ->
    when op is
        # Plus operator has special rules
        "+" ->
            # If either operand is string, result is string
            if left == TStr || right == TStr then
                TStr
            else
                TNum

        # Other arithmetic operators always produce numbers
        "-" | "*" | "/" | "%" | "**" -> TNum

        # Bitwise operators always produce numbers
        "&" | "|" | "^" | "<<" | ">>" | ">>>" -> TNum

        # Comparison operators always produce booleans
        "<" | ">" | "<=" | ">=" -> TBool

        # Instanceof and in operators
        "instanceof" | "in" -> TBool

        _ -> TUnknown

# Infer type for equality operators
infer_equality : TType, TType, Str -> TType
infer_equality = \left, right, op ->
    when op is
        # Strict equality doesn't coerce
        "===" | "!==" -> TBool

        # Loose equality coerces types
        "==" | "!=" -> TBool

        _ -> TUnknown
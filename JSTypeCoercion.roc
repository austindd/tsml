module [
    coerce_to_boolean,
    coerce_to_number,
    coerce_to_string,
    is_truthy,
    is_falsy,
    infer_binary_op,
    infer_equality,
]

import SimpleComprehensiveType as Type exposing [Type]

# JavaScript type coercion rules

# Convert any type to boolean (for conditions, logical operators, etc.)
coerce_to_boolean : Type -> Type
coerce_to_boolean = \t ->
    when t is
        TBoolean -> TBoolean
        TNumber -> TBoolean  # 0, NaN are false, others are true
        TString -> TBoolean  # "" is false, others are true
        TUnknown -> TBoolean  # null, undefined are false
        _ -> TBoolean

# Convert any type to number (for arithmetic operations)
coerce_to_number : Type -> Type
coerce_to_number = \t ->
    when t is
        TNumber -> TNumber
        TBoolean -> TNumber  # true -> 1, false -> 0
        TString -> TNumber  # Parsed or NaN
        TUnknown -> TNumber  # null -> 0, undefined -> NaN
        _ -> TNumber

# Convert any type to string (for concatenation)
coerce_to_string : Type -> Type
coerce_to_string = \t ->
    when t is
        TString -> TString
        TNumber -> TString  # toString()
        TBoolean -> TString  # "true" or "false"
        TUnknown -> TString  # "null" or "undefined"
        _ -> TString

# Check if a type is always truthy
is_truthy : Type -> Bool
is_truthy = \t ->
    when t is
        TBoolean -> Bool.false  # Could be false
        TNumber -> Bool.false  # Could be 0 or NaN
        TString -> Bool.false  # Could be ""
        TUnknown -> Bool.false  # null/undefined are falsy
        _ -> Bool.false  # Conservative

# Check if a type is always falsy
is_falsy : Type -> Bool
is_falsy = \t ->
    when t is
        TUnknown -> Bool.true  # null/undefined always falsy (simplified)
        _ -> Bool.false  # Others could be truthy

# Infer type for binary operators with coercion
infer_binary_op : Type, Type, Str -> Type
infer_binary_op = \left, right, op ->
    when op is
        # Plus operator has special rules
        "+" ->
            # If either operand is string, result is string
            if left == TString || right == TString then
                TString
            else
                TNumber

        # Other arithmetic operators always produce numbers
        "-" | "*" | "/" | "%" | "**" -> TNumber

        # Bitwise operators always produce numbers
        "&" | "|" | "^" | "<<" | ">>" | ">>>" -> TNumber

        # Comparison operators always produce booleans
        "<" | ">" | "<=" | ">=" -> TBoolean

        # Instanceof and in operators
        "instanceof" | "in" -> TBoolean

        _ -> TUnknown

# Infer type for equality operators
infer_equality : Type, Type, Str -> Type
infer_equality = \left, right, op ->
    when op is
        # Strict equality doesn't coerce
        "===" | "!==" -> TBoolean

        # Loose equality coerces types
        "==" | "!=" -> TBoolean

        _ -> TUnknown

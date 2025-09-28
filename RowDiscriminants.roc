module [
    add_discriminant_to_row,
    check_discriminant_in_row,
    narrow_union_by_discriminant,
]

import ComprehensiveTypeIndexed exposing [
    TypeStore,
    TypeId,
    RowId,
    TypeDef,
    RowDef,
]

# Row-based Discriminant Support
#
# This module provides a different approach to discriminated unions
# that works within the row type system. Instead of creating nominal types,
# we encode discriminant information directly in the row structure.
#
# Key idea: For discriminant fields, we create singleton literal types
# that must match exactly. This preserves type information through unions.

# Add a discriminant field to a row
# The field must have a literal string type for the discriminant to work
add_discriminant_to_row : TypeStore, RowId, Str, Str -> (TypeStore, RowId)
add_discriminant_to_row = \store, row_id, field_name, field_value ->
    # Create a literal type for the discriminant value
    (store1, lit_type) = ComprehensiveTypeIndexed.make_literal store (StrLit field_value)

    # Add this field to the row with readonly = true (discriminants shouldn't change)
    ComprehensiveTypeIndexed.make_row_extend store1 field_name lit_type Bool.false Bool.true row_id

# Check if a row has a specific discriminant value
check_discriminant_in_row : TypeStore, RowId, Str, Str -> Bool
check_discriminant_in_row = \store, row_id, field_name, field_value ->
    when ComprehensiveTypeIndexed.get_row store row_id is
        Ok row_def ->
            when row_def is
                RExtend { field_name: fname, field_type, rest } ->
                    if fname == field_name then
                        # Check if field_type is the expected literal
                        when ComprehensiveTypeIndexed.get_type store field_type is
                            Ok (TLiteral (StrLit value)) ->
                                value == field_value
                            _ -> Bool.false
                    else
                        # Check rest of the row
                        check_discriminant_in_row store rest field_name field_value
                _ -> Bool.false
        Err _ -> Bool.false

# Narrow a union type based on a discriminant check
# This filters the union to only include types with matching discriminant
narrow_union_by_discriminant : TypeStore, TypeId, Str, Str -> Result (TypeStore, TypeId) [NotAUnion, NoMatches]
narrow_union_by_discriminant = \store, type_id, field_name, field_value ->
    when ComprehensiveTypeIndexed.get_type store type_id is
        Ok (TUnion types) ->
            # Filter union members by discriminant
            matching_types = List.keep_if types \member_id ->
                type_has_discriminant store member_id field_name field_value

            when matching_types is
                [] -> Err NoMatches
                [single] -> Ok (store, single)
                multiple ->
                    # Create new union with only matching types
                    (new_store, narrowed) = ComprehensiveTypeIndexed.make_union store multiple
                    Ok (new_store, narrowed)
        _ -> Err NotAUnion

# Helper: Check if a type has a specific discriminant
type_has_discriminant : TypeStore, TypeId, Str, Str -> Bool
type_has_discriminant = \store, type_id, field_name, field_value ->
    when ComprehensiveTypeIndexed.get_type store type_id is
        Ok type_def ->
            when type_def is
                TObject row_id ->
                    check_discriminant_in_row store row_id field_name field_value
                TIntersection types ->
                    # Check if any member has the discriminant
                    List.any types \t ->
                        type_has_discriminant store t field_name field_value
                _ -> Bool.false
        Err _ -> Bool.false
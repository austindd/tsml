module [
    DiscriminantId,
    DiscriminantStore,
    empty_discriminant_store,
    register_discriminant,
    get_discriminant_for_field,
    create_discriminated_object_type,
    narrow_by_discriminant,
]

import ComprehensiveTypeIndexed exposing [
    TypeStore,
    TypeId,
    RowId,
]

# Discriminated Union Support
#
# This module provides support for TypeScript-style discriminated unions
# by treating discriminant fields as nominal types. Each unique combination
# of (field_name, field_value) gets a unique nominal type ID.
#
# For example, in the union:
#   type Shape =
#     | { kind: "circle", radius: number }
#     | { kind: "square", side: number }
#
# We create nominal types for:
#   - ("kind", "circle") -> CircleKind
#   - ("kind", "square") -> SquareKind
#
# Objects with these fields are treated as "inheriting" from these nominal types,
# allowing the type system to preserve the distinction even through unions.

DiscriminantId : U64

DiscriminantInfo : {
    field_name: Str,
    field_value: Str,  # We only support string literal discriminants for now
    nominal_type_id: TypeId,
}

DiscriminantStore : {
    next_id: DiscriminantId,
    discriminants: List DiscriminantInfo,
    # Index for fast lookup
    by_field_value: List { key: (Str, Str), id: DiscriminantId },
}

empty_discriminant_store : DiscriminantStore
empty_discriminant_store = {
    next_id: 1000000,  # Start with high ID to avoid conflicts
    discriminants: [],
    by_field_value: [],
}

# Register a discriminant field/value pair and get its nominal type
register_discriminant : DiscriminantStore, TypeStore, Str, Str -> (DiscriminantStore, TypeStore, TypeId)
register_discriminant = \disc_store, type_store, field_name, field_value ->
    key = (field_name, field_value)

    # Check if this discriminant already exists
    when List.find_first disc_store.by_field_value \entry -> entry.key == key is
        Ok existing ->
            # Find the discriminant info
            when List.find_first disc_store.discriminants \d ->
                d.field_name == field_name && d.field_value == field_value is
                Ok disc_info ->
                    (disc_store, type_store, disc_info.nominal_type_id)
                Err _ ->
                    # Shouldn't happen, but create a new one
                    create_new_discriminant disc_store type_store field_name field_value
        Err _ ->
            create_new_discriminant disc_store type_store field_name field_value

create_new_discriminant : DiscriminantStore, TypeStore, Str, Str -> (DiscriminantStore, TypeStore, TypeId)
create_new_discriminant = \disc_store, type_store, field_name, field_value ->
    # Create a nominal class type for this discriminant
    # This acts like a phantom type that objects can "inherit" from
    class_name = "Discriminant_$(field_name)_$(field_value)"

    class_def = {
        name: class_name,
        type_params: [],
        extends: None,
        implements: [],
        members: [],
        is_abstract: Bool.false,
    }

    (type_store1, class_type) = ComprehensiveTypeIndexed.make_class type_store class_def

    disc_info = {
        field_name,
        field_value,
        nominal_type_id: class_type,
    }

    new_disc_store = {
        next_id: disc_store.next_id + 1,
        discriminants: List.append disc_store.discriminants disc_info,
        by_field_value: List.append disc_store.by_field_value { key: (field_name, field_value), id: disc_store.next_id },
    }

    (new_disc_store, type_store1, class_type)

# Get the discriminant type for a field/value pair if it exists
get_discriminant_for_field : DiscriminantStore, Str, Str -> Result TypeId [NotFound]
get_discriminant_for_field = \disc_store, field_name, field_value ->
    List.find_first disc_store.discriminants \d ->
        d.field_name == field_name && d.field_value == field_value
    |> Result.map \d -> d.nominal_type_id
    |> Result.map_err \_ -> NotFound

# Create an object type that includes discriminant information
# This wraps the regular object type with intersection of discriminant nominal types
create_discriminated_object_type : TypeStore, DiscriminantStore, RowId, List (Str, Str) -> (TypeStore, DiscriminantStore, TypeId)
create_discriminated_object_type = \type_store, disc_store, row_id, discriminants ->
    # First create the base object type
    (store1, obj_type) = ComprehensiveTypeIndexed.make_object type_store row_id

    # Then add discriminant nominal types
    result = List.walk discriminants (store1, disc_store, []) \(store, dstore, types), (field, value) ->
        (new_dstore, new_store, disc_type) = register_discriminant dstore store field value
        (new_store, new_dstore, List.append types disc_type)

    when result is
        (final_store, final_disc_store, disc_types) ->
            if List.is_empty disc_types then
                (final_store, final_disc_store, obj_type)
            else
                # Create intersection of object with all discriminant types
                all_types = List.prepend disc_types obj_type
                (store2, intersect_type) = ComprehensiveTypeIndexed.make_intersection final_store all_types
                (store2, final_disc_store, intersect_type)

# Type narrowing by discriminant field
# Given a union type and a discriminant check, narrow to matching branch
narrow_by_discriminant : TypeStore, TypeId, Str, Str -> Result TypeId [CannotNarrow]
narrow_by_discriminant = \store, union_type, field_name, field_value ->
    # This is a simplified version - full implementation would:
    # 1. Check if union_type is actually a union
    # 2. Filter union members that have the matching discriminant
    # 3. Return the narrowed type

    # For now, just return the original type
    Ok union_type

# Example usage:
#
# Shape = { kind: "circle", radius: number } | { kind: "square", side: number }
#
# When we encounter { kind: "circle", radius: 5 }, we:
# 1. Register discriminant ("kind", "circle")
# 2. Create object type for { kind: "circle", radius: number }
# 3. Intersect with the discriminant nominal type
# 4. Result: CircleObject & DiscriminantKindCircle
#
# When forming the union, both types retain their discriminant information,
# allowing type narrowing to work correctly.
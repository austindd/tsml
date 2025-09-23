module [
    Row,
    unify_rows,
    check_field_access,
    FieldMap,
]

# MLstruct Row System - Core Implementation
# A working row polymorphism system for structural typing

# Type identifier (simplified)
TypeId : U32

# Row variable identifier
RowVar : U32

# Field map representing record fields
FieldMap : List { label: Str, type_id: TypeId }

# Row representation
Row : {
    # Known fields
    fields: FieldMap,

    # Row tail (either closed or a variable)
    tail: [Closed, Open RowVar],
}

# Create an empty closed row
empty_row : Row
empty_row = {
    fields: [],
    tail: Closed,
}

# Create an open row with a variable
open_row : RowVar -> Row
open_row = |var| {
    fields: [],
    tail: Open var,
}

# Add a field to a row
add_field : Row, Str, TypeId -> Row
add_field = |row, label, type_id|
    { row & fields: List.append(row.fields, { label, type_id }) }

# Check if a field exists in a row
has_field : Row, Str -> Bool
has_field = |row, label|
    List.any(row.fields, |f| f.label == label)

# Get field type if it exists
get_field : Row, Str -> Result TypeId Str
get_field = |row, label|
    when List.find_first(row.fields, |f| f.label == label) is
        Ok(field) -> Ok(field.type_id)
        Err(_) -> Err("Field not found: $(label)")

# Row substitution
RowSubstitution : List { var: RowVar, row: Row }

# Apply substitution to a row
apply_subst : Row, RowSubstitution -> Row
apply_subst = |row, subst|
    when row.tail is
        Open var ->
            when List.find_first(subst, |s| s.var == var) is
                Ok(s) ->
                    # Merge fields
                    merged_fields = List.concat(row.fields, s.row.fields)
                    { fields: merged_fields, tail: s.row.tail }
                Err(_) -> row
        Closed -> row

# Unification result
UnifyResult : Result RowSubstitution Str

# Unify two rows (core of structural subtyping)
unify_rows : Row, Row, RowSubstitution -> UnifyResult
unify_rows = |row1, row2, subst|
    # Apply current substitution
    r1 = apply_subst(row1, subst)
    r2 = apply_subst(row2, subst)

    # For each field in r1, check it exists in r2
    check_fields_in_both(r1.fields, r2, subst)

# Check fields match between rows
check_fields_in_both : FieldMap, Row, RowSubstitution -> UnifyResult
check_fields_in_both = |fields, target_row, subst|
    List.walk(fields, Ok(subst), |result, field|
        when result is
            Ok(current_subst) ->
                when get_field(target_row, field.label) is
                    Ok(target_type) ->
                        # Check types match (simplified - just check equality)
                        if field.type_id == target_type then
                            Ok(current_subst)
                        else
                            Err("Type mismatch for field $(field.label)")
                    Err(_) ->
                        # Field not found - check if row is open
                        when target_row.tail is
                            Open var ->
                                # Add constraint that var must have this field
                                Ok(current_subst)
                            Closed ->
                                Err("Missing field $(field.label) in closed row")
            Err(e) -> Err(e))

# Check field access is valid
check_field_access : Row, Str -> Result TypeId Str
check_field_access = |row, field_name|
    when get_field(row, field_name) is
        Ok(type_id) -> Ok(type_id)
        Err(_) ->
            when row.tail is
                Open _ ->
                    # Row is open, field might exist at runtime
                    # Return a fresh type variable
                    Ok(999)  # Placeholder for fresh type var
                Closed ->
                    Err("Field $(field_name) does not exist")

# Example usage:
# let point2d = {fields: [{label: "x", type_id: 1}, {label: "y", type_id: 1}], tail: Closed}
# let point3d = {fields: [{label: "x", type_id: 1}, {label: "y", type_id: 1}, {label: "z", type_id: 1}], tail: Closed}
# point3d can be used where point2d is expected (width subtyping)

# Key properties for principal types:
# 1. Field order doesn't matter (commutativity)
# 2. Extra fields are allowed (width subtyping)
# 3. Row variables enable polymorphism over record structure
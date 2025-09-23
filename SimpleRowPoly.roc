module [
    Row,
    RowVar,
    Field,
    unify_rows,
    extend_row,
    empty_row,
    open_row,
    find_field,
]

# Simple Row Polymorphism without recursive types
# Using lists and tags to avoid Roc compiler issues

# Type identifier
TypeId : U32

# Row variable identifier
RowVar : U32

# Field in a record
Field : { label: Str, type_id: TypeId }

# Row representation using a simple list
Row : {
    # Known fields
    fields: List Field,

    # Row tail - either closed or has a variable
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

# Extend a row with a new field
extend_row : Row, Str, TypeId -> Row
extend_row = |row, label, type_id|
    new_field = { label, type_id }
    { row & fields: List.append(row.fields, new_field) }

# Find a field in a row
find_field : Row, Str -> Result Field [NotFound]
find_field = |row, label|
    row.fields
    |> List.find_first(|field| field.label == label)
    |> Result.map_err(|_| NotFound)

# Check if a row has a field
has_field : Row, Str -> Bool
has_field = |row, label|
    List.any(row.fields, |field| field.label == label)

# Substitution for row variables
Subst : List { var: RowVar, row: Row }

# Apply substitution to a row
apply_subst : Row, Subst -> Row
apply_subst = |row, subst|
    when row.tail is
        Closed -> row
        Open var ->
            when List.find_first(subst, |s| s.var == var) is
                Ok(s) ->
                    # Merge fields from both rows
                    merged_fields = List.concat(row.fields, s.row.fields)
                    { fields: merged_fields, tail: s.row.tail }
                Err(_) -> row

# Check if a row variable occurs in a row (for occurs check)
occurs_in_row : RowVar, Row -> Bool
occurs_in_row = |var, row|
    when row.tail is
        Open v -> v == var
        Closed -> Bool.false

# Unification result
UnifyResult : Result Subst [TypeMismatch Str, InfiniteRow]

# Unify two rows - the core of structural typing
unify_rows : Row, Row, Subst -> UnifyResult
unify_rows = |r1, r2, subst|
    # Apply current substitution
    row1 = apply_subst(r1, subst)
    row2 = apply_subst(r2, subst)

    # Check all fields in row1 exist in row2
    check_fields_result = List.walk(row1.fields, Ok(subst), |acc_result, field1|
        when acc_result is
            Ok(current_subst) ->
                when find_field(row2, field1.label) is
                    Ok(field2) ->
                        # Check that types match
                        if field1.type_id == field2.type_id then
                            Ok(current_subst)
                        else
                            Err(TypeMismatch("Type mismatch for field $(field1.label)"))
                    Err(NotFound) ->
                        # Field not in row2 - check if row2 is open
                        when row2.tail is
                            Open _ ->
                                # Row is open, field could be added
                                Ok(current_subst)
                            Closed ->
                                Err(TypeMismatch("Missing field $(field1.label) in closed row"))
            Err(e) -> Err(e))

    when check_fields_result is
        Ok(subst1) ->
            # Check all fields in row2 exist in row1
            List.walk(row2.fields, Ok(subst1), |acc_result, field2|
                when acc_result is
                    Ok(current_subst) ->
                        when find_field(row1, field2.label) is
                            Ok(_) ->
                                # Already checked above
                                Ok(current_subst)
                            Err(NotFound) ->
                                # Field not in row1 - check if row1 is open
                                when row1.tail is
                                    Open _ ->
                                        Ok(current_subst)
                                    Closed ->
                                        Err(TypeMismatch("Missing field $(field2.label) in closed row"))
                    Err(e) -> Err(e))
        Err(e) -> Err(e)

# Example usage:
# point2d = { fields: [{ label: "x", type_id: 1 }, { label: "y", type_id: 1 }], tail: Closed }
# point3d = { fields: [{ label: "x", type_id: 1 }, { label: "y", type_id: 1 }, { label: "z", type_id: 1 }], tail: Closed }
#
# unify_rows(point2d, point3d, []) would fail (point2d missing z)
# unify_rows(point3d, point2d, []) would succeed if point2d had Open tail (width subtyping)
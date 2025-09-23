module [
    Row,
    RowVar,
    TypeId,
    unify_rows,
    extend_row,
    empty_closed_row,
    empty_open_row,
    check_field,
    row_to_string,
    # For testing
    example_unification,
]

# Type identifier (simplified)
TypeId : U32

# Row variable identifier
RowVar : U32

# A row is a collection of fields plus a tail
Row : {
    fields: List { label: Str, type_id: TypeId },
    tail: [Closed, Open RowVar],
}

# Create an empty closed row
empty_closed_row : Row
empty_closed_row = {
    fields: [],
    tail: Closed,
}

# Create an empty open row
empty_open_row : RowVar -> Row
empty_open_row = \var -> {
    fields: [],
    tail: Open var,
}

# Add a field to a row
extend_row : Row, Str, TypeId -> Row
extend_row = \row, label, type_id ->
    new_field = { label, type_id }
    { row & fields: List.append row.fields new_field }

# Check if a field exists in a row
check_field : Row, Str -> Result TypeId [FieldNotFound, RequiresOpenRow]
check_field = \row, label ->
    when List.find_first row.fields \f -> f.label == label is
        Ok field -> Ok field.type_id
        Err _ ->
            when row.tail is
                Open _ -> Err RequiresOpenRow  # Field might exist
                Closed -> Err FieldNotFound

# Row substitution (maps row variables to rows)
RowSubst : List { var: RowVar, row: Row }

# Apply substitution to a row
apply_subst : Row, RowSubst -> Row
apply_subst = \row, subst ->
    when row.tail is
        Closed -> row
        Open var ->
            when List.find_first subst \s -> s.var == var is
                Ok s ->
                    # Merge fields and apply recursively
                    merged = {
                        fields: List.concat row.fields s.row.fields,
                        tail: s.row.tail,
                    }
                    apply_subst merged subst
                Err _ -> row

# Result of unification
UnifyResult : Result RowSubst [
    FieldTypeMismatch Str,
    MissingField Str,
    InfiniteType,
]

# The core unification algorithm for row polymorphism
unify_rows : Row, Row, RowSubst -> UnifyResult
unify_rows = \row1, row2, subst ->
    # Apply current substitution
    r1 = apply_subst row1 subst
    r2 = apply_subst row2 subst

    # First, check all fields in r1 exist in r2
    check_r1_fields = List.walk r1.fields (Ok subst) \acc, field1 ->
        when acc is
            Ok current_subst ->
                when List.find_first r2.fields \f -> f.label == field1.label is
                    Ok field2 ->
                        if field1.type_id == field2.type_id then
                            Ok current_subst
                        else
                            Err (FieldTypeMismatch field1.label)
                    Err _ ->
                        # Field not in r2 - check if r2 is open
                        when r2.tail is
                            Open var2 ->
                                # Add constraint: var2 must have field1
                                new_row = {
                                    fields: [field1],
                                    tail: Open (var2 + 1000),  # Fresh var
                                }
                                Ok (List.append current_subst { var: var2, row: new_row })
                            Closed ->
                                Err (MissingField field1.label)
            Err e -> Err e

    # Then, check all fields in r2 exist in r1
    when check_r1_fields is
        Ok subst1 ->
            List.walk r2.fields (Ok subst1) \acc, field2 ->
                when acc is
                    Ok current_subst ->
                        when List.find_first r1.fields \f -> f.label == field2.label is
                            Ok _ -> Ok current_subst  # Already checked type above
                            Err _ ->
                                # Field not in r1 - check if r1 is open
                                when r1.tail is
                                    Open var1 ->
                                        # Add constraint: var1 must have field2
                                        new_row = {
                                            fields: [field2],
                                            tail: Open (var1 + 2000),  # Fresh var
                                        }
                                        Ok (List.append current_subst { var: var1, row: new_row })
                                    Closed ->
                                        Err (MissingField field2.label)
                    Err e -> Err e
        Err e -> Err e

# Convert row to string for debugging
row_to_string : Row -> Str
row_to_string = \row ->
    field_strs = List.map row.fields \f ->
        "$(f.label): T$(Num.to_str f.type_id)"

    fields_str = Str.join_with field_strs ", "

    tail_str = when row.tail is
        Closed -> ""
        Open var -> ", ...ρ$(Num.to_str var)"

    "{$(fields_str)$(tail_str)}"

# Example: Demonstrate row polymorphism in action
example_unification : {} -> Str
example_unification = \{} ->
    # Create {x: T1, y: T2}
    point2d = empty_closed_row
        |> extend_row "x" 1
        |> extend_row "y" 2

    # Create {x: T1, y: T2, z: T3}
    point3d = point2d
        |> extend_row "z" 3

    # Create {x: T1, y: T2, ...ρ42}
    open_point = empty_open_row 42
        |> extend_row "x" 1
        |> extend_row "y" 2

    # Test 1: Two identical closed rows should unify
    result1 = when unify_rows point2d point2d [] is
        Ok _ -> "✓ Test 1 passed: identical rows unify"
        Err _ -> "✗ Test 1 failed"

    # Test 2: Closed row missing field should fail
    result2 = when unify_rows point2d point3d [] is
        Ok _ -> "✗ Test 2 failed: should have rejected extra field"
        Err (MissingField _) -> "✓ Test 2 passed: detected missing field"
        Err _ -> "✗ Test 2 failed with wrong error"

    # Test 3: Open row can accept extra fields
    result3 = when unify_rows open_point point3d [] is
        Ok _ -> "✓ Test 3 passed: open row accepts extra fields"
        Err _ -> "✗ Test 3 failed"

    # Test 4: Row commutation - field order doesn't matter
    # Create {y: T2, x: T1} (reversed order)
    reversed = empty_closed_row
        |> extend_row "y" 2
        |> extend_row "x" 1

    result4 = when unify_rows point2d reversed [] is
        Ok _ -> "✓ Test 4 passed: field order doesn't matter"
        Err _ -> "✗ Test 4 failed"

    # Combine all results
    results = [result1, result2, result3, result4]
    Str.join_with results "\n"
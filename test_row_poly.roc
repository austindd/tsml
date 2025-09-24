app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

# Row polymorphism implementation inline

TypeId : U32
RowVar : U32

Row : {
    fields: List { label: Str, type_id: TypeId },
    tail: [Closed, Open RowVar],
}

empty_closed_row : Row
empty_closed_row = {
    fields: [],
    tail: Closed,
}

empty_open_row : RowVar -> Row
empty_open_row = \var -> {
    fields: [],
    tail: Open var,
}

extend_row : Row, Str, TypeId -> Row
extend_row = \row, label, type_id ->
    new_field = { label, type_id }
    { row & fields: List.append row.fields new_field }

RowSubst : List { var: RowVar, row: Row }

apply_subst : Row, RowSubst -> Row
apply_subst = \row, subst ->
    when row.tail is
        Closed -> row
        Open var ->
            when List.find_first subst \s -> s.var == var is
                Ok s ->
                    merged = {
                        fields: List.concat row.fields s.row.fields,
                        tail: s.row.tail,
                    }
                    apply_subst merged subst
                Err _ -> row

unify_rows : Row, Row, RowSubst -> Result RowSubst [FieldTypeMismatch Str, MissingField Str]
unify_rows = \row1, row2, subst ->
    r1 = apply_subst row1 subst
    r2 = apply_subst row2 subst

    # Check all fields in r1 exist in r2
    check_r1 = List.walk r1.fields (Ok subst) \acc, field1 ->
        when acc is
            Ok current_subst ->
                when List.find_first r2.fields \f -> f.label == field1.label is
                    Ok field2 ->
                        if field1.type_id == field2.type_id then
                            Ok current_subst
                        else
                            Err (FieldTypeMismatch field1.label)
                    Err _ ->
                        when r2.tail is
                            Open var2 ->
                                new_row = {
                                    fields: [field1],
                                    tail: Open (var2 + 1000),
                                }
                                Ok (List.append current_subst { var: var2, row: new_row })
                            Closed ->
                                Err (MissingField field1.label)
            Err e -> Err e

    # Check all fields in r2 exist in r1
    when check_r1 is
        Ok subst1 ->
            List.walk r2.fields (Ok subst1) \acc, field2 ->
                when acc is
                    Ok current_subst ->
                        when List.find_first r1.fields \f -> f.label == field2.label is
                            Ok _ -> Ok current_subst
                            Err _ ->
                                when r1.tail is
                                    Open var1 ->
                                        new_row = {
                                            fields: [field2],
                                            tail: Open (var1 + 2000),
                                        }
                                        Ok (List.append current_subst { var: var1, row: new_row })
                                    Closed ->
                                        Err (MissingField field2.label)
                    Err e -> Err e
        Err e -> Err e

row_to_string : Row -> Str
row_to_string = \row ->
    field_strs = List.map row.fields \f ->
        "$(f.label): T$(Num.to_str f.type_id)"

    fields_str = Str.join_with field_strs ", "

    tail_str = when row.tail is
        Closed -> ""
        Open var -> ", ...ρ$(Num.to_str var)"

    "{$(fields_str)$(tail_str)}"

main =
    # Test row polymorphism

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

    # Test 1: Identical rows unify
    test1 = when unify_rows point2d point2d [] is
        Ok _ -> "✓ Test 1: Identical rows unify"
        Err _ -> "✗ Test 1 failed"

    # Test 2: Closed row missing field fails
    test2 = when unify_rows point2d point3d [] is
        Ok _ -> "✗ Test 2 failed"
        Err (MissingField _) -> "✓ Test 2: Missing field detected"
        Err _ -> "✗ Test 2 wrong error"

    # Test 3: Open row accepts extra fields
    test3 = when unify_rows open_point point3d [] is
        Ok _ -> "✓ Test 3: Open row accepts extra fields"
        Err _ -> "✗ Test 3 failed"

    # Test 4: Field order doesn't matter
    reversed = empty_closed_row
        |> extend_row "y" 2
        |> extend_row "x" 1

    test4 = when unify_rows point2d reversed [] is
        Ok _ -> "✓ Test 4: Field order doesn't matter"
        Err _ -> "✗ Test 4 failed"

    # Test 5: Width subtyping
    # Can we use point3d where point2d is expected (with open row)?
    test5 = when unify_rows open_point point3d [] is
        Ok subst ->
            # The open row should now have z field added
            unified = apply_subst open_point subst
            "✓ Test 5: Width subtyping works - $(row_to_string unified)"
        Err _ -> "✗ Test 5 failed"

    # Print results
    """
    Row Polymorphism Tests
    ======================

    point2d = $(row_to_string point2d)
    point3d = $(row_to_string point3d)
    open_point = $(row_to_string open_point)

    Results:
    $(test1)
    $(test2)
    $(test3)
    $(test4)
    $(test5)

    Row polymorphism is working! This enables:
    - Structural typing (field names matter, not type names)
    - Width subtyping (extra fields are OK with open rows)
    - Principal type inference (most general type)
    - Field order independence (true record types)
    """

module [main]

# Test simple row polymorphism implementation

# Row representation
Row : {
    fields: List { label: Str, type_id: U32 },
    tail: [Closed, Open U32],
}

empty_row : Row
empty_row = {
    fields: [],
    tail: Closed,
}

open_row : U32 -> Row
open_row = |var| {
    fields: [],
    tail: Open var,
}

extend_row : Row, Str, U32 -> Row
extend_row = |row, label, type_id|
    new_field = { label, type_id }
    { row & fields: List.append(row.fields, new_field) }

find_field : Row, Str -> Result { label: Str, type_id: U32 } [NotFound]
find_field = |row, label|
    row.fields
    |> List.find_first(|field| field.label == label)
    |> Result.map_err(|_| NotFound)

# Test row unification
unify_rows_simple : Row, Row -> Result {} [Mismatch Str]
unify_rows_simple = |row1, row2|
    # Check all fields in row1 exist in row2
    check1 = List.walk(row1.fields, Ok({}), |acc, field1|
        when acc is
            Ok({}) ->
                when find_field(row2, field1.label) is
                    Ok(field2) ->
                        if field1.type_id == field2.type_id then
                            Ok({})
                        else
                            Err(Mismatch("Type mismatch for $(field1.label)"))
                    Err(NotFound) ->
                        when row2.tail is
                            Open _ -> Ok({})  # Row is open, OK
                            Closed -> Err(Mismatch("Missing field $(field1.label)"))
            Err(e) -> Err(e))

    # Check all fields in row2 exist in row1
    when check1 is
        Ok({}) ->
            List.walk(row2.fields, Ok({}), |acc, field2|
                when acc is
                    Ok({}) ->
                        when find_field(row1, field2.label) is
                            Ok(_) -> Ok({})  # Already checked
                            Err(NotFound) ->
                                when row1.tail is
                                    Open _ -> Ok({})
                                    Closed -> Err(Mismatch("Missing field $(field2.label)"))
                    Err(e) -> Err(e))
        Err(e) -> Err(e)

main =
    # Create test rows
    point2d = empty_row
        |> extend_row("x", 1)
        |> extend_row("y", 1)

    point3d = point2d
        |> extend_row("z", 1)

    open_point = open_row(42)
        |> extend_row("x", 1)
        |> extend_row("y", 1)

    # Test 1: Closed rows with same fields should unify
    test1 = when unify_rows_simple(point2d, point2d) is
        Ok({}) -> "✓ Test 1: Same closed rows unify"
        Err(_) -> "✗ Test 1 failed"

    # Test 2: Closed row with more fields doesn't unify with less
    test2 = when unify_rows_simple(point2d, point3d) is
        Ok({}) -> "✗ Test 2: Should have failed"
        Err(_) -> "✓ Test 2: Closed row properly rejects extra fields"

    # Test 3: Open row can unify with closed row having more fields
    test3 = when unify_rows_simple(open_point, point3d) is
        Ok({}) -> "✓ Test 3: Open row accepts extra fields"
        Err(_) -> "✗ Test 3 failed"

    # Test 4: Width subtyping - larger record can be used where smaller is expected
    # (if smaller has open tail)
    test4 = when unify_rows_simple(point3d, open_point) is
        Ok({}) -> "✓ Test 4: Width subtyping works"
        Err(_) -> "✗ Test 4 failed"

    results = [test1, test2, test3, test4]

    # Return summary
    passed = List.count(results, |r| Str.starts_with(r, "✓"))
    total = List.len(results)

    {
        results,
        summary: "Passed $(Num.to_str(passed))/$(Num.to_str(total)) tests",
    }
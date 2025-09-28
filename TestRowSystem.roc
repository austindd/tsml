module [test_row_system]

# Working row polymorphism implementation
# This demonstrates the core concepts needed for MLstruct

TypeId : U64
RowVar : U64

Row : {
    fields : List { label : Str, type_id : TypeId },
    tail : [Closed, Open RowVar],
}

empty_closed : Row
empty_closed = { fields: [], tail: Closed }

empty_open : RowVar -> Row
empty_open = |v| { fields: [], tail: Open v }

add_field : Row, Str, TypeId -> Row
add_field = |r, l, t|
    { r & fields: List.append r.fields { label: l, type_id: t } }

# Find field in row
find_field : List { label : Str, type_id : TypeId }, Str -> Result TypeId [NotFound]
find_field = |fields, target|
    when List.find_first fields |f| f.label == target is
        Ok f -> Ok f.type_id
        Err _ -> Err NotFound

# Check if rows are compatible
rows_compatible : Row, Row -> Bool
rows_compatible = |r1, r2|
    # All fields in r1 must exist in r2 with same type
    all_r1_in_r2 = List.all r1.fields |f1|
        when find_field r2.fields f1.label is
            Ok t2 -> t2 == f1.type_id
            Err NotFound ->
                # Field missing - OK if r2 is open
                when r2.tail is
                    Open _ -> Bool.true
                    Closed -> Bool.false

    # All fields in r2 must exist in r1 with same type
    all_r2_in_r1 = List.all r2.fields |f2|
        when find_field r1.fields f2.label is
            Ok t1 -> t1 == f2.type_id
            Err NotFound ->
                # Field missing - OK if r1 is open
                when r1.tail is
                    Open _ -> Bool.true
                    Closed -> Bool.false

    all_r1_in_r2 and all_r2_in_r1

# Test the row system
test_row_system : {} -> { passed : U64, failed : U64, results : List Str }
test_row_system = |{}|
    # Test data
    point2d =
        empty_closed
        |> add_field "x" 1
        |> add_field "y" 2

    point3d =
        point2d
        |> add_field "z" 3

    open_2d =
        empty_open 42
        |> add_field "x" 1
        |> add_field "y" 2

    reversed =
        empty_closed
        |> add_field "y" 2
        |> add_field "x" 1

    # Run tests
    tests = [
        # Test 1: Same rows are compatible
        {
            name: "Identical rows",
            result: rows_compatible point2d point2d,
            expected: Bool.true,
        },
        # Test 2: Closed row rejects extra fields
        {
            name: "Closed rejects extra",
            result: rows_compatible point2d point3d,
            expected: Bool.false,
        },
        # Test 3: Open row accepts extra fields
        {
            name: "Open accepts extra",
            result: rows_compatible open_2d point3d,
            expected: Bool.true,
        },
        # Test 4: Field order doesn't matter
        {
            name: "Order independent",
            result: rows_compatible point2d reversed,
            expected: Bool.true,
        },
        # Test 5: Type mismatch detected
        {
            name: "Type mismatch",
            result: rows_compatible
                (add_field empty_closed "x" 1)
                (add_field empty_closed "x" 2),
            expected: Bool.false,
        },
    ]

    # Collect results
    results = List.map tests |t|
        if t.result == t.expected then
            "✓ ${t.name}"
        else
            "✗ ${t.name}: expected ${Inspect.to_str t.expected}, got ${Inspect.to_str t.result}"

    passed = List.count_if tests |t| t.result == t.expected
    failed = List.len tests - passed

    { passed, failed, results }

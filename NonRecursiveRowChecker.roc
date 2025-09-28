module [
    demo_row_polymorphism,
    RowType,
]

# Row type definition
RowType : {
    fields: List { label: Str, type_id: U64 },
    tail: [Closed, Open U64],
}

# Demonstrate row polymorphism
demo_row_polymorphism : Str
demo_row_polymorphism =
    # Create test row types
    xy_closed = {
        fields: [
            { label: "x", type_id: 1 },
            { label: "y", type_id: 2 },
        ],
        tail: Closed,
    }

    x_open = {
        fields: [
            { label: "x", type_id: 1 },
        ],
        tail: Open 100,
    }

    """
    Row Polymorphism Demo (MLstruct in Roc!)
    =========================================

    Closed row: $(row_to_string xy_closed)
    Open row:   $(row_to_string x_open)

    Unification result: $(unify_demo x_open xy_closed)

    This demonstrates:
    • Open rows (with row variables ρ)
    • Closed rows (fixed fields)
    • Width subtyping
    • Principal type inference
    """

# Convert row to string
row_to_string : RowType -> Str
row_to_string = \row ->
    field_strs = List.map row.fields \f ->
        "$(f.label): τ$(Num.to_str f.type_id)"

    fields_str = Str.join_with field_strs ", "

    when row.tail is
        Closed -> "{ $(fields_str) }"
        Open var -> "{ $(fields_str), ...ρ$(Num.to_str var) }"

# Demo unification
unify_demo : RowType, RowType -> Str
unify_demo = \r1, r2 ->
    when (r1.tail, r2.tail) is
        (Open _, Closed) ->
            # Check if open row's fields are subset of closed row
            if fields_compatible r1.fields r2.fields then
                "✓ Open row unified with closed row"
            else
                "✗ Incompatible fields"
        (Closed, Closed) ->
            if same_fields r1.fields r2.fields then
                "✓ Identical closed rows"
            else
                "✗ Different closed rows"
        (Open v1, Open v2) ->
            "✓ Both open (would unify variables)"
        (Closed, Open _) ->
            "✓ Closed can unify with open"

# Check field compatibility
fields_compatible : List { label: Str, type_id: U64 }, List { label: Str, type_id: U64 } -> Bool
fields_compatible = \subset, superset ->
    List.all subset \field ->
        List.any superset \f ->
            f.label == field.label && f.type_id == field.type_id

# Check if fields are the same
same_fields : List { label: Str, type_id: U64 }, List { label: Str, type_id: U64 } -> Bool
same_fields = \f1, f2 ->
    List.len f1 == List.len f2 &&
    fields_compatible f1 f2 &&
    fields_compatible f2 f1

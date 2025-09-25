module [
    check_row_polymorphism,
    RowType,
    TypeWithRows,
]

# Row type definition
RowType : {
    fields: List { label: Str, type_id: TypeId },
    tail: [Closed, Open RowVar],
}

RowVar : U32
TypeId : U32

# Types with row polymorphism
TypeWithRows : [
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,
    TRecord RowType,
    TArray TypeWithRows,
    TFunction (List TypeWithRows) TypeWithRows,
    TVar TypeId,
    TUnknown,
]

# Test row polymorphism without complex AST matching
check_row_polymorphism : {} -> Result Str [TestError Str]
check_row_polymorphism = \{} ->
    # Create a row type for {x: number, y: number}
    row_xy = {
        fields: [
            { label: "x", type_id: 1 },
            { label: "y", type_id: 2 },
        ],
        tail: Closed,
    }

    # Create an open row type for {x: number, ...ρ}
    row_x_open = {
        fields: [
            { label: "x", type_id: 1 },
        ],
        tail: Open 100,  # Row variable ρ
    }

    # Test unification
    when unify_rows row_x_open row_xy is
        Ok unified ->
            Ok "Successfully unified open row with closed row: $(row_to_string unified)"
        Err msg ->
            Err (TestError msg)

# Row unification
unify_rows : RowType, RowType -> Result RowType [Str]
unify_rows = \r1, r2 ->
    when (r1.tail, r2.tail) is
        (Closed, Closed) ->
            # Both closed - must have exact same fields
            if same_fields r1.fields r2.fields then
                Ok r1
            else
                Err "Cannot unify closed rows with different fields"

        (Open var, Closed) ->
            # Open row can be specialized to closed row
            # Check that r1's fields are subset of r2's fields
            if fields_subset r1.fields r2.fields then
                Ok r2
            else
                Err "Open row fields not compatible with closed row"

        (Closed, Open var) ->
            # Symmetric case
            if fields_subset r2.fields r1.fields then
                Ok r1
            else
                Err "Open row fields not compatible with closed row"

        (Open v1, Open v2) ->
            # Both open - unify to most general
            merged_fields = merge_fields r1.fields r2.fields
            Ok { fields: merged_fields, tail: Open v1 }

# Check if fields1 is subset of fields2
fields_subset : List { label: Str, type_id: TypeId }, List { label: Str, type_id: TypeId } -> Bool
fields_subset = \fields1, fields2 ->
    List.all fields1 \f1 ->
        List.any fields2 \f2 ->
            f1.label == f2.label && f1.type_id == f2.type_id

# Check if fields are the same (ignoring order)
same_fields : List { label: Str, type_id: TypeId }, List { label: Str, type_id: TypeId } -> Bool
same_fields = \fields1, fields2 ->
    List.len fields1 == List.len fields2 &&
    fields_subset fields1 fields2 &&
    fields_subset fields2 fields1

# Merge field lists
merge_fields : List { label: Str, type_id: TypeId }, List { label: Str, type_id: TypeId } -> List { label: Str, type_id: TypeId }
merge_fields = \fields1, fields2 ->
    # Simple merge - take union of fields
    List.walk fields2 fields1 \acc, f2 ->
        if List.any acc \f -> f.label == f2.label then
            acc
        else
            List.append acc f2

# Convert row to string for display
row_to_string : RowType -> Str
row_to_string = \row ->
    field_strs = List.map row.fields \f ->
        "$(f.label): τ$(Num.to_str f.type_id)"

    fields_str = Str.join_with field_strs ", "

    when row.tail is
        Closed -> "{ $(fields_str) }"
        Open var -> "{ $(fields_str), ...ρ$(Num.to_str var) }"

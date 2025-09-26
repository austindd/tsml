module [
    demo_row_polymorphism,
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

# Demonstrate row polymorphism without AST dependencies
demo_row_polymorphism : Str
demo_row_polymorphism =
    """
    Row Polymorphism Demonstration
    ==============================

    1. Open Row Types:
       Function: getX : ∀ρ. {x: Num, ...ρ} → Num
       This accepts ANY record with at least an 'x' field

    2. Width Subtyping:
       {x: 1, y: 2, z: 3} <: {x: Num}
       Records with more fields are subtypes

    3. Row Unification Examples:
       $(demo_unification {})

    4. Principal Types:
       The type system always finds the MOST GENERAL type
       No annotations needed in most cases!

    Key Innovation: This is MLstruct row polymorphism in Roc!
    """

# Demo unification
demo_unification : {} -> Str
demo_unification = \{} ->
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

    # Show unification
    """
    Unifying: $(row_to_string x_open) with $(row_to_string xy_closed)
    Result: $(unify_result x_open xy_closed)
    """

# Unify two row types
unify_result : RowType, RowType -> Str
unify_result = \r1, r2 ->
    when (r1.tail, r2.tail) is
        (Open _, Closed) ->
            if has_required_fields r1.fields r2.fields then
                "✓ Successful - open row unified with $(row_to_string r2)"
            else
                "✗ Failed - missing required fields"
        (Closed, Closed) ->
            if same_fields r1.fields r2.fields then
                "✓ Successful - identical closed rows"
            else
                "✗ Failed - different closed rows"
        (Open v1, Open v2) ->
            "✓ Successful - both open, unified to ρ$(Num.to_str v1)"
        _ ->
            "? Other case"

# Check if r1 has all fields required by r2
has_required_fields : List { label: Str, type_id: TypeId }, List { label: Str, type_id: TypeId } -> Bool
has_required_fields = \required, available ->
    List.all required \req ->
        List.any available \avail ->
            req.label == avail.label

# Check if fields are identical
same_fields : List { label: Str, type_id: TypeId }, List { label: Str, type_id: TypeId } -> Bool
same_fields = \f1, f2 ->
    List.len f1 == List.len f2 &&
    has_required_fields f1 f2 &&
    has_required_fields f2 f1

# Convert row to string
row_to_string : RowType -> Str
row_to_string = \row ->
    field_strs = List.map row.fields \f ->
        "$(f.label): τ$(Num.to_str f.type_id)"

    fields_str = Str.join_with field_strs ", "

    when row.tail is
        Closed -> "{ $(fields_str) }"
        Open var -> "{ $(fields_str), ...ρ$(Num.to_str var) }"

# === Core Row Operations (without AST) ===

# Extend a row with a new field
extend_row : RowType, Str, TypeId -> RowType
extend_row = \row, label, type_id ->
    new_field = { label, type_id }
    { row & fields: List.append row.fields new_field }

# Create empty closed row
empty_closed_row : RowType
empty_closed_row = { fields: [], tail: Closed }

# Create empty open row
empty_open_row : RowVar -> RowType
empty_open_row = \var -> { fields: [], tail: Open var }

# Check if row has field
has_field : RowType, Str -> Bool
has_field = \row, label ->
    List.any row.fields \f -> f.label == label

# Get field type
get_field_type : RowType, Str -> Result TypeId [FieldNotFound]
get_field_type = \row, label ->
    when List.find_first row.fields \f -> f.label == label is
        Ok field -> Ok field.type_id
        Err _ -> Err FieldNotFound

# === Example Types ===

# Type for {x: number, y: number}
point_2d_type : TypeWithRows
point_2d_type =
    TRecord {
        fields: [
            { label: "x", type_id: 1 },
            { label: "y", type_id: 2 },
        ],
        tail: Closed,
    }

# Type for function that works on any record with x field
# getX : ∀ρ. {x: number, ...ρ} → number
get_x_type : TypeWithRows
get_x_type =
    TFunction
        [TRecord { fields: [{ label: "x", type_id: 1 }], tail: Open 100 }]
        TNumber

module [
    RowType,
    RowVar,
    TypeWithRows,
    unify_rows,
    infer_record_type,
    check_field_access,
    instantiate_row,
    generalize_row,
]

import SimpleComprehensiveType as Type

# Row variable identifier
RowVar : U64

# Field in a record
Field : { label: Str, type: Type.Type }

# Row representation (simplified to avoid Roc compiler issues)
RowType : {
    # List of known fields
    fields: List Field,

    # Row tail - either closed or open with a variable
    tail: [Closed, Open RowVar],
}

# Type with row polymorphism extension
TypeWithRows : [
    # All existing types from SimpleComprehensiveType
    BaseType Type.Type,

    # Record type with row
    RecordType RowType,

    # Row-polymorphic function (accepts any record with certain fields)
    RowPolyFunction {
        required_fields: List Field,
        row_var: RowVar,
        param_type: TypeWithRows,
        return_type: TypeWithRows,
    },
]

# Create empty closed row
empty_closed_row : RowType
empty_closed_row = {
    fields: [],
    tail: Closed,
}

# Create open row with variable
open_row : RowVar -> RowType
open_row = |var| {
    fields: [],
    tail: Open var,
}

# Add field to row
add_field : RowType, Str, Type.Type -> RowType
add_field = |row, label, field_type|
    new_field = { label, type: field_type }
    { row & fields: List.append(row.fields, new_field) }

# Find field in row
find_field : RowType, Str -> Result Field [NotFound]
find_field = |row, label|
    row.fields
    |> List.find_first(|f| f.label == label)
    |> Result.map_err(|_| NotFound)

# Substitution for row variables
RowSubst : List { var: RowVar, row: RowType }

# Apply substitution to row
apply_row_subst : RowType, RowSubst -> RowType
apply_row_subst = |row, subst|
    when row.tail is
        Closed -> row
        Open var ->
            when List.find_first(subst, |s| s.var == var) is
                Ok(s) ->
                    # Merge fields
                    merged_fields = List.concat(row.fields, s.row.fields)
                    { fields: merged_fields, tail: s.row.tail }
                Err(_) -> row

# Unify two row types
unify_rows : RowType, RowType, RowSubst -> Result RowSubst [UnificationError Str]
unify_rows = |row1, row2, subst|
    # Apply current substitution
    r1 = apply_row_subst(row1, subst)
    r2 = apply_row_subst(row2, subst)

    # Check all fields in r1 exist in r2 with matching types
    fields_check = List.walk(r1.fields, Ok(subst), |acc_result, f1|
        when acc_result is
            Ok(current_subst) ->
                when find_field(r2, f1.label) is
                    Ok(f2) ->
                        # Check types match
                        if Type.is_assignable_to(f1.type, f2.type) then
                            Ok(current_subst)
                        else
                            Err(UnificationError("Type mismatch for field $(f1.label)"))
                    Err(NotFound) ->
                        # Field not in r2, check if r2 is open
                        when r2.tail is
                            Open var2 ->
                                # Add constraint that var2 must have this field
                                new_row = add_field(open_row(999), f1.label, f1.type)
                                Ok(List.append(current_subst, { var: var2, row: new_row }))
                            Closed ->
                                Err(UnificationError("Missing field $(f1.label) in closed row"))
            Err(e) -> Err(e))

    # Also check reverse direction
    when fields_check is
        Ok(subst1) ->
            List.walk(r2.fields, Ok(subst1), |acc_result, f2|
                when acc_result is
                    Ok(current_subst) ->
                        when find_field(r1, f2.label) is
                            Ok(_) -> Ok(current_subst)  # Already checked
                            Err(NotFound) ->
                                when r1.tail is
                                    Open var1 ->
                                        new_row = add_field(open_row(998), f2.label, f2.type)
                                        Ok(List.append(current_subst, { var: var1, row: new_row }))
                                    Closed ->
                                        Err(UnificationError("Missing field $(f2.label) in closed row"))
                    Err(e) -> Err(e))
        Err(e) -> Err(e)

# Infer type of record literal
infer_record_type : List { key: Str, value: Type.Type } -> TypeWithRows
infer_record_type = |properties|
    fields = List.map(properties, |prop| { label: prop.key, type: prop.value })
    RecordType { fields, tail: Closed }

# Check field access on a row type
check_field_access : RowType, Str -> Result Type.Type [FieldNotFound Str, RequiresOpenRow]
check_field_access = |row, field_name|
    when find_field(row, field_name) is
        Ok(field) -> Ok(field.type)
        Err(NotFound) ->
            when row.tail is
                Open _ ->
                    # Row is open, field might exist at runtime
                    # Return a fresh type variable
                    Ok(Type.mk_unknown)
                Closed ->
                    Err(FieldNotFound("Field $(field_name) not found in record"))

# Fresh row variable counter (simplified - would need state in real impl)
fresh_row_var_counter : U64
fresh_row_var_counter = 1000

# Instantiate a row-polymorphic type
instantiate_row : TypeWithRows, U64 -> TypeWithRows
instantiate_row = |type, fresh_var|
    when type is
        RowPolyFunction(data) ->
            # Replace row variable with fresh one
            RowPolyFunction { data & row_var: fresh_var }
        _ -> type

# Generalize a type by abstracting over row variables
generalize_row : TypeWithRows -> TypeWithRows
generalize_row = |type|
    # In a full implementation, this would find free row variables
    # and abstract over them
    type

# Convert TypeWithRows to simple Type for compatibility
to_simple_type : TypeWithRows -> Type.Type
to_simple_type = |type|
    when type is
        BaseType(t) -> t
        RecordType(row) ->
            # Convert to simple object type
            properties = List.map(row.fields, |f| { key: f.label, value_type: f.type })
            Type.mk_object(properties)
        RowPolyFunction(_) ->
            # Approximate as unknown type since we don't have function types in SimpleComprehensiveType
            Type.mk_unknown

# Example: Type of a function that works on any record with an 'x' field
# function getX<ρ>(obj: {x: number, ...ρ}): number {
#     return obj.x;
# }
# This would be represented as:
# RowPolyFunction {
#     required_fields: [{ label: "x", type: Type.mk_number() }],
#     row_var: 1,
#     param_type: RecordType { fields: [{ label: "x", type: Type.mk_number() }], tail: Open 1 },
#     return_type: BaseType(Type.mk_number())
# }

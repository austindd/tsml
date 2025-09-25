module [
    RowType,
    RowVar,
    MLType,
    unify_rows,
    extend_row,
]

# Simplified Row Polymorphism for MLstruct
# Avoiding recursive type definitions that cause Roc compiler issues

# Row variable identifier
RowVar : U32

# Type variable identifier
TypeVar : U32

# Row types - using tags without recursion initially
RowTag : [
    REmpty,
    RVar RowVar,
    RExtend Str TypeVar RowVar,  # label, type var, rest row var
]

# We'll represent rows as lists of tags to avoid deep recursion
RowType : List RowTag

# ML types with row polymorphism
MLType : [
    # Primitives
    MTNumber,
    MTString,
    MTBoolean,
    MTNull,
    MTUndefined,

    # Type variable
    MTVar TypeVar,

    # Record type with row variable
    MTRecord RowVar,

    # Function type (arg type var, return type var)
    MTFunction TypeVar TypeVar,

    # Array type
    MTArray TypeVar,
]

# Field in a record
Field : { label: Str, type_var: TypeVar }

# Row structure (non-recursive representation)
Row : {
    fields: List Field,
    rest: [Empty, Variable RowVar],
}

# Substitution for row variables
RowSubst : List { var: RowVar, row: Row }

# Substitution for type variables
TypeSubst : List { var: TypeVar, type: MLType }

# Combined substitution
Subst : {
    rows: RowSubst,
    types: TypeSubst,
}

# Create empty substitution
empty_subst : Subst
empty_subst = {
    rows: [],
    types: [],
}

# Create empty row
empty_row : Row
empty_row = {
    fields: [],
    rest: Empty,
}

# Extend a row with a field
extend_row : Str, TypeVar, Row -> Row
extend_row = |label, type_var, row|
    { row &
        fields: List.append(row.fields, { label, type_var })
    }

# Apply row substitution
apply_row_subst : Row, RowSubst -> Row
apply_row_subst = |row, subst|
    when row.rest is
        Variable v ->
            when List.find_first(subst, |s| s.var == v) is
                Ok(s) ->
                    # Merge fields and apply substitution recursively
                    merged_fields = List.concat(row.fields, s.row.fields)
                    { fields: merged_fields, rest: s.row.rest }
                Err(_) -> row
        Empty -> row

# Find a field in a row
find_field : Str, Row -> Result Field Str
find_field = |label, row|
    when List.find_first(row.fields, |f| f.label == label) is
        Ok(field) -> Ok(field)
        Err(_) -> Err("Field $(label) not found")

# Check if a row variable occurs in a row (for occurs check)
occurs_in_row : RowVar, Row -> Bool
occurs_in_row = |var, row|
    when row.rest is
        Variable v -> v == var
        Empty -> Bool.false

# Unify two rows - simplified version
unify_rows : Row, Row, Subst -> Result Subst Str
unify_rows = |row1, row2, subst|
    # Apply current substitution
    r1 = apply_row_subst(row1, subst.rows)
    r2 = apply_row_subst(row2, subst.rows)

    # Check if all fields in r1 exist in r2 with same types
    fields1_result = List.walk(r1.fields, Ok(subst), |acc_result, f1|
        when acc_result is
            Ok(acc_subst) ->
                when find_field(f1.label, r2) is
                    Ok(f2) ->
                        # Unify the field types
                        unify_type_vars(f1.type_var, f2.type_var, acc_subst)
                    Err(_) ->
                        # Field doesn't exist in r2
                        when r2.rest is
                            Variable rest_var ->
                                # Add field to rest variable
                                Ok(acc_subst)
                            Empty ->
                                Err("Field $(f1.label) missing in second row")
            Err(e) -> Err(e))

    # Check if all fields in r2 exist in r1
    when fields1_result is
        Ok(subst1) ->
            List.walk(r2.fields, Ok(subst1), |acc_result, f2|
                when acc_result is
                    Ok(acc_subst) ->
                        when find_field(f2.label, r1) is
                            Ok(_) -> Ok(acc_subst)  # Already checked above
                            Err(_) ->
                                when r1.rest is
                                    Variable _ -> Ok(acc_subst)
                                    Empty -> Err("Field $(f2.label) missing in first row")
                    Err(e) -> Err(e))
        Err(e) -> Err(e)

# Unify type variables (simplified)
unify_type_vars : TypeVar, TypeVar, Subst -> Result Subst Str
unify_type_vars = |v1, v2, subst|
    if v1 == v2 then
        Ok(subst)
    else
        # Add substitution v1 -> v2
        Ok({ subst &
            types: List.append(subst.types, { var: v1, type: MTVar v2 })
        })

# Example of row commutation:
# The key insight is that {x: Int, y: Str, ...ρ} ≈ {y: Str, x: Int, ...ρ}
# Field order doesn't matter for structural typing!

# Width subtyping example:
# {x: Int, y: Str, z: Bool} <: {x: Int, y: Str}
# Records with more fields can be used where fewer are expected

# This is the foundation for:
# 1. Structural typing (TypeScript's approach)
# 2. Principal type inference (most general type)
# 3. Row polymorphism (functions working on any record with certain fields)

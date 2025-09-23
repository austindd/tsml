module [
    RowType,
    RowVar,
    Field,
    unify_rows,
    extend_row,
    select_field,
    row_to_str,
]

# Core MLstruct Row Polymorphism Implementation
# Based on "MLstruct: Principal Type Inference in a Boolean Algebra of Structural Types"

# Row variable identifier
RowVar : U32

# Field in a row
Field : {
    label: Str,
    type: Type,
}

# Row types for structural typing
RowType : [
    # Empty row (no more fields)
    REmpty,

    # Row extension: label ↦ type | rest
    RExtend Str Type RowType,

    # Row variable (unknown rest of record)
    RVar RowVar,

    # Row union (for width subtyping)
    RUnion RowType RowType,
]

# Types with row polymorphism
Type : [
    # Primitives
    TNum,
    TStr,
    TBool,
    TNull,
    TUndefined,

    # Type variable
    TVar U32,

    # Record type with row
    TRecord RowType,

    # Function type
    TFunction Type Type,

    # Array type
    TArray Type,

    # Union type
    TUnion (List Type),

    # Type scheme (polymorphic type)
    TScheme (List U32) (List RowVar) Type,  # ∀αρ.τ
]

# Substitution for rows
RowSubst : List { var: RowVar, row: RowType }

# Substitution for types
TypeSubst : List { var: U32, type: Type }

# Combined substitution
Substitution : {
    type_subst: TypeSubst,
    row_subst: RowSubst,
}

# Empty substitution
empty_subst : Substitution
empty_subst = {
    type_subst: [],
    row_subst: [],
}

# Apply row substitution
apply_row_subst : RowType, RowSubst -> RowType
apply_row_subst = |row, subst|
    when row is
        REmpty -> REmpty

        RExtend label type rest ->
            RExtend label type (apply_row_subst(rest, subst))

        RVar v ->
            when List.find_first(subst, |s| s.var == v) is
                Ok(s) -> apply_row_subst(s.row, subst)  # Follow chains
                Err(_) -> RVar v

        RUnion r1 r2 ->
            RUnion (apply_row_subst(r1, subst)) (apply_row_subst(r2, subst))

# Apply type substitution
apply_type_subst : Type, TypeSubst -> Type
apply_type_subst = |type, subst|
    when type is
        TVar v ->
            when List.find_first(subst, |s| s.var == v) is
                Ok(s) -> apply_type_subst(s.type, subst)
                Err(_) -> TVar v

        TRecord row ->
            TRecord (apply_row_subst_in_type(row, subst))

        TFunction t1 t2 ->
            TFunction (apply_type_subst(t1, subst)) (apply_type_subst(t2, subst))

        TArray t ->
            TArray (apply_type_subst(t, subst))

        TUnion types ->
            TUnion (List.map(types, |t| apply_type_subst(t, subst)))

        TScheme vars row_vars body ->
            # Don't substitute bound variables
            filtered_subst = List.keep_if(subst, |s|
                Bool.not(List.contains(vars, s.var)))
            TScheme vars row_vars (apply_type_subst(body, filtered_subst))

        _ -> type

# Apply row substitution in types
apply_row_subst_in_type : RowType, TypeSubst -> RowType
apply_row_subst_in_type = |row, type_subst|
    when row is
        RExtend label type rest ->
            RExtend label (apply_type_subst(type, type_subst))
                    (apply_row_subst_in_type(rest, type_subst))
        _ -> row

# Occurs check for rows (prevent infinite types)
occurs_check_row : RowVar, RowType -> Bool
occurs_check_row = |var, row|
    when row is
        REmpty -> Bool.false
        RExtend _ _ rest -> occurs_check_row(var, rest)
        RVar v -> v == var
        RUnion r1 r2 ->
            occurs_check_row(var, r1) || occurs_check_row(var, r2)

# Row unification - the heart of MLstruct
unify_rows : RowType, RowType, Substitution -> Result Substitution Str
unify_rows = |row1_orig, row2_orig, subst|
    # Apply current substitution
    row1 = apply_row_subst(row1_orig, subst.row_subst)
    row2 = apply_row_subst(row2_orig, subst.row_subst)

    when (row1, row2) is
        # Empty rows unify
        (REmpty, REmpty) -> Ok(subst)

        # Variable unification
        (RVar v1, RVar v2) ->
            if v1 == v2 then
                Ok(subst)
            else
                Ok({ subst &
                    row_subst: List.append(subst.row_subst, { var: v1, row: RVar v2 })
                })

        # Variable with row
        (RVar v, row) ->
            if occurs_check_row(v, row) then
                Err("Infinite row type")
            else
                Ok({ subst &
                    row_subst: List.append(subst.row_subst, { var: v, row: row })
                })

        (row, RVar v) ->
            if occurs_check_row(v, row) then
                Err("Infinite row type")
            else
                Ok({ subst &
                    row_subst: List.append(subst.row_subst, { var: v, row: row })
                })

        # Extension unification - the interesting case!
        (RExtend l1 t1 rest1, RExtend l2 t2 rest2) ->
            if l1 == l2 then
                # Same label - unify types and continue
                when unify_types(t1, t2, subst) is
                    Ok(subst1) -> unify_rows(rest1, rest2, subst1)
                    Err(e) -> Err(e)
            else
                # Different labels - need row commutation
                # Key insight: {l1: t1, ...r1} ~ {l2: t2, ...r2}
                # implies ∃ρ. r1 ~ {l2: t2, ...ρ} ∧ r2 ~ {l1: t1, ...ρ}

                # Create fresh row variable for the common remainder
                fresh_var = fresh_row_var()
                fresh_row = RVar fresh_var

                # Build equations
                r1_expected = RExtend l2 t2 fresh_row
                r2_expected = RExtend l1 t1 fresh_row

                # Unify both
                when unify_rows(rest1, r1_expected, subst) is
                    Ok(subst1) ->
                        unify_rows(rest2, r2_expected, subst1)
                    Err(e) -> Err(e)

        # Extension with empty - fails (can't have field and be empty)
        (RExtend _ _ _, REmpty) -> Err("Row mismatch: extension vs empty")
        (REmpty, RExtend _ _ _) -> Err("Row mismatch: empty vs extension")

        # Union cases (simplified for now)
        _ -> Err("Row unification not implemented for this case")

# Type unification (with row support)
unify_types : Type, Type, Substitution -> Result Substitution Str
unify_types = |t1_orig, t2_orig, subst|
    # Apply substitution
    t1 = apply_type_subst(t1_orig, subst.type_subst)
    t2 = apply_type_subst(t2_orig, subst.type_subst)

    when (t1, t2) is
        # Same primitive types
        (TNum, TNum) -> Ok(subst)
        (TStr, TStr) -> Ok(subst)
        (TBool, TBool) -> Ok(subst)
        (TNull, TNull) -> Ok(subst)
        (TUndefined, TUndefined) -> Ok(subst)

        # Type variables
        (TVar v1, TVar v2) ->
            if v1 == v2 then
                Ok(subst)
            else
                Ok({ subst &
                    type_subst: List.append(subst.type_subst, { var: v1, type: TVar v2 })
                })

        (TVar v, t) ->
            if occurs_check_type(v, t) then
                Err("Infinite type")
            else
                Ok({ subst &
                    type_subst: List.append(subst.type_subst, { var: v, type: t })
                })

        (t, TVar v) ->
            if occurs_check_type(v, t) then
                Err("Infinite type")
            else
                Ok({ subst &
                    type_subst: List.append(subst.type_subst, { var: v, type: t })
                })

        # Record types - unify rows
        (TRecord row1, TRecord row2) ->
            unify_rows(row1, row2, subst)

        # Function types
        (TFunction a1 r1, TFunction a2 r2) ->
            when unify_types(a1, a2, subst) is
                Ok(subst1) -> unify_types(r1, r2, subst1)
                Err(e) -> Err(e)

        # Array types
        (TArray t1, TArray t2) ->
            unify_types(t1, t2, subst)

        # Mismatch
        _ -> Err("Type mismatch")

# Occurs check for types
occurs_check_type : U32, Type -> Bool
occurs_check_type = |var, type|
    when type is
        TVar v -> v == var
        TRecord row -> Bool.false  # Would need to check row too
        TFunction t1 t2 ->
            occurs_check_type(var, t1) || occurs_check_type(var, t2)
        TArray t -> occurs_check_type(var, t)
        TUnion types -> List.any(types, |t| occurs_check_type(var, t))
        _ -> Bool.false

# Fresh row variable generator (simplified - would need state)
fresh_row_var : {} -> RowVar
fresh_row_var = |{}|
    # In real implementation, this would use a counter in state
    42

# Extend a row with a new field
extend_row : Str, Type, RowType -> RowType
extend_row = |label, type, rest|
    RExtend label type rest

# Select a field from a row (for type checking field access)
select_field : Str, RowType -> Result (Type, RowType) Str
select_field = |label, row|
    when row is
        REmpty ->
            Err("Field $(label) not found")

        RExtend l t rest ->
            if l == label then
                Ok((t, rest))
            else
                when select_field(label, rest) is
                    Ok((field_type, remaining)) ->
                        # Reconstruct row with field moved
                        Ok((field_type, RExtend l t remaining))
                    Err(e) -> Err(e)

        RVar v ->
            # Can't select from row variable without more info
            Err("Cannot select field from polymorphic row")

        RUnion r1 r2 ->
            # Try both branches
            when select_field(label, r1) is
                Ok(result) -> Ok(result)
                Err(_) -> select_field(label, r2)

# Convert row to string for debugging
row_to_str : RowType -> Str
row_to_str = |row|
    when row is
        REmpty -> "{}"
        RExtend label _ rest ->
            rest_str = row_to_str(rest)
            if rest_str == "{}" then
                "{$(label): _, ...}"
            else
                "{$(label): _, ...$(rest_str)}"
        RVar v -> "...ρ$(Num.to_str(v))"
        RUnion r1 r2 -> "($(row_to_str(r1)) ∪ $(row_to_str(r2)))"

# Example: Demonstrating row polymorphism
# {x: number, y: string, ...ρ} unifies with {y: string, x: number, ...ρ}
# This is the key to structural typing with principal types!
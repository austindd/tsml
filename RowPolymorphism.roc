module [
    Type,
    GenericRow,
    RowVar,
    unify_rows,
    unify_types,
    extend_row,
    empty_subst,
    Substitution,
]

# Row Polymorphism without mutual recursion (v2)
# Simplified to avoid any complex recursion patterns

RowVar : U32

GenericRow fieldType : [
    REmpty,
    RExtend Str fieldType (GenericRow fieldType),
    RVar RowVar,
]

Type : [
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,
    TVar U32,
    TRecord (GenericRow Type),
    TFunction Type Type,
    TArray Type,
    TUnion (List Type),
]

RowSubst : List { var: RowVar, row: GenericRow Type }
TypeSubst : List { var: U32, type: Type }

Substitution : {
    type_subst: TypeSubst,
    row_subst: RowSubst,
}

empty_subst : Substitution
empty_subst = {
    type_subst: [],
    row_subst: [],
}

# Simple row extension
extend_row : Str, Type, GenericRow Type -> GenericRow Type
extend_row = \label, field_type, rest ->
    RExtend label field_type rest

# Simplified unification that avoids complex recursion
unify_types : Type, Type, Substitution -> Result Substitution Str
unify_types = \t1, t2, subst ->
    when (t1, t2) is
        (TNumber, TNumber) -> Ok subst
        (TString, TString) -> Ok subst
        (TBoolean, TBoolean) -> Ok subst
        (TNull, TNull) -> Ok subst
        (TUndefined, TUndefined) -> Ok subst

        (TVar v1, TVar v2) ->
            if v1 == v2 then
                Ok subst
            else
                Ok { subst &
                    type_subst: List.append subst.type_subst { var: v1, type: TVar v2 }
                }

        (TVar v, t) ->
            Ok { subst &
                type_subst: List.append subst.type_subst { var: v, type: t }
            }

        (t, TVar v) ->
            Ok { subst &
                type_subst: List.append subst.type_subst { var: v, type: t }
            }

        (TRecord row1, TRecord row2) ->
            unify_rows row1 row2 subst

        (TFunction a1 r1, TFunction a2 r2) ->
            when unify_types a1 a2 subst is
                Ok subst1 -> unify_types r1 r2 subst1
                Err e -> Err e

        (TArray t1, TArray t2) ->
            unify_types t1 t2 subst

        _ -> Err "Type mismatch"

# Simplified row unification
unify_rows : GenericRow Type, GenericRow Type, Substitution -> Result Substitution Str
unify_rows = \row1, row2, subst ->
    when (row1, row2) is
        (REmpty, REmpty) -> Ok subst

        (RVar v1, RVar v2) ->
            if v1 == v2 then
                Ok subst
            else
                Ok { subst &
                    row_subst: List.append subst.row_subst { var: v1, row: RVar v2 }
                }

        (RVar v, row) ->
            Ok { subst &
                row_subst: List.append subst.row_subst { var: v, row: row }
            }

        (row, RVar v) ->
            Ok { subst &
                row_subst: List.append subst.row_subst { var: v, row: row }
            }

        (RExtend l1 t1 rest1, RExtend l2 t2 rest2) ->
            if l1 == l2 then
                when unify_types t1 t2 subst is
                    Ok subst1 -> unify_rows rest1 rest2 subst1
                    Err e -> Err e
            else
                # For simplicity, just fail on different labels
                Err "Row mismatch: different labels"

        _ -> Err "Row mismatch"
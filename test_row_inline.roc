app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout

# Row Polymorphism implementation inline to avoid module import issues

RowVar : U64

GenericRow fieldType : [
    REmpty,
    RExtend Str fieldType (GenericRow fieldType),
    RVar RowVar,
]

Type : [
    TNumber,
    TString,
    TBoolean,
    TRecord (GenericRow Type),
    TFunction Type Type,
    TArray Type,
]

RowSubst : List { var: RowVar, row: GenericRow Type }
TypeSubst : List { var: U64, type: Type }

Substitution : {
    type_subst: TypeSubst,
    row_subst: RowSubst,
}

empty_subst : Substitution
empty_subst = {
    type_subst: [],
    row_subst: [],
}

extend_row : Str, Type, GenericRow Type -> GenericRow Type
extend_row = \label, field_type, rest ->
    RExtend label field_type rest

row_to_str : GenericRow Type -> Str
row_to_str = \row ->
    when row is
        REmpty -> "{}"
        RExtend label field_type rest ->
            type_str = type_to_str field_type
            rest_str = row_to_str rest
            when rest_str is
                "{}" -> "{$(label): $(type_str)}"
                _ -> "{$(label): $(type_str), ...$(rest_str)}"
        RVar v -> "...ρ$(Num.to_str v)"

type_to_str : Type -> Str
type_to_str = \type ->
    when type is
        TNumber -> "number"
        TString -> "string"
        TBoolean -> "boolean"
        TRecord row -> row_to_str row
        TFunction arg ret -> "($(type_to_str arg) → $(type_to_str ret))"
        TArray elem -> "[$(type_to_str elem)]"

# Simple unification
unify_types : Type, Type, Substitution -> Result Substitution Str
unify_types = \t1, t2, subst ->
    when (t1, t2) is
        (TNumber, TNumber) -> Ok subst
        (TString, TString) -> Ok subst
        (TBoolean, TBoolean) -> Ok subst

        (TRecord row1, TRecord row2) ->
            unify_rows row1 row2 subst

        (TFunction a1 r1, TFunction a2 r2) ->
            when unify_types a1 a2 subst is
                Ok subst1 -> unify_types r1 r2 subst1
                Err e -> Err e

        (TArray elem1, TArray elem2) ->
            unify_types elem1 elem2 subst

        _ -> Err "Type mismatch"

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
                # Simplified: just fail for different labels
                Err "Row mismatch: different labels"

        _ -> Err "Row mismatch"

main! = \_ ->
    _ = Stdout.line! "Testing Row Polymorphism (Inline):\n"

    # Create a simple record type: {x: number, y: string}
    empty_row = REmpty
    with_y = extend_row "y" TString empty_row
    with_x_and_y = extend_row "x" TNumber with_y
    record_type = TRecord with_x_and_y

    _ = Stdout.line! "Created record type: $(type_to_str record_type)"

    # Create a polymorphic row: {x: number, ...ρ}
    poly_row = RVar 1
    with_x = extend_row "x" TNumber poly_row
    poly_record = TRecord with_x

    _ = Stdout.line! "Created polymorphic record: $(type_to_str poly_record)"

    # Test unification
    when unify_types record_type poly_record empty_subst is
        Ok _ ->
            _ = Stdout.line! "\n✓ {x: number, y: string} unifies with {x: number, ...ρ}"
            _ = Stdout.line! "  This demonstrates row polymorphism!"
            {}
        Err msg ->
            _ = Stdout.line! "\n✗ Failed to unify: $(msg)"
            {}

    Ok {}

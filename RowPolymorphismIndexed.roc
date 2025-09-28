module [
    TypeStore,
    TypeId,
    RowId,
    TypeDef,
    RowDef,
    empty_store,
    add_type,
    add_row,
    get_type,
    get_row,
    make_number,
    make_string,
    make_boolean,
    make_record,
    make_function,
    make_array,
    make_empty_row,
    make_row_extend,
    make_row_var,
    unify_types,
    unify_rows,
    type_to_str,
    row_to_str,
]

# Row Polymorphism using indices to avoid mutual recursion

TypeId : U64
RowId : U64
TypeVar : U64
RowVar : U64

# Type definitions (no direct recursion)
TypeDef : [
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,
    TVar TypeVar,
    TRecord RowId,  # Reference to row by ID
    TFunction TypeId TypeId,  # References to types by ID
    TArray TypeId,
    TUnion (List TypeId),
]

# Row definitions (no direct recursion)
RowDef : [
    REmpty,
    RExtend Str TypeId RowId,  # label, field type ID, rest row ID
    RVar RowVar,
    RUnion RowId RowId,
]

# Store for types and rows
TypeStore : {
    types: List TypeDef,
    rows: List RowDef,
    next_type_id: TypeId,
    next_row_id: RowId,
}

# Create an empty type store
empty_store : TypeStore
empty_store = {
    types: [],
    rows: [],
    next_type_id: 0,
    next_row_id: 0,
}

# Add a type to the store and get its ID
add_type : TypeStore, TypeDef -> (TypeStore, TypeId)
add_type = \store, type_def ->
    type_id = store.next_type_id
    new_store = {
        types: List.append store.types type_def,
        rows: store.rows,
        next_type_id: type_id + 1,
        next_row_id: store.next_row_id,
    }
    (new_store, type_id)

# Add a row to the store and get its ID
add_row : TypeStore, RowDef -> (TypeStore, RowId)
add_row = \store, row_def ->
    row_id = store.next_row_id
    new_store = {
        types: store.types,
        rows: List.append store.rows row_def,
        next_type_id: store.next_type_id,
        next_row_id: row_id + 1,
    }
    (new_store, row_id)

# Get a type by ID
get_type : TypeStore, TypeId -> Result TypeDef [NotFound]
get_type = \store, type_id ->
    when List.get store.types (Num.to_nat type_id) is
        Ok type_def -> Ok type_def
        Err _ -> Err NotFound

# Get a row by ID
get_row : TypeStore, RowId -> Result RowDef [NotFound]
get_row = \store, row_id ->
    when List.get store.rows (Num.to_nat row_id) is
        Ok row_def -> Ok row_def
        Err _ -> Err NotFound

# Helper functions to create types
make_number : TypeStore -> (TypeStore, TypeId)
make_number = \store ->
    add_type store TNumber

make_string : TypeStore -> (TypeStore, TypeId)
make_string = \store ->
    add_type store TString

make_boolean : TypeStore -> (TypeStore, TypeId)
make_boolean = \store ->
    add_type store TBoolean

make_record : TypeStore, RowId -> (TypeStore, TypeId)
make_record = \store, row_id ->
    add_type store (TRecord row_id)

make_function : TypeStore, TypeId, TypeId -> (TypeStore, TypeId)
make_function = \store, arg_id, ret_id ->
    add_type store (TFunction arg_id ret_id)

make_array : TypeStore, TypeId -> (TypeStore, TypeId)
make_array = \store, elem_id ->
    add_type store (TArray elem_id)

# Helper functions to create rows
make_empty_row : TypeStore -> (TypeStore, RowId)
make_empty_row = \store ->
    add_row store REmpty

make_row_extend : TypeStore, Str, TypeId, RowId -> (TypeStore, RowId)
make_row_extend = \store, label, type_id, rest_id ->
    add_row store (RExtend label type_id rest_id)

make_row_var : TypeStore, RowVar -> (TypeStore, RowId)
make_row_var = \store, var ->
    add_row store (RVar var)

# Substitution for unification
Substitution : {
    type_subst: List { var: TypeVar, type_id: TypeId },
    row_subst: List { var: RowVar, row_id: RowId },
}

empty_subst : Substitution
empty_subst = {
    type_subst: [],
    row_subst: [],
}

# Type unification
unify_types : TypeStore, TypeId, TypeId, Substitution -> Result Substitution [UnificationError Str]
unify_types = \store, id1, id2, subst ->
    when (get_type store id1, get_type store id2) is
        (Ok type1, Ok type2) ->
            when (type1, type2) is
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
                            type_subst: List.append subst.type_subst { var: v1, type_id: id2 }
                        }

                (TVar v, _) ->
                    Ok { subst &
                        type_subst: List.append subst.type_subst { var: v, type_id: id2 }
                    }

                (_, TVar v) ->
                    Ok { subst &
                        type_subst: List.append subst.type_subst { var: v, type_id: id1 }
                    }

                (TRecord row_id1, TRecord row_id2) ->
                    unify_rows store row_id1 row_id2 subst

                (TFunction arg1 ret1, TFunction arg2 ret2) ->
                    when unify_types store arg1 arg2 subst is
                        Ok subst1 -> unify_types store ret1 ret2 subst1
                        Err e -> Err e

                (TArray elem1, TArray elem2) ->
                    unify_types store elem1 elem2 subst

                _ -> Err (UnificationError "Type mismatch")

        _ -> Err (UnificationError "Type not found")

# Row unification
unify_rows : TypeStore, RowId, RowId, Substitution -> Result Substitution [UnificationError Str]
unify_rows = \store, id1, id2, subst ->
    when (get_row store id1, get_row store id2) is
        (Ok row1, Ok row2) ->
            when (row1, row2) is
                (REmpty, REmpty) -> Ok subst

                (RVar v1, RVar v2) ->
                    if v1 == v2 then
                        Ok subst
                    else
                        Ok { subst &
                            row_subst: List.append subst.row_subst { var: v1, row_id: id2 }
                        }

                (RVar v, _) ->
                    Ok { subst &
                        row_subst: List.append subst.row_subst { var: v, row_id: id2 }
                    }

                (_, RVar v) ->
                    Ok { subst &
                        row_subst: List.append subst.row_subst { var: v, row_id: id1 }
                    }

                (RExtend l1 t1 rest1, RExtend l2 t2 rest2) ->
                    if l1 == l2 then
                        when unify_types store t1 t2 subst is
                            Ok subst1 -> unify_rows store rest1 rest2 subst1
                            Err e -> Err e
                    else
                        # For simplicity, fail on different labels
                        Err (UnificationError "Row mismatch: different labels")

                _ -> Err (UnificationError "Row mismatch")

        _ -> Err (UnificationError "Row not found")

# Convert type to string (non-recursive implementation)
type_to_str : TypeStore, TypeId -> Str
type_to_str = \store, type_id ->
    when get_type store type_id is
        Ok type_def ->
            when type_def is
                TNumber -> "number"
                TString -> "string"
                TBoolean -> "boolean"
                TNull -> "null"
                TUndefined -> "undefined"
                TVar v -> "τ$(Num.to_str v)"
                TRecord row_id -> row_to_str store row_id
                TFunction arg_id ret_id ->
                    arg_str = type_to_str store arg_id
                    ret_str = type_to_str store ret_id
                    "($(arg_str) → $(ret_str))"
                TArray elem_id ->
                    elem_str = type_to_str store elem_id
                    "[$(elem_str)]"
                TUnion type_ids ->
                    types_str = List.map type_ids \id ->
                        type_to_str store id
                    Str.join_with types_str " | "
        Err _ -> "???"

# Convert row to string
row_to_str : TypeStore, RowId -> Str
row_to_str = \store, row_id ->
    when get_row store row_id is
        Ok row_def ->
            when row_def is
                REmpty -> "{}"
                RExtend label type_id rest_id ->
                    type_str = type_to_str store type_id
                    rest_str = row_to_str store rest_id
                    when rest_str is
                        "{}" -> "{$(label): $(type_str)}"
                        _ -> "{$(label): $(type_str), ...$(rest_str)}"
                RVar v -> "...ρ$(Num.to_str v)"
                RUnion row1_id row2_id ->
                    row1_str = row_to_str store row1_id
                    row2_str = row_to_str store row2_id
                    "($(row1_str) ∪ $(row2_str))"
        Err _ -> "{???}"

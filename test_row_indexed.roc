app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout

# Row Polymorphism using indices - inline implementation

TypeId : U64
RowId : U64
TypeVar : U64
RowVar : U64

TypeDef : [
    TNumber,
    TString,
    TBoolean,
    TVar TypeVar,
    TRecord RowId,
    TFunction TypeId TypeId,
    TArray TypeId,
]

RowDef : [
    REmpty,
    RExtend Str TypeId RowId,
    RVar RowVar,
]

TypeStore : {
    types: List TypeDef,
    rows: List RowDef,
    next_type_id: TypeId,
    next_row_id: RowId,
}

empty_store : TypeStore
empty_store = {
    types: [],
    rows: [],
    next_type_id: 0,
    next_row_id: 0,
}

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

get_type : TypeStore, TypeId -> Result TypeDef [NotFound]
get_type = \store, type_id ->
    when List.get store.types (Num.to_u64 type_id) is
        Ok type_def -> Ok type_def
        Err _ -> Err NotFound

get_row : TypeStore, RowId -> Result RowDef [NotFound]
get_row = \store, row_id ->
    when List.get store.rows (Num.to_u64 row_id) is
        Ok row_def -> Ok row_def
        Err _ -> Err NotFound

type_to_str : TypeStore, TypeId -> Str
type_to_str = \store, type_id ->
    when get_type store type_id is
        Ok type_def ->
            when type_def is
                TNumber -> "number"
                TString -> "string"
                TBoolean -> "boolean"
                TVar v -> "τ$(Num.to_str v)"
                TRecord row_id -> row_to_str store row_id
                TFunction arg_id ret_id ->
                    arg_str = type_to_str store arg_id
                    ret_str = type_to_str store ret_id
                    "($(arg_str) → $(ret_str))"
                TArray elem_id ->
                    elem_str = type_to_str store elem_id
                    "[$(elem_str)]"
        Err _ -> "???"

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
        Err _ -> "{???}"

main! = \_ ->
    _ = Stdout.line! "Testing Indexed Row Polymorphism:\n"

    # Build a simple record type: {x: number, y: string}
    store0 = empty_store

    # Add number type
    (store1, num_type) = add_type store0 TNumber

    # Add string type
    (store2, str_type) = add_type store1 TString

    # Build the row: {x: number, y: string}
    (store3, empty_row) = add_row store2 REmpty
    (store4, with_y) = add_row store3 (RExtend "y" str_type empty_row)
    (store5, with_x_y) = add_row store4 (RExtend "x" num_type with_y)

    # Create the record type
    (store6, record_type) = add_type store5 (TRecord with_x_y)

    _ = Stdout.line! "Created record type: $(type_to_str store6 record_type)"

    # Build a polymorphic record: {x: number, ...ρ1}
    (store7, row_var) = add_row store6 (RVar 1)
    (store8, with_x_poly) = add_row store7 (RExtend "x" num_type row_var)
    (final_store, poly_record_type) = add_type store8 (TRecord with_x_poly)

    _ = Stdout.line! "Created polymorphic record: $(type_to_str final_store poly_record_type)"

    _ = Stdout.line! "\nRow polymorphism allows {x: number, y: string} to be"
    _ = Stdout.line! "used where {x: number, ...ρ} is expected!"

    Ok {}

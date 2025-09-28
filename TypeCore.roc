module [
    TypeId,
    SimpleType,
    Literal,
    TypeInfo,
    TypeTable,
    new_type_table,
    fresh_type_id,
    add_type,
    get_type,
    mk_var,
    mk_prim,
    mk_fun,
    mk_obj,
    mk_arr,
    mk_union_id,
    mk_lit,
    type_to_string,
]

TypeId : U64

SimpleType : [
    TVar TypeId,
    TPrim Str,
    TLit Literal,
    TFunction TypeId TypeId,
    TArray TypeId,
    TObject (List FieldInfo),
    TUnion (List TypeId),
    TTop,
    TBot,
]

FieldInfo : { key : Str, tid : TypeId, optional : Bool }

Literal : [
    LStr Str,
    LNum F64,
    LBool Bool,
    LNull,
    LUndefined,
]

TypeInfo : {
    id : TypeId,
    type : SimpleType,
}

TypeTable : {
    types : List TypeInfo,
    next_id : TypeId,
}

new_type_table : {} -> TypeTable
new_type_table = \{} ->
    {
        types: [],
        next_id: 1,
    }

fresh_type_id : TypeTable -> (TypeId, TypeTable)
fresh_type_id = \table ->
    id = table.next_id
    new_table = { table & next_id: id + 1 }
    (id, new_table)

add_type : TypeTable, SimpleType -> (TypeId, TypeTable)
add_type = \table, simple_type ->
    (id, table_with_id) = fresh_type_id table
    info = { id, type: simple_type }
    new_table = { table_with_id & types: List.append table_with_id.types info }
    (id, new_table)

get_type : TypeTable, TypeId -> Result SimpleType [TypeNotFound]
get_type = \table, id ->
    when List.find_first table.types \info -> info.id == id is
        Ok info -> Ok info.type
        Err _ -> Err TypeNotFound

mk_var : TypeTable -> (TypeId, TypeTable)
mk_var = \table ->
    (id, new_table) = fresh_type_id table
    add_type new_table (TVar id)

mk_prim : TypeTable, Str -> (TypeId, TypeTable)
mk_prim = \table, name ->
    add_type table (TPrim name)

mk_fun : TypeTable, TypeId, TypeId -> (TypeId, TypeTable)
mk_fun = \table, params_id, return_id ->
    add_type table (TFunction params_id return_id)

mk_arr : TypeTable, TypeId -> (TypeId, TypeTable)
mk_arr = \table, elem_id ->
    add_type table (TArray elem_id)

mk_obj : TypeTable, List FieldInfo -> (TypeId, TypeTable)
mk_obj = \table, fields ->
    add_type table (TObject fields)

mk_union_id : TypeTable, List TypeId -> (TypeId, TypeTable)
mk_union_id = \table, type_ids ->
    add_type table (TUnion type_ids)

mk_lit : TypeTable, Literal -> (TypeId, TypeTable)
mk_lit = \table, lit ->
    add_type table (TLit lit)

type_to_string : TypeTable, TypeId -> Str
type_to_string = \table, id ->
    when get_type table id is
        Ok type -> simple_type_to_string table type
        Err _ -> "T$(Num.to_str id)"

simple_type_to_string : TypeTable, SimpleType -> Str
simple_type_to_string = \table, stype ->
    when stype is
        TVar tid -> "T$(Num.to_str tid)"
        TPrim name -> name
        TLit lit ->
            when lit is
                LStr s -> "\"$(s)\""
                LNum n -> Num.to_str n
                LBool b -> if b then "true" else "false"
                LNull -> "null"
                LUndefined -> "undefined"
        TFunction params ret ->
            "($(type_to_string table params)) => $(type_to_string table ret)"
        TArray elem -> "$(type_to_string table elem)[]"
        TObject fields ->
            field_strs = List.map fields \{ key, tid, optional } ->
                opt = if optional then "?" else ""
                "$(key)$(opt): $(type_to_string table tid)"
            "{ $(Str.join_with field_strs ", ") }"
        TUnion ids ->
            type_strs = List.map ids \tid -> type_to_string table tid
            "($(Str.join_with type_strs " | "))"
        TTop -> "any"
        TBot -> "never"

module [
    TypeStore,
    TypeId,
    RowId,
    TypeDef,
    RowDef,
    LiteralValue,
    empty_store,
    add_type,
    add_row,
    get_type,
    get_row,
    # Type constructors
    make_primitive,
    make_literal,
    make_object,
    make_array,
    make_tuple,
    make_function,
    make_union,
    make_intersection,
    make_type_var,
    make_generic,
    make_conditional,
    make_mapped_type,
    make_template_literal,
    # Row constructors
    make_empty_row,
    make_row_extend,
    make_row_var,
    # Utilities
    type_to_str,
    row_to_str,
]

# Comprehensive Type System using indices to avoid mutual recursion

TypeId : U32
RowId : U32
TypeVar : U32
RowVar : U32

# Literal values
LiteralValue : [
    NumLit Str,
    StrLit Str,
    BoolLit Bool,
    NullLit,
    UndefinedLit,
    BigIntLit Str,
]

# Helper record types for complex tags
FunctionInfo : {
    params: List TypeId,
    ret: TypeId,
    type_params: List TypeVar,
    is_async: Bool,
}

GenericInfo : {
    base: TypeId,
    args: List TypeId,
}

ConditionalInfo : {
    check: TypeId,
    extends: TypeId,
    true_type: TypeId,
    false_type: TypeId,
}

MappedTypeInfo : {
    key_type: TypeId,
    value_type: TypeId,
    readonly: Bool,
    optional: Bool,
}

TemplateLiteralInfo : {
    parts: List Str,
    types: List TypeId,
}

IndexedAccessInfo : {
    object: TypeId,
    index: TypeId,
}

# Type definitions (no direct recursion)
TypeDef : [
    TPrimitive Str,
    TLiteral LiteralValue,
    TObject RowId,
    TArray TypeId,
    TTuple (List TypeId),
    TFunction FunctionInfo,
    TUnion (List TypeId),
    TIntersection (List TypeId),
    TypeVariable TypeVar,
    TGeneric GenericInfo,
    TConditional ConditionalInfo,
    TMappedType MappedTypeInfo,
    TTemplateLiteral TemplateLiteralInfo,
    TKeyOf TypeId,
    TIndexedAccess IndexedAccessInfo,
]

# Helper record types for row definitions
ExtendInfo : {
    label: Str,
    field_type: TypeId,
    optional: Bool,
    readonly: Bool,
    rest: RowId,
}

IndexInfo : {
    key_type: TypeId,
    value_type: TypeId,
    rest: RowId,
}

# Row definitions for object types
RowDef : [
    REmpty,
    RExtend ExtendInfo,
    RVar RowVar,
    RIndex IndexInfo,
]

# Store for types and rows
TypeStore : {
    types: List TypeDef,
    rows: List RowDef,
    next_type_id: TypeId,
    next_row_id: RowId,
    next_type_var: TypeVar,
    next_row_var: RowVar,
}

# Create an empty type store
empty_store : TypeStore
empty_store = {
    types: [],
    rows: [],
    next_type_id: 0,
    next_row_id: 0,
    next_type_var: 0,
    next_row_var: 0,
}

# Add a type to the store and get its ID
add_type : TypeStore, TypeDef -> (TypeStore, TypeId)
add_type = \store, type_def ->
    type_id = store.next_type_id
    new_store = { store &
        types: List.append store.types type_def,
        next_type_id: type_id + 1,
    }
    (new_store, type_id)

# Add a row to the store and get its ID
add_row : TypeStore, RowDef -> (TypeStore, RowId)
add_row = \store, row_def ->
    row_id = store.next_row_id
    new_store = { store &
        rows: List.append store.rows row_def,
        next_row_id: row_id + 1,
    }
    (new_store, row_id)

# Get a type by ID
get_type : TypeStore, TypeId -> Result TypeDef [NotFound]
get_type = \store, type_id ->
    when List.get store.types (Num.to_u64 type_id) is
        Ok type_def -> Ok type_def
        Err _ -> Err NotFound

# Get a row by ID
get_row : TypeStore, RowId -> Result RowDef [NotFound]
get_row = \store, row_id ->
    when List.get store.rows (Num.to_u64 row_id) is
        Ok row_def -> Ok row_def
        Err _ -> Err NotFound

# Type constructor helpers
make_primitive : TypeStore, Str -> (TypeStore, TypeId)
make_primitive = \store, name ->
    add_type store (TPrimitive name)

make_literal : TypeStore, LiteralValue -> (TypeStore, TypeId)
make_literal = \store, value ->
    add_type store (TLiteral value)

make_object : TypeStore, RowId -> (TypeStore, TypeId)
make_object = \store, row_id ->
    add_type store (TObject row_id)

make_array : TypeStore, TypeId -> (TypeStore, TypeId)
make_array = \store, elem_type ->
    add_type store (TArray elem_type)

make_tuple : TypeStore, List TypeId -> (TypeStore, TypeId)
make_tuple = \store, types ->
    add_type store (TTuple types)

make_function : TypeStore, List TypeId, TypeId, List TypeVar, Bool -> (TypeStore, TypeId)
make_function = \store, params, return_type, type_params, is_async ->
    add_type store (TFunction { params, ret: return_type, type_params, is_async })

make_union : TypeStore, List TypeId -> (TypeStore, TypeId)
make_union = \store, types ->
    add_type store (TUnion types)

make_intersection : TypeStore, List TypeId -> (TypeStore, TypeId)
make_intersection = \store, types ->
    add_type store (TIntersection types)

make_type_var : TypeStore, TypeVar -> (TypeStore, TypeId)
make_type_var = \store, var ->
    add_type store (TypeVariable var)

make_generic : TypeStore, TypeId, List TypeId -> (TypeStore, TypeId)
make_generic = \store, base, args ->
    add_type store (TGeneric { base, args })

make_conditional : TypeStore, TypeId, TypeId, TypeId, TypeId -> (TypeStore, TypeId)
make_conditional = \store, check, extends, true_type, false_type ->
    add_type store (TConditional { check, extends, true_type, false_type })

make_mapped_type : TypeStore, TypeId, TypeId, Bool, Bool -> (TypeStore, TypeId)
make_mapped_type = \store, key_type, value_type, readonly, optional ->
    add_type store (TMappedType { key_type, value_type, readonly, optional })

make_template_literal : TypeStore, List Str, List TypeId -> (TypeStore, TypeId)
make_template_literal = \store, parts, types ->
    add_type store (TTemplateLiteral { parts, types })

# Row constructor helpers
make_empty_row : TypeStore -> (TypeStore, RowId)
make_empty_row = \store ->
    add_row store REmpty

make_row_extend : TypeStore, Str, TypeId, Bool, Bool, RowId -> (TypeStore, RowId)
make_row_extend = \store, label, field_type, optional, readonly, rest ->
    add_row store (RExtend { label, field_type, optional, readonly, rest })

make_row_var : TypeStore, RowVar -> (TypeStore, RowId)
make_row_var = \store, var ->
    add_row store (RVar var)

# Convert type to string (avoiding infinite recursion)
type_to_str : TypeStore, TypeId -> Str
type_to_str = |store, type_id|
    type_to_str_helper(store, type_id, 0, 5)  # Max depth of 5

type_to_str_helper : TypeStore, TypeId, U32, U32 -> Str
type_to_str_helper = |store, type_id, depth, max_depth|
    if depth > max_depth then
        "..."
    else
        when get_type store type_id is
            Ok type_def ->
                when type_def is
                    TPrimitive name -> name

                    TLiteral lit ->
                        when lit is
                            NumLit(n) -> n
                            StrLit(s) -> "\"${s}\""
                            BoolLit(b) -> if b then "true" else "false"
                            NullLit -> "null"
                            UndefinedLit -> "undefined"
                            BigIntLit(n) -> "${n}n"

                    TObject(row_id) -> row_to_str_helper(store, row_id, depth + 1, max_depth)

                    TArray(elem_id) ->
                        elem_str = type_to_str_helper(store, elem_id, depth + 1, max_depth)
                        "${elem_str}[]"

                    TTuple(types) ->
                        types_str = List.map(types, |id|
                            type_to_str_helper(store, id, depth + 1, max_depth)
                        )
                        "[" |> Str.concat (Str.join_with(types_str, ", ")) |> Str.concat "]"

                    TFunction({ params, ret, is_async }) ->
                        params_str = List.map(params, |id|
                            type_to_str_helper(store, id, depth + 1, max_depth)
                        )
                        return_str = type_to_str_helper(store, ret, depth + 1, max_depth)
                        async_prefix = if is_async then "async " else ""
                        "${async_prefix}(${Str.join_with params_str ", "}) => ${return_str}"

                    TUnion(types) ->
                        types_str = List.map(types, |id|
                            type_to_str_helper(store, id, depth + 1, max_depth)
                        )
                        Str.join_with(types_str, " | ")

                    TIntersection(types) ->
                        types_str = List.map(types, |id|
                            type_to_str_helper(store, id, depth + 1, max_depth)
                        )
                        Str.join_with(types_str, " & ")

                    TypeVariable(var) -> "T${Num.to_str var}"

                    TGeneric({ base, args }) ->
                        base_str = type_to_str_helper(store, base, depth + 1, max_depth)
                        args_str = List.map(args, |id|
                            type_to_str_helper(store, id, depth + 1, max_depth)
                        )
                        "${base_str}<${Str.join_with(args_str, ", ")}>"

                    TConditional({ check, extends, true_type, false_type }) ->
                        check_str = type_to_str_helper(store, check, depth + 1, max_depth)
                        extends_str = type_to_str_helper(store, extends, depth + 1, max_depth)
                        true_str = type_to_str_helper(store, true_type, depth + 1, max_depth)
                        false_str = type_to_str_helper(store, false_type, depth + 1, max_depth)
                        "${check_str} extends ${extends_str} ? ${true_str} : ${false_str}"

                    TKeyOf(object_id) ->
                        object_str = type_to_str_helper(store, object_id, depth + 1, max_depth)
                        "keyof ${object_str}"

                    TIndexedAccess({ object, index }) ->
                        object_str = type_to_str_helper(store, object, depth + 1, max_depth)
                        index_str = type_to_str_helper(store, index, depth + 1, max_depth)
                        "${object_str}[${index_str}]"

                    TMappedType({ key_type, value_type, readonly, optional }) ->
                        key_str = type_to_str_helper(store, key_type, depth + 1, max_depth)
                        value_str = type_to_str_helper(store, value_type, depth + 1, max_depth)
                        ro = if readonly then "readonly " else ""
                        opt = if optional then "?" else ""
                        "{ ${ro}[K in ${key_str}]${opt}: ${value_str} }"

                    TTemplateLiteral({ parts, types }) ->
                        # Simplified template literal display
                        "`...template...`"

            Err _ -> "???"

# Convert row to string
row_to_str : TypeStore, RowId -> Str
row_to_str = \store, row_id ->
    row_to_str_helper store row_id 0 5

row_to_str_helper : TypeStore, RowId, U32, U32 -> Str
row_to_str_helper = \store, row_id, depth, max_depth ->
    if depth > max_depth then
        "{...}"
    else
        when get_row store row_id is
            Ok row_def ->
                when row_def is
                    REmpty -> "{}"

                    RExtend({ label, field_type, optional, readonly, rest }) ->
                        type_str = type_to_str_helper(store, field_type, depth + 1, max_depth)
                        rest_str = row_to_str_helper(store, rest, depth + 1, max_depth)
                        ro = if readonly then "readonly " else ""
                        opt = if optional then "?" else ""
                        field = "${ro}${label}${opt}: ${type_str}"
                        when rest_str is
                            "{}" -> "{ ${field} }"
                            _ ->
                                # Remove the closing brace from rest_str and append
                                inner = Str.drop_suffix rest_str " }"
                                    |> Str.drop_suffix "}"
                                    |> Str.drop_prefix "{ "
                                    |> Str.drop_prefix "{"
                                if Str.is_empty inner then
                                    "{ ${field} }"
                                else
                                    "{ ${field}, ${inner} }"

                    RVar(var) -> "{ ...ρ${Num.to_str var} }"

                    RIndex({ key_type, value_type, rest }) ->
                        key_str = type_to_str_helper(store, key_type, depth + 1, max_depth)
                        value_str = type_to_str_helper(store, value_type, depth + 1, max_depth)
                        rest_str = row_to_str_helper(store, rest, depth + 1, max_depth)
                        index = "[key: ${key_str}]: ${value_str}"
                        when rest_str is
                            "{}" -> "{ ${index} }"
                            _ -> "{ ${index}, ...${rest_str} }"

            Err _ -> "{???}"

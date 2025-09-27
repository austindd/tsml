module [
    TypeStore,
    TypeId,
    RowId,
    TypeDef,
    RowDef,
    LiteralValue,
    ClassId,
    InterfaceId,
    ClassDef,
    InterfaceDef,
    TypeParamId,
    TypeParamDef,
    EnumId,
    EnumDef,
    EnumMemberValue,
    empty_store,
    add_type,
    add_row,
    add_class,
    add_interface,
    add_type_param,
    add_enum,
    get_type,
    get_row,
    get_class,
    get_interface,
    get_type_param,
    get_enum,
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
    make_class,
    make_interface,
    make_enum,
    make_enum_member,
    make_any,
    make_never,
    make_unknown,
    make_void,
    make_null,
    make_undefined,
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
ClassId : U32
InterfaceId : U32
TypeParamId : U32
EnumId : U32

# Literal values
LiteralValue : [
    NumLit F64,
    StrLit Str,
    BoolLit Bool,
    BigIntLit Str,
]

# Enum member values
EnumMemberValue : [
    EnumNumValue F64,
    EnumStrValue Str,
    EnumAutoValue,  # Auto-incremented numeric value
]

# Enum definition (stored separately)
EnumDef : {
    name: Str,
    members: List { name: Str, value: EnumMemberValue },
    is_const: Bool,  # const enum
}

# Type parameter definition (stored separately to avoid recursion)
TypeParamDef : {
    name: Str,
    constraint: Result TypeId [NoConstraint],
    default_type: Result TypeId [NoDefault],
}

# Class definition (stored separately)
ClassDef : {
    name: Str,
    type_params: List TypeParamId,
    extends: Result TypeId [NoExtends],
    implements: List TypeId,
    constructor_params: List { name: Str, param_type: TypeId, optional: Bool },
    properties: List { name: Str, prop_type: TypeId, is_static: Bool, is_private: Bool },
    methods: List { name: Str, params: List TypeId, return_type: TypeId, is_static: Bool, is_private: Bool },
}

# Interface definition (stored separately)
InterfaceDef : {
    name: Str,
    type_params: List TypeParamId,
    extends: List TypeId,
    properties: List { name: Str, prop_type: TypeId, optional: Bool, readonly: Bool },
    methods: List { name: Str, params: List TypeId, return_type: TypeId, optional: Bool },
}

# Helper record types for complex tags
FunctionInfo : {
    params: List { name: Str, param_type: TypeId, optional: Bool },
    ret: TypeId,
    type_params: List TypeParamId,
    is_async: Bool,
    is_generator: Bool,
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
    # Primitives
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,
    TBigInt,
    TSymbol,

    # Special types
    TAny,
    TNever,
    TUnknown,
    TVoid,

    # Literal types
    TLiteral LiteralValue,

    # Structural types
    TObject RowId,
    TArray TypeId,
    TTuple (List TypeId),

    # Function types
    TFunction FunctionInfo,

    # Compound types
    TUnion (List TypeId),
    TIntersection (List TypeId),

    # Type parameters and generics
    TypeVariable TypeVar,
    TTypeParam TypeParamId,
    TGeneric GenericInfo,

    # Advanced types
    TConditional ConditionalInfo,
    TMappedType MappedTypeInfo,
    TTemplateLiteral TemplateLiteralInfo,
    TKeyOf TypeId,
    TIndexedAccess IndexedAccessInfo,

    # Class and interface references
    TClass ClassId,
    TInterface InterfaceId,

    # Enum types
    TEnum EnumId,
    TEnumMember { enum_id: EnumId, member_name: Str },
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
    classes: List ClassDef,
    interfaces: List InterfaceDef,
    type_params: List TypeParamDef,
    enums: List EnumDef,
    next_type_id: TypeId,
    next_row_id: RowId,
    next_type_var: TypeVar,
    next_row_var: RowVar,
    next_class_id: ClassId,
    next_interface_id: InterfaceId,
    next_type_param_id: TypeParamId,
    next_enum_id: EnumId,
}

# Create an empty type store
empty_store : TypeStore
empty_store = {
    types: [],
    rows: [],
    classes: [],
    interfaces: [],
    type_params: [],
    enums: [],
    next_type_id: 0,
    next_row_id: 0,
    next_type_var: 0,
    next_row_var: 0,
    next_class_id: 0,
    next_interface_id: 0,
    next_type_param_id: 0,
    next_enum_id: 0,
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

# Add a class to the store and get its ID
add_class : TypeStore, ClassDef -> (TypeStore, ClassId)
add_class = \store, class_def ->
    class_id = store.next_class_id
    new_store = { store &
        classes: List.append store.classes class_def,
        next_class_id: class_id + 1,
    }
    (new_store, class_id)

# Add an interface to the store and get its ID
add_interface : TypeStore, InterfaceDef -> (TypeStore, InterfaceId)
add_interface = \store, interface_def ->
    interface_id = store.next_interface_id
    new_store = { store &
        interfaces: List.append store.interfaces interface_def,
        next_interface_id: interface_id + 1,
    }
    (new_store, interface_id)

# Add a type parameter to the store and get its ID
add_type_param : TypeStore, TypeParamDef -> (TypeStore, TypeParamId)
add_type_param = \store, type_param_def ->
    type_param_id = store.next_type_param_id
    new_store = { store &
        type_params: List.append store.type_params type_param_def,
        next_type_param_id: type_param_id + 1,
    }
    (new_store, type_param_id)

# Get a class by ID
get_class : TypeStore, ClassId -> Result ClassDef [NotFound]
get_class = \store, class_id ->
    when List.get store.classes (Num.to_u64 class_id) is
        Ok class_def -> Ok class_def
        Err _ -> Err NotFound

# Get an interface by ID
get_interface : TypeStore, InterfaceId -> Result InterfaceDef [NotFound]
get_interface = \store, interface_id ->
    when List.get store.interfaces (Num.to_u64 interface_id) is
        Ok interface_def -> Ok interface_def
        Err _ -> Err NotFound

# Get a type parameter by ID
get_type_param : TypeStore, TypeParamId -> Result TypeParamDef [NotFound]
get_type_param = \store, type_param_id ->
    when List.get store.type_params (Num.to_u64 type_param_id) is
        Ok type_param_def -> Ok type_param_def
        Err _ -> Err NotFound

# Add an enum to the store and get its ID
add_enum : TypeStore, EnumDef -> (TypeStore, EnumId)
add_enum = \store, enum_def ->
    enum_id = store.next_enum_id
    new_store = { store &
        enums: List.append store.enums enum_def,
        next_enum_id: enum_id + 1,
    }
    (new_store, enum_id)

# Get an enum by ID
get_enum : TypeStore, EnumId -> Result EnumDef [NotFound]
get_enum = \store, enum_id ->
    when List.get store.enums (Num.to_u64 enum_id) is
        Ok enum_def -> Ok enum_def
        Err _ -> Err NotFound

# Type constructor helpers
make_primitive : TypeStore, Str -> (TypeStore, TypeId)
make_primitive = \store, name ->
    type_def = when name is
        "number" -> TNumber
        "string" -> TString
        "boolean" -> TBoolean
        "null" -> TNull
        "undefined" -> TUndefined
        "bigint" -> TBigInt
        "symbol" -> TSymbol
        _ -> TString # fallback
    add_type store type_def

# Special type constructors
make_any : TypeStore -> (TypeStore, TypeId)
make_any = \store ->
    add_type store TAny

make_never : TypeStore -> (TypeStore, TypeId)
make_never = \store ->
    add_type store TNever

make_unknown : TypeStore -> (TypeStore, TypeId)
make_unknown = \store ->
    add_type store TUnknown

make_void : TypeStore -> (TypeStore, TypeId)
make_void = \store ->
    add_type store TVoid

make_null : TypeStore -> (TypeStore, TypeId)
make_null = \store ->
    add_type store TNull

make_undefined : TypeStore -> (TypeStore, TypeId)
make_undefined = \store ->
    add_type store TUndefined

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

make_function : TypeStore, List { name: Str, param_type: TypeId, optional: Bool }, TypeId, List TypeParamId, Bool, Bool -> (TypeStore, TypeId)
make_function = \store, params, return_type, type_params, is_async, is_generator ->
    add_type store (TFunction { params, ret: return_type, type_params, is_async, is_generator })

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

# Class and interface constructors
make_class : TypeStore, ClassDef -> (TypeStore, TypeId)
make_class = \store, class_def ->
    (store_with_class, class_id) = add_class store class_def
    add_type store_with_class (TClass class_id)

make_interface : TypeStore, InterfaceDef -> (TypeStore, TypeId)
make_interface = \store, interface_def ->
    (store_with_interface, interface_id) = add_interface store interface_def
    add_type store_with_interface (TInterface interface_id)

# Enum constructors
make_enum : TypeStore, EnumDef -> (TypeStore, TypeId)
make_enum = \store, enum_def ->
    (store_with_enum, enum_id) = add_enum store enum_def
    add_type store_with_enum (TEnum enum_id)

make_enum_member : TypeStore, EnumId, Str -> (TypeStore, TypeId)
make_enum_member = \store, enum_id, member_name ->
    add_type store (TEnumMember { enum_id, member_name })

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
                    TNumber -> "number"
                    TString -> "string"
                    TBoolean -> "boolean"
                    TNull -> "null"
                    TUndefined -> "undefined"
                    TBigInt -> "bigint"
                    TSymbol -> "symbol"
                    TAny -> "any"
                    TNever -> "never"
                    TUnknown -> "unknown"
                    TVoid -> "void"

                    TLiteral lit ->
                        when lit is
                            NumLit(n) -> Num.to_str n
                            StrLit(s) -> "\"${s}\""
                            BoolLit(b) -> if b then "true" else "false"
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

                    TFunction({ params, ret, is_async, is_generator }) ->
                        params_str = List.map(params, |p|
                            opt = if p.optional then "?" else ""
                            "${p.name}${opt}: ${type_to_str_helper(store, p.param_type, depth + 1, max_depth)}"
                        )
                        return_str = type_to_str_helper(store, ret, depth + 1, max_depth)
                        async_prefix = if is_async then "async " else ""
                        gen_prefix = if is_generator then "*" else ""
                        "${async_prefix}${gen_prefix}(${Str.join_with params_str ", "}) => ${return_str}"

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

                    TTypeParam(param_id) ->
                        when get_type_param store param_id is
                            Ok param_def ->
                                constraint_str = when param_def.constraint is
                                    Ok c -> " extends ${type_to_str_helper(store, c, depth + 1, max_depth)}"
                                    Err _ -> ""
                                "${param_def.name}${constraint_str}"
                            Err _ -> "T???"

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

                    TTemplateLiteral({ parts: _, types: _ }) ->
                        # Simplified template literal display
                        "`...template...`"

                    TClass(class_id) ->
                        when get_class store class_id is
                            Ok class_def ->
                                type_params_str = if List.is_empty class_def.type_params then
                                    ""
                                else
                                    params = List.map class_def.type_params |param_id|
                                        when get_type_param store param_id is
                                            Ok p -> p.name
                                            Err _ -> "?"
                                    "<${Str.join_with(params, ", ")}>"
                                "${class_def.name}${type_params_str}"
                            Err _ -> "Class???"

                    TInterface(interface_id) ->
                        when get_interface store interface_id is
                            Ok interface_def ->
                                type_params_str = if List.is_empty interface_def.type_params then
                                    ""
                                else
                                    params = List.map interface_def.type_params |param_id|
                                        when get_type_param store param_id is
                                            Ok p -> p.name
                                            Err _ -> "?"
                                    "<${Str.join_with(params, ", ")}>"
                                "${interface_def.name}${type_params_str}"
                            Err _ -> "Interface???"

                    TEnum(enum_id) ->
                        when get_enum store enum_id is
                            Ok enum_def -> enum_def.name
                            Err _ -> "Enum???"

                    TEnumMember({ enum_id, member_name }) ->
                        when get_enum store enum_id is
                            Ok enum_def -> "${enum_def.name}.${member_name}"
                            Err _ -> "Enum.${member_name}"

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

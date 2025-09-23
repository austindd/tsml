module [
    GenericType,
    TypeParam,
    instantiate_generic,
    infer_generic_function,
    check_generic_constraint,
]

# Generic Type Parameters and Type Arguments Implementation

TypeId : U32
TypeParamId : U32

# Type parameter with constraints
TypeParam : {
    id: TypeParamId,
    name: Str,
    constraint: Option GenericType,  # T extends Constraint
    default: Option GenericType,     # T = DefaultType
    variance: [Covariant, Contravariant, Invariant],
}

# Generic type system
GenericType : [
    # Primitives
    GNum,
    GStr,
    GBool,
    GNull,
    GUndefined,

    # Type variable
    GVar TypeId,

    # Type parameter (like T in function<T>)
    GParam TypeParamId,

    # Generic function type
    GFunction {
        type_params: List TypeParam,
        params: List GenericType,
        return: GenericType,
    },

    # Generic class/interface
    GClass {
        name: Str,
        type_params: List TypeParam,
        properties: List { key: Str, type: GenericType },
    },

    # Instantiated generic (like Array<string>)
    GInstantiated {
        base: GenericType,  # The generic type (e.g., Array)
        args: List GenericType,  # Type arguments (e.g., [string])
    },

    # Object type
    GObject (List { key: Str, type: GenericType }),

    # Array (generic by default)
    GArray GenericType,

    # Union
    GUnion (List GenericType),

    # Intersection
    GIntersection (List GenericType),

    # Conditional type: T extends U ? X : Y
    GConditional {
        check: GenericType,
        extends_type: GenericType,
        true_type: GenericType,
        false_type: GenericType,
    },

    # Mapped type: { [K in T]: U }
    GMapped {
        param: TypeParamId,
        constraint: GenericType,
        template: GenericType,
    },

    # Keyof type
    GKeyof GenericType,

    # Tuple
    GTuple (List GenericType),

    # Unknown
    GUnknown,
]

Option : [Some GenericType, None]

# === Core Generic Operations ===

# Substitute type parameters with concrete types
substitute_params : GenericType, List { param: TypeParamId, replacement: GenericType } -> GenericType
substitute_params = \typ, substitutions ->
    when typ is
        GParam id ->
            when List.find_first substitutions \s -> s.param == id is
                Ok s -> s.replacement
                Err _ -> typ

        GFunction data ->
            GFunction {
                data &
                params: List.map data.params \p -> substitute_params p substitutions,
                return: substitute_params data.return substitutions,
            }

        GClass data ->
            GClass {
                data &
                properties: List.map data.properties \prop ->
                    { prop & type: substitute_params prop.type substitutions },
            }

        GInstantiated data ->
            GInstantiated {
                base: substitute_params data.base substitutions,
                args: List.map data.args \a -> substitute_params a substitutions,
            }

        GObject props ->
            GObject (List.map props \p -> { p & type: substitute_params p.type substitutions })

        GArray elem -> GArray (substitute_params elem substitutions)

        GUnion types -> GUnion (List.map types \t -> substitute_params t substitutions)

        GIntersection types -> GIntersection (List.map types \t -> substitute_params t substitutions)

        GConditional data ->
            GConditional {
                data &
                check: substitute_params data.check substitutions,
                extends_type: substitute_params data.extends_type substitutions,
                true_type: substitute_params data.true_type substitutions,
                false_type: substitute_params data.false_type substitutions,
            }

        GTuple types -> GTuple (List.map types \t -> substitute_params t substitutions)

        _ -> typ

# Instantiate a generic type with type arguments
instantiate_generic : GenericType, List GenericType -> Result GenericType [GenericError Str]
instantiate_generic = \generic_type, type_args ->
    when generic_type is
        GFunction data ->
            if List.len type_args != List.len data.type_params then
                Err (GenericError "Wrong number of type arguments")
            else
                substitutions = List.map2 data.type_params type_args \param, arg ->
                    { param: param.id, replacement: arg }

                Ok (substitute_params generic_type substitutions)

        GClass data ->
            if List.len type_args != List.len data.type_params then
                Err (GenericError "Wrong number of type arguments")
            else
                substitutions = List.map2 data.type_params type_args \param, arg ->
                    { param: param.id, replacement: arg }

                Ok (substitute_params generic_type substitutions)

        _ ->
            Ok (GInstantiated { base: generic_type, args: type_args })

# === Generic Inference ===

# Infer type parameters for a generic function call
infer_generic_function : GenericType, List GenericType -> Result (List GenericType) [InferenceError Str]
infer_generic_function = \func_type, arg_types ->
    when func_type is
        GFunction data ->
            # Match argument types against parameter types to infer type params
            inferred = infer_from_arguments data.params arg_types data.type_params
            Ok inferred
        _ ->
            Err (InferenceError "Not a generic function")

# Infer type parameters from arguments
infer_from_arguments : List GenericType, List GenericType, List TypeParam -> List GenericType
infer_from_arguments = \param_types, arg_types, type_params ->
    # Simplified: would use unification to infer type parameters
    List.map type_params \_ -> GUnknown

# Check generic constraint (T extends Constraint)
check_generic_constraint : GenericType, GenericType -> Bool
check_generic_constraint = \typ, constraint ->
    when (typ, constraint) is
        (GNum, GNum) -> Bool.true
        (GStr, GStr) -> Bool.true
        (GBool, GBool) -> Bool.true

        # Structural checking for objects
        (GObject props1, GObject props2) ->
            # Check if props1 has all fields of props2
            List.all props2 \p2 ->
                List.any props1 \p1 ->
                    p1.key == p2.key && types_compatible p1.type p2.type

        # Union constraint: type must extend at least one member
        (_, GUnion constraints) ->
            List.any constraints \c -> check_generic_constraint typ c

        # Intersection constraint: type must extend all members
        (_, GIntersection constraints) ->
            List.all constraints \c -> check_generic_constraint typ c

        _ -> Bool.false

# Check if types are compatible
types_compatible : GenericType, GenericType -> Bool
types_compatible = \t1, t2 ->
    when (t1, t2) is
        (GNum, GNum) -> Bool.true
        (GStr, GStr) -> Bool.true
        (GBool, GBool) -> Bool.true
        (GNull, GNull) -> Bool.true
        (GUndefined, GUndefined) -> Bool.true
        (GParam id1, GParam id2) -> id1 == id2
        (GVar v1, GVar v2) -> v1 == v2
        _ -> Bool.false

# === Common Generic Patterns ===

# Array<T> type
make_array_type : GenericType -> GenericType
make_array_type = \elem_type ->
    GArray elem_type

# Map<K, V> type
make_map_type : GenericType, GenericType -> GenericType
make_map_type = \key_type, value_type ->
    GClass {
        name: "Map",
        type_params: [
            { id: 1, name: "K", constraint: None, default: None, variance: Invariant },
            { id: 2, name: "V", constraint: None, default: None, variance: Invariant },
        ],
        properties: [],  # Simplified
    }

# Promise<T> type
make_promise_type : GenericType -> GenericType
make_promise_type = \resolved_type ->
    GClass {
        name: "Promise",
        type_params: [
            { id: 3, name: "T", constraint: None, default: None, variance: Covariant },
        ],
        properties: [],  # Simplified
    }

# === TypeScript Utility Type Examples ===

# Partial<T> - Makes all properties optional
make_partial : GenericType -> GenericType
make_partial = \typ ->
    GMapped {
        param: 10,
        constraint: GKeyof typ,
        template: GUnion [typ, GUndefined],
    }

# Required<T> - Makes all properties required
make_required : GenericType -> GenericType
make_required = \typ ->
    GMapped {
        param: 11,
        constraint: GKeyof typ,
        template: typ,  # Remove optionality
    }

# Pick<T, K> - Pick properties K from T
make_pick : GenericType, GenericType -> GenericType
make_pick = \typ, keys ->
    GMapped {
        param: 12,
        constraint: keys,
        template: typ,
    }

# === Higher-Kinded Types (Advanced) ===

# Functor pattern: F<T> where F is a type constructor
FunctorType : {
    constructor: TypeParamId -> GenericType,
    map: GenericType,  # <A, B>(f: A -> B) -> F<A> -> F<B>
}

# Monad pattern
MonadType : {
    constructor: TypeParamId -> GenericType,
    bind: GenericType,  # <A, B>(f: A -> M<B>) -> M<A> -> M<B>
    return: GenericType,  # <A>(a: A) -> M<A>
}

# === Examples ===

# function identity<T>(x: T): T { return x; }
example_identity : GenericType
example_identity =
    GFunction {
        type_params: [
            { id: 20, name: "T", constraint: None, default: None, variance: Invariant },
        ],
        params: [GParam 20],
        return: GParam 20,
    }

# function map<T, U>(arr: T[], fn: (x: T) => U): U[]
example_map : GenericType
example_map =
    GFunction {
        type_params: [
            { id: 21, name: "T", constraint: None, default: None, variance: Invariant },
            { id: 22, name: "U", constraint: None, default: None, variance: Invariant },
        ],
        params: [
            GArray (GParam 21),  # T[]
            GFunction {  # (x: T) => U
                type_params: [],
                params: [GParam 21],
                return: GParam 22,
            },
        ],
        return: GArray (GParam 22),  # U[]
    }

# interface Container<T extends string | number> {
#     value: T;
#     transform<U>(fn: (x: T) => U): Container<U>;
# }
example_container : GenericType
example_container =
    GClass {
        name: "Container",
        type_params: [
            {
                id: 23,
                name: "T",
                constraint: Some (GUnion [GStr, GNum]),
                default: None,
                variance: Invariant,
            },
        ],
        properties: [
            { key: "value", type: GParam 23 },
        ],
    }
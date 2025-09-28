module [
    RecursiveType,
    unroll,
    make_linked_list_type,
    make_tree_type,
    check_recursive_equality,
]

# Simplified Recursive Types that work with Roc's limitations
# Each type alias only references itself, no mutual recursion

# Type identifier
TypeId : U64

# Recursive binding identifier
RecVar : U64

# Single recursive type definition that can reference itself
RecursiveType : [
    # Primitives
    RNum,
    RStr,
    RBool,
    RNull,
    RUndefined,

    # Type variable
    RVar TypeId,

    # Record with fields that can be recursive
    RRecord (List { label: Str, type: RecursiveType }),

    # Array of recursive type
    RArray RecursiveType,

    # Function with recursive params/return
    RFunction (List RecursiveType) RecursiveType,

    # Union of recursive types
    RUnion (List RecursiveType),

    # Recursive reference (like a type variable bound by μ)
    RRecRef RecVar,

    # Recursive binding μα.T
    RMu RecVar RecursiveType,

    # Unknown
    RUnknown,
]

# === Core Operations ===

# Substitute a recursive reference with a type
substitute : RecursiveType, RecVar, RecursiveType -> RecursiveType
substitute = \typ, var, replacement ->
    when typ is
        RRecRef v ->
            if v == var then replacement else typ

        RRecord fields ->
            RRecord (List.map fields \f ->
                { f & type: substitute f.type var replacement })

        RArray elem ->
            RArray (substitute elem var replacement)

        RFunction params ret ->
            RFunction
                (List.map params \p -> substitute p var replacement)
                (substitute ret var replacement)

        RUnion types ->
            RUnion (List.map types \t -> substitute t var replacement)

        RMu bound_var body ->
            if bound_var == var then
                # Don't substitute inside a μ that rebinds the same variable
                typ
            else
                RMu bound_var (substitute body var replacement)

        _ -> typ

# Unroll a recursive type one level
unroll : RecursiveType -> RecursiveType
unroll = \typ ->
    when typ is
        RMu var body ->
            # μα.T → T[α := μα.T]
            substitute body var (RMu var body)
        _ -> typ

# === Example Type Constructors ===

# Linked list: μα.{value: T, next: α | null}
make_linked_list_type : RecursiveType -> RecursiveType
make_linked_list_type = \elem_type ->
    RMu 1 (RRecord [
        { label: "value", type: elem_type },
        { label: "next", type: RUnion [RRecRef 1, RNull] },
    ])

# Binary tree: μα.{value: T, left: α | null, right: α | null}
make_tree_type : RecursiveType -> RecursiveType
make_tree_type = \elem_type ->
    RMu 2 (RRecord [
        { label: "value", type: elem_type },
        { label: "left", type: RUnion [RRecRef 2, RNull] },
        { label: "right", type: RUnion [RRecRef 2, RNull] },
    ])

# JSON type: μα.(null | bool | number | string | α[] | {[key]: α})
make_json_type : {} -> RecursiveType
make_json_type = \{} ->
    RMu 3 (RUnion [
        RNull,
        RBool,
        RNum,
        RStr,
        RArray (RRecRef 3),  # JSON array
        RRecord [  # JSON object (simplified)
            { label: "data", type: RRecRef 3 },
        ],
    ])

# === Type Equality Checking ===

# Check if two types are equal (with depth limit for recursion)
check_recursive_equality : RecursiveType, RecursiveType, U64 -> Bool
check_recursive_equality = \t1, t2, depth ->
    if depth > 10 then
        # Assume equal at depth limit (coinductive equality)
        Bool.true
    else
        when (t1, t2) is
            (RNum, RNum) -> Bool.true
            (RStr, RStr) -> Bool.true
            (RBool, RBool) -> Bool.true
            (RNull, RNull) -> Bool.true
            (RUndefined, RUndefined) -> Bool.true
            (RUnknown, RUnknown) -> Bool.true

            (RVar v1, RVar v2) -> v1 == v2
            (RRecRef r1, RRecRef r2) -> r1 == r2

            (RRecord f1, RRecord f2) ->
                check_fields_equal f1 f2 depth

            (RArray e1, RArray e2) ->
                check_recursive_equality e1 e2 (depth + 1)

            (RFunction p1 r1, RFunction p2 r2) ->
                (List.len p1 == List.len p2) &&
                (List.map2 p1 p2 \a, b ->
                    check_recursive_equality a b (depth + 1)
                |> List.all \x -> x) &&
                (check_recursive_equality r1 r2 (depth + 1))

            (RUnion t1s, RUnion t2s) ->
                (List.len t1s == List.len t2s) &&
                List.all t1s \t ->
                    List.any t2s \t2_ ->
                        check_recursive_equality t t2_ (depth + 1)

            (RMu v1 b1, RMu v2 b2) ->
                # For simplicity, assume μ-types with same structure are equal
                # A full implementation would use α-equivalence
                check_recursive_equality b1 b2 (depth + 1)

            # Handle unrolling
            (RMu _ _, _) ->
                check_recursive_equality (unroll t1) t2 (depth + 1)

            (_, RMu _ _) ->
                check_recursive_equality t1 (unroll t2) (depth + 1)

            _ -> Bool.false

# Check if record fields are equal
check_fields_equal : List { label: Str, type: RecursiveType }, List { label: Str, type: RecursiveType }, U64 -> Bool
check_fields_equal = \f1, f2, depth ->
    (List.len f1 == List.len f2) &&
    List.all f1 \field1 ->
        List.any f2 \field2 ->
            (field1.label == field2.label) &&
            check_recursive_equality field1.type field2.type (depth + 1)

# === Examples of TypeScript Types ===

# interface Node {
#   value: number;
#   next: Node | null;
# }
example_node_interface : RecursiveType
example_node_interface =
    RMu 10 (RRecord [
        { label: "value", type: RNum },
        { label: "next", type: RUnion [RRecRef 10, RNull] },
    ])

# type LinkedList<T> = {
#   head: Node<T> | null;
#   tail: Node<T> | null;
# }
example_linked_list : RecursiveType -> RecursiveType
example_linked_list = \elem_type ->
    node_type = make_linked_list_type elem_type

    RRecord [
        { label: "head", type: RUnion [node_type, RNull] },
        { label: "tail", type: RUnion [node_type, RNull] },
    ]

# class TreeNode {
#   value: string;
#   children: TreeNode[];
# }
example_tree_class : RecursiveType
example_tree_class =
    RMu 20 (RRecord [
        { label: "value", type: RStr },
        { label: "children", type: RArray (RRecRef 20) },
    ])

# === Demonstration Functions ===

# Check if a type is recursive
is_recursive : RecursiveType -> Bool
is_recursive = \typ ->
    when typ is
        RMu _ _ -> Bool.true
        RRecord fields -> List.any fields \f -> is_recursive f.type
        RArray elem -> is_recursive elem
        RFunction params ret ->
            List.any params is_recursive || is_recursive ret
        RUnion types -> List.any types is_recursive
        _ -> Bool.false

# Get the depth of recursion nesting
recursion_depth : RecursiveType -> U64
recursion_depth = \typ ->
    when typ is
        RMu _ body -> 1 + recursion_depth body
        RRecord fields ->
            fields
            |> List.map \f -> recursion_depth f.type
            |> List.walk 0 \max, d -> if d > max then d else max
        RArray elem -> recursion_depth elem
        RFunction params ret ->
            param_depth = params
                |> List.map recursion_depth
                |> List.walk 0 \max, d -> if d > max then d else max
            ret_depth = recursion_depth ret
            if param_depth > ret_depth then param_depth else ret_depth
        RUnion types ->
            types
            |> List.map recursion_depth
            |> List.walk 0 \max, d -> if d > max then d else max
        _ -> 0

module [
    RecType,
    TypeWithRec,
    unroll_recursive,
    check_recursive_type,
    occurs_check,
]

# Recursive Types (μ-types) Implementation
# Handles self-referential types like linked lists, trees, classes

# Type variable for recursion binding
RecVar : U32

# Type identifier
TypeId : U32

# Recursive type: μα.T where α can appear in T
RecType : {
    # The bound variable (α in μα.T)
    var: RecVar,
    # The body where var can appear
    body: TypeWithRec,
}

# Extended type system with recursive types
TypeWithRec : [
    # Primitives
    TNumber,
    TString,
    TBoolean,
    TNull,
    TUndefined,

    # Type variable
    TVar TypeId,

    # Recursive type variable (bound by μ)
    TRecVar RecVar,

    # Record type
    TRecord (List { label: Str, type: TypeWithRec }),

    # Array
    TArray TypeWithRec,

    # Function
    TFunction (List TypeWithRec) TypeWithRec,

    # Union
    TUnion (List TypeWithRec),

    # Recursive type (μ-type)
    TMu RecType,

    # Class type (nominal with recursive self)
    TClass {
        name: Str,
        self_var: RecVar,  # For 'this' type
        properties: List { label: Str, type: TypeWithRec },
        methods: List { name: Str, type: TypeWithRec },
    },

    # Interface (structural with potential recursion)
    TInterface {
        name: Str,
        extends: List TypeWithRec,
        members: List { label: Str, type: TypeWithRec },
    },

    # Unknown
    TUnknown,
]

# === Core Operations ===

# Substitute a recursive variable with a type
subst_rec_var : TypeWithRec, RecVar, TypeWithRec -> TypeWithRec
subst_rec_var = \t, var, replacement ->
    when t is
        TRecVar(v) ->
            if v == var then replacement else t

        TRecord(fields) ->
            TRecord(List.map fields \f -> { f & type: subst_rec_var f.type var replacement })

        TArray(elem) ->
            TArray(subst_rec_var elem var replacement)

        TFunction(params, ret) ->
            TFunction(
                List.map params \p -> subst_rec_var p var replacement,
                subst_rec_var ret var replacement
            )

        TUnion(types) ->
            TUnion(List.map types \t2 -> subst_rec_var t2 var replacement)

        TMu(rec) ->
            if rec.var == var then
                # Don't substitute inside a μ that rebinds the same variable
                TMu(rec)
            else
                TMu({ rec & body: subst_rec_var rec.body var replacement })

        TClass(data) ->
            if data.self_var == var then
                # Don't substitute the self variable inside its own class
                TClass(data)
            else
                TClass({
                    data &
                    properties: List.map data.properties \p ->
                        { p & type: subst_rec_var p.type var replacement },
                    methods: List.map data.methods \m ->
                        { m & type: subst_rec_var m.type var replacement },
                })

        TInterface(data) ->
            TInterface({
                data &
                extends: List.map data.extends \e -> subst_rec_var e var replacement,
                members: List.map data.members \m ->
                    { m & type: subst_rec_var m.type var replacement },
            })

        _ -> t

# Unroll a recursive type one level
unroll_recursive : TypeWithRec -> TypeWithRec
unroll_recursive = \t ->
    when t is
        TMu(rec) ->
            # μα.T → T[α := μα.T]
            # Replace the bound variable with the whole recursive type
            subst_rec_var rec.body rec.var (TMu(rec))
        _ -> t

# Check if a type variable occurs in a type (for occurs check)
occurs_check : TypeId, TypeWithRec -> Bool
occurs_check = \var, t ->
    when t is
        TVar(v) -> v == var
        TRecord(fields) ->
            List.any fields \f -> occurs_check var f.type
        TArray(elem) -> occurs_check var elem
        TFunction(params, ret) ->
            (List.any params \p -> occurs_check var p) ||
            (occurs_check var ret)
        TUnion(types) ->
            List.any types \t2 -> occurs_check var t2
        TMu(rec) -> occurs_check var rec.body
        TClass(data) ->
            (List.any data.properties \p -> occurs_check var p.type) ||
            (List.any data.methods \m -> occurs_check var m.type)
        TInterface(data) ->
            (List.any data.extends \e -> occurs_check var e) ||
            (List.any data.members \m -> occurs_check var m.type)
        _ -> Bool.false

# === Example Recursive Types ===

# Linked list: μα. {value: T, next: α | null}
make_linked_list : TypeWithRec -> TypeWithRec
make_linked_list = \elem_type ->
    TMu({
        var: 1,  # α
        body: TRecord([
            { label: "value", type: elem_type },
            { label: "next", type: TUnion([TRecVar(1), TNull]) },
        ]),
    })

# Binary tree: μα. {value: T, left: α | null, right: α | null}
make_binary_tree : TypeWithRec -> TypeWithRec
make_binary_tree = \elem_type ->
    TMu({
        var: 2,  # α
        body: TRecord([
            { label: "value", type: elem_type },
            { label: "left", type: TUnion([TRecVar(2), TNull]) },
            { label: "right", type: TUnion([TRecVar(2), TNull]) },
        ]),
    })

# === Type Checking with Recursive Types ===

# Check if two recursive types are compatible
check_recursive_type : TypeWithRec, TypeWithRec -> Bool
check_recursive_type = \t1, t2 ->
    when (t1, t2) is
        # Two recursive types - unroll and compare
        (TMu(rec1), TMu(rec2)) ->
            # Use equi-recursive approach: unroll both one level
            unrolled1 = unroll_recursive t1
            unrolled2 = unroll_recursive t2
            check_type_equality unrolled1 unrolled2 0  # Depth limit to prevent infinite recursion

        # Recursive type vs non-recursive - unroll recursive one
        (TMu(_), _) ->
            check_type_equality (unroll_recursive t1) t2 0

        (_, TMu(_)) ->
            check_type_equality t1 (unroll_recursive t2) 0

        _ ->
            check_type_equality t1 t2 0

# Check type equality with depth limit for recursive types
check_type_equality : TypeWithRec, TypeWithRec, U32 -> Bool
check_type_equality = \t1, t2, depth ->
    if depth > 10 then
        # Depth limit reached - assume equal (coinductive)
        Bool.true
    else
        when (t1, t2) is
            (TNumber, TNumber) -> Bool.true
            (TString, TString) -> Bool.true
            (TBoolean, TBoolean) -> Bool.true
            (TNull, TNull) -> Bool.true
            (TUndefined, TUndefined) -> Bool.true

            (TVar(v1), TVar(v2)) -> v1 == v2
            (TRecVar(v1), TRecVar(v2)) -> v1 == v2

            (TRecord(f1), TRecord(f2)) ->
                check_record_fields f1 f2 depth

            (TArray(e1), TArray(e2)) ->
                check_type_equality e1 e2 (depth + 1)

            (TFunction(p1, r1), TFunction(p2, r2)) ->
                (List.len p1 == List.len p2) &&
                (List.map2 p1 p2 \a, b -> check_type_equality a b (depth + 1)
                |> List.all \x -> x) &&
                (check_type_equality r1 r2 (depth + 1))

            (TMu(_), TMu(_)) ->
                # Unroll both and check
                check_type_equality (unroll_recursive t1) (unroll_recursive t2) (depth + 1)

            (TMu(_), _) ->
                check_type_equality (unroll_recursive t1) t2 (depth + 1)

            (_, TMu(_)) ->
                check_type_equality t1 (unroll_recursive t2) (depth + 1)

            _ -> Bool.false

# Check record fields match
check_record_fields : List { label: Str, type: TypeWithRec }, List { label: Str, type: TypeWithRec }, U32 -> Bool
check_record_fields = \fields1, fields2, depth ->
    (List.len fields1 == List.len fields2) &&
    (List.all fields1 \f1 ->
        List.any fields2 \f2 ->
            (f1.label == f2.label) &&
            (check_type_equality f1.type f2.type (depth + 1)))

# === TypeScript Class Example ===

# class Node {
#     value: number;
#     next: Node | null;
#     constructor(value: number) { ... }
#     append(node: Node): void { ... }
# }

example_node_class : TypeWithRec
example_node_class =
    TClass({
        name: "Node",
        self_var: 10,  # Variable for 'this' type
        properties: [
            { label: "value", type: TNumber },
            { label: "next", type: TUnion([TRecVar(10), TNull]) },  # Self-reference!
        ],
        methods: [
            {
                name: "append",
                type: TFunction([TRecVar(10)], TUndefined),  # Takes Node, returns void
            },
        ],
    })

# === Interface with Recursion ===

# interface TreeNode<T> {
#     value: T;
#     children: TreeNode<T>[];
# }

example_tree_interface : TypeWithRec -> TypeWithRec
example_tree_interface = \elem_type ->
    TMu({
        var: 20,  # α for TreeNode
        body: TRecord([
            { label: "value", type: elem_type },
            { label: "children", type: TArray(TRecVar(20)) },  # Array of TreeNode
        ]),
    })

# === JSON Type (Recursive Union) ===

# type JSON =
#   | null
#   | boolean
#   | number
#   | string
#   | JSON[]
#   | { [key: string]: JSON }

make_json_type : {} -> TypeWithRec
make_json_type = \{} ->
    TMu({
        var: 30,  # α for JSON
        body: TUnion([
            TNull,
            TBoolean,
            TNumber,
            TString,
            TArray(TRecVar(30)),  # JSON[]
            TRecord([  # Simplified: just one field for demo
                { label: "_", type: TRecVar(30) },  # {[key: string]: JSON}
            ]),
        ]),
    })

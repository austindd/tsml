module [
    demo_row_polymorphism,
    demonstrate_width_subtyping,
    demonstrate_field_commutation,
]

# Demonstrate key features of MLstruct row polymorphism

TypeId : U32
RowVar : U32

RowType : {
    fields: List { label: Str, type_id: TypeId },
    tail: [Closed, Open RowVar],
}

# === Core Row Operations ===

make_closed_row : List { label: Str, type_id: TypeId } -> RowType
make_closed_row = \fields -> { fields, tail: Closed }

make_open_row : List { label: Str, type_id: TypeId }, RowVar -> RowType
make_open_row = \fields, var -> { fields, tail: Open var }

# === Key Property 1: Width Subtyping ===
# A record with more fields can be used where fewer are expected

demonstrate_width_subtyping : {} -> Str
demonstrate_width_subtyping = \{} ->
    # Point2D type: {x: number, y: number}
    point2d = make_closed_row [
        { label: "x", type_id: 1 },
        { label: "y", type_id: 1 },
    ]

    # Point3D type: {x: number, y: number, z: number}
    point3d = make_closed_row [
        { label: "x", type_id: 1 },
        { label: "y", type_id: 1 },
        { label: "z", type_id: 1 },
    ]

    # Open Point2D: {x: number, y: number, ...ρ}
    open_point2d = make_open_row [
        { label: "x", type_id: 1 },
        { label: "y", type_id: 1 },
    ] 42

    # Check: Can point3d be used where open_point2d is expected?
    # Yes! This is width subtyping - extra fields are allowed
    can_use_3d_as_2d = check_width_subtype point3d open_point2d

    """
    Width Subtyping Demo:
    point2d = {x: T1, y: T1}
    point3d = {x: T1, y: T1, z: T1}
    open_point2d = {x: T1, y: T1, ...ρ42}

    Can use point3d where open_point2d expected? $(Bool.to_str can_use_3d_as_2d)
    This allows functions to work on any record with required fields!
    """

# === Key Property 2: Row Commutation ===
# Field order doesn't matter - {x, y} ≡ {y, x}

demonstrate_field_commutation : {} -> Str
demonstrate_field_commutation = \{} ->
    # Two records with same fields in different order
    xy_order = make_closed_row [
        { label: "x", type_id: 1 },
        { label: "y", type_id: 2 },
    ]

    yx_order = make_closed_row [
        { label: "y", type_id: 2 },
        { label: "x", type_id: 1 },
    ]

    # These should be equivalent
    are_equivalent = check_row_equivalence xy_order yx_order

    """
    Field Commutation Demo:
    {x: T1, y: T2} vs {y: T2, x: T1}
    Are they equivalent? $(Bool.to_str are_equivalent)

    This is crucial for structural typing!
    Field order is irrelevant, only field names and types matter.
    """

# === Key Property 3: Principal Types ===
# The type system finds the most general type

demo_row_polymorphism : {} -> Str
demo_row_polymorphism = \{} ->
    """
    MLstruct Row Polymorphism Features:

    1. WIDTH SUBTYPING
    $(demonstrate_width_subtyping {})

    2. FIELD COMMUTATION
    $(demonstrate_field_commutation {})

    3. PRINCIPAL TYPES
    Consider: function getX(obj) { return obj.x; }

    Most general type: ∀α ρ. {x: α, ...ρ} → α

    This function works on ANY record with an 'x' field:
    - {x: 5} ✓
    - {x: 5, y: 10} ✓
    - {x: "hello", name: "world", z: true} ✓

    4. ROW POLYMORPHIC FUNCTIONS
    function distance(p) {
        return Math.sqrt(p.x * p.x + p.y * p.y);
    }

    Type: ∀ρ. {x: number, y: number, ...ρ} → number

    Works on any record with x and y fields of type number,
    regardless of what other fields it has!

    5. INFERENCE vs TYPESCRIPT
    TypeScript: Requires explicit type annotations or loses precision
    MLstruct: Infers principal (most general) type automatically

    Example TypeScript loses information:
    const first = <T>(x: T, y: T) => x;  // Both params forced to same type

    MLstruct infers correctly:
    first : ∀α β. α → β → α  // Each param can have different type

    This is why MLstruct + Row Polymorphism gives us:
    - Better type inference
    - More flexible code reuse
    - Type-safe structural typing
    - No type annotations needed in many cases
    """

# Helper functions

check_width_subtype : RowType, RowType -> Bool
check_width_subtype = \sub, super ->
    # Check if 'sub' can be used where 'super' is expected
    # All fields in super must exist in sub with same type
    List.all super.fields \req_field ->
        List.any sub.fields \sub_field ->
            sub_field.label == req_field.label && sub_field.type_id == req_field.type_id

check_row_equivalence : RowType, RowType -> Bool
check_row_equivalence = \r1, r2 ->
    # Same number of fields
    same_length = List.len r1.fields == List.len r2.fields

    # All fields in r1 exist in r2 with same type
    all_r1_in_r2 = List.all r1.fields \f1 ->
        List.any r2.fields \f2 ->
            f1.label == f2.label && f1.type_id == f2.type_id

    # All fields in r2 exist in r1 with same type
    all_r2_in_r1 = List.all r2.fields \f2 ->
        List.any r1.fields \f1 ->
            f1.label == f2.label && f1.type_id == f2.type_id

    same_length && all_r1_in_r2 && all_r2_in_r1
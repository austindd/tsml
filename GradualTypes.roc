module [
    GradualType,
    is_consistent_with,
    join_gradual_types,
    meet_gradual_types,
    gradual_subtype,
    apply_cast,
    check_runtime_cast,
]

# Gradual Typing System (any and unknown handling)

TypeId : U32

# Gradual type representation
GradualType : [
    # Fully static types
    GNum,
    GStr,
    GBool,
    GNull,
    GUndefined,
    GObject (List { key: Str, type: GradualType, optional: Bool }),
    GArray GradualType,
    GFunction (List GradualType) GradualType,
    GTuple (List GradualType),
    GUnion (List GradualType),
    GIntersection (List GradualType),
    GLiteral LiteralValue,
    GTypeVar TypeId,

    # Gradual types
    GAny,      # Top type for values (can be anything, no checking)
    GUnknown,  # Top type for static checking (requires narrowing)
    GNever,    # Bottom type (no values)

    # Gradual type operators
    GPlaceholder TypeId,  # For inference
    GBounded GradualType GradualType,  # Lower and upper bounds
]

LiteralValue : [
    LNum F64,
    LStr Str,
    LBool Bool,
]

# Cast representation for runtime checks
Cast : {
    from: GradualType,
    to: GradualType,
    blame_label: Str,  # For tracking cast failures
}

# === Core Gradual Typing Operations ===

# Consistency relation (~)
# Two types are consistent if they might have values in common
is_consistent_with : GradualType, GradualType -> Bool
is_consistent_with = |t1, t2|
    when (t1, t2) is
        # Any is consistent with everything
        (GAny, _) -> Bool.true
        (_, GAny) -> Bool.true

        # Unknown requires explicit checking
        (GUnknown, _) -> Bool.true
        (_, GUnknown) -> Bool.true

        # Never is only consistent with itself and Any/Unknown
        (GNever, GNever) -> Bool.true
        (GNever, _) -> Bool.false
        (_, GNever) -> Bool.false

        # Same types are consistent
        (GNum, GNum) -> Bool.true
        (GStr, GStr) -> Bool.true
        (GBool, GBool) -> Bool.true
        (GNull, GNull) -> Bool.true
        (GUndefined, GUndefined) -> Bool.true

        # Objects are consistent if fields are consistent
        (GObject fields1, GObject fields2) ->
            objects_consistent fields1 fields2

        # Arrays are consistent if elements are consistent
        (GArray elem1, GArray elem2) ->
            is_consistent_with elem1 elem2

        # Functions are consistent if parameters and returns are consistent
        (GFunction params1 ret1, GFunction params2 ret2) ->
            if List.len params1 != List.len params2 then
                Bool.false
            else
                params_consistent =
                    List.map2 params1 params2 |p1, p2|
                        # Contravariant in parameters
                        is_consistent_with p2 p1
                    |> List.all |x| x

                params_consistent && is_consistent_with ret1 ret2

        # Unions are consistent if any member is consistent
        (GUnion members1, _t2) ->
            List.any members1 |m| is_consistent_with m t2

        (_t1, GUnion members2) ->
            List.any members2 |m| is_consistent_with t1 m

        # Intersections are consistent if all members are consistent
        (GIntersection members1, _t2) ->
            List.all members1 |m| is_consistent_with m t2

        (_t1, GIntersection members2) ->
            List.all members2 |m| is_consistent_with t1 m

        # Literals are consistent with their base types
        (GLiteral (LNum _), GNum) -> Bool.true
        (GNum, GLiteral (LNum _)) -> Bool.true
        (GLiteral (LStr _), GStr) -> Bool.true
        (GStr, GLiteral (LStr _)) -> Bool.true
        (GLiteral (LBool _), GBool) -> Bool.true
        (GBool, GLiteral (LBool _)) -> Bool.true

        # Literals are consistent with same value
        (GLiteral l1, GLiteral l2) ->
            literals_equal l1 l2

        _ -> Bool.false

# Check if objects are consistent
objects_consistent : List { key: Str, type: GradualType, optional: Bool }, List { key: Str, type: GradualType, optional: Bool } -> Bool
objects_consistent = |fields1, fields2|
    # For each required field in fields2, check consistency in fields1
    List.all fields2 |f2|
        if f2.optional then
            Bool.true  # Optional fields don't need to exist
        else
            when List.find_first fields1 |f1| f1.key == f2.key is
                Ok f1 -> is_consistent_with f1.type f2.type
                Err _ -> Bool.false  # Required field missing

# === Gradual Subtyping ===

# Gradual subtype relation (≤~)
gradual_subtype : GradualType, GradualType -> Bool
gradual_subtype = |sub, super|
    when (sub, super) is
        # Any and Unknown at top
        (_, GAny) -> Bool.true
        (_, GUnknown) -> Bool.true

        # Never at bottom
        (GNever, _) -> Bool.true

        # Any is not a subtype of concrete types (except Any/Unknown)
        (GAny, GAny) -> Bool.true
        (GAny, GUnknown) -> Bool.true
        (GAny, _) -> Bool.false

        # Unknown requires explicit narrowing
        (GUnknown, GUnknown) -> Bool.true
        (GUnknown, GAny) -> Bool.true
        (GUnknown, _) -> Bool.false

        # Standard subtyping for concrete types
        (GNum, GNum) -> Bool.true
        (GStr, GStr) -> Bool.true
        (GBool, GBool) -> Bool.true
        (GNull, GNull) -> Bool.true
        (GUndefined, GUndefined) -> Bool.true

        # Literals are subtypes of their base types
        (GLiteral (LNum _), GNum) -> Bool.true
        (GLiteral (LStr _), GStr) -> Bool.true
        (GLiteral (LBool _), GBool) -> Bool.true

        # Object subtyping (width and depth)
        (GObject fields_sub, GObject fields_super) ->
            object_subtype fields_sub fields_super

        # Array covariance
        (GArray elem_sub, GArray elem_super) ->
            gradual_subtype elem_sub elem_super

        # Function subtyping (contravariant in params, covariant in return)
        (GFunction params_sub ret_sub, GFunction params_super ret_super) ->
            if List.len params_sub != List.len params_super then
                Bool.false
            else
                params_ok =
                    List.map2 params_super params_sub |p_super, p_sub|
                        gradual_subtype p_super p_sub  # Contravariant
                    |> List.all |x| x

                params_ok && gradual_subtype ret_sub ret_super

        # Union subtyping
        (GUnion members_sub, _super) ->
            List.all members_sub |m| gradual_subtype m super

        (_sub, GUnion members_super) ->
            List.any members_super |m| gradual_subtype sub m

        # Intersection subtyping
        (GIntersection members_sub, _super) ->
            List.any members_sub |m| gradual_subtype m super

        (_sub, GIntersection members_super) ->
            List.all members_super |m| gradual_subtype sub m

        _ -> Bool.false

# Object subtyping (structural)
object_subtype : List { key: Str, type: GradualType, optional: Bool }, List { key: Str, type: GradualType, optional: Bool } -> Bool
object_subtype = |fields_sub, fields_super|
    # Subtype must have all required fields of supertype
    List.all fields_super |f_super|
        if f_super.optional then
            # Optional field - if present, must be subtype
            when List.find_first fields_sub |f| f.key == f_super.key is
                Ok f_sub -> gradual_subtype f_sub.type f_super.type
                Err _ -> Bool.true  # Optional field can be missing
        else
            # Required field must exist and be subtype
            when List.find_first fields_sub |f| f.key == f_super.key is
                Ok f_sub -> gradual_subtype f_sub.type f_super.type
                Err _ -> Bool.false

# === Join and Meet Operations ===

# Join (least upper bound) - finds common supertype
join_gradual_types : GradualType, GradualType -> GradualType
join_gradual_types = |t1, t2|
    when (t1, t2) is
        # Any absorbs everything in join
        (GAny, _) -> GAny
        (_, GAny) -> GAny

        # Unknown is supertype of all static types
        (GUnknown, _) -> GUnknown
        (_, GUnknown) -> GUnknown

        # Never is identity for join
        (GNever, t) -> t
        (t, GNever) -> t

        # Same types
        (GNum, GNum) -> GNum
        (GStr, GStr) -> GStr
        (GBool, GBool) -> GBool
        (GNull, GNull) -> GNull
        (GUndefined, GUndefined) -> GUndefined

        # Literals join to their base type
        (GLiteral (LNum _), GLiteral (LNum _)) -> GNum
        (GLiteral (LNum _), GNum) -> GNum
        (GNum, GLiteral (LNum _)) -> GNum

        (GLiteral (LStr _), GLiteral (LStr _)) -> GStr
        (GLiteral (LStr _), GStr) -> GStr
        (GStr, GLiteral (LStr _)) -> GStr

        (GLiteral (LBool _), GLiteral (LBool _)) -> GBool
        (GLiteral (LBool _), GBool) -> GBool
        (GBool, GLiteral (LBool _)) -> GBool

        # Objects - join common fields
        (GObject fields1, GObject fields2) ->
            GObject (join_object_fields fields1 fields2)

        # Arrays
        (GArray elem1, GArray elem2) ->
            GArray (join_gradual_types elem1 elem2)

        # Different types become union
        _ -> GUnion [t1, t2]

# Meet (greatest lower bound) - finds common subtype
meet_gradual_types : GradualType, GradualType -> GradualType
meet_gradual_types = |t1, t2|
    when (t1, t2) is
        # Never absorbs everything in meet
        (GNever, _) -> GNever
        (_, GNever) -> GNever

        # Any/Unknown are identity for meet with static types
        (GAny, t) -> t
        (t, GAny) -> t
        (GUnknown, t) -> t
        (t, GUnknown) -> t

        # Same types
        (GNum, GNum) -> GNum
        (GStr, GStr) -> GStr
        (GBool, GBool) -> GBool
        (GNull, GNull) -> GNull
        (GUndefined, GUndefined) -> GUndefined

        # Objects - meet all fields
        (GObject fields1, GObject fields2) ->
            GObject (meet_object_fields fields1 fields2)

        # Arrays
        (GArray elem1, GArray elem2) ->
            GArray (meet_gradual_types elem1 elem2)

        # Different types have no meet
        _ -> GNever

# === Cast Operations ===

# Apply a cast from one type to another
apply_cast : GradualType, GradualType, Str -> Cast
apply_cast = |from, to, blame|
    {
        from: from,
        to: to,
        blame_label: blame,
    }

# Check if a runtime cast would succeed
check_runtime_cast : Cast, GradualType -> Result {} [CastError Str]
check_runtime_cast = |cast, actual_type|
    if is_consistent_with actual_type cast.to then
        Ok {}
    else
        Err (CastError "Cast failed at $(cast.blame_label): expected $(type_to_str cast.to), got $(type_to_str actual_type)")

# === Unknown Type Handling ===

# Narrow unknown type with type guard
narrow_unknown : GradualType, GradualType -> GradualType
narrow_unknown = |unknown_type, guard|
    when unknown_type is
        GUnknown -> guard  # Unknown narrows to the guard type
        GUnion members ->
            # Narrow each member that could be unknown
            GUnion (List.map members |m| narrow_unknown m guard)
        _ -> unknown_type  # Other types unchanged

# Check if value needs runtime checking (has Any or Unknown)
needs_runtime_check : GradualType -> Bool
needs_runtime_check = |typ|
    when typ is
        GAny -> Bool.true
        GUnknown -> Bool.true
        GObject fields -> List.any fields |f| needs_runtime_check f.type
        GArray elem -> needs_runtime_check elem
        GFunction params ret ->
            List.any params needs_runtime_check || needs_runtime_check ret
        GUnion members -> List.any members needs_runtime_check
        GIntersection members -> List.any members needs_runtime_check
        _ -> Bool.false

# === Helper Functions ===

# Join object fields
join_object_fields : List { key: Str, type: GradualType, optional: Bool }, List { key: Str, type: GradualType, optional: Bool } -> List { key: Str, type: GradualType, optional: Bool }
join_object_fields = |fields1, fields2|
    # Keep only common fields, make optional if not in both
    common_keys = List.keep_oks fields1 |f1|
        when List.find_first fields2 |f2| f2.key == f1.key is
            Ok f2 -> Ok (f1.key, f1, f2)
            Err _ -> Err NotCommon

    List.map common_keys |(key, f1, f2)|
        {
            key: key,
            type: join_gradual_types f1.type f2.type,
            optional: f1.optional || f2.optional,
        }

# Meet object fields
meet_object_fields : List { key: Str, type: GradualType, optional: Bool }, List { key: Str, type: GradualType, optional: Bool } -> List { key: Str, type: GradualType, optional: Bool }
meet_object_fields = |fields1, fields2|
    # Union of all fields
    all_fields = List.concat fields1 fields2

    # Deduplicate and meet types for common fields
    List.walk all_fields [] |acc, field|
        when List.find_first acc |f| f.key == field.key is
            Ok existing ->
                # Update existing field with meet of types
                List.map acc |f|
                    if f.key == field.key then
                        {
                            key: f.key,
                            type: meet_gradual_types f.type field.type,
                            optional: f.optional && field.optional,
                        }
                    else
                        f
            Err _ ->
                List.append acc field

# Check literal equality
literals_equal : LiteralValue, LiteralValue -> Bool
literals_equal = |l1, l2|
    when (l1, l2) is
        (LNum n1, LNum n2) -> Num.is_approx_eq n1 n2 {}
        (LStr s1, LStr s2) -> s1 == s2
        (LBool b1, LBool b2) -> b1 == b2
        _ -> Bool.false

# Convert type to string for error messages
type_to_str : GradualType -> Str
type_to_str = |typ|
    when typ is
        GAny -> "any"
        GUnknown -> "unknown"
        GNever -> "never"
        GNum -> "number"
        GStr -> "string"
        GBool -> "boolean"
        GNull -> "null"
        GUndefined -> "undefined"
        GObject _ -> "object"
        GArray _ -> "array"
        GFunction _ _ -> "function"
        GUnion _ -> "union"
        GIntersection _ -> "intersection"
        GLiteral (LNum n) -> Num.to_str n
        GLiteral (LStr s) -> "\"$(s)\""
        GLiteral (LBool b) -> if b then "true" else "false"
        _ -> "type"

# === Examples ===

# any type - can be assigned from/to anything
example_any : GradualType
example_any = GAny

# unknown type - requires narrowing before use
example_unknown : GradualType
example_unknown = GUnknown

# Function accepting any
example_any_function : GradualType
example_any_function = GFunction [GAny] GAny

# Function accepting unknown (requires runtime check)
example_unknown_function : GradualType
example_unknown_function = GFunction [GUnknown] GStr

# Gradual object type
example_gradual_object : GradualType
example_gradual_object =
    GObject [
        { key: "name", type: GStr, optional: Bool.false },
        { key: "data", type: GAny, optional: Bool.false },  # Can hold anything
        { key: "metadata", type: GUnknown, optional: Bool.true },  # Requires checking when used
    ]

# Type with runtime checking needed
example_mixed_type : GradualType
example_mixed_type =
    GUnion [
        GNum,
        GStr,
        GAny,  # This branch needs runtime checking
    ]

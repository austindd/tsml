module [
    NarrowedType,
    TypeGuard,
    narrow_type,
    analyze_control_flow,
    check_exhaustiveness,
]

# Control Flow Type Narrowing Implementation

TypeId : U32

# Type representation with narrowing support
NarrowableType : [
    NNum,
    NStr,
    NBool,
    NNull,
    NUndefined,
    NObject (List { key: Str, type: TypeId }),
    NArray TypeId,
    NUnion (List NarrowableType),
    NLiteral LiteralValue,
    NTypeVar TypeId,
    NUnknown,
    NAny,
    NNever,
]

LiteralValue : [
    LNum F64,
    LStr Str,
    LBool Bool,
]

# Narrowed type with control flow context
NarrowedType : {
    original: NarrowableType,
    narrowed: NarrowableType,
    guards: List TypeGuard,
}

# Type guard representations
TypeGuard : [
    # typeof checks
    TypeofGuard {
        variable: Str,
        expected: Str,  # "string", "number", "boolean", etc.
        negated: Bool,
    },

    # instanceof checks
    InstanceofGuard {
        variable: Str,
        constructor: Str,
        negated: Bool,
    },

    # Truthiness checks
    TruthinessGuard {
        variable: Str,
        negated: Bool,
    },

    # Null/undefined checks
    NullishGuard {
        variable: Str,
        is_null: Bool,
        is_undefined: Bool,
        negated: Bool,
    },

    # Property existence checks ('in' operator)
    PropertyGuard {
        property: Str,
        object: Str,
        negated: Bool,
    },

    # Equality checks
    EqualityGuard {
        variable: Str,
        value: LiteralValue,
        strict: Bool,
        negated: Bool,
    },

    # Custom type predicate (user-defined type guard)
    PredicateGuard {
        variable: Str,
        predicate: Str,  # Function name
        negated: Bool,
    },
]

# Control flow context
ControlFlowContext : {
    # Active type guards in current branch
    guards: List TypeGuard,

    # Variable type mappings
    types: List { name: Str, type: NarrowableType },

    # Reachability
    reachable: Bool,
}

# === Core Narrowing Operations ===

# Apply a type guard to narrow a type
narrow_type : NarrowableType, TypeGuard -> NarrowableType
narrow_type = \typ, guard ->
    when guard is
        TypeofGuard data ->
            narrow_by_typeof typ data.expected data.negated

        InstanceofGuard data ->
            narrow_by_instanceof typ data.constructor data.negated

        TruthinessGuard data ->
            narrow_by_truthiness typ data.negated

        NullishGuard data ->
            narrow_by_nullish typ data.is_null data.is_undefined data.negated

        PropertyGuard data ->
            narrow_by_property typ data.property data.negated

        EqualityGuard data ->
            narrow_by_equality typ data.value data.strict data.negated

        PredicateGuard _ ->
            # Custom predicates would need external function info
            typ

# Narrow type by typeof check
narrow_by_typeof : NarrowableType, Str, Bool -> NarrowableType
narrow_by_typeof = \typ, expected, negated ->
    when typ is
        NUnion members ->
            filtered = if negated then
                List.keep_if members \m -> Bool.not (matches_typeof m expected)
            else
                List.keep_if members \m -> matches_typeof m expected

            when filtered is
                [] -> NNever
                [single] -> single
                multiple -> NUnion multiple

        _ ->
            if negated then
                if matches_typeof typ expected then NNever else typ
            else
                if matches_typeof typ expected then typ else NNever

# Check if type matches typeof result
matches_typeof : NarrowableType, Str -> Bool
matches_typeof = \typ, expected ->
    when (typ, expected) is
        (NNum, "number") -> Bool.true
        (NStr, "string") -> Bool.true
        (NBool, "boolean") -> Bool.true
        (NUndefined, "undefined") -> Bool.true
        (NNull, "object") -> Bool.true  # typeof null === "object" (JS quirk)
        (NObject _, "object") -> Bool.true
        (NArray _, "object") -> Bool.true
        (NLiteral (LNum _), "number") -> Bool.true
        (NLiteral (LStr _), "string") -> Bool.true
        (NLiteral (LBool _), "boolean") -> Bool.true
        _ -> Bool.false

# Narrow type by instanceof check
narrow_by_instanceof : NarrowableType, Str, Bool -> NarrowableType
narrow_by_instanceof = \typ, constructor, negated ->
    # Simplified: would check against constructor type
    if negated then
        typ  # Can't narrow negated instanceof much
    else
        when constructor is
            "Array" ->
                when typ is
                    NArray _ -> typ
                    NUnion members ->
                        arrays = List.keep_if members \m ->
                            when m is
                                NArray _ -> Bool.true
                                _ -> Bool.false
                        when arrays is
                            [] -> NNever
                            [single] -> single
                            multiple -> NUnion multiple
                    _ -> NNever
            _ -> typ

# Narrow type by truthiness
narrow_by_truthiness : NarrowableType, Bool -> NarrowableType
narrow_by_truthiness = \typ, negated ->
    when typ is
        NUnion members ->
            filtered = if negated then
                # Keep falsy values
                List.keep_if members is_falsy
            else
                # Remove falsy values
                List.keep_if members \m -> Bool.not (is_falsy m)

            when filtered is
                [] -> NNever
                [single] -> single
                multiple -> NUnion multiple

        _ ->
            if negated then
                if is_falsy typ then typ else NNever
            else
                if is_falsy typ then NNever else typ

# Check if type is falsy
is_falsy : NarrowableType -> Bool
is_falsy = \typ ->
    when typ is
        NNull -> Bool.true
        NUndefined -> Bool.true
        NLiteral (LBool(false)) -> Bool.true
        NLiteral (LNum(0.0)) -> Bool.true
        NLiteral (LStr("")) -> Bool.true
        _ -> Bool.false

# Narrow by null/undefined checks
narrow_by_nullish : NarrowableType, Bool, Bool, Bool -> NarrowableType
narrow_by_nullish = \typ, check_null, check_undefined, negated ->
    when typ is
        NUnion members ->
            filtered = List.keep_if members \m ->
                is_match = (check_null && matches_null m) ||
                          (check_undefined && matches_undefined m)
                if negated then Bool.not is_match else is_match

            when filtered is
                [] -> NNever
                [single] -> single
                multiple -> NUnion multiple

        _ ->
            is_nullish = (check_null && matches_null typ) ||
                        (check_undefined && matches_undefined typ)

            if negated then
                if is_nullish then NNever else typ
            else
                if is_nullish then typ else NNever

# Check type matches null
matches_null : NarrowableType -> Bool
matches_null = \typ ->
    when typ is
        NNull -> Bool.true
        _ -> Bool.false

# Check type matches undefined
matches_undefined : NarrowableType -> Bool
matches_undefined = \typ ->
    when typ is
        NUndefined -> Bool.true
        _ -> Bool.false

# Narrow by property existence
narrow_by_property : NarrowableType, Str, Bool -> NarrowableType
narrow_by_property = \typ, property, negated ->
    when typ is
        NObject props ->
            has_prop = List.any props \p -> p.key == property
            if negated then
                if has_prop then NNever else typ
            else
                typ  # Has the property

        NUnion members ->
            filtered = List.keep_if members \m ->
                when m is
                    NObject props ->
                        has_prop = List.any props \p -> p.key == property
                        if negated then Bool.not has_prop else has_prop
                    _ -> negated  # Non-objects don't have properties

            when filtered is
                [] -> NNever
                [single] -> single
                multiple -> NUnion multiple

        _ ->
            if negated then typ else NNever

# Narrow by equality
narrow_by_equality : NarrowableType, LiteralValue, Bool, Bool -> NarrowableType
narrow_by_equality = \typ, value, strict, negated ->
    when typ is
        NLiteral lit ->
            matches = if strict then
                literals_equal lit value
            else
                literals_loose_equal lit value

            if negated then
                if matches then NNever else typ
            else
                if matches then typ else NNever

        NUnion members ->
            filtered = List.keep_if members \m ->
                when m is
                    NLiteral lit ->
                        matches = if strict then
                            literals_equal lit value
                        else
                            literals_loose_equal lit value
                        if negated then Bool.not matches else matches
                    _ ->
                        negated  # Non-literals can't equal literal value

            when filtered is
                [] -> NNever
                [single] -> single
                multiple -> NUnion multiple

        _ ->
            if negated then typ else NNever

# Check literal equality (strict)
literals_equal : LiteralValue, LiteralValue -> Bool
literals_equal = \l1, l2 ->
    when (l1, l2) is
        (LNum n1, LNum n2) -> Num.compare(n1, n2) == EQ
        (LStr s1, LStr s2) -> s1 == s2
        (LBool b1, LBool b2) -> b1 == b2
        _ -> Bool.false

# Check literal equality (loose, with coercion)
literals_loose_equal : LiteralValue, LiteralValue -> Bool
literals_loose_equal = \l1, l2 ->
    when (l1, l2) is
        # Same type - use strict equality
        (LNum n1, LNum n2) -> Num.compare(n1, n2) == EQ
        (LStr s1, LStr s2) -> s1 == s2
        (LBool b1, LBool b2) -> b1 == b2

        # Coercion cases
        (LNum 0.0, LBool(false)) -> Bool.true
        (LBool(false), LNum 0.0) -> Bool.true
        (LNum 1.0, LBool(true)) -> Bool.true
        (LBool(true), LNum 1.0) -> Bool.true

        _ -> Bool.false

# === Control Flow Analysis ===

# Analyze control flow for type narrowing
analyze_control_flow : List TypeGuard, ControlFlowContext -> ControlFlowContext
analyze_control_flow = \guards, context ->
    # Apply each guard to narrow types
    narrowed_types = List.map context.types \binding ->
        narrowed_type = List.walk guards binding.type \acc, guard ->
            # Only apply guard if it matches this variable
            if guard_applies_to_var guard binding.name then
                narrow_type acc guard
            else
                acc

        { binding & type: narrowed_type }

    { context &
        guards: List.concat context.guards guards,
        types: narrowed_types
    }

# Check if guard applies to variable
guard_applies_to_var : TypeGuard, Str -> Bool
guard_applies_to_var = \guard, var_name ->
    when guard is
        TypeofGuard data -> data.variable == var_name
        InstanceofGuard data -> data.variable == var_name
        TruthinessGuard data -> data.variable == var_name
        NullishGuard data -> data.variable == var_name
        PropertyGuard data -> data.object == var_name
        EqualityGuard data -> data.variable == var_name
        PredicateGuard data -> data.variable == var_name

# === Exhaustiveness Checking ===

# Check if a switch/if-else chain is exhaustive
check_exhaustiveness : NarrowableType, List NarrowableType -> Bool
check_exhaustiveness = \original_type, covered_cases ->
    # Check if union of covered cases equals original type
    when original_type is
        NUnion members ->
            # Each member must be covered
            List.all members \member ->
                List.any covered_cases \case ->
                    types_overlap member case

        _ ->
            # Single type - check if covered
            List.any covered_cases \case ->
                types_overlap original_type case

# Check if two types overlap
types_overlap : NarrowableType, NarrowableType -> Bool
types_overlap = \t1, t2 ->
    when (t1, t2) is
        (NAny, _) -> Bool.true
        (_, NAny) -> Bool.true
        (NNever, _) -> Bool.false
        (_, NNever) -> Bool.false
        (NNum, NNum) -> Bool.true
        (NStr, NStr) -> Bool.true
        (NBool, NBool) -> Bool.true
        (NNull, NNull) -> Bool.true
        (NUndefined, NUndefined) -> Bool.true
        (NLiteral l1, NLiteral l2) -> literals_equal l1 l2
        (NUnion members, _) ->
            List.any members \m -> types_overlap m t2
        (_, NUnion members) ->
            List.any members \m -> types_overlap t1 m
        _ -> Bool.false

# === Examples ===

# Example: typeof narrowing
# if (typeof x === 'string') { /* x is string */ }
example_typeof_narrowing : NarrowableType -> NarrowableType
example_typeof_narrowing = \x_type ->
    guard = TypeofGuard {
        variable: "x",
        expected: "string",
        negated: Bool.false,
    }
    narrow_type x_type guard

# Example: null check narrowing
# if (x != null) { /* x is non-null */ }
example_null_check : NarrowableType -> NarrowableType
example_null_check = \x_type ->
    guard = NullishGuard {
        variable: "x",
        is_null: Bool.true,
        is_undefined: Bool.true,
        negated: Bool.true,
    }
    narrow_type x_type guard

# Example: discriminated union narrowing
# type Result = { success: true, data: T } | { success: false, error: string }
# if (result.success) { /* result is { success: true, data: T } */ }
example_discriminated_union : NarrowableType -> NarrowableType
example_discriminated_union = \result_type ->
    guard = PropertyGuard {
        property: "success",
        object: "result",
        negated: Bool.false,
    }
    narrowed = narrow_type result_type guard

    # Further narrow by truthiness of success property
    truthiness_guard = TruthinessGuard {
        variable: "result.success",
        negated: Bool.false,
    }
    narrow_type narrowed truthiness_guard

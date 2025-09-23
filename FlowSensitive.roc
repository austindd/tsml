module [
    TypeRefinement,
    refine_type,
    analyze_type_guard,
    TypeGuard,
]

import MinimalType exposing [TType]
import Ast exposing [Node, BinaryOperator]

# Type refinement based on control flow
TypeRefinement : {
    variable : Str,
    refined_type : TType,
}

# Type guards that refine types
TypeGuard : [
    TypeofGuard { variable : Str, type_name : Str },
    EqualityGuard { variable : Str, value_type : TType },
    TruthinessGuard { variable : Str, is_truthy : Bool },
    InstanceofGuard { variable : Str, constructor : Str },
    NoGuard,
]

# Analyze a condition to extract type guards
analyze_type_guard : Node -> TypeGuard
analyze_type_guard = \node ->
    when node is
        # typeof checks: typeof x === 'string'
        BinaryExpression {
            operator: EqualEqualEqual,
            left: UnaryExpression {
                operator: Typeof,
                argument: Identifier { name: var_name }
            },
            right: StringLiteral { value: type_str }
        } -> TypeofGuard { variable: var_name, type_name: type_str }

        # typeof checks with loose equality: typeof x == 'number'
        BinaryExpression {
            operator: EqualEqual,
            left: UnaryExpression {
                operator: Typeof,
                argument: Identifier { name: var_name }
            },
            right: StringLiteral { value: type_str }
        } -> TypeofGuard { variable: var_name, type_name: type_str }

        # Negated typeof: typeof x !== 'undefined'
        BinaryExpression {
            operator: BangEqualEqual,
            left: UnaryExpression {
                operator: Typeof,
                argument: Identifier { name: var_name }
            },
            right: StringLiteral { value: "undefined" }
        } -> TruthinessGuard { variable: var_name, is_truthy: Bool.true }

        # Direct equality checks: x === null
        BinaryExpression {
            operator: EqualEqualEqual,
            left: Identifier { name: var_name },
            right: NullLiteral _
        } -> EqualityGuard { variable: var_name, value_type: TUnknown }

        # Direct equality checks: x === true/false
        BinaryExpression {
            operator: EqualEqualEqual,
            left: Identifier { name: var_name },
            right: BooleanLiteral _
        } -> EqualityGuard { variable: var_name, value_type: TBool }

        # Truthiness check: if (x) or while (x)
        Identifier { name: var_name } ->
            TruthinessGuard { variable: var_name, is_truthy: Bool.true }

        # Negated truthiness: if (!x)
        UnaryExpression {
            operator: Bang,
            argument: Identifier { name: var_name }
        } -> TruthinessGuard { variable: var_name, is_truthy: Bool.false }

        _ -> NoGuard

# Refine a variable's type based on a type guard
refine_type : TType, TypeGuard -> TType
refine_type = \current_type, guard ->
    when guard is
        TypeofGuard { type_name } ->
            when type_name is
                "string" -> TStr
                "number" -> TNum
                "boolean" -> TBool
                "undefined" -> TUnknown
                "object" ->
                    # In JS, typeof null === 'object'
                    if current_type == TUnknown then TUnknown else current_type
                _ -> current_type

        EqualityGuard { value_type } -> value_type

        TruthinessGuard { is_truthy } ->
            if is_truthy then
                # If truthy, can't be null/undefined/0/false/""
                when current_type is
                    TUnknown -> TUnknown  # Still could be various truthy values
                    _ -> current_type
            else
                # If falsy, likely null/undefined
                TUnknown

        InstanceofGuard { constructor } ->
            # Would map to specific types based on constructor
            when constructor is
                "Array" -> TUnknown  # Would be array type
                "String" -> TStr
                "Number" -> TNum
                "Boolean" -> TBool
                _ -> current_type

        NoGuard -> current_type
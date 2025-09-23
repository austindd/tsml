module [
    infer_type,
]

import MinimalType exposing [TType]
import Ast exposing [Node]

infer_type : Node -> TType
infer_type = \node ->
    when node is
        NumberLiteral _ -> TNum
        StringLiteral _ -> TStr
        BooleanLiteral _ -> TBool
        NullLiteral _ -> TUnknown
        UndefinedLiteral _ -> TUnknown

        BinaryExpression { operator } ->
            when operator is
                Plus | Minus | Star | Slash | Percent -> TNum
                EqualEqual | BangEqual | LessThan | GreaterThan | LessThanEqual | GreaterThanEqual -> TBool
                _ -> TUnknown

        ArrayExpression _ -> TUnknown  # Would be array type
        ObjectExpression _ -> TUnknown  # Would be object type

        Program { body } ->
            when body is
                [] -> TUnknown
                [single] -> infer_type single
                [first, .. as rest] ->
                    # Just infer the last expression for now
                    when List.last rest is
                        Ok last -> infer_type last
                        Err _ -> infer_type first

        BlockStatement { body: block_body } ->
            when block_body is
                [] -> TUnknown
                [.. as init, last] -> infer_type last

        FunctionBody { body: fn_body } ->
            when fn_body is
                [] -> TUnknown
                [.. as init, last] -> infer_type last

        _ -> TUnknown
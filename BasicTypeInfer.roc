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

        BinaryExpression { operator, left, right } ->
            when operator is
                # Arithmetic
                Plus ->
                    # Plus can be string concatenation or numeric addition
                    left_type = infer_type left
                    right_type = infer_type right
                    when (left_type, right_type) is
                        (TStr, _) | (_, TStr) -> TStr  # String concatenation
                        _ -> TNum  # Numeric addition
                Minus | Star | Slash | Percent -> TNum
                # Comparison - always returns boolean
                EqualEqual | BangEqual | LessThan | GreaterThan | LessThanEqual | GreaterThanEqual -> TBool
                EqualEqualEqual | BangEqualEqual -> TBool
                # Bitwise - always returns number
                Pipe | Caret | Ampersand | LeftShift | RightShift | UnsignedRightShift -> TNum
                # Logical operators (in binary context)
                In | Instanceof -> TBool
                _ -> TUnknown

        # Unary expressions
        UnaryExpression { operator, argument } ->
            when operator is
                Bang -> TBool  # !expr always returns boolean
                Plus | Minus | Tilde -> TNum  # Numeric operators
                Typeof -> TStr  # typeof always returns string
                Void -> TUnknown  # void always returns undefined
                Delete -> TBool  # delete returns boolean
                _ -> TUnknown

        # Update expressions
        UpdateExpression _ -> TNum  # ++ and -- always work with numbers

        # Logical expressions
        LogicalExpression { operator, left, right } ->
            when operator is
                LogicalAnd ->
                    # && returns right if left is truthy, else left
                    left_type = infer_type left
                    right_type = infer_type right
                    when (left_type, right_type) is
                        (TBool, TBool) -> TBool
                        (TBool, other) | (_, other) -> other
                LogicalOr ->
                    # || returns left if truthy, else right
                    # Since we can't determine truthiness statically, return right type for non-booleans
                    left_type = infer_type left
                    right_type = infer_type right
                    when (left_type, right_type) is
                        (TBool, TBool) -> TBool
                        (TNum, other) -> other  # Numbers might be falsy (0, NaN)
                        (TStr, _) -> TStr  # Non-empty strings are truthy
                        _ -> right_type  # Conservative: assume might be falsy
                _ -> TUnknown

        # Conditional expression (ternary)
        ConditionalExpression { test, consequent, alternate } ->
            # The type is the union of consequent and alternate types
            # For now, just check if they match
            cons_type = infer_type consequent
            alt_type = infer_type alternate
            if cons_type == alt_type then cons_type else TUnknown

        # Identifiers and literals
        Identifier { name } ->
            # Some well-known global identifiers
            when name is
                "undefined" -> TUnknown
                "NaN" | "Infinity" -> TNum
                _ -> TUnknown  # Would need symbol table
        ThisExpression _ -> TUnknown  # Would be object type
        ArrayExpression _ -> TUnknown  # Would be array type
        ObjectExpression _ -> TUnknown  # Would be object type

        # Functions
        FunctionExpression _ -> TUnknown  # Would be function type
        ArrowFunctionExpression _ -> TUnknown  # Would be function type
        CallExpression _ -> TUnknown  # Would need function return type
        NewExpression _ -> TUnknown  # Would be constructor return type

        # Member access
        MemberExpression { object, property, computed } ->
            # Special case for array.length and string.length
            when property is
                Identifier { name: "length" } -> TNum
                _ -> TUnknown  # Would need object property types

        # Assignment
        AssignmentExpression { right } ->
            # Return type of right side
            infer_type right

        # Sequence
        SequenceExpression { expressions } ->
            when List.last expressions is
                Ok last -> infer_type last
                Err _ -> TUnknown

        # Template literals
        TemplateLiteral _ -> TStr
        TaggedTemplateExpression _ -> TUnknown  # Depends on tag function

        Program { body } ->
            when body is
                [] -> TUnknown
                [single] -> infer_type single
                [first, .. as rest] ->
                    # Just infer the last expression for now
                    when List.last rest is
                        Ok last -> infer_type last
                        Err _ -> infer_type first

        # Statements (most return undefined/unknown)
        BlockStatement { body: block_body } ->
            when block_body is
                [] -> TUnknown
                [.., last] -> infer_type last

        ReturnStatement _ -> TUnknown  # Would need argument handling

        VariableDeclaration _ -> TUnknown  # Statements don't have values
        FunctionDeclaration _ -> TUnknown
        ClassDeclaration _ -> TUnknown
        IfStatement _ -> TUnknown
        WhileStatement _ -> TUnknown
        DoWhileStatement _ -> TUnknown
        ForStatement _ -> TUnknown
        ForInStatement _ -> TUnknown
        ForOfStatement _ -> TUnknown
        SwitchStatement _ -> TUnknown
        ThrowStatement _ -> TUnknown
        TryStatement _ -> TUnknown
        WithStatement _ -> TUnknown
        DebuggerStatement _ -> TUnknown
        EmptyStatement _ -> TUnknown
        LabeledStatement _ -> TUnknown
        BreakStatement _ -> TUnknown
        ContinueStatement _ -> TUnknown

        FunctionBody { body: fn_body } ->
            when fn_body is
                [] -> TUnknown
                [.., last] -> infer_type last

        # Patterns (for destructuring)
        ArrayPattern _ -> TUnknown
        ObjectPattern _ -> TUnknown
        RestElement _ -> TUnknown
        AssignmentPattern _ -> TUnknown

        # Other literals
        RegExpLiteral _ -> TUnknown  # Would be RegExp type
        BigIntLiteral _ -> TNum  # Simplified - would be bigint type

        # Import/Export
        ExportDefaultDeclaration _ -> TUnknown
        ExportNamedDeclaration _ -> TUnknown
        ExportAllDeclaration _ -> TUnknown
        ImportDeclaration _ -> TUnknown

        # Async
        AwaitExpression _ -> TUnknown
        YieldExpression _ -> TUnknown

        _ -> TUnknown
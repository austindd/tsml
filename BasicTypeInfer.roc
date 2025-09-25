module [
    infer_type,
]

import SimpleComprehensiveType as Type exposing [Type]
import Ast exposing [Node]
import JSTypeCoercion

infer_type : Node -> Type
infer_type = \node ->
    when node is
        NumberLiteral _ -> TNumber
        StringLiteral _ -> TString
        BooleanLiteral _ -> TBoolean
        NullLiteral _ -> TUnknown
        UndefinedLiteral _ -> TUnknown

        BinaryExpression { operator, left, right } ->
            left_type = infer_type left
            right_type = infer_type right
            when operator is
                # Arithmetic
                Plus -> JSTypeCoercion.infer_binary_op left_type right_type "+"
                Minus -> JSTypeCoercion.infer_binary_op left_type right_type "-"
                Star -> JSTypeCoercion.infer_binary_op left_type right_type "*"
                Slash -> JSTypeCoercion.infer_binary_op left_type right_type "/"
                Percent -> JSTypeCoercion.infer_binary_op left_type right_type "%"
                # Comparison
                LessThan -> JSTypeCoercion.infer_binary_op left_type right_type "<"
                LessThanEqual -> JSTypeCoercion.infer_binary_op left_type right_type "<="
                GreaterThan -> JSTypeCoercion.infer_binary_op left_type right_type ">"
                GreaterThanEqual -> JSTypeCoercion.infer_binary_op left_type right_type ">="
                # Equality
                EqualEqual -> JSTypeCoercion.infer_equality left_type right_type "=="
                BangEqual -> JSTypeCoercion.infer_equality left_type right_type "!="
                EqualEqualEqual -> JSTypeCoercion.infer_equality left_type right_type "==="
                BangEqualEqual -> JSTypeCoercion.infer_equality left_type right_type "!=="
                # Bitwise
                Pipe -> JSTypeCoercion.infer_binary_op left_type right_type "|"
                Caret -> JSTypeCoercion.infer_binary_op left_type right_type "^"
                Ampersand -> JSTypeCoercion.infer_binary_op left_type right_type "&"
                LeftShift -> JSTypeCoercion.infer_binary_op left_type right_type "<<"
                RightShift -> JSTypeCoercion.infer_binary_op left_type right_type ">>"
                UnsignedRightShift -> JSTypeCoercion.infer_binary_op left_type right_type ">>>"
                # Special
                In -> JSTypeCoercion.infer_binary_op left_type right_type "in"
                Instanceof -> JSTypeCoercion.infer_binary_op left_type right_type "instanceof"
                _ -> TUnknown

        # Unary expressions
        UnaryExpression { operator, argument } ->
            when operator is
                Bang -> TBoolean  # !expr always returns boolean
                Plus | Minus | Tilde -> TNumber  # Numeric operators
                Typeof -> TString  # typeof always returns string
                Void -> TUnknown  # void always returns undefined
                Delete -> TBoolean  # delete returns boolean
                _ -> TUnknown

        # Update expressions
        UpdateExpression _ -> TNumber  # ++ and -- always work with numbers

        # Logical expressions
        LogicalExpression { operator, left, right } ->
            when operator is
                LogicalAnd ->
                    # && returns right if left is truthy, else left
                    left_type = infer_type left
                    right_type = infer_type right
                    when (left_type, right_type) is
                        (TBoolean, TBoolean) -> TBoolean
                        (TBoolean, other) | (_, other) -> other
                LogicalOr ->
                    # || returns left if truthy, else right
                    # Since we can't determine truthiness statically, return right type for non-booleans
                    left_type = infer_type left
                    right_type = infer_type right
                    when (left_type, right_type) is
                        (TBoolean, TBoolean) -> TBoolean
                        (TNumber, other) -> other  # Numbers might be falsy (0, NaN)
                        (TString, _) -> TString  # Non-empty strings are truthy
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
                "NaN" | "Infinity" -> TNumber
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
                Identifier { name: "length" } -> TNumber
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
        TemplateLiteral _ -> TString
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
        BigIntLiteral _ -> TNumber  # Simplified - would be bigint type

        # Import/Export
        ExportDefaultDeclaration _ -> TUnknown
        ExportNamedDeclaration _ -> TUnknown
        ExportAllDeclaration _ -> TUnknown
        ImportDeclaration _ -> TUnknown

        # Async
        AwaitExpression _ -> TUnknown
        YieldExpression _ -> TUnknown

        _ -> TUnknown

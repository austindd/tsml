module [
    type_check_with_rows,
    RowType,
    TypeWithRows,
]

import Ast
import Token
import Parser

# === Row Polymorphism Types ===

TypeId : U32
RowVar : U32

# Row type for records
RowType : {
    fields: List { label: Str, type_id: TypeId },
    tail: [Closed, Open RowVar],
}

# Extended type system with rows
TypeWithRows : [
    # Primitives
    TNum,
    TStr,
    TBool,
    TNull,
    TUndefined,

    # Type variable
    TVar TypeId,

    # Record with row
    TRecord RowType,

    # Array
    TArray TypeWithRows,

    # Function (simplified)
    TFunction (List TypeWithRows) TypeWithRows,

    # Union
    TUnion (List TypeWithRows),

    # Unknown
    TUnknown,
]

# === Row Operations ===

empty_closed_row : RowType
empty_closed_row = {
    fields: [],
    tail: Closed,
}

empty_open_row : RowVar -> RowType
empty_open_row = \var -> {
    fields: [],
    tail: Open var,
}

extend_row : RowType, Str, TypeId -> RowType
extend_row = \row, label, type_id ->
    { row & fields: List.append row.fields { label, type_id } }

# === Row Unification ===

RowSubst : List { var: RowVar, row: RowType }

apply_row_subst : RowType, RowSubst -> RowType
apply_row_subst = \row, subst ->
    when row.tail is
        Closed -> row
        Open var ->
            when List.find_first subst \s -> s.var == var is
                Ok s ->
                    merged = {
                        fields: List.concat row.fields s.row.fields,
                        tail: s.row.tail,
                    }
                    apply_row_subst merged subst
                Err _ -> row

unify_rows : RowType, RowType, RowSubst -> Result RowSubst [RowError Str]
unify_rows = \row1, row2, subst ->
    r1 = apply_row_subst row1 subst
    r2 = apply_row_subst row2 subst

    # Check all fields in r1 exist in r2
    check_fields_r1 = List.walk r1.fields (Ok subst) \acc, f1 ->
        when acc is
            Ok s ->
                when List.find_first r2.fields \f2 -> f2.label == f1.label is
                    Ok f2 ->
                        if f1.type_id == f2.type_id then
                            Ok s
                        else
                            Err (RowError "Type mismatch for field $(f1.label)")
                    Err _ ->
                        when r2.tail is
                            Open v2 ->
                                # Add field to open row
                                new_row = extend_row (empty_open_row (v2 + 1000)) f1.label f1.type_id
                                Ok (List.append s { var: v2, row: new_row })
                            Closed ->
                                Err (RowError "Missing field $(f1.label) in closed row")
            Err e -> Err e

    # Check all fields in r2 exist in r1
    when check_fields_r1 is
        Ok s1 ->
            List.walk r2.fields (Ok s1) \acc, f2 ->
                when acc is
                    Ok s ->
                        when List.find_first r1.fields \f1 -> f1.label == f2.label is
                            Ok _ -> Ok s  # Already checked
                            Err _ ->
                                when r1.tail is
                                    Open v1 ->
                                        new_row = extend_row (empty_open_row (v1 + 2000)) f2.label f2.type_id
                                        Ok (List.append s { var: v1, row: new_row })
                                    Closed ->
                                        Err (RowError "Missing field $(f2.label) in closed row")
                    Err e -> Err e
        Err e -> Err e

# === Type Checking with Rows ===

TypeEnv : {
    bindings: List { name: Str, type: TypeWithRows },
    next_var: U32,
    row_subst: RowSubst,
}

empty_env : TypeEnv
empty_env = {
    bindings: [],
    next_var: 0,
    row_subst: [],
}

fresh_var : TypeEnv -> (TypeId, TypeEnv)
fresh_var = \env ->
    (env.next_var, { env & next_var: env.next_var + 1 })

fresh_row_var : TypeEnv -> (RowVar, TypeEnv)
fresh_row_var = \env ->
    (env.next_var, { env & next_var: env.next_var + 1 })

# Type check expression with row polymorphism
check_expr : Ast.Node, TypeEnv -> Result (TypeWithRows, TypeEnv) [TypeError Str]
check_expr = \node, env ->
    when node is
        # Literals
        NumberLiteral _ -> Ok (TNum, env)
        StringLiteral _ -> Ok (TStr, env)
        BooleanLiteral _ -> Ok (TBool, env)
        NullLiteral _ -> Ok (TNull, env)
        UndefinedLiteral _ -> Ok (TUndefined, env)

        # Variable
        Identifier { name } ->
            when List.find_first env.bindings \b -> b.name == name is
                Ok binding -> Ok (binding.type, env)
                Err _ -> Err (TypeError "Undefined variable: $(name)")

        # Object literal - uses row types!
        ObjectExpression { properties } ->
            # Build row type from properties
            (row, env1) = List.walk properties (empty_closed_row, env) \(r, e), prop ->
                when prop is
                    Property { key, value, kind, method, shorthand, computed } ->
                        when key is
                            Identifier { name: field_name } ->
                                when value is
                                    Some val ->
                                        # Generate fresh type var for field
                                        (field_type_var, e1) = fresh_var e
                                        new_row = extend_row r field_name field_type_var
                                        (new_row, e1)
                                    None ->
                                        # Shorthand property
                                        (field_type_var, e1) = fresh_var e
                                        new_row = extend_row r field_name field_type_var
                                        (new_row, e1)
                            _ -> (r, e)
                    _ -> (r, e)

            Ok (TRecord row, env1)

        # Member access - check field exists in row
        MemberExpression { object, property, computed, optional } ->
            when check_expr object env is
                Ok (TRecord row, env1) ->
                    when property is
                        Identifier { name: field_name } ->
                            when List.find_first row.fields \f -> f.label == field_name is
                                Ok field ->
                                    # Found field, return its type
                                    Ok (TVar field.type_id, env1)
                                Err _ ->
                                    when row.tail is
                                        Open var ->
                                            # Open row - field might exist
                                            (new_type, env2) = fresh_var env1
                                            # Add constraint that row must have this field
                                            new_row = extend_row (empty_open_row (var + 3000)) field_name new_type
                                            new_subst = List.append env2.row_subst { var, row: new_row }
                                            Ok (TVar new_type, { env2 & row_subst: new_subst })
                                        Closed ->
                                            Err (TypeError "Field $(field_name) not found in record")
                        _ -> Ok (TUnknown, env1)
                _ -> Ok (TUnknown, env)

        # Array literal
        ArrayExpression { elements } ->
            # Get element type (simplified - just use first element)
            when List.first elements is
                Ok (Some elem) ->
                    when check_expr elem env is
                        Ok (elem_type, env1) -> Ok (TArray elem_type, env1)
                        Err e -> Err e
                _ -> Ok (TArray TUnknown, env)

        # Binary expression
        BinaryExpression { left, operator, right } ->
            when operator is
                Plus | Minus | Star | Slash | Percent ->
                    Ok (TNum, env)
                LessThan | LessThanEqual | GreaterThan | GreaterThanEqual |
                EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual ->
                    Ok (TBool, env)
                _ -> Ok (TUnknown, env)

        _ -> Ok (TUnknown, env)

# Main type checking function
type_check_with_rows : Str -> Result Str [ParseError, TypeError Str]
type_check_with_rows = \source ->
    # Tokenize
    tokens = Token.tokenize_str source

    # Filter trivia
    non_trivia = List.keep_if tokens \t ->
        when t is
            Whitespace _ | BlockComment _ | LineComment _ -> Bool.false
            _ -> Bool.true

    # Parse
    when Parser.parse_program non_trivia is
        Program { body, sourceType } ->
            # Type check first statement
            when List.first body is
                Ok stmt ->
                    when check_expr stmt empty_env is
                        Ok (type, _) -> Ok (type_to_string type)
                        Err (TypeError msg) -> Err (TypeError msg)
                Err _ -> Ok "Empty program"
        _ -> Err ParseError

# Convert type to string
type_to_string : TypeWithRows -> Str
type_to_string = \t ->
    when t is
        TNum -> "number"
        TStr -> "string"
        TBool -> "boolean"
        TNull -> "null"
        TUndefined -> "undefined"
        TVar id -> "T$(Num.to_str id)"
        TRecord row -> row_to_string row
        TArray elem -> "$(type_to_string elem)[]"
        TFunction params ret -> "function"
        TUnion types -> "union"
        TUnknown -> "unknown"

row_to_string : RowType -> Str
row_to_string = \row ->
    field_strs = List.map row.fields \f ->
        "$(f.label): T$(Num.to_str f.type_id)"

    fields_str = Str.join_with field_strs ", "

    tail_str = when row.tail is
        Closed -> ""
        Open var -> ", ...ρ$(Num.to_str var)"

    "{$(fields_str)$(tail_str)}"
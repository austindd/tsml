module [
    Constraint,
    ConstraintSet,
    Solution,
    TypeVar,
    generate_constraints,
    solve_constraints,
    apply_solution,
    unify,
    generalize,
    instantiate,
]

import ComprehensiveTypeIndexed as T
import Ast exposing [BinaryOperator, LogicalOperator]
import Option exposing [Option]

# Type variables for constraint solving
TypeVar : U64

# Constraints represent type relationships that must be satisfied
Constraint : [
    # Type equality: t1 = t2
    Equality { left : T.TypeId, right : T.TypeId, source : ConstraintSource },

    # Subtype constraint: t1 <: t2
    Subtype { sub : T.TypeId, super : T.TypeId, source : ConstraintSource },

    # Member access: t has member m of type t'
    HasMember { object : T.TypeId, member : Str, member_type : T.TypeId, source : ConstraintSource },

    # Function application: t1(args) = t2
    Application { func : T.TypeId, args : List T.TypeId, result : T.TypeId, source : ConstraintSource },

    # Conditional constraint: if guard then c1 else c2
    Conditional { guard : T.TypeId, then_constraint : Box Constraint, else_constraint : Box Constraint },
]

# Source information for error reporting
ConstraintSource : [
    Assignment { var_name : Str },
    FunctionCall { func_name : Str },
    Return,
    BinaryOp { op : BinaryOperator },
    LogicalOp { op : LogicalOperator },
    MemberAccess { object_name : Str, member : Str },
    Literal,
    TypeAnnotation,
]

# Set of constraints to be solved
ConstraintSet : {
    constraints : List Constraint,
    type_vars : List TypeVar,
    store : T.TypeStore,
}

# Solution maps type variables to concrete types
Solution : {
    substitutions : List { var : TypeVar, type_id : T.TypeId },
    store : T.TypeStore,
}

# Generate constraints from an expression
generate_constraints : T.TypeStore, Ast.Node -> (ConstraintSet, T.TypeId)
generate_constraints = \store, node ->
    when node is
        # Literals generate no constraints, just return their type
        NumberLiteral(data) ->
            (store1, lit_type) = T.make_literal(store, NumLit(data.value))
            (
                { constraints: [], type_vars: [], store: store1 },
                lit_type
            )

        StringLiteral(data) ->
            (store1, lit_type) = T.make_literal(store, StrLit(data.value))
            (
                { constraints: [], type_vars: [], store: store1 },
                lit_type
            )

        BooleanLiteral(data) ->
            (store1, lit_type) = T.make_literal(store, BoolLit(data.value))
            (
                { constraints: [], type_vars: [], store: store1 },
                lit_type
            )

        # Identifiers generate type variables
        Identifier(data) ->
            var = create_type_var(data.name)
            (store1, var_type) = create_type_var_type(store, var)
            (
                { constraints: [], type_vars: [var], store: store1 },
                var_type
            )

        # Binary expressions generate constraints based on operator
        BinaryExpression(data) ->
            generate_binary_constraints(store, data.left, data.operator, data.right)

        # Function calls generate application constraints
        CallExpression(data) ->
            generate_call_constraints(store, data.callee, data.arguments)

        # Member access generates HasMember constraints
        MemberExpression(data) ->
            generate_member_constraints(store, data.object, data.property, data.computed)

        # Conditional expressions
        ConditionalExpression(data) ->
            generate_conditional_constraints(store, data.test, data.consequent, data.alternate)

        # Array expressions
        ArrayExpression(data) ->
            generate_array_constraints(store, data.elements |> List.map(Some))

        # Object expressions
        ObjectExpression(data) ->
            generate_object_constraints(store, data.properties)

        _ ->
            # For unsupported nodes, return unknown type with no constraints
            (store1, unknown) = T.make_unknown(store)
            (
                { constraints: [], type_vars: [], store: store1 },
                unknown
            )

# Generate constraints for binary expressions
generate_binary_constraints : T.TypeStore, Ast.Node, Ast.BinaryOperator, Ast.Node -> (ConstraintSet, T.TypeId)
generate_binary_constraints = \store, left, op, right ->
    # Generate constraints for operands
    (left_constraints, left_type) = generate_constraints(store, left)
    (right_constraints, right_type) = generate_constraints(left_constraints.store, right)

    # Combine constraint sets
    combined_store = right_constraints.store
    combined_vars = List.concat(left_constraints.type_vars, right_constraints.type_vars)
    combined_constraints = List.concat(left_constraints.constraints, right_constraints.constraints)

    # Add operator-specific constraints
    when op is
        Plus ->
            # Addition can be number + number = number OR string + string = string
            (store1, number_type) = T.make_primitive(combined_store, "number")
            (store2, string_type) = T.make_primitive(store1, "string")
            (store3, result_var) = create_type_var_type(store2, create_type_var("add_result"))

            # Create constraints for both possibilities
            # We need a disjunction: (left = number & right = number & result = number) OR
            #                        (left = string & right = string & result = string) OR
            #                        (one is string & result = string)

            # For simplicity, we'll just constrain to number for now
            constraints = List.concat(
                combined_constraints,
                [
                    Subtype({ sub: left_type, super: number_type, source: BinaryOp({ op: Plus }) }),
                    Subtype({ sub: right_type, super: number_type, source: BinaryOp({ op: Plus }) }),
                ]
            )

            (
                { constraints: constraints, type_vars: combined_vars, store: store3 },
                number_type
            )

        Minus | Star | Slash | Percent | Ampersand | Pipe | Caret | LeftShift | RightShift | UnsignedRightShift ->
            # Arithmetic operators require number operands
            (store1, number_type) = T.make_primitive(combined_store, "number")
            constraints = List.concat(
                combined_constraints,
                [
                    Equality({ left: left_type, right: number_type, source: BinaryOp({ op: op }) }),
                    Equality({ left: right_type, right: number_type, source: BinaryOp({ op: op }) }),
                ]
            )

            (
                { constraints: constraints, type_vars: combined_vars, store: store1 },
                number_type
            )

        EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual ->
            # Comparison operators return boolean
            (store1, bool_type) = T.make_primitive(combined_store, "boolean")

            (
                { constraints: combined_constraints, type_vars: combined_vars, store: store1 },
                bool_type
            )

        Instanceof ->
            # instanceof requires some identifier on the righthand side, and returns a boolean
            (store1, bool_type) = T.make_unknown(combined_store)

            (
                { constraints: combined_constraints, type_vars: combined_vars, store: store1 },
                bool_type
            )

        In -> 
            # in requires some identifier on the righthand side, and returns a boolean
            (store1, bool_type) = T.make_unknown(combined_store)

            (
                { constraints: combined_constraints, type_vars: combined_vars, store: store1 },
                bool_type
            )
        
        NullishCoalesce ->
            # ?? operator - left type without null/undefined, or right type
            (store1, union_type) = T.make_union(combined_store, [left_type, right_type])

            (
                { constraints: combined_constraints, type_vars: combined_vars, store: store1 },
                union_type
            )

generate_logical_constraints : T.TypeStore, Ast.Node, Ast.LogicalOperator, Ast.Node -> (ConstraintSet, T.TypeId)
generate_logical_constraints = |store, left, op, right|
    # Generate constraints for operands
    (left_constraints, left_type) = generate_constraints(store, left)
    (right_constraints, right_type) = generate_constraints(left_constraints.store, right)

    # Combine constraint sets
    combined_store = right_constraints.store
    combined_vars = List.concat(left_constraints.type_vars, right_constraints.type_vars)
    combined_constraints = List.concat(left_constraints.constraints, right_constraints.constraints)

    when op is
        LogicalAnd | LogicalOr ->
            # Logical operators - result is union of operand types
            (store1, result_type) = T.join(combined_store, left_type, right_type)

            (
                { constraints: combined_constraints, type_vars: combined_vars, store: store1 },
                result_type
            )

# Generate constraints for function calls
generate_call_constraints : T.TypeStore, Ast.Node, List Ast.Node -> (ConstraintSet, T.TypeId)
generate_call_constraints = \store, callee, arguments ->
    # Generate constraints for callee
    (callee_constraints, callee_type) = generate_constraints(store, callee)

    # Generate constraints for arguments
    (arg_constraints, arg_types, final_store) = List.walk(
        arguments,
        ([], [], callee_constraints.store),
        \(acc_constraints, acc_types, acc_store), arg ->
            (arg_constraint_set, arg_type) = generate_constraints(acc_store, arg)
            (
                List.concat(acc_constraints, arg_constraint_set.constraints),
                List.append(acc_types, arg_type),
                arg_constraint_set.store
            )
    )

    # Create result type variable
    (store1, result_type) = create_type_var_type(final_store, create_type_var("call_result"))

    # Add application constraint
    all_constraints = List.concat(
        List.concat(callee_constraints.constraints, arg_constraints),
        [Application({
            func: callee_type,
            args: arg_types,
            result: result_type,
            source: FunctionCall({ func_name: "unknown" })
        })]
    )

    all_vars = List.concat(callee_constraints.type_vars, [create_type_var("call_result")])

    (
        { constraints: all_constraints, type_vars: all_vars, store: store1 },
        result_type
    )

# Generate constraints for member access
generate_member_constraints : T.TypeStore, Ast.Node, Ast.Node, Bool -> (ConstraintSet, T.TypeId)
generate_member_constraints = \store, object, property, _computed ->
    # Generate constraints for object
    (obj_constraints, obj_type) = generate_constraints(store, object)

    # Get property name
    prop_name = when property is
        Identifier(data) -> data.name
        StringLiteral(data) -> data.value
        _ -> "unknown"

    # Create result type variable
    (store1, result_type) = create_type_var_type(obj_constraints.store, create_type_var("member_$(prop_name)"))

    # Add HasMember constraint
    constraint = HasMember({
        object: obj_type,
        member: prop_name,
        member_type: result_type,
        source: MemberAccess({ object_name: "obj", member: prop_name })
    })

    (
        {
            constraints: List.append(obj_constraints.constraints, constraint),
            type_vars: List.append(obj_constraints.type_vars, create_type_var("member_$(prop_name)")),
            store: store1
        },
        result_type
    )

# Generate constraints for conditional expressions
generate_conditional_constraints : T.TypeStore, Ast.Node, Ast.Node, Ast.Node -> (ConstraintSet, T.TypeId)
generate_conditional_constraints = \store, test, consequent, alternate ->
    # Generate constraints for all branches
    (test_constraints, test_type) = generate_constraints(store, test)
    (cons_constraints, cons_type) = generate_constraints(test_constraints.store, consequent)
    (alt_constraints, alt_type) = generate_constraints(cons_constraints.store, alternate)

    # Result type is the join of both branches
    (store1, result_type) = T.join(alt_constraints.store, cons_type, alt_type)

    # Combine all constraints
    all_constraints = List.concat(
        test_constraints.constraints,
        List.concat(cons_constraints.constraints, alt_constraints.constraints)
    )

    all_vars = List.concat(
        test_constraints.type_vars,
        List.concat(cons_constraints.type_vars, alt_constraints.type_vars)
    )

    (
        { constraints: all_constraints, type_vars: all_vars, store: store1 },
        result_type
    )

# Generate constraints for array expressions
generate_array_constraints : T.TypeStore, List (Option Ast.Node) -> (ConstraintSet, T.TypeId)
generate_array_constraints = \store, elements ->
    # Generate constraints for each element
    (all_constraints, elem_types, final_store) =
        elements |> List.walk(
            ([], [], store),
            |(acc_constraints, acc_types, acc_store), elem_opt|
                when elem_opt is
                    Some(elem) ->
                        (elem_constraints, elem_type) = generate_constraints(acc_store, elem)
                        (
                            List.concat(acc_constraints, elem_constraints.constraints),
                            List.append(acc_types, elem_type),
                            elem_constraints.store
                        )
                    None ->
                        # Hole or spread - use unknown
                        (store1, unknown) = T.make_unknown(acc_store)
                        (
                            acc_constraints,
                            List.append(acc_types, unknown),
                            store1
                        )
        )

    # Find common element type
    elem_type2 = when elem_types is
        [] ->
            (s, never) = T.make_never(final_store)
            never
        [single] -> single
        multiple ->
            # Join all element types
            (s, joined) = List.walk(
                List.drop_first(multiple, 1),
                (final_store, List.first(multiple) |> Result.with_default(0)),
                \(acc_store, acc_type), next_type ->
                    T.join(acc_store, acc_type, next_type)
            )
            joined

    # Create array type
    (store2, array_type) = T.make_array(final_store, elem_type2)

    (
        { constraints: all_constraints, type_vars: [], store: store2 },
        array_type
    )

# Generate constraints for object expressions
generate_object_constraints : T.TypeStore, List Ast.Node -> (ConstraintSet, T.TypeId)
generate_object_constraints = |store, properties|
    # Build object type with properties
    (all_constraints, row_id, final_store) = List.walk(
        properties,
        ([], 0, store),
        |(acc_constraints, acc_row, acc_store), prop|
            when prop is
                Property(data) ->
                    # Get property name
                    prop_name = when data.key is
                        Identifier(id_data) -> id_data.name
                        StringLiteral(str_data) -> str_data.value
                        _ -> "unknown"

                    value = data.value |> Option.with_default(data.key)
                    # Generate constraints for value
                    (val_constraints, val_type) = generate_constraints(acc_store, value)

                    # Extend row type
                    if acc_row == 0 then
                        (store1, empty_row) = T.make_empty_row(val_constraints.store)
                        (store2, new_row) = T.make_row_extend(
                            store1,
                            prop_name,
                            val_type,
                            Bool.false,
                            Bool.false,
                            empty_row
                        )
                        (
                            List.concat(acc_constraints, val_constraints.constraints),
                            new_row,
                            store2
                        )
                    else
                        (store1, new_row) = T.make_row_extend(
                            val_constraints.store,
                            prop_name,
                            val_type,
                            Bool.false,
                            Bool.false,
                            acc_row
                        )
                        (
                            List.concat(acc_constraints, val_constraints.constraints),
                            new_row,
                            store1
                        )

                SpreadElement(_) ->
                    # Skip spread for now
                    (acc_constraints, acc_row, acc_store)
                _ ->
                  crash("Unexpected spread element")
    )

    # Create object type
    if row_id == 0 then
        (store1, empty_row) = T.make_empty_row(final_store)
        (store2, obj_type) = T.make_object(store1, empty_row)
        (
            { constraints: all_constraints, type_vars: [], store: store2 },
            obj_type
        )
    else
        (store1, obj_type) = T.make_object(final_store, row_id)
        (
            { constraints: all_constraints, type_vars: [], store: store1 },
            obj_type
        )

# Helper: Create a type variable
create_type_var : Str -> TypeVar
create_type_var = \name ->
    # Simple hash of the name for now
    Str.to_utf8(name)
    |> List.walk(0, \acc, byte ->
        acc * 31 + Num.to_u64(byte)
    )

# Helper: Create a type for a type variable (initially unknown)
create_type_var_type : T.TypeStore, TypeVar -> (T.TypeStore, T.TypeId)
create_type_var_type = \store, _var ->
    # For now, create an unknown type
    # In a full implementation, we'd track type variables separately
    T.make_unknown(store)

# Solve a set of constraints to find a solution
solve_constraints : ConstraintSet -> Result Solution [UnificationError Str]
solve_constraints = \constraint_set ->
    # Initialize solution with empty substitutions
    initial_solution = {
        substitutions: [],
        store: constraint_set.store
    }

    # Process each constraint
    List.walk(
        constraint_set.constraints,
        Ok(initial_solution),
        \solution_result, constraint ->
            solution_result
            |> Result.try(\solution ->
                solve_single_constraint(solution, constraint)
            )
    )

# Solve a single constraint
solve_single_constraint : Solution, Constraint -> Result Solution [UnificationError Str]
solve_single_constraint = |solution, constraint|
    when constraint is
        Equality({ left, right, source: _ }) ->
            unify(solution.store, left, right)
            |> Result.map_ok(|unified_store|
                { solution & store: unified_store }
            )

        Subtype({ sub, super, source: _ }) ->
            # Check if sub is a subtype of super
            if T.is_subtype_of(solution.store, sub, super) then
                Ok(solution)
            else
                # Try to make sub a subtype by unifying with a subtype
                # For now, just unify them
                unify(solution.store, sub, super)
                |> Result.map_ok(|unified_store|
                    { solution & store: unified_store }
                )

        HasMember({ object, member, member_type, source: _ }) ->
            # Check if object has the member
            when T.get_type(solution.store, object) is
                Ok(TObject(row_id)) ->
                    # Look for the member in the row
                    find_member_type(solution.store, row_id, member)
                    |> Result.try(|found_type|
                        unify(solution.store, found_type, member_type)
                    )
                    |> Result.map_ok(|unified_store|
                        { solution & store: unified_store }
                    )
                _ ->
                    # Not an object or can't determine - fail
                    Err(UnificationError("Type does not have member $(member)"))

        Application({ func, args: _, result, source: _ }) ->
            # Check if func is a function type
            when T.get_type(solution.store, func) is
                Ok(TFunction(fn_type)) ->
                    # Unify result with return type
                    unify(solution.store, fn_type.ret, result)
                    |> Result.map_ok(|unified_store|
                        { solution & store: unified_store }
                    )
                _ ->
                    # Not a function - for now, just make result unknown
                    Ok(solution)

        Conditional({ guard: _, then_constraint: _, else_constraint: _ }) ->
            # For now, skip conditional constraints
            Ok(solution)

# Find member type in a row
find_member_type : T.TypeStore, T.RowId, Str -> Result T.TypeId [UnificationError Str]
find_member_type = \store, row_id, member ->
    when T.get_row(store, row_id) is
        Ok(RExtend(extend)) ->
            if extend.label == member then
                Ok(extend.field_type)
            else
                find_member_type(store, extend.rest, member)
        _ ->
            Err(UnificationError("Member $(member) not found"))

# Unify two types
unify : T.TypeStore, T.TypeId, T.TypeId -> Result T.TypeStore [UnificationError Str]
unify = \store, type1, type2 ->
    if type1 == type2 then
        Ok(store)
    else
        when (T.get_type(store, type1), T.get_type(store, type2)) is
            (Ok(TUnknown), _) ->
                # Unknown unifies with anything
                Ok(store)

            (_, Ok(TUnknown)) ->
                # Unknown unifies with anything
                Ok(store)

            (Ok(TNever), Ok(TNever)) ->
                Ok(store)

            (Ok(def1), Ok(def2)) ->
                unify_types(store, def1, def2, type1, type2)

            _ ->
                Err(UnificationError("Cannot unify types"))

# Unify specific type definitions
unify_types : T.TypeStore, T.TypeDef, T.TypeDef, T.TypeId, T.TypeId -> Result T.TypeStore [UnificationError Str]
unify_types = \store, def1, def2, id1, id2 ->
    when (def1, def2) is
        # Same primitive types unify
        (TNumber, TNumber) -> Ok(store)
        (TString, TString) -> Ok(store)
        (TBoolean, TBoolean) -> Ok(store)
        (TNull, TNull) -> Ok(store)
        (TUndefined, TUndefined) -> Ok(store)

        # Literals unify with their base types
        (TLiteral(NumLit(_)), TNumber) -> Ok(store)
        (TNumber, TLiteral(NumLit(_))) -> Ok(store)
        (TLiteral(StrLit(_)), TString) -> Ok(store)
        (TString, TLiteral(StrLit(_))) -> Ok(store)
        (TLiteral(BoolLit(_)), TBoolean) -> Ok(store)
        (TBoolean, TLiteral(BoolLit(_))) -> Ok(store)

        # Arrays unify if elements unify
        (TArray(elem1), TArray(elem2)) ->
            unify(store, elem1, elem2)

        # For other cases, check subtyping
        _ ->
            if T.is_subtype_of(store, id1, id2) then
                Ok(store)
            else if T.is_subtype_of(store, id2, id1) then
                Ok(store)
            else
                Err(UnificationError("Types do not unify"))

# Apply a solution to a type
apply_solution : Solution, T.TypeId -> T.TypeId
apply_solution = \solution, type_id ->
    # For now, just return the type as-is
    # In a full implementation, we'd substitute type variables
    type_id

# Generalize a type by abstracting over free type variables
generalize : T.TypeStore, T.TypeId, List TypeVar -> T.TypeId
generalize = \_store, type_id, _vars ->
    # For now, just return the type as-is
    # In a full implementation, we'd create a polymorphic type
    type_id

# Instantiate a polymorphic type with fresh type variables
instantiate : T.TypeStore, T.TypeId -> (T.TypeStore, T.TypeId)
instantiate = \store, type_id ->
    # For now, just return the type as-is
    # In a full implementation, we'd replace bound variables with fresh ones
    (store, type_id)

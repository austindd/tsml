module [
    Constraint,
    ConstraintSet,
    new_constraint_set,
    add_equal,
    add_subtype,
    add_has_field,
    generate_simple_constraints,
]

import TypeCore exposing [TypeId, TypeTable, SimpleType]
import Ast exposing [Node]

Constraint : [
    CEq TypeId TypeId,
    CSub TypeId TypeId,
    CField TypeId Str TypeId,
    CCall TypeId (List TypeId) TypeId,
]

ConstraintSet : {
    constraints : List Constraint,
    type_table : TypeTable,
}

new_constraint_set : TypeTable -> ConstraintSet
new_constraint_set = \table ->
    {
        constraints: [],
        type_table: table,
    }

add_constraint : ConstraintSet, Constraint -> ConstraintSet
add_constraint = \set, constraint ->
    { set & constraints: List.append set.constraints constraint }

add_equal : ConstraintSet, TypeId, TypeId -> ConstraintSet
add_equal = \set, t1, t2 ->
    add_constraint set (CEq t1 t2)

add_subtype : ConstraintSet, TypeId, TypeId -> ConstraintSet
add_subtype = \set, sub, super ->
    add_constraint set (CSub sub super)

add_has_field : ConstraintSet, TypeId, Str, TypeId -> ConstraintSet
add_has_field = \set, obj, field, field_type ->
    add_constraint set (CField obj field field_type)

generate_simple_constraints : Node, TypeTable -> (TypeId, ConstraintSet)
generate_simple_constraints = \node, table ->
    set = new_constraint_set table
    gen_node node set

gen_node : Node, ConstraintSet -> (TypeId, ConstraintSet)
gen_node = \node, set ->
    when node is
        NumberLiteral { value } ->
            (tid, new_table) = TypeCore.mk_lit set.type_table (LNum 0.0)
            (tid, { set & type_table: new_table })

        StringLiteral { value } ->
            (tid, new_table) = TypeCore.mk_lit set.type_table (LStr value)
            (tid, { set & type_table: new_table })

        BooleanLiteral { value } ->
            (tid, new_table) = TypeCore.mk_lit set.type_table (LBool value)
            (tid, { set & type_table: new_table })

        NullLiteral _ ->
            (tid, new_table) = TypeCore.mk_lit set.type_table LNull
            (tid, { set & type_table: new_table })

        UndefinedLiteral _ ->
            (tid, new_table) = TypeCore.mk_lit set.type_table LUndefined
            (tid, { set & type_table: new_table })

        Identifier { name } ->
            # For now, just create a type variable for identifiers
            (tid, new_table) = TypeCore.mk_var set.type_table
            (tid, { set & type_table: new_table })

        ArrayExpression { elements } ->
            # Create element type variable
            (elem_tid, table1) = TypeCore.mk_var set.type_table

            # Process each element and constrain to elem type
            (set_with_elems, table2) = List.walk elements ({ set & type_table: table1 }, table1) \(current_set, current_table), elem ->
                elem_set = { current_set & type_table: current_table }
                (elem_type, new_set) = gen_node elem elem_set
                constrained = add_equal new_set elem_type elem_tid
                (constrained, constrained.type_table)

            # Create array type
            (arr_tid, final_table) = TypeCore.mk_arr table2 elem_tid
            (arr_tid, { set_with_elems & type_table: final_table })

        ObjectExpression { properties } ->
            # Process properties
            (fields, final_set) = List.walk properties ([], set) \acc, prop ->
                (field_list, current_set) = acc
                when prop is
                    Property { key, value } ->
                        key_name = get_property_key key
                        (val_tid, new_set) = gen_node value current_set
                        field = { key: key_name, tid: val_tid, optional: Bool.false }
                        (List.append field_list field, new_set)
                    _ ->
                        (field_list, current_set)

            # Create object type
            (obj_tid, new_table) = TypeCore.mk_obj final_set.type_table fields
            (obj_tid, { final_set & type_table: new_table })

        BinaryExpression { left, operator, right } ->
            (left_tid, set1) = gen_node left set
            (right_tid, set2) = gen_node right set1

            # Create result type based on operator
            (result_tid, new_table) = when operator is
                Plus ->
                    # Could be number or string
                    TypeCore.mk_prim set2.type_table "number"  # Simplified for now
                EqualEqual | BangEqual | LessThan | GreaterThan ->
                    TypeCore.mk_prim set2.type_table "boolean"
                _ ->
                    TypeCore.mk_prim set2.type_table "number"

            (result_tid, { set2 & type_table: new_table })

        CallExpression { callee, arguments } ->
            (callee_tid, set1) = gen_node callee set

            # Process arguments
            (arg_tids, set2) = List.walk arguments ([], set1) \(tids, current_set), arg ->
                (arg_tid, new_set) = gen_node arg current_set
                (List.append tids arg_tid, new_set)

            # Create result type variable
            (result_tid, new_table) = TypeCore.mk_var set2.type_table

            # Add call constraint
            final_set = { set2 & type_table: new_table }
                |> add_constraint (CCall callee_tid arg_tids result_tid)

            (result_tid, final_set)

        _ ->
            # Default: create a type variable
            (tid, new_table) = TypeCore.mk_var set.type_table
            (tid, { set & type_table: new_table })

get_property_key : Node -> Str
get_property_key = \node ->
    when node is
        Identifier { name } -> name
        StringLiteral { value } -> value
        NumberLiteral { value } -> value
        _ -> "_unknown_"

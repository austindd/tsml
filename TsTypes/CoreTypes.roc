module [
    TypeId,
    TConstraint,
    Kind,
    Property,
    TypeScope,
    TypeCtx,
]

import Stack exposing [Stack]
import Option exposing [Option, Some, None]

TypeId := U64
    implements [
        Eq,
        Hash,
    ]

TermId := U64
    implements [
        Eq,
        Hash,
    ]

TConstraint : [
    TypeOfTerm TermId Kind, # A type that is the type of a term with a specific kind
    SubtypeOf TypeId Kind, # A type that is a subtype of another type with a specific kind
    Boolean Kind, # Boolean type with its kind
    Number Kind, # Number type with its kind
    Struct (Dict Str Property) Kind, # Record type with properties and its kind
    Union (List TConstraint) Kind, # Union type with its kind
    Intersection (List TConstraint) Kind, # Intersection type with its kind
    TypeConstructor (List TypeId) Kind, # Type constructor with parameters and its kind
    TypeVariable Str Kind, # Type variable with its kind
    Unknown Kind, # Unknown type with its kind
    Never Kind, # Never type with its kind
]

Kind : [
    KStar, # Kind of all types
    KArrow Kind Kind, # Kind of type constructors
]

TSubtype : [
    SubtypeOf TypeId Kind,
]

TStruct : [
    Struct (Dict Str Property) Kind,
]

TUnion : [
    Union (List TConstraint) Kind,
]

TIntersection : [
    Intersection (List TConstraint) Kind,
]

TIntersection2 : [
    Intersection {
            merged_struct : Option TStruct,
            constraints : List TConstraint,
        } Kind,
]

Property : {
    name : Str,
    type_id : TypeId,
}

TypeScope : Dict TypeId TConstraint

TermScope : Dict TermId TConstraint

TypeCtx := {
    last_id : TypeId,
    scope_stack : Stack.Stack TypeScope,
}

TermCtx := {
    last_id : TermId,
    scope_stack : Stack.Stack TermScope,
}

get_type_from_scope : TypeScope, TypeId -> Result TConstraint [TypeNotFoundInScope]
get_type_from_scope = |type_scope, type_id|
    when Dict.get(type_scope, type_id) is
        Ok(constraint) -> Ok(constraint)
        Err(KeyNotFound) -> Err(TypeNotFoundInScope)

get_type_from_ctx : TypeCtx, TypeId -> Result TConstraint [TypeNotFoundInCtx]
get_type_from_ctx = |@TypeCtx({ scope_stack }), type_id|
    initial_state : Result TConstraint [NotFound]
    initial_state = Err(NotFound)
    res = Stack.walk_from_top_until(
        scope_stack,
        initial_state,
        |state, type_scope|
            type_result = get_type_from_scope(type_scope, type_id)
            when type_result is
                Ok(type) -> Break(Ok(type))
                Err(_) -> Continue(Err(NotFound)),
    )
    Result.map_err(res, |_| TypeNotFoundInCtx)

get_term_from_scope : TermScope, TermId -> Result TConstraint [TermNotFoundInScope]
get_term_from_scope = |term_scope, term_id|
    when Dict.get(term_scope, term_id) is
        Ok(constraint) -> Ok(constraint)
        Err(KeyNotFound) -> Err(TermNotFoundInScope)

get_term_from_ctx : TermCtx, TermId -> Result TConstraint [TermNotFoundInCtx]
get_term_from_ctx = |@TermCtx({ scope_stack }), term_id|
    initial_state : Result TConstraint [NotFound]
    initial_state = Err(NotFound)
    res = Stack.walk_from_top_until(
        scope_stack,
        initial_state,
        |state, term_scope|
            term_result = get_term_from_scope(term_scope, term_id)
            when term_result is
                Ok(term) -> Break(Ok(term))
                Err(_) -> Continue(Err(NotFound)),
    )
    Result.map_err(res, |_| TermNotFoundInCtx)

constraint_is_subtype_of : TermCtx, TypeCtx, TConstraint, TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
constraint_is_subtype_of = |term_ctx, type_ctx, constraint_a, constraint_b|
    pair : (TConstraint, TConstraint)
    pair = (constraint_a, constraint_b)
    when pair is
        (c_a, Boolean(_)) -> is_subtype_of_boolean(term_ctx, type_ctx, c_a)
        (c_a, Number(_)) -> is_subtype_of_number(term_ctx, type_ctx, c_a)
        (c_a, SubtypeOf(type_id_b, _)) -> is_subtype_of_type_id(term_ctx, type_ctx, c_a, type_id_b)
        (c_a, Struct(struct_b, kind_b)) -> is_subtype_of_struct(term_ctx, type_ctx, c_a, Struct(struct_b, kind_b))
        (Union(constraints_a, _), Union(constraints_b, _)) -> Err(SubtypeMismatch) # TODO: Implement
        (Intersection(constraints_a, _), Intersection(constraints_b, _)) -> Err(SubtypeMismatch) # TODO: Implement
        (TypeConstructor(params_a, _), TypeConstructor(params_b, _)) -> Err(SubtypeMismatch) # TODO: Implement
        (TypeVariable(name_a, _), TypeVariable(name_b, _)) ->
            when name_a == name_b is
                true -> Ok({})
                false -> Err(SubtypeMismatch)

        (Unknown(_), Unknown(_)) -> Ok({})
        _ -> Err(SubtypeMismatch)

is_subtype_of_boolean : TermCtx, TypeCtx, TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_subtype_of_boolean = |term_ctx, type_ctx, constraint|
    when constraint is
        Boolean(_) -> Ok({})
        SubtypeOf(type_id, _) ->
            inner_result = get_type_from_ctx(type_ctx, type_id)
            when inner_result is
                Ok(inner) -> is_subtype_of_boolean(term_ctx, type_ctx, inner)
                Err(_) -> Err(TypeNotFoundInCtx)

        _ -> Err(SubtypeMismatch)

is_subtype_of_number : TermCtx, TypeCtx, TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_subtype_of_number = |term_ctx, type_ctx, constraint|
    when constraint is
        Number(_) -> Ok({})
        SubtypeOf(type_id, _) ->
            inner_result = get_type_from_ctx(type_ctx, type_id)
            when inner_result is
                Ok(inner) -> is_subtype_of_number(term_ctx, type_ctx, inner)
                Err(_) -> Err(TypeNotFoundInCtx)

        _ -> Err(SubtypeMismatch)

is_subtype_of_type_id : TermCtx, TypeCtx, TConstraint, TypeId -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_subtype_of_type_id = |term_ctx, type_ctx, constraint, type_id|
    when constraint is
        SubtypeOf(inner_id, _) ->
            inner_result = get_type_from_ctx(type_ctx, inner_id)
            when inner_result is
                Ok(inner) -> constraint_is_subtype_of(term_ctx, type_ctx, inner, SubtypeOf(type_id, KStar))
                Err(_) -> Err(TypeNotFoundInCtx)

        _ -> Err(SubtypeMismatch)

is_type_id_subtype_of_type_id : TermCtx, TypeCtx, TypeId, TypeId -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_type_id_subtype_of_type_id = |term_ctx, type_ctx, type_id_a, type_id_b|
    type_a_result = get_type_from_ctx(type_ctx, type_id_a)
    type_b_result = get_type_from_ctx(type_ctx, type_id_b)
    when type_a_result is
        Ok(type_a) ->
            when type_b_result is
                Ok(type_b) -> constraint_is_subtype_of(term_ctx, type_ctx, type_a, type_b)
                Err(_) -> Err(TypeNotFoundInCtx)

        Err(_) -> Err(TypeNotFoundInCtx)

is_subtype_of_struct : TermCtx, TypeCtx, TConstraint, TStruct -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_subtype_of_struct = |term_ctx, type_ctx, constraint, Struct(struct_b, _)|
    when constraint is
        Struct(struct_a, _) ->
            Dict.walk(
                struct_b,
                Ok({}),
                |prev_result, key_b, prop_b|
                    prop_a_result = Dict.get(struct_a, key_b)
                    when prop_a_result is
                        Ok(prop_a) ->
                            prop_a_type_id = prop_a.type_id
                            prop_a_type_result = get_type_from_ctx(type_ctx, prop_a_type_id)
                            when prop_a_type_result is
                                Err(_) -> Err(TypeNotFoundInCtx)
                                Ok(prop_a_type) ->
                                    is_subtype_of_type_id(term_ctx, type_ctx, prop_a_type, prop_b.type_id)

                        Err(_) -> Err(SubtypeMismatch),
            )

        _ -> Err(SubtypeMismatch)

is_subtype_of_subtype : TermCtx, TypeCtx, TConstraint, TSubtype -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_subtype_of_subtype = |term_ctx, type_ctx, constraint, SubtypeOf(type_id_b, _)|
    is_subtype_of_type_id(term_ctx, type_ctx, constraint, type_id_b)

is_subtype_of_union : TermCtx, TypeCtx, TConstraint, List TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_subtype_of_union = |term_ctx, type_ctx, constraint_a, union_b|
    when constraint_a is
        Union(union_a, _) ->
            is_satisfied = List.any(
                union_b,
                |c_b|
                    List.all(
                        union_a,
                        |c_a|
                            when constraint_is_subtype_of(term_ctx, type_ctx, c_a, c_b) is
                                Ok(_) -> Bool.true
                                Err(_) -> Bool.false,
                    ),
            )

            when is_satisfied is
                true -> Ok({})
                false -> Err(SubtypeMismatch)

        _ -> Ok({})

# A union is satisfied by a constraint if the constraint is a subtype of any of the constraints in the union
# An intersection is satisfied by a constraint if the constraint is a subtype of all of the constraints in the intersection

is_subtype_of_intersection : TermCtx, TypeCtx, TConstraint, List TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
is_subtype_of_intersection = |term_ctx, type_ctx, constraint_a, intersection_b|
    when constraint_a is
        Union(union_a, _) ->
            is_satisfied = List.all(
                intersection_b,
                |c_b|
                    List.any(
                        union_a,
                        |c_a|
                            when constraint_is_subtype_of(term_ctx, type_ctx, c_a, c_b) is
                                Ok(_) -> Bool.true
                                Err(_) -> Bool.false,
                    ),
            )
            when is_satisfied is
                true -> Ok({})
                false -> Err(SubtypeMismatch)

        _ -> Err(SubtypeMismatch)

merge_structs :
    TermCtx,
    TypeCtx,
    TStruct,
    TStruct
    -> {
        struct : TStruct,
        type_ctx : TypeCtx,
        term_ctx : TermCtx,
    }
merge_structs = |term_ctx, type_ctx, Struct(struct_a, kind_a), Struct(struct_b, kind_b)|
    if (kind_a == kind_b) or (kind_a == KStar) or (kind_b == KStar) then
        highest_kind =
            when kind_a == KStar is
                true -> kind_b
                false -> kind_a
        largest_size = ((Dict.len(struct_a)) + (Dict.len(struct_b)))
        initial_state = Ok(
            {
                struct: struct_a,
                type_ctx: type_ctx,
            },
        )
        final_state_result = Dict.walk(
            struct_b,
            initial_state,
            |state_result, key, prop_b|
                prop_a_result = Dict.get(struct_a, key)
                when state_result is
                    Err(_) -> state_result
                    Ok(state) ->
                        when prop_a_result is
                            Ok(prop_a) ->
                                prop_a_type_result = get_type_from_ctx(type_ctx, prop_a.type_id)
                                when prop_a_type_result is
                                    Err(_) -> Ok(state)
                                    Ok(prop_a_type) ->
                                        prop_b_type_result = get_type_from_ctx(type_ctx, prop_b.type_id)
                                        when prop_b_type_result is
                                            Err(_) -> Ok(state)
                                            Ok(prop_b_type) ->
                                                when constraint_is_subtype_of(term_ctx, type_ctx, prop_a_type, prop_b_type) is
                                                    Ok(_) ->
                                                        new_type = Union([prop_a_type, prop_b_type], highest_kind)
                                                        (new_type_ctx, fresh_type_id) = register_type(type_ctx, new_type)
                                                        new_prop = {
                                                            name: key,
                                                            type_id: fresh_type_id,
                                                        }
                                                        new_struct = Dict.insert(state.struct, key, new_prop)
                                                        Ok(
                                                            {
                                                                struct: new_struct,
                                                                type_ctx: new_type_ctx,
                                                            },
                                                        )

                                                    Err(_) -> Ok(state)

                            Err(_) ->
                                Ok(
                                    {
                                        struct: Dict.insert(state.struct, key, prop_b),
                                        type_ctx: state.type_ctx,
                                    },
                                ),
        )
        when final_state_result is
            Ok(final_state) ->
                final_struct = {
                    struct: Struct(final_state.struct, highest_kind),
                    type_ctx: final_state.type_ctx,
                    term_ctx: term_ctx,
                }
                final_struct

            Err(_) ->
                crash("Failed to merge structs")
    else
        crash("Mismatched kinds")
# if (kindA == kindB) || (kindA == KStar) || (kindB == KStar) then
#    largestSize = ((Dict.len structA) + (Dict.len structB))
#    newStruct = Dict.walk structB structA \result, key, propB ->
#        Dict.update result key \propAResult ->
#            when propAResult is
#                Ok propA ->
#                    when isTypeIdSubtypeOfTypeId termCtx typeCtx propA.typeId propB.typeId is
#                        Ok _ ->
#                            Ok (Union [propA, propB])

#                        Err _ -> propAResult

#                Err _ -> Ok propB
#    Struct newStruct kindB
# else
#    crash "Mismatched kinds"

register_type : TypeCtx, TConstraint -> (TypeCtx, TypeId)
register_type = |@TypeCtx({ last_id: @TypeId(last_id_), scope_stack }), constraint|
    next_id = @TypeId((last_id_ + 1))
    nearest_scope = Stack.peek(scope_stack)
    new_scope_stack =
        when nearest_scope is
            Ok(scope) ->
                new_scope = Dict.insert(scope, next_id, constraint)
                (Stack.pop(scope_stack)) |> Stack.push(new_scope)

            Err(_) -> Stack.push(scope_stack, Dict.single(next_id, constraint))

    (@TypeCtx({ last_id: next_id, scope_stack: new_scope_stack }), next_id)

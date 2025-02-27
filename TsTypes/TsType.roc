module []

import Stack
import ListMap

TypeId := U64
    implements [
        Eq,
        Hash,
    ]

TypeTag : [
    Boolean,
    Number,
    Struct { props : Dict Str TypeId },
    ConstraintList (List ConstraintTag),
    Existential,
]

StructTag : [
    Struct {
            props : Dict Str TypeId,
        },
]

ConstraintTag : [
    SubtypeOf TypeId,
    EqualTo TypeId,
]

TypeScope : Dict TypeId TypeTag

TypeEnv := {
    last_id : TypeId,
    scope_stack : Stack.Stack TypeScope,
}

TypeEvalResult : {
    type_env : TypeEnv,
    type_id : TypeId,
    type : TypeTag,
}

TypeError : {
    actual : TypeTag,
    expected : TypeTag,
    message : Str,
}

register_type : TypeEnv, TypeTag -> (TypeEnv, TypeId)
register_type = |@TypeEnv({ last_id: @TypeId(last_id_), scope_stack }), type_tag|
    next_id = @TypeId((last_id_ + 1))
    nearest_scope = Stack.peek(scope_stack)
    new_scope_stack =
        when nearest_scope is
            Ok(scope) ->
                new_scope = Dict.insert(scope, next_id, type_tag)
                (Stack.pop(scope_stack)) |> Stack.push(new_scope)

            Err(_) -> Stack.push(scope_stack, Dict.single(next_id, type_tag))

    (@TypeEnv({ last_id: next_id, scope_stack: new_scope_stack }), next_id)

get_type_from_scope : TypeScope, TypeId -> Result TypeTag [NotFound]
get_type_from_scope = |type_scope, type_id|
    when Dict.get(type_scope, type_id) is
        Ok(type_tag) -> Ok(type_tag)
        Err(KeyNotFound) -> Err(NotFound)

get_type_from_env : TypeEnv, TypeId -> Result TypeTag [NotFound]
get_type_from_env = |@TypeEnv({ scope_stack }), type_id|
    initial_state : Result TypeTag [NotFound]
    initial_state = Err(NotFound)
    Stack.walk_from_top_until(
        scope_stack,
        initial_state,
        |state, type_scope|
            result = get_type_from_scope(type_scope, type_id)
            when result is
                Ok(_) -> Break(result)
                Err(_) -> Continue(result),
    )

push_empty_scope_to_env : TypeEnv -> TypeEnv
push_empty_scope_to_env = |@TypeEnv({ last_id, scope_stack })|
    @TypeEnv({ last_id, scope_stack: Stack.push(scope_stack, Dict.empty({})) })

pop_scope_from_env : TypeEnv -> TypeEnv
pop_scope_from_env = |@TypeEnv({ last_id, scope_stack })|
    @TypeEnv({ last_id, scope_stack: Stack.pop(scope_stack) })

# Constraint Solving
type_id_satisfies_type_id : TypeEnv, TypeId, TypeId -> Result {} {}
type_id_satisfies_type_id = |type_env, type_id_a, type_id_b|
    if type_id_a == type_id_b then
        Ok({})
    else
        crash("Not Implemented: typeIdSatisfiesTypeId")

type_tag_satisfies_boolean : TypeTag, [Boolean] -> Result {} {}
type_tag_satisfies_boolean = |type_a, type_b|
    when type_a is
        Boolean -> Ok({})
        _ -> Err({})

type_id_satisfies_boolean : TypeEnv, TypeId, [Boolean] -> Result {} {}
type_id_satisfies_boolean = |type_env, type_id_a, type_b|
    when get_type_from_env(type_env, type_id_a) is
        Ok(type_a) -> type_tag_satisfies_boolean(type_a, type_b)
        Err(err) -> Err({})

type_tag_satisfies_number : TypeTag, [Number] -> Result {} {}
type_tag_satisfies_number = |type_a, type_b|
    when type_a is
        Number -> Ok({})
        _ -> Err({})

type_id_satisfies_number : TypeEnv, TypeId, [Number] -> Result {} {}
type_id_satisfies_number = |type_env, type_id_a, type_b|
    when get_type_from_env(type_env, type_id_a) is
        Ok(type_a) -> type_tag_satisfies_number(type_a, type_b)
        Err(err) -> Err({})

struct_tag_satisfies_struct : TypeEnv, StructTag, StructTag -> Result {} {}
struct_tag_satisfies_struct = |type_env, Struct({ props: props_a }), Struct({ props: props_b })|
    Dict.walk(
        props_b,
        Ok({}),
        |prev_result, key_b, prop_b|
            current_result =
                when Dict.get(props_a, key_b) is
                    Ok(prop_a) ->
                        type_id_satisfies_type_id(type_env, prop_a, prop_b)

                    Err(err) -> Err({})
            when current_result is
                Ok(_) -> prev_result
                Err(err) ->
                    when prev_result is
                        Ok(_) -> Err({})
                        Err(prev_errors) -> Err({}),
    )

type_id_satisfies_struct : TypeEnv, TypeId, StructTag -> Result {} {}
type_id_satisfies_struct = |type_env, type_id_a, type_b|
    when get_type_from_env(type_env, type_id_a) is
        Ok(type_a) ->
            when type_a is
                Struct(struct_a) -> struct_tag_satisfies_struct(type_env, Struct(struct_a), type_b)
                _ -> Err({})

        Err(err) -> Err({})

type_tag_satisfies_existential : TypeTag, [Existential] -> Result {} {}
type_tag_satisfies_existential = |type_a, type_b|
    Ok({})

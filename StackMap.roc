module []

import Stack
import ResultUtils

StackMap a b :=
    Stack.Stack (Dict a b)
    implements [
        Eq,
        Hash,
    ]

StackMapId a := {
    level : U64,
    key : a,
}
    implements [
        Eq,
        Hash,
    ]

empty : {} -> StackMap a b
empty = |{}| @StackMap(Stack.empty)

is_empty : StackMap a b -> Bool
is_empty = |@StackMap(stack_map)|
    Stack.is_empty(stack_map)

expect is_empty(@StackMap(Stack.empty))
expect is_empty(empty({}))

from_list : List (Dict a b) -> StackMap a b
from_list = |list|
    list |> Stack.from_list |> @StackMap

expect
    from_list(
        [Dict.single("1", 1), Dict.single("2", 2)],
    )
    == @StackMap(
        Stack.from_list(
            [Dict.single("1", 1), Dict.single("2", 2)],
        ),
    )

get_by_id : StackMap a b, StackMapId a -> Result b {}
get_by_id = |@StackMap(stack_map), @StackMapId(id)|
    when Stack.peek_at(stack_map, id.level) is
        Ok(level) ->
            (Dict.get(level, id.key)) |> ResultUtils.map_err_to_empty

        Err(_) -> Err({})

get_by_key : StackMap a b, a -> Result b {}
get_by_key = |@StackMap(stack_map), key|
    initialState : Result b {}
    initialState = Err({})
    Stack.walk_from_top_until(
        stack_map,
        initialState,
        |_, level|
            when Dict.get(level, key) is
                Ok(value) -> Break(Ok(value))
                Err(_) -> Continue(Err({})),
    )

insert : StackMap a b, a, b -> StackMap a b
insert = |@StackMap(stack_map), key, item|
    level_index : U64
    level_index = (Stack.size(stack_map)) - 1
    level_result : Result (Dict a b) [StackWasEmpty]
    level_result = Stack.peek(stack_map)
    when level_result is
        Ok(level) ->
            new_level : Dict a b
            new_level = Dict.insert(level, key, item)
            new_stack_map = @StackMap(((Stack.pop(stack_map)) |> Stack.push(new_level)))
            new_stack_map

        Err(StackWasEmpty) ->
            new_stack = Stack.single(Dict.single(key, item))
            @StackMap(new_stack)

set : StackMap a b, StackMapId a, b -> Result (StackMap a b) [InvalidKey]
set = |@StackMap(stack_map), @StackMapId(id), item|
    when Stack.peek_at(stack_map, id.level) is
        Ok(level) ->
            when Dict.get(level, id.key) is
                Ok(_) ->
                    new_level = Dict.insert(level, id.key, item)
                    when Stack.set_at_level(stack_map, id.level, new_level) is
                        Ok(new_stack_map) -> Ok(@StackMap(new_stack_map))
                        Err(_) -> Err(InvalidKey)

                Err(_) -> Err(InvalidKey)

        Err(_) -> Err(InvalidKey)

pop_level : StackMap a b -> StackMap a b
pop_level = |@StackMap(stack_map)|
    Stack.pop(stack_map) |> @StackMap

push_empty_level : StackMap a b -> StackMap a b
push_empty_level = |@StackMap(stack_map)|
    Stack.push(stack_map, Dict.empty({})) |> @StackMap

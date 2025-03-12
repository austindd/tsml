module []

import Stack

StackMap a :=
    Stack.Stack (List a)
    implements [
        Eq,
        Hash,
    ]

StackMapKey := {
    level : U64,
    index : U64,
}
    implements [
        Eq,
        Hash,
    ]

empty : StackMap _
empty = @StackMap(Stack.empty)

is_empty : StackMap a -> Bool
is_empty = |@StackMap(stack_map)|
    Stack.is_empty(stack_map)

expect is_empty(@StackMap(Stack.empty))

from_list : List (List a) -> StackMap a
from_list = |list|
    @StackMap(Stack.from_list(list))

expect from_list([[1, 2, 3], [4, 5, 6]]) == @StackMap(Stack.from_list([[1, 2, 3], [4, 5, 6]]))

get : StackMap a, StackMapKey -> Result a [NotFound]
get = |@StackMap(stack_map), @StackMapKey(key)|
    when Stack.peek_at(stack_map, key.level) is
        Ok(level) ->
            (List.get(level, key.index)) |> Result.map_err(|_| NotFound)

        Err(a) -> Err(NotFound)

expect (get(@StackMap(Stack.from_list([[1, 2, 3], [4, 5, 6]])), @StackMapKey({ level: 1, index: 1 }))) == Ok(2)
expect (get(@StackMap(Stack.from_list([[1, 2, 3], [4, 5, 6]])), @StackMapKey({ level: 0, index: 2 }))) == Ok(6)

insert : StackMap a, a -> (StackMap a, StackMapKey)
insert = |@StackMap(stack_map), item|
    level_index : U64
    level_index = (Stack.size(stack_map)) - 1
    level_result : Result (List a) [StackWasEmpty]
    level_result = Stack.peek(stack_map)
    when level_result is
        Ok(level) ->
            new_level : List a
            new_level = List.append(level, item)
            index : U64
            index = (List.len(new_level)) - 1
            new_stack_map = @StackMap(((Stack.pop(stack_map)) |> Stack.push(new_level)))
            (
                new_stack_map,
                @StackMapKey(
                    {
                        level: level_index,
                        index: index,
                    },
                ),
            )

        Err(StackWasEmpty) ->
            new_stack = Stack.single([item])
            (
                @StackMap(new_stack),
                @StackMapKey(
                    {
                        level: 0,
                        index: 0,
                    },
                ),
            )

# expect
#     insert(@StackMap(Stack.from_list([[1, 2, 3]])), 7)
#     == (
#         @StackMap(Stack.from_list([[1, 2, 3, 7], []])),
#         @StackMapKey({ level: 1, index: 3 }),
#     )

update : StackMap a, StackMapKey, a -> Result (StackMap a) [InvalidKey]
update = |@StackMap(stack_map), @StackMapKey(key), item|
    when Stack.peek_at(stack_map, key.level) is
        Ok(level) ->
            when List.get(level, key.index) is
                Ok(_) ->
                    new_level = List.set(level, key.index, item)
                    when Stack.set_at_level(stack_map, key.level, new_level) is
                        Ok(new_stack_map) -> Ok(@StackMap(new_stack_map))
                        Err(_) -> Err(InvalidKey)

                Err(_) -> Err(InvalidKey)

        Err(_) -> Err(InvalidKey)

pop_level : StackMap a -> StackMap a
pop_level = |@StackMap(stack_map)|
    new_stack_map = Stack.pop(stack_map)
    @StackMap(new_stack_map)

push_empty_level : StackMap a -> StackMap a
push_empty_level = |@StackMap(stack_map)|
    new_stack_map = Stack.push(stack_map, [])
    @StackMap(new_stack_map)

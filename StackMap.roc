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

empty : StackMap a
empty = @StackMap (Stack.empty)

isEmpty : StackMap a -> Bool
isEmpty = \@StackMap stackMap ->
    Stack.isEmpty stackMap

expect isEmpty (@StackMap (Stack.empty))

fromList : List (List a) -> StackMap a
fromList = \list ->
    @StackMap (Stack.fromList list)

expect fromList [[1, 2, 3], [4, 5, 6]] == @StackMap (Stack.fromList [[1, 2, 3], [4, 5, 6]])

get : StackMap a, StackMapKey -> Result a [NotFound]
get = \@StackMap stackMap, @StackMapKey key ->
    when Stack.peekAt stackMap key.level is
        Ok level ->
            (List.get level key.index) |> Result.mapErr (\_ -> NotFound)

        Err a -> Err NotFound

expect (get (@StackMap (Stack.fromList [[1, 2, 3], [4, 5, 6]])) (@StackMapKey { level: 1, index: 1 })) == Ok 2
expect (get (@StackMap (Stack.fromList [[1, 2, 3], [4, 5, 6]])) (@StackMapKey { level: 0, index: 2 })) == Ok 6

insert : StackMap a, a -> (StackMap a, StackMapKey)
insert = \@StackMap stackMap, item ->
    levelIndex : U64
    levelIndex = (Stack.size stackMap) - 1
    levelResult : Result (List a) [StackWasEmpty]
    levelResult = Stack.peek stackMap
    when levelResult is
        Ok level ->
            newLevel : List a
            newLevel = List.append level item
            index : U64
            index = (List.len newLevel) - 1
            newStackMap = @StackMap ((Stack.pop stackMap) |> Stack.push newLevel)
            (
                newStackMap,
                @StackMapKey {
                    level: levelIndex,
                    index: index,
                },
            )

        Err StackWasEmpty ->
            newStack = Stack.single [item]
            (
                @StackMap newStack,
                @StackMapKey {
                    level: 0,
                    index: 0,
                },
            )

expect
    insert (@StackMap (Stack.fromList [[1, 2, 3]])) 7
    == (
        @StackMap (Stack.fromList [[1, 2, 3, 7], []]),
        @StackMapKey { level: 1, index: 3 },
    )

update : StackMap a, StackMapKey, a -> Result (StackMap a) [InvalidKey]
update = \@StackMap stackMap, @StackMapKey key, item ->
    when Stack.peekAt stackMap key.level is
        Ok level ->
            when List.get level key.index is
                Ok _ ->
                    newLevel = List.set level key.index item
                    when Stack.setAtLevel stackMap key.level newLevel is
                        Ok newStackMap -> Ok (@StackMap newStackMap)
                        Err _ -> Err InvalidKey

                Err _ -> Err InvalidKey

        Err _ -> Err InvalidKey

popLevel : StackMap a -> StackMap a
popLevel = \@StackMap stackMap ->
    newStackMap = Stack.pop stackMap
    @StackMap newStackMap

pushEmptyLevel : StackMap a -> StackMap a
pushEmptyLevel = \@StackMap stackMap ->
    newStackMap = Stack.push stackMap []
    @StackMap newStackMap

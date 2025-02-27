module [
    Stack,
    empty,
    isEmpty,
    size,
    fromList,
    toList,
    single,
    push,
    pushList,
    pushStack,
    pop,
    popElement,
    popN,
    popNElements,
    shrinkTo,
    peek,
    peekAt,
    peekN,
    map,
    mapFromTopWithIndex,
    mapFromBottomWithIndex,
    walkFromTop,
    walkFromBottom,
    walkFromTopWithIndex,
    walkFromBottomWithIndex,
    walkFromTopUntil,
    walkFromBottomUntil,
    findFromTop,
    findFromBottom,
    findIndexFromTop,
    findIndexFromBottom,
    invert,
    forEach!,
    forEachTry!,
    walkTry,
    setAtLevel,
]

import Hash

Stack a := List a implements [
        Eq,
        Hash,
    ]

empty : Stack a
empty = @Stack []

isEmpty : Stack a -> Bool
isEmpty = \@Stack stack ->
    List.isEmpty stack

size : Stack a -> U64
size = \@Stack stack ->
    List.len stack

# Creates a `Stack` from an insertion-ordered `List`
fromList : List a -> Stack a
fromList = \list ->
    @Stack list

# Returns `List` of elements in order of insertion
toList : Stack a -> List a
toList = \@Stack stack ->
    stack

single : a -> Stack a
single = \a ->
    @Stack [a]

push : Stack a, a -> Stack a
push = \@Stack stack, a ->
    @Stack (List.append stack a)

expect (push (@Stack [1, 2, 3]) 4 == @Stack [1, 2, 3, 4])

pushList : Stack a, List a -> Stack a
pushList = \@Stack stack, list ->
    @Stack (List.concat stack list)

expect (pushList (@Stack [1, 2, 3]) [4, 5, 6] == @Stack [1, 2, 3, 4, 5, 6])
expect (pushList (@Stack []) [4, 5, 6] == @Stack [4, 5, 6])
expect (pushList (@Stack [1, 2, 3]) [] == @Stack [1, 2, 3])

pushStack : Stack a, Stack a -> Stack a
pushStack = \@Stack stack1, @Stack stack2 ->
    @Stack (List.concat stack1 stack2)

expect (pushStack (@Stack [1, 2, 3]) (@Stack [4, 5, 6]) == @Stack [1, 2, 3, 4, 5, 6])

pop : Stack a -> Stack a
pop = \@Stack stack ->
    newStack = List.dropLast stack 1
    @Stack newStack

expect (pop (@Stack [1, 2, 3, 4]) == @Stack [1, 2, 3])
expect (pop (@Stack []) == @Stack [])

popElement : Stack a -> (Stack a, Result a [ListWasEmpty])
popElement = \@Stack stack ->
    elementResult = List.last stack
    newStack = pop (@Stack stack)
    (newStack, elementResult)

expect (popElement (@Stack [1, 2, 3, 4]) == (@Stack [1, 2, 3], Ok 4))
expect (popElement (@Stack []) == (@Stack [], Err ListWasEmpty))

popN : Stack a, U64 -> Stack a
popN = \@Stack stack, n ->
    newStack = List.dropLast stack n
    @Stack newStack

expect (popN (@Stack [1, 2, 3, 4, 5, 6]) 3 == @Stack [1, 2, 3])
expect (popN (@Stack [1, 2, 3, 4, 5, 6]) 0 == @Stack [1, 2, 3, 4, 5, 6])
expect (popN (@Stack [1, 2, 3, 4, 5, 6]) 6 == @Stack [])
expect (popN (@Stack []) 3 == @Stack [])

popNElements : Stack a, U64 -> (Stack a, List a)
popNElements = \@Stack stack, n ->
    elements = List.takeLast stack n
    newStack = popN (@Stack stack) n
    (newStack, elements)

expect (popNElements (@Stack [1, 2, 3, 4, 5, 6]) 3 == (@Stack [1, 2, 3], [4, 5, 6]))
expect (popNElements (@Stack [1, 2, 3, 4, 5, 6]) 0 == (@Stack [1, 2, 3, 4, 5, 6], []))
expect (popNElements (@Stack [1, 2, 3, 4, 5, 6]) 6 == (@Stack [], [1, 2, 3, 4, 5, 6]))
expect (popNElements (@Stack []) 3 == (@Stack [], []))

shrinkTo : Stack a, U64 -> Stack a
shrinkTo = \@Stack stack, n ->
    @Stack (List.takeFirst stack n)

expect (shrinkTo (@Stack [1, 2, 3, 4, 5, 6]) 3 == @Stack [1, 2, 3])
expect (shrinkTo (@Stack [1, 2, 3, 4, 5, 6]) 0 == @Stack [])
expect (shrinkTo (@Stack []) 3 == @Stack [])

peek : Stack a -> Result a [StackWasEmpty]
peek = \@Stack stack ->
    Result.mapErr (List.last stack) (\_ -> StackWasEmpty)

expect (peek (@Stack [1, 2, 3, 4]) == Ok 4)
expect (peek (@Stack []) == Err StackWasEmpty)

peekAt : Stack a, U64 -> Result a [OutOfBounds]
peekAt = \@Stack stack, index ->
    List.get (List.reverse stack) index

expect (peekAt (@Stack [1, 2, 3, 4]) 0 == Ok 4)
expect (peekAt (@Stack [1, 2, 3, 4]) 3 == Ok 1)
expect (peekAt (@Stack [1, 2, 3, 4]) 4 == Err OutOfBounds)
expect (peekAt (@Stack []) 0 == Err OutOfBounds)

peekN : Stack a, U64 -> List a
peekN = \@Stack stack, n ->
    List.reverse (List.takeLast stack n)

expect (peekN (@Stack [1, 2, 3, 4]) 2 == [4, 3])
expect (peekN (@Stack [1, 2, 3, 4]) 0 == [])
expect (peekN (@Stack [1, 2, 3, 4]) 4 == [4, 3, 2, 1])

map : Stack a, (a -> b) -> Stack b
map = \@Stack stack, f ->
    @Stack (List.map stack f)

expect (map (@Stack [1, 2, 3]) (\x -> x + 1) == @Stack [2, 3, 4])
expect (map (@Stack []) (\x -> x + 1) == @Stack [])

# Maps over a `Stack` from top to bottom
mapFromTopWithIndex : Stack a, (a, U64 -> b) -> Stack b
mapFromTopWithIndex = \@Stack stack, f ->
    @Stack (List.mapWithIndex (List.reverse stack) f)

expect (mapFromTopWithIndex (@Stack [1, 2, 3]) (\x, i -> x + i) == @Stack [3, 3, 3])
expect (mapFromTopWithIndex (@Stack []) (\x, i -> x + i) == @Stack [])

# Maps over a `Stack` from bottom to top
mapFromBottomWithIndex : Stack a, (a, U64 -> b) -> Stack b
mapFromBottomWithIndex = \@Stack stack, f ->
    @Stack (List.mapWithIndex stack f)

expect (mapFromBottomWithIndex (@Stack [1, 2, 3]) (\x, i -> x + i) == @Stack [1, 3, 5])
expect (mapFromBottomWithIndex (@Stack []) (\x, i -> x + i) == @Stack [])

walkFromTop : Stack a, state, (state, a -> state) -> state
walkFromTop = \@Stack stack, state, f ->
    List.walkBackwards stack state f

expect (walkFromTop (@Stack [1, 2, 3]) 0 (\s, x -> s + x) == 6)
expect (walkFromTop (@Stack []) 0 (\s, x -> s + x) == 0)

walkFromBottom : Stack a, state, (state, a -> state) -> state
walkFromBottom = \@Stack stack, state, f ->
    List.walk stack state f

expect (walkFromBottom (@Stack [1, 2, 3]) 0 (\s, x -> s + x) == 6)
expect (walkFromBottom (@Stack []) 0 (\s, x -> s + x) == 0)

walkFromTopWithIndex : Stack a, state, (state, a, U64 -> state) -> state
walkFromTopWithIndex = \@Stack stack, state, f ->
    List.walkWithIndex (List.reverse stack) state f

walkFromBottomWithIndex : Stack a, state, (state, a, U64 -> state) -> state
walkFromBottomWithIndex = \@Stack stack, state, f ->
    List.walkWithIndex stack state f

walkFromTopUntil : Stack a, state, (state, a -> [Continue state, Break state]) -> state
walkFromTopUntil = \@Stack stack, state, f ->
    List.walkBackwardsUntil stack state f

expect (walkFromTopUntil (@Stack [1, 2, 3]) 0 (\s, x -> if x == 2 then Break s else Continue (s + x)) == 3)
expect (walkFromTopUntil (@Stack []) 0 (\s, x -> if x == 2 then Break s else Continue (s + x)) == 0)

walkFromBottomUntil : Stack a, state, (state, a -> [Continue state, Break state]) -> state
walkFromBottomUntil = \@Stack stack, state, f ->
    List.walkUntil stack state f

expect (walkFromBottomUntil (@Stack [1, 2, 3]) 0 (\s, x -> if x == 2 then Break s else Continue (s + x)) == 1)
expect (walkFromBottomUntil (@Stack []) 0 (\s, x -> if x == 2 then Break s else Continue (s + x)) == 0)

findFromTop : Stack a, (a -> Bool) -> Result a [NotFound]
findFromTop = \@Stack stack, f ->
    List.findLast stack f

expect (findFromTop (@Stack [1, 2, 3]) (\x -> x == 2) == Ok 2)
expect (findFromTop (@Stack [1, 2, 3]) (\x -> x == 4) == Err NotFound)
expect (findFromTop (@Stack []) (\x -> x == 4) == Err NotFound)

findFromBottom : Stack a, (a -> Bool) -> Result a [NotFound]
findFromBottom = \@Stack stack, f ->
    List.findFirst stack f

expect (findFromBottom (@Stack [1, 2, 3]) (\x -> x == 2) == Ok 2)
expect (findFromBottom (@Stack [1, 2, 3]) (\x -> x == 4) == Err NotFound)
expect (findFromBottom (@Stack []) (\x -> x == 4) == Err NotFound)

findIndexFromTop : Stack a, (a -> Bool) -> Result U64 [NotFound]
findIndexFromTop = \@Stack stack, f ->
    Result.map (List.findLastIndex stack f) (\x -> List.len stack - x - 1)

expect (findIndexFromTop (@Stack [1, 2, 3, 4]) (\x -> x == 2) == Ok 2)
expect (findIndexFromTop (@Stack [1, 2, 3, 4]) (\x -> x == 5) == Err NotFound)
expect (findIndexFromTop (@Stack [1, 2, 3, 4]) (\x -> x == 4) == Ok 0)
expect (findIndexFromTop (@Stack []) (\x -> x == 5) == Err NotFound)

findIndexFromBottom : Stack a, (a -> Bool) -> Result U64 [NotFound]
findIndexFromBottom = \@Stack stack, f ->
    List.findFirstIndex stack f

expect (findIndexFromBottom (@Stack [1, 2, 3, 4]) (\x -> x == 2) == Ok 1)
expect (findIndexFromBottom (@Stack [1, 2, 3, 4]) (\x -> x == 5) == Err NotFound)
expect (findIndexFromBottom (@Stack []) (\x -> x == 5) == Err NotFound)

invert : Stack a -> Stack a
invert = \@Stack stack ->
    @Stack (List.reverse stack)

expect (invert (@Stack [1, 2, 3]) == @Stack [3, 2, 1])
expect (invert (@Stack []) == @Stack [])

forEach! : Stack a, (a -> {}) -> {}
forEach! = \@Stack stack, f ->
    List.forEach! (List.reverse stack) f

forEachTry! : Stack a, (a -> Result {} err) -> Result {} err
forEachTry! = \@Stack stack, f ->
    List.forEachTry! (List.reverse stack) f

walkTry : Stack a, b, (b, a -> Result b err) -> Result b err
walkTry = \@Stack stack, b, f ->
    List.walkTry (List.reverse stack) b f

expect (walkTry (@Stack [1, 2, 3]) 0 (\s, x -> Ok (s + x)) == Ok 6)

setAtLevel : Stack a, U64, a -> Result (Stack a) [OutOfBounds]
setAtLevel = \@Stack stack, level, item ->
    when List.get stack level is
        Ok _ ->
            newStack : List a
            newStack = List.set stack level item
            Ok (@Stack newStack)

        Err OutOfBounds -> Err OutOfBounds

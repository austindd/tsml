module [
    Stack,
    empty,
    is_empty,
    size,
    from_list,
    to_list,
    single,
    push,
    push_list,
    push_stack,
    pop,
    pop_element,
    pop_n,
    pop_n_elements,
    shrink_to,
    peek,
    peek_at,
    peek_n,
    map,
    map_from_top_with_index,
    map_from_bottom_with_index,
    walk_from_top,
    walk_from_bottom,
    walk_from_top_with_index,
    walk_from_bottom_with_index,
    walk_from_top_until,
    walk_from_bottom_until,
    find_from_top,
    find_from_bottom,
    find_index_from_top,
    find_index_from_bottom,
    invert,
    for_each!,
    for_each_try!,
    walk_try,
    set_at_level,
    update,
]

import Hash

Stack a := List a implements [
        Eq,
        Hash,
    ]

empty : Stack _
empty = @Stack([])

is_empty : Stack a -> Bool
is_empty = |@Stack(stack)|
    List.is_empty(stack)

size : Stack a -> U64
size = |@Stack(stack)|
    List.len(stack)

# Creates a `Stack` from an insertion-ordered `List`
from_list : List a -> Stack a
from_list = |list|
    @Stack(list)

# Returns `List` of elements in order of insertion
to_list : Stack a -> List a
to_list = |@Stack(stack)|
    stack

single : a -> Stack a
single = |a|
    @Stack([a])

push : Stack a, a -> Stack a
push = |@Stack(stack), a|
    @Stack(List.append(stack, a))

expect (push(@Stack([1, 2, 3]), 4) == @Stack([1, 2, 3, 4]))

push_list : Stack a, List a -> Stack a
push_list = |@Stack(stack), list|
    @Stack(List.concat(stack, list))

expect (push_list(@Stack([1, 2, 3]), [4, 5, 6]) == @Stack([1, 2, 3, 4, 5, 6]))
expect (push_list(@Stack([]), [4, 5, 6]) == @Stack([4, 5, 6]))
expect (push_list(@Stack([1, 2, 3]), []) == @Stack([1, 2, 3]))

push_stack : Stack a, Stack a -> Stack a
push_stack = |@Stack(stack1), @Stack(stack2)|
    @Stack(List.concat(stack1, stack2))

expect (push_stack(@Stack([1, 2, 3]), @Stack([4, 5, 6])) == @Stack([1, 2, 3, 4, 5, 6]))

pop : Stack a -> Stack a
pop = |@Stack(stack)|
    new_stack = List.drop_last(stack, 1)
    @Stack(new_stack)

expect (pop(@Stack([1, 2, 3, 4])) == @Stack([1, 2, 3]))
expect (pop(@Stack([])) == @Stack([]))

pop_element : Stack a -> (Stack a, Result a [ListWasEmpty])
pop_element = |@Stack(stack)|
    element_result = List.last(stack)
    new_stack = pop(@Stack(stack))
    (new_stack, element_result)

expect (pop_element(@Stack([1, 2, 3, 4])) == (@Stack([1, 2, 3]), Ok(4)))
expect (pop_element(@Stack([])) == (@Stack([]), Err(ListWasEmpty)))

pop_n : Stack a, U64 -> Stack a
pop_n = |@Stack(stack), n|
    new_stack = List.drop_last(stack, n)
    @Stack(new_stack)

expect (pop_n(@Stack([1, 2, 3, 4, 5, 6]), 3) == @Stack([1, 2, 3]))
expect (pop_n(@Stack([1, 2, 3, 4, 5, 6]), 0) == @Stack([1, 2, 3, 4, 5, 6]))
expect (pop_n(@Stack([1, 2, 3, 4, 5, 6]), 6) == @Stack([]))
expect (pop_n(@Stack([]), 3) == @Stack([]))

pop_n_elements : Stack a, U64 -> (Stack a, List a)
pop_n_elements = |@Stack(stack), n|
    elements = List.take_last(stack, n)
    new_stack = pop_n(@Stack(stack), n)
    (new_stack, elements)

expect (pop_n_elements(@Stack([1, 2, 3, 4, 5, 6]), 3) == (@Stack([1, 2, 3]), [4, 5, 6]))
expect (pop_n_elements(@Stack([1, 2, 3, 4, 5, 6]), 0) == (@Stack([1, 2, 3, 4, 5, 6]), []))
expect (pop_n_elements(@Stack([1, 2, 3, 4, 5, 6]), 6) == (@Stack([]), [1, 2, 3, 4, 5, 6]))
expect (pop_n_elements(@Stack([]), 3) == (@Stack([]), []))

shrink_to : Stack a, U64 -> Stack a
shrink_to = |@Stack(stack), n|
    @Stack(List.take_first(stack, n))

expect (shrink_to(@Stack([1, 2, 3, 4, 5, 6]), 3) == @Stack([1, 2, 3]))
expect (shrink_to(@Stack([1, 2, 3, 4, 5, 6]), 0) == @Stack([]))
expect (shrink_to(@Stack([]), 3) == @Stack([]))

peek : Stack a -> Result a [StackWasEmpty]
peek = |@Stack(stack)|
    Result.map_err(List.last(stack), |_| StackWasEmpty)

expect (peek(@Stack([1, 2, 3, 4])) == Ok(4))
expect (peek(@Stack([])) == Err(StackWasEmpty))

peek_at : Stack a, U64 -> Result a [OutOfBounds]
peek_at = |@Stack(stack), index|
    List.get(List.reverse(stack), index)

expect (peek_at(@Stack([1, 2, 3, 4]), 0) == Ok(4))
expect (peek_at(@Stack([1, 2, 3, 4]), 3) == Ok(1))
expect (peek_at(@Stack([1, 2, 3, 4]), 4) == Err(OutOfBounds))
expect (peek_at(@Stack([]), 0) == Err(OutOfBounds))

peek_n : Stack a, U64 -> List a
peek_n = |@Stack(stack), n|
    List.reverse(List.take_last(stack, n))

expect (peek_n(@Stack([1, 2, 3, 4]), 2) == [4, 3])
expect (peek_n(@Stack([1, 2, 3, 4]), 0) == [])
expect (peek_n(@Stack([1, 2, 3, 4]), 4) == [4, 3, 2, 1])

map : Stack a, (a -> b) -> Stack b
map = |@Stack(stack), f|
    @Stack(List.map(stack, f))

expect (map(@Stack([1, 2, 3]), |x| x + 1) == @Stack([2, 3, 4]))
expect (map(@Stack([]), |x| x + 1) == @Stack([]))

# Maps over a `Stack` from top to bottom
map_from_top_with_index : Stack a, (a, U64 -> b) -> Stack b
map_from_top_with_index = |@Stack(stack), f|
    @Stack(List.map_with_index(List.reverse(stack), f))

expect (map_from_top_with_index(@Stack([1, 2, 3]), |x, i| x + i) == @Stack([3, 3, 3]))
expect (map_from_top_with_index(@Stack([]), |x, i| x + i) == @Stack([]))

# Maps over a `Stack` from bottom to top
map_from_bottom_with_index : Stack a, (a, U64 -> b) -> Stack b
map_from_bottom_with_index = |@Stack(stack), f|
    @Stack(List.map_with_index(stack, f))

expect (map_from_bottom_with_index(@Stack([1, 2, 3]), |x, i| x + i) == @Stack([1, 3, 5]))
expect (map_from_bottom_with_index(@Stack([]), |x, i| x + i) == @Stack([]))

walk_from_top : Stack a, state, (state, a -> state) -> state
walk_from_top = |@Stack(stack), state, f|
    List.walk_backwards(stack, state, f)

expect (walk_from_top(@Stack([1, 2, 3]), 0, |s, x| s + x) == 6)
expect (walk_from_top(@Stack([]), 0, |s, x| s + x) == 0)

walk_from_bottom : Stack a, state, (state, a -> state) -> state
walk_from_bottom = |@Stack(stack), state, f|
    List.walk(stack, state, f)

expect (walk_from_bottom(@Stack([1, 2, 3]), 0, |s, x| s + x) == 6)
expect (walk_from_bottom(@Stack([]), 0, |s, x| s + x) == 0)

walk_from_top_with_index : Stack a, state, (state, a, U64 -> state) -> state
walk_from_top_with_index = |@Stack(stack), state, f|
    List.walk_with_index(List.reverse(stack), state, f)

walk_from_bottom_with_index : Stack a, state, (state, a, U64 -> state) -> state
walk_from_bottom_with_index = |@Stack(stack), state, f|
    List.walk_with_index(stack, state, f)

walk_from_top_until : Stack a, state, (state, a -> [Continue state, Break state]) -> state
walk_from_top_until = |@Stack(stack), state, f|
    List.walk_backwards_until(stack, state, f)

expect (walk_from_top_until(@Stack([1, 2, 3]), 0, |s, x| if x == 2 then Break(s) else Continue((s + x))) == 3)
expect (walk_from_top_until(@Stack([]), 0, |s, x| if x == 2 then Break(s) else Continue((s + x))) == 0)

walk_from_bottom_until : Stack a, state, (state, a -> [Continue state, Break state]) -> state
walk_from_bottom_until = |@Stack(stack), state, f|
    List.walk_until(stack, state, f)

expect (walk_from_bottom_until(@Stack([1, 2, 3]), 0, |s, x| if x == 2 then Break(s) else Continue((s + x))) == 1)
expect (walk_from_bottom_until(@Stack([]), 0, |s, x| if x == 2 then Break(s) else Continue((s + x))) == 0)

find_from_top : Stack a, (a -> Bool) -> Result a [NotFound]
find_from_top = |@Stack(stack), f|
    List.find_last(stack, f)

expect (find_from_top(@Stack([1, 2, 3]), |x| x == 2) == Ok(2))
expect (find_from_top(@Stack([1, 2, 3]), |x| x == 4) == Err(NotFound))
expect (find_from_top(@Stack([]), |x| x == 4) == Err(NotFound))

find_from_bottom : Stack a, (a -> Bool) -> Result a [NotFound]
find_from_bottom = |@Stack(stack), f|
    List.find_first(stack, f)

expect (find_from_bottom(@Stack([1, 2, 3]), |x| x == 2) == Ok(2))
expect (find_from_bottom(@Stack([1, 2, 3]), |x| x == 4) == Err(NotFound))
expect (find_from_bottom(@Stack([]), |x| x == 4) == Err(NotFound))

find_index_from_top : Stack a, (a -> Bool) -> Result U64 [NotFound]
find_index_from_top = |@Stack(stack), f|
    Result.map_ok(List.find_last_index(stack, f), |x| List.len(stack) - x - 1)

expect (find_index_from_top(@Stack([1, 2, 3, 4]), |x| x == 2) == Ok(2))
expect (find_index_from_top(@Stack([1, 2, 3, 4]), |x| x == 5) == Err(NotFound))
expect (find_index_from_top(@Stack([1, 2, 3, 4]), |x| x == 4) == Ok(0))
expect (find_index_from_top(@Stack([]), |x| x == 5) == Err(NotFound))

find_index_from_bottom : Stack a, (a -> Bool) -> Result U64 [NotFound]
find_index_from_bottom = |@Stack(stack), f|
    List.find_first_index(stack, f)

expect (find_index_from_bottom(@Stack([1, 2, 3, 4]), |x| x == 2) == Ok(1))
expect (find_index_from_bottom(@Stack([1, 2, 3, 4]), |x| x == 5) == Err(NotFound))
expect (find_index_from_bottom(@Stack([]), |x| x == 5) == Err(NotFound))

invert : Stack a -> Stack a
invert = |@Stack(stack)|
    @Stack(List.reverse(stack))

expect (invert(@Stack([1, 2, 3])) == @Stack([3, 2, 1]))
expect (invert(@Stack([])) == @Stack([]))

for_each! : Stack a, (a -> {}) -> {}
for_each! = |@Stack(stack), f|
    List.for_each!(List.reverse(stack), f)

for_each_try! : Stack a, (a -> Result {} err) -> Result {} err
for_each_try! = |@Stack(stack), f|
    List.for_each_try!(List.reverse(stack), f)

walk_try : Stack a, b, (b, a -> Result b err) -> Result b err
walk_try = |@Stack(stack), b, f|
    List.walk_try(List.reverse(stack), b, f)

expect (walk_try(@Stack([1, 2, 3]), 0, |s, x| Ok((s + x))) == Ok(6))

set_at_level : Stack a, U64, a -> Result (Stack a) [OutOfBounds]
set_at_level = |@Stack(stack), level, item|
    when List.get(stack, level) is
        Ok(_) ->
            new_stack : List a
            new_stack = List.set(stack, level, item)
            Ok(@Stack(new_stack))

        Err(OutOfBounds) -> Err(OutOfBounds)

update : Stack a, U64, (a -> a) -> Stack a
update = |@Stack(stack), level, update_value|
    List.update(stack, level, update_value) |> @Stack

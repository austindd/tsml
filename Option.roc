module [
    Option,
    Some,
    None,
    is_some,
    is_none,
    map,
    map2,
    map3,
    join_map,
    unsafe_get,
    combine2,
    combine3,
    gather,
    compact,
]

Option a : [Some a, None]

Some a : [Some a]

None : [None]

is_some : Option a -> Bool
is_some = |option|
    when option is
        Some(_) -> Bool.true
        None -> Bool.false

is_none : Option a -> Bool
is_none = |option|
    when option is
        Some(_) -> Bool.false
        None -> Bool.true

map : Option a, (a -> b) -> Option b
map = |option, f|
    when option is
        Some(x) -> Some(f(x))
        None -> None

map2 : Option a, Option b, (a, b -> c) -> Option c
map2 = |option_a, option_b, f|
    when (option_a, option_b) is
        (Some(a), Some(b)) -> Some(f(a, b))
        (None, _) -> None
        (_, None) -> None

map3 : Option a, Option b, Option c, (a, b, c -> d) -> Option d
map3 = |option_a, option_b, option_c, f|
    when (option_a, option_b, option_c) is
        (Some(a), Some(b), Some(c)) -> Some(f(a, b, c))
        (None, _, _) -> None
        (_, None, _) -> None
        (_, _, None) -> None

join_map : Option a, (a -> Option b) -> Option b
join_map = |option, f|
    when option is
        Some(x) -> f(x)
        None -> None

unsafe_get : Option a -> a
unsafe_get = |option|
    when option is
        Some(x) -> x
        None -> crash("Option.unsafeGet: called on None")

combine2 : Option a, Option b -> Option (a, b)
combine2 = |option_a, option_b|
    when option_a is
        Some(a) ->
            when option_b is
                Some(b) -> Some((a, b))
                None -> None

        None -> None

combine3 : Option a, Option b, Option c -> Option (a, b, c)
combine3 = |option_a, option_b, option_c|
    when option_a is
        Some(a) ->
            when option_b is
                Some(b) ->
                    when option_c is
                        Some(c) -> Some((a, b, c))
                        None -> None

                None -> None

        None -> None

gather : List (Option a) -> Option (List a)
gather = |options|
    List.walk_until(
        options,
        Some([]),
        |state, option|
            when state is
                Some(list) ->
                    when option is
                        Some(x) -> Continue(Some(List.concat(list, [x])))
                        None -> Break(None)

                None -> Break(None),
    )

compact : List (Option a) -> List a
compact = |options|
    List.walk(
        options,
        List.with_capacity(List.len(options)),
        |state, option|
            when option is
                Some(x) -> List.concat(state, [x])
                None -> state,
    )

from_result : Result a b -> Option a
from_result = |result|
    when result is
        Ok(value) -> Some(value)
        Err(_) -> None

to_result : Option a -> Result a {}
to_result = |option|
    when option is
        Some(value) -> Ok(value)
        None -> Err({})

with_default : Option a, a -> a
with_default = |option, default|
    when option is
        Some(value) -> value
        None -> default


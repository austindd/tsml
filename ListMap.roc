module [
    ListMap,
    empty,
    single,
]

import Option exposing [Option, Some, None]

Ordering : [EQ, LT, GT]

Ord implements
    compare : a, a -> Ordering where a implements Ord

ListMap a b := {
    keys : List a,
    values : List (Option b),
}

empty : {} -> ListMap a b
empty = |{}|
    @ListMap(
        {
            keys: [],
            values: [],
        },
    )

single : a, b -> ListMap a b
single = |key, value|
    @ListMap(
        {
            keys: [key],
            values: [Some(value)],
        },
    )

insert : ListMap a b, a, b -> ListMap a b where a implements Ord
insert = |@ListMap(list_map), key, value|
    newKeys = List.append(list_map.keys, key)
    newValues = List.append(list_map.values, Some(value))
    @ListMap(
        {
            keys: newKeys,
            values: newValues,
        },
    )

# get : ListMap a b, a -> ListMap a b
# get = |@ListMap(list_map), key|

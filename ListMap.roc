module [
    ListMap,
    empty,
    single,
    insert,
    get,
    remove,
    map,
]

import Option exposing [Option, Some, None]

ListMap a b := {
    key_eq : a, a -> Bool,
    keys : List a,
    values : List Option b,
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
            values: [value],
        },
    )


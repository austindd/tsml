module [
    empty,
    insert,
    get,
    map,
    walk_entries,
    to_list,
]

import AvlTree exposing [Ordering, Ord]

NumKey a := Num a implements [
        Ord {
            compare: num_compare,
        },
    ]

num_compare : NumKey a, NumKey a -> Ordering
num_compare = |@NumKey(a), @NumKey(b)|
    Num.compare(a, b)

AvlTreeNum k v := AvlTree.Avl (NumKey k) v

empty : {} -> AvlTreeNum k v
empty = |{}| AvlTree.empty({}) |> @AvlTreeNum

get : AvlTreeNum k v, Num k -> Result v {}
get = |@AvlTreeNum(tree), key|
    AvlTree.get(tree, @NumKey(key))

insert : AvlTreeNum k v, Num k, v -> AvlTreeNum k v
insert = |@AvlTreeNum(tree), key, value|
    AvlTree.insert(tree, @NumKey(key), value) |> @AvlTreeNum

map : AvlTreeNum k v, (v -> w) -> AvlTreeNum k w
map = |@AvlTreeNum(tree), fn|
    AvlTree.map(tree, fn) |> @AvlTreeNum

walk_entries : AvlTreeNum k v, state, (state, Num k, v -> state) -> state
walk_entries = |@AvlTreeNum(tree), state, fn|
    AvlTree.walk_entries(
        tree,
        state,
        |s, @NumKey(k), v| fn(s, k, v),
    )

to_list : AvlTreeNum k v -> List (Num k, v)
to_list = |@AvlTreeNum(tree)|
    AvlTree.walk_entries(
        tree,
        [],
        |list, @NumKey(key), value|
            List.append(list, (key, value)),
    )

test : {} -> AvlTreeNum a Str
test = |{}|
    empty({})
    |> insert(1, "1")

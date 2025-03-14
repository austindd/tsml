module [
    Avl,
    Ordering,
    Ord,
    compare,
    empty,
    insert,
    get,
    map,
    walk_entries,
    to_list,
]

Ordering : [EQ, LT, GT]

Ord implements
    compare : a, a -> Ordering where a implements Ord

Avl a b : [
    Empty,
    Node {
            l : Avl a b,
            k : a,
            v : b,
            h : U64,
            r : Avl a b,
        },
    Leaf {
            k : a,
            v : b,
        },
] where a implements Ord

impossible = "Impossible"

empty : {} -> Avl a b
empty = |{}| Empty

mknode = |l, k, v, r|
    Node({ l, k, v, h: 1 + Num.max(height(l), height(r)), r })

height : Avl a b -> U64
height = |avl|
    when avl is
        Empty -> 0
        Leaf(_) -> 1
        Node({ h }) -> h

rotate_right : Avl a b -> Avl a b
rotate_right = |avl|
    when avl is
        Node({ l: Node({ l: t1, k: y2k, v: y2v, r: t3 }), k: y4k, v: y4v, r: t5 }) ->
            mknode(mknode(t1, y2k, y2v, t3), y4k, y4v, t5)

        _ -> crash impossible

rotate_left : Avl a b -> Avl a b
rotate_left = |avl|
    when avl is
        Node({ l: t1, k: y2k, v: y2v, r: Node({ l: t3, k: y4k, v: y4v, r: t5 }) }) ->
            mknode(t1, y2k, y2v, mknode(t3, y4k, y4v, t5))

        _ -> crash impossible

balance : Avl a b -> Avl a b
balance = |avl|
    when avl is
        Empty | Leaf(_) -> avl
        Node(root_node) ->
            { l, r } = root_node
            hl = height(l)
            hr = height(r)
            if hl > hr + 2 then
                when l is
                    Empty | Leaf(_) -> crash impossible
                    Node(l_node) ->
                        { l: l_node_l, r: l_node_r } = l_node
                        if height(l_node_l) >= height(l_node_r) then
                            rotate_right(avl)
                        else
                            avl
            else if hr > hl + 2 then
                when r is
                    Empty | Leaf(_) -> crash impossible
                    Node(r_node) ->
                        { l: r_node_l, r: r_node_r } = r_node
                        if height(r_node_r) >= height(r_node_l) then
                            rotate_left(avl)
                        else
                            avl
            else
                avl

insert : Avl a b, a, b -> Avl a b
insert = |avl, key, value|
    when avl is
        Empty -> Leaf({ k: key, v: value })
        Leaf({ k, v }) ->
            when compare(key, k) is
                EQ -> Leaf({ k, v: value })
                LT -> balance(mknode(Empty, key, value, avl))
                GT -> balance(mknode(avl, key, value, Empty))

        Node({ l, k, v, h, r }) ->
            when compare(key, k) is
                EQ -> mknode(l, k, value, r)
                LT ->
                    new_left = insert(l, key, value)
                    mknode(new_left, k, v, r)

                GT ->
                    new_right = insert(r, key, value)
                    mknode(l, k, v, new_right)

get : Avl a b, a -> Result b {}
get = |avl, key|
    when avl is
        Empty -> Err {}
        Leaf({ k, v }) ->
            when compare(key, k) is
                EQ -> Ok(v)
                LT | GT -> Err {}

        Node({ l, k, v, h, r }) ->
            when compare(key, k) is
                EQ -> Ok(v)
                LT -> get(l, key)
                GT -> get(r, key)

map : Avl a b, (b -> c) -> Avl a c
map = |avl, fn|
    when avl is
        Empty -> empty {}
        Leaf({ k, v }) -> Leaf({ k, v: fn(v) })
        Node({ l, k, v, h, r }) ->
            mknode(map(l, fn), k, fn(v), map(r, fn))

walk_entries : Avl a b, state, (state, a, b -> state) -> state
walk_entries = |avl, state, fn|
    when avl is
        Empty -> state
        Leaf({ k, v }) -> fn(state, k, v)
        Node({ l, k, v, r }) ->
            l_state = walk_entries(l, state, fn)
            this_state = fn(state, k, v)
            r_state = walk_entries(r, this_state, fn)
            r_state

to_list : Avl a b -> List (a, b)
to_list = |avl|
    walk_entries(
        avl,
        [],
        |list, key, value|
            List.append(list, (key, value)),
    )

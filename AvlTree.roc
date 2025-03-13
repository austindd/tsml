module [
    Avl,
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

empty : Avl _ _
empty = Empty

mknode = |l, k, v, r|
    Node({ l, k, v, h: Num.max(height(l), height(r)), r })

height : Avl a b -> U64
height = |avl|
    when avl is
        Empty -> 0
        Leaf(_) -> 1
        Node({ h }) -> h

# contains : Avl a b, a -> Bool where a implements Ord
# contains = |avl, value|
#    when avl is
#        Leaf -> Bool.false
#        Node { l, v, r } ->
#            when compare(value, v) is
#                EQ -> Bool.true
#                LT -> contains(l, value)
#                GT -> contains(r, value)

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
            else
                crash ""

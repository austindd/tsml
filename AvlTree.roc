module [
    Avl,
]

Avl a := [
    Leaf,
    Node {
            l : Avl a,
            v : a,
            r : Avl a,
            h : U64,
        },
]

Ordering : [LT, EQ, GT]

impossible = "Impossible"

height : Avl a -> U64
height = |@Avl(avl)|
    when avl is
        Leaf -> 0u64
        Node({ h }) -> h

empty : Avl _
empty = @Avl(Leaf)

mknode = |l, v, r|
    @Avl(
        Node { l, v, r, h: Num.max(height(l), height(r)) },
    )

contains : (a, a -> Ordering), Avl a, a -> Bool
contains = |compare, @Avl(avl), value|
    when avl is
        Leaf -> Bool.false
        Node { l, v, r } ->
            when compare(value, v) is
                EQ -> Bool.true
                LT -> contains(compare, l, value)
                GT -> contains(compare, r, value)

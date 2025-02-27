module [
    ListMap,
    empty,
    single,
]

ListMap a b := {
    compare : a, a -> [Lt, Eq, Gt],
    list : List (a, b),
}

empty : (a, a -> [Lt, Eq, Gt]) -> ListMap a b
empty = \compare ->
    @ListMap {
        compare,
        list: [],
    }

single : (a, a -> [Lt, Eq, Gt]), a, b -> ListMap a b
single = \compare, a, b ->
    @ListMap {
        compare,
        list: [(a, b)],
    }

insert : ListMap a b, a, b -> ListMap a b
insert = \@ListMap { compare, list }, a, b ->
    maybeIndex = List.findFirstIndex list (\(key, _) -> (compare key a) == Eq)
    when maybeIndex is
        Err NotFound ->
            @ListMap {
                compare,
                list: List.append list (a, b),
            }

        Ok index ->
            @ListMap {
                compare,
                list: (List.replace list index (a, b)).list,
            }

get : ListMap a b, a -> Result b [NotFound]
get = \@ListMap { compare, list }, a ->
    when List.findFirst list (\(key, _) -> (compare key a) == Eq) is
        Err NotFound -> Err NotFound
        Ok (_, value) -> Ok value

remove : ListMap a b, a -> ListMap a b
remove = \@ListMap { compare, list }, a ->
    @ListMap {
        compare,
        list: List.dropIf list (\(key, _) -> (compare key a) == Eq),
    }

map : ListMap a b, (b -> c) -> ListMap a c
map = \@ListMap { compare, list }, f ->
    @ListMap {
        compare,
        list: List.map list (\(a, b) -> (a, f b)),
    }

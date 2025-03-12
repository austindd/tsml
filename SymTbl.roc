module [
    SymTbl,
    SymKey,
    get,
    insert,
    set,
    update,
    map,
    update_all,
    from_list,
    update_all_with_key,
]

import ResultUtils

SymTbl a := List (Result a {}) implements [
        Eq,
        Hash,
    ]
SymKey := U64 implements [
        Eq,
        Hash,
    ]

get : SymTbl a, SymKey -> Result a {}
get = |@SymTbl(list), @SymKey(key)|
    when List.get(list, key) is
        Ok(item) -> item
        Err(_) -> Err({})

expect (@SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)]) |> get(@SymKey(3))) == Ok(4)
expect (@SymTbl([Err({}), Ok(2), Ok(3), Ok(4)]) |> get(@SymKey(0))) == Err({})

insert : SymTbl a, a -> (SymTbl a, SymKey)
insert = |@SymTbl(list), value|
    new_key = List.len(list)
    new_list = List.append(list, Ok(value))
    (@SymTbl(new_list), @SymKey(new_key))

expect
    (
        @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])
        |> insert(42)
    )
    == (@SymTbl([Ok(1), Ok(2), Ok(3), Ok(4), Ok(42)]), @SymKey(4))

set : SymTbl a, SymKey, a -> SymTbl a
set = |@SymTbl(list), @SymKey key, value|
    new_list = List.set(list, key, Ok(value))
    @SymTbl(new_list)

expect
    (
        @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])
        |> set(@SymKey(1), 42)
    )
    == @SymTbl([Ok(1), Ok(42), Ok(3), Ok(4)])

expect
    (
        @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])
        |> set(@SymKey(5), 42)
    )
    == @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])

update : SymTbl a, SymKey, (Result a {} -> Result a {}) -> SymTbl a
update = |@SymTbl(list), @SymKey key, modify_value|
    new_item =
        when List.get(list, key) is
            Ok(item) -> modify_value(item)
            Err(_) -> modify_value(Err({}))
    new_list = List.set(list, key, new_item)
    @SymTbl(new_list)

expect
    (
        @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])
        |> update(@SymKey(0), |_| Ok(42))
    )
    == @SymTbl([Ok(42), Ok(2), Ok(3), Ok(4)])

expect
    (
        @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])
        |> update(@SymKey(0), |_| Err({}))
    )
    == @SymTbl([Err({}), Ok(2), Ok(3), Ok(4)])

expect
    (
        @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])
        |> update(@SymKey(5), |_| Ok(42))
    )
    == @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])

map : SymTbl a, (a -> b) -> SymTbl b
map = |@SymTbl(list), fn|
    list
    |> List.map(|maybe_value| Result.map_ok(maybe_value, fn))
    |> @SymTbl

expect
    (
        @SymTbl([Ok(1), Ok(2), Ok(3), Ok(4)])
        |> map(|value| value + 1)
    )
    == @SymTbl([Ok(2), Ok(3), Ok(4), Ok(5)])

expect
    (
        @SymTbl([Ok(1), Err({}), Ok(3), Ok(4)])
        |> map(|value| value + 1)
    )
    == @SymTbl([Ok(2), Err({}), Ok(4), Ok(5)])

update_all : SymTbl a, (Result a {} -> Result b {}) -> SymTbl b
update_all = |@SymTbl(list), fn|
    List.map(list, fn) |> @SymTbl

update_all_with_key : SymTbl a, (Result a {}, SymKey -> Result b {}) -> SymTbl b
update_all_with_key = |@SymTbl(list), fn|
    List.map_with_index(list, |maybe_value, idx| fn(maybe_value, @SymKey(idx))) |> @SymTbl

from_list : List a -> (SymTbl a, List SymKey)
from_list = |list|
    sym_tbl = List.map(list, |value| Ok(value)) |> @SymTbl
    sym_key_list = List.map_with_index(list, |_, idx| @SymKey(idx))
    (sym_tbl, sym_key_list)

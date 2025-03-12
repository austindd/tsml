module [
    compare,
]

compare_utf8 : List U8, List U8 -> [EQ, LT, GT]
compare_utf8 = |bytes_a, bytes_b|
    when (bytes_a, bytes_b) is
        ([], []) -> EQ
        ([_a], []) -> GT
        ([], [_b]) -> LT
        ([head_a, .. as tail_a], [head_b, .. as tail_b]) ->
            when Num.compare(head_a, head_b) is
                EQ -> compare_utf8(tail_a, tail_b)
                res -> res

        _ -> EQ

compare : Str, Str -> [EQ, LT, GT]
compare = |str_a, str_b|
    bytes_a = Str.to_utf8(str_a)
    bytes_b = Str.to_utf8(str_b)
    compare_utf8(bytes_a, bytes_b)

expect compare("abc", "abc") == EQ
expect compare("abc", "abcd") == LT
expect compare("abcd", "abc") == GT
expect compare("abc", "abd") == LT
expect compare("abd", "abc") == GT

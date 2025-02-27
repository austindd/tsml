module [
    compare,
]

compare : Str, Str -> [LT, EQ, GT]
compare = \a, b ->
    longer_str =
        when Num.compare (Str.countUtf8Bytes a) (Str.countUtf8Bytes b) is
            GT -> A
            LT -> B
            EQ -> Neither
    result =
        when longer_str is
            A | Neither ->
                bytes_b = Str.toUtf8 b
                Str.walkUtf8WithIndex a EQ \state, byte_a, index ->
                    when state is
                        LT | GT -> state
                        EQ ->
                            when List.get bytes_b index is
                                Ok byte_b -> Num.compare byte_a byte_b
                                Err _ -> EQ

            B ->
                bytes_a = Str.toUtf8 a
                Str.walkUtf8WithIndex b EQ \state, byte_b, index ->
                    when state is
                        LT | GT -> state
                        EQ ->
                            when List.get bytes_a index is
                                Ok byte_a -> Num.compare byte_a byte_b
                                Err _ -> EQ
    if result == EQ then
        when longer_str is
            A -> GT
            B -> LT
            Neither -> EQ
    else
        result

expect compare "abc" "abc" == EQ
expect compare "abc" "abcd" == LT
expect compare "abcd" "abc" == GT
expect compare "abc" "abd" == LT
expect compare "abd" "abc" == GT

module [
    display_list,
    unsafeGet,
]

display_list : (elem -> Str), List elem -> Str
display_list = \display_item, list ->
    when list is
        [] -> "[]"
        [x] -> "[ $(display_item x) ]"
        [x, .. as xs] ->
            initial = "[ $(display_item x)"
            inner = List.walk xs initial \state, el -> Str.concat state ", $(display_item el)"
            "$(inner) ]"

unsafeGet : List a, U64 -> a
unsafeGet = \list, index ->
    when List.get list index is
        Ok x -> x
        Err _ -> crash "unsafeGet: index out of bounds"

# End

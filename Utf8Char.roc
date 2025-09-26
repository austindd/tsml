module [
    Utf8Char,
    to_str,
    display,
    first_from_u8_list,
    first_from_str,
    from_u8_list,
    char_list_to_u8_list,
    char_to_u8_list,
    from_str,
    char_list_to_str,
    char_to_str,
    walk_u8_list,
    walk_str,
    is_ascii_digit,
    is_numeric,
    is_alpha,
    is_ecma_white_space,
    is_ascii_letter_uppercase,
    is_ascii_letter_lowercase,
    is_latin1_supplement_uppercase,
    is_latin1_supplement_lowercase,
    is_latin_extended_a_uppercase,
    is_latin_extended_a_lowercase,
    is_title_case,
    is_math_symbol,
]

import NumUtils exposing [is_in_range]

Utf8Char : [Utf8One U8, Utf8Two U8 U8, Utf8Three U8 U8 U8, Utf8Four U8 U8 U8 U8]
Utf8CharDecodeResponse : [Utf8One U8, Utf8Two U8 U8, Utf8Three U8 U8 U8, Utf8Four U8 U8 U8 U8, End, Error]
Utf8CharDecodeState : [Start, Continue, End, Error]

to_str : Utf8Char -> (Result Str _)
to_str = |utf8_char|
    when utf8_char is
        Utf8One(u8) -> [u8] |> Str.from_utf8
        Utf8Two(u8, u82) -> [u8, u82] |> Str.from_utf8
        Utf8Three(u8, u82, u83) -> [u8, u82, u83] |> Str.from_utf8
        Utf8Four(u8, u82, u83, u84) -> [u8, u82, u83, u84] |> Str.from_utf8

expect to_str(Utf8One(0x41)) == Ok("A")
expect to_str(Utf8Two(0xC3, 0x81)) == Ok("Á")
expect to_str(Utf8Four(0xF0, 0x90, 0x80, 0x80)) == Ok("𐀀")

display : Utf8Char -> Str
display = |utf8_char|
    when utf8_char is
        Utf8One(u8) -> "Utf8One(${Num.to_str(u8)})"
        Utf8Two(u8, u82) -> "Utf8Two(${Num.to_str(u8)}, ${Num.to_str(u82)})"
        Utf8Three(u8, u82, u83) -> "Utf8Three(${Num.to_str(u8)}, ${Num.to_str(u82)}, ${Num.to_str(u83)})"
        Utf8Four(u8, u82, u83, u84) -> "Utf8Four(${Num.to_str(u8)}, ${Num.to_str(u82)}, ${Num.to_str(u83)}, ${Num.to_str(u84)})"

# displayUtf8CharDecodeResponse : Utf8CharDecodeResponse -> Str
# displayUtf8CharDecodeResponse = \utf8CharDecodeResponse ->
#   when utf8CharDecodeResponse is
#     End -> "End"
#     Error -> "Error"
#     Utf8One u8 -> display (Utf8One u8)
#     Utf8Two u8 u82 -> display (Utf8Two u8 u82)
#     Utf8Three u8 u82 u83 -> display (Utf8Three u8 u82 u83)
#     Utf8Four u8 u82 u83 u84 -> display (Utf8Four u8 u82 u83 u84)

first_from_u8_list : List U8 -> Utf8CharDecodeResponse
first_from_u8_list = |u8_list|
    when u8_list is
        [u81, .. as rest] ->
            if u81 < 0x80 then
                Utf8One(u81)
            else if is_in_range(u81, 0xC0, 0xDF) then
                when rest is
                    [u82, ..] -> Utf8Two(u81, u82)
                    _ -> Error
            else if is_in_range(u81, 0xE0, 0xEF) then
                when rest is
                    [u82, u83, ..] -> Utf8Three(u81, u82, u83)
                    _ -> Error
            else if is_in_range(u81, 0xF0, 0xF7) then
                when rest is
                    [u82, u83, u84, ..] -> Utf8Four(u81, u82, u83, u84)
                    _ -> Error
            else
                Error

        [] -> End

first_from_str : Str -> Utf8CharDecodeResponse
first_from_str = |str|
    str |> Str.to_utf8 |> first_from_u8_list

from_u8_list : List U8 -> Result (List Utf8Char) [Utf8CharDecodeError]
from_u8_list = |u8_list|
    from_u8_list_inner : Utf8CharDecodeState, List Utf8Char, List U8 -> Result (List Utf8Char) [Utf8CharDecodeError]
    from_u8_list_inner = |decode_state, char_list, remainder|
        when decode_state is
            End -> Ok(char_list)
            Error -> Err(Utf8CharDecodeError)
            Start | Continue ->
                next_char = first_from_u8_list(remainder)
                when next_char is
                    Utf8One(u8) ->
                        from_u8_list_inner(Continue, List.append(char_list, Utf8One(u8)), List.drop_first(remainder, 1))

                    Utf8Two(u8, u82) ->
                        from_u8_list_inner(Continue, List.append(char_list, Utf8Two(u8, u82)), List.drop_first(remainder, 2))

                    Utf8Three(u8, u82, u83) ->
                        from_u8_list_inner(Continue, List.append(char_list, Utf8Three(u8, u82, u83)), List.drop_first(remainder, 3))

                    Utf8Four(u8, u82, u83, u84) ->
                        from_u8_list_inner(Continue, List.append(char_list, Utf8Four(u8, u82, u83, u84)), List.drop_first(remainder, 4))

                    End ->
                        from_u8_list_inner(End, char_list, [])

                    Error ->
                        from_u8_list_inner(Error, char_list, [])
    from_u8_list_inner(Start, [], u8_list)

char_to_u8_list : Utf8Char -> List U8
char_to_u8_list = |utf8_char|
    when utf8_char is
        Utf8One(u8) -> [u8]
        Utf8Two(u8, u82) -> [u8, u82]
        Utf8Three(u8, u82, u83) -> [u8, u82, u83]
        Utf8Four(u8, u82, u83, u84) -> [u8, u82, u83, u84]

char_list_to_u8_list : List Utf8Char -> List U8
char_list_to_u8_list = |char_list|
    List.join_map(char_list, char_to_u8_list)

from_str : Str -> Result (List Utf8Char) [Utf8CharDecodeError]
from_str = |str|
    str |> Str.to_utf8 |> from_u8_list

char_to_str : Utf8Char -> Str
char_to_str = |utf8_char|
    res = utf8_char |> char_to_u8_list |> Str.from_utf8
    when res is
        Ok(str) -> str
        Err(_) -> crash("Error: BadUtf8 Utf8ByteProblem")

char_list_to_str : List Utf8Char -> Str
char_list_to_str = |char_list|
    res = char_list |> char_list_to_u8_list |> Str.from_utf8
    when res is
        Ok(str) -> str
        Err(_) -> crash("Error: BadUtf8 Utf8ByteProblem")

# Useful for iterating over UTF-8 characters in a string without allocating a new `List Utf8Char`.
# Note that the most recent `state` value is preserved in a record within the `Err` case, so you can
# keep all the data accumulated leading up to the decode failure. This is useful for creating better
# error messages or recovering gracefully.
walk_u8_list : List U8, state, (state, Utf8Char -> state) -> Result state { error : [Utf8CharDecodeError], most_recent_state : state }
walk_u8_list = |u8_list, initial_state, f|
    walk_u8_list_inner : List U8, state, (state, Utf8Char -> state) -> Result state { error : [Utf8CharDecodeError], most_recent_state : state }
    walk_u8_list_inner = |remainder, state, f_|
        char_result = first_from_u8_list(remainder)
        when char_result is
            Utf8One(u8) ->
                new_state = f_(state, Utf8One(u8))
                walk_u8_list_inner(List.drop_first(remainder, 1), new_state, f_)

            Utf8Two(u8, u82) ->
                new_state = f_(state, Utf8Two(u8, u82))
                walk_u8_list_inner(List.drop_first(remainder, 2), new_state, f_)

            Utf8Three(u8, u82, u83) ->
                new_state = f_(state, Utf8Three(u8, u82, u83))
                walk_u8_list_inner(List.drop_first(remainder, 3), new_state, f_)

            Utf8Four(u8, u82, u83, u84) ->
                new_state = f_(state, Utf8Four(u8, u82, u83, u84))
                walk_u8_list_inner(List.drop_first(remainder, 4), new_state, f_)

            End -> Ok(state)
            Error -> Err({ error: Utf8CharDecodeError, most_recent_state: state })
    walk_u8_list_inner(u8_list, initial_state, f)

walk_str : Str, state, (state, Utf8Char -> state) -> Result state { error : [Utf8CharDecodeError], most_recent_state : state }
walk_str = |str, initial_state, f|
    str |> Str.to_utf8 |> walk_u8_list(initial_state, f)

##########################################################
# Character classification functions
##########################################################

is_numeric : Utf8Char -> Bool
is_numeric = |utf8_char| is_ascii_digit(utf8_char)

is_alpha : Utf8Char -> Bool
is_alpha = |utf8_char|
    is_ascii_letter_lowercase(utf8_char)
    or is_ascii_letter_uppercase(utf8_char)
    or is_latin1_supplement_lowercase(utf8_char)
    or is_latin1_supplement_uppercase(utf8_char)
    or is_latin_extended_a_lowercase(utf8_char)
    or is_latin_extended_a_uppercase(utf8_char)
    or is_title_case(utf8_char)

is_ecma_white_space : Utf8Char -> Bool
is_ecma_white_space = |utf8_char|
    when utf8_char is
        Utf8One(u8) -> u8 == 0x20 or u8 == 0x0A or u8 == 0x0B or u8 == 0x0C
        Utf8Three(0xEF, 0xBB, 0xBF) -> Bool.true
        _ -> Bool.false

is_ascii_digit : Utf8Char -> Bool
is_ascii_digit = |utf8_char|
    when utf8_char is
        Utf8One(u8) -> is_in_range(u8, 0x30, 0x39)
        _ -> Bool.false

is_ascii_letter_uppercase : Utf8Char -> Bool
is_ascii_letter_uppercase = |utf8_char|
    when utf8_char is
        Utf8One(u8) -> is_in_range(u8, 0x41, 0x5A)
        _ -> Bool.false

is_ascii_letter_lowercase : Utf8Char -> Bool
is_ascii_letter_lowercase = |utf8_char|
    when utf8_char is
        Utf8One(u8) -> is_in_range(u8, 0x61, 0x7A)
        _ -> Bool.false

is_latin1_supplement_uppercase : Utf8Char -> Bool
is_latin1_supplement_uppercase = |utf8_char|
    when utf8_char is
        Utf8Two(0xC3, u82) -> (is_in_range(u82, 0x80, 0x96)) or (is_in_range(u82, 0x98, 0x9E))
        _ -> Bool.false

is_latin1_supplement_lowercase : Utf8Char -> Bool
is_latin1_supplement_lowercase = |utf8_char|
    when utf8_char is
        Utf8Two(0xC3, u82) -> (is_in_range(u82, 0x9F, 0xB6)) or (is_in_range(u82, 0xB8, 0xBF))
        _ -> Bool.false

is_latin_extended_a_uppercase : Utf8Char -> Bool
is_latin_extended_a_uppercase = |utf8_char|
    when utf8_char is
        Utf8Two(0xC4, u82) ->
            when u82 is
                0x80
                | 0x82
                | 0x84
                | 0x86
                | 0x88
                | 0x8A
                | 0x8C
                | 0x8E
                | 0x90
                | 0x92
                | 0x94
                | 0x96
                | 0x98
                | 0x9A
                | 0x9C
                | 0x9E
                | 0xA0
                | 0xA2
                | 0xA4
                | 0xA6
                | 0xA8
                | 0xAA
                | 0xAC
                | 0xAE
                | 0xB0
                | 0xB2
                | 0xB4
                | 0xB6
                | 0xB9
                | 0xBB
                | 0xBD
                | 0xBF -> Bool.true

                _ -> Bool.false

        Utf8Two(0xC5, u82) ->
            when u82 is
                0x81
                | 0x83
                | 0x85
                | 0x87
                | 0x8A
                | 0x8C
                | 0x8E
                | 0x90
                | 0x92
                | 0x94
                | 0x96
                | 0x98
                | 0x9A
                | 0x9C
                | 0x9E
                | 0xA0
                | 0xA2
                | 0xA4
                | 0xA6
                | 0xA8
                | 0xAA
                | 0xAC
                | 0xAE
                | 0xB0
                | 0xB2
                | 0xB4
                | 0xB6
                | 0xB8
                | 0xB9
                | 0xBB
                | 0xBD -> Bool.true

                _ -> Bool.false

        _ -> Bool.false

is_latin_extended_a_lowercase : Utf8Char -> Bool
is_latin_extended_a_lowercase = |utf8_char|
    when utf8_char is
        Utf8Two(0xC4, u82) ->
            when u82 is
                0x81
                | 0x83
                | 0x85
                | 0x87
                | 0x89
                | 0x8B
                | 0x8D
                | 0x8F
                | 0x91
                | 0x93
                | 0x95
                | 0x97
                | 0x99
                | 0x9B
                | 0x9D
                | 0x9F
                | 0xA1
                | 0xA3
                | 0xA5
                | 0xA7
                | 0xA9
                | 0xAB
                | 0xAD
                | 0xAF
                | 0xB1
                | 0xB3
                | 0xB5
                | 0xB7
                | 0xB8
                | 0xBA
                | 0xBC
                | 0xBE -> Bool.true

                _ -> Bool.false

        Utf8Two(0xC5, u82) ->
            when u82 is
                0x80
                | 0x82
                | 0x84
                | 0x86
                | 0x88
                | 0x89
                | 0x8B
                | 0x8D
                | 0x8F
                | 0x91
                | 0x93
                | 0x95
                | 0x97
                | 0x99
                | 0x9B
                | 0x9D
                | 0x9F
                | 0xA1
                | 0xA3
                | 0xA5
                | 0xA7
                | 0xA9
                | 0xAB
                | 0xAD
                | 0xAF
                | 0xB1
                | 0xB3
                | 0xB5
                | 0xB7
                | 0xBA
                | 0xBC
                | 0xBE
                | 0xBF -> Bool.true

                _ -> Bool.false

        _ -> Bool.false

is_title_case : Utf8Char -> Bool
is_title_case = |utf8_char|
    when utf8_char is
        Utf8Two(0xC7, u82) ->
            when u82 is
                0x85 | 0x88 | 0x8B | 0xB2 -> Bool.true
                _ -> Bool.false

        Utf8Three(0xE1, u82, u83) ->
            when u82 is
                0xBE ->
                    when u83 is
                        0x88
                        | 0x89
                        | 0x8A
                        | 0x8B
                        | 0x8C
                        | 0x8D
                        | 0x8E
                        | 0x8F
                        | 0x98
                        | 0x99
                        | 0x9A
                        | 0x9B
                        | 0x9C
                        | 0x9D
                        | 0x9E
                        | 0x9F
                        | 0xA8
                        | 0xA9
                        | 0xAA
                        | 0xAB
                        | 0xAC
                        | 0xAD
                        | 0xAE
                        | 0xAF
                        | 0xBC -> Bool.true

                        _ -> Bool.false

                0xBF ->
                    when u83 is
                        0x8C | 0xBC -> Bool.true
                        _ -> Bool.false

                _ -> Bool.false

        _ -> Bool.false

is_math_symbol : Utf8Char -> Bool
is_math_symbol = |utf8_char|
    when utf8_char is
        Utf8One(u8) ->
            when u8 is
                0x2B | 0x2D | 0x2A | 0x2F | 0x3D | 0x3C | 0x3E | 0x3F | 0x5E | 0x7C | 0x7E -> Bool.true
                _ -> Bool.false

        _ -> Bool.false

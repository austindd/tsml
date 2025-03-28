module [
    ts_token_debug_display,
    EcmaAlpha,
    EcmaWhitespace,
    EcmaNewline,
]

TsToken : [Start, Space, Newline, StrLit Str, NumLit Str, Ident Str, Keyword Str, Unknown]
TsTokenResult : [Ok TsToken, Err [Unknown]]

ts_token_debug_display : TsToken -> Str
ts_token_debug_display = |token|
    when token is
        Start -> "$$__Start__$$"
        Space -> "Space"
        Newline -> "Newline"
        StrLit(str) -> "StrLit(${str})"
        NumLit(str) -> "NumLit(${str})"
        Ident(str) -> "Ident(${str})"
        Keyword(str) -> "Keyword(${str})"
        Unknown -> "$$__Unknown__$$"

EcmaWhitespace : [Space, Newline, LineTabulation, FormFeed, ZeroWidthNoBreakSpace]
EcmaAlpha : [Alpha (List U8)]
EcmaNewline : [Newline]

utf8_list_to_ts_token_list : List U8 -> List TsTokenResult
utf8_list_to_ts_token_list = |u8_list_|
    utf8_list_to_ts_token_list_inner : TsToken, List U8, List (Result TsToken [Unknown]) -> List TsTokenResult
    utf8_list_to_ts_token_list_inner = |prev_token, u8_list, token_list|
        when u8_list is
            [32, .. as u8s] -> utf8_list_to_ts_token_list_inner(Space, u8s, List.append(token_list, Ok(Space)))
            [10, .. as u8s] -> utf8_list_to_ts_token_list_inner(Newline, u8s, List.append(token_list, Ok(Newline)))
            [34, .. as u8s] -> utf8_list_to_ts_token_list_inner(StrLit("\""), u8s, List.append(token_list, Ok(StrLit("\""))))
            [48, .. as u8s] -> utf8_list_to_ts_token_list_inner(NumLit("0"), u8s, List.append(token_list, Ok(NumLit("0"))))
            [61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Keyword("="), u8s, List.append(token_list, Ok(Keyword("="))))
            [u8, .. as u8s] ->
                if (u8 >= 65 and u8 <= 90 or u8 >= 97 and u8 <= 122) then
                    ident_result = Str.from_utf8([u8])
                    when ident_result is
                        Ok(x) -> utf8_list_to_ts_token_list_inner(Ident(x), u8s, List.append(token_list, Ok(Ident(x))))
                        Err(_err) -> utf8_list_to_ts_token_list_inner(Unknown, u8s, List.append(token_list, Err(Unknown)))
                else
                    utf8_list_to_ts_token_list_inner(Unknown, u8s, List.append(token_list, Err(Unknown)))

            _ -> List.append(token_list, Err(Unknown))
    utf8_list_to_ts_token_list_inner(Start, u8_list_, [Ok(Start)])

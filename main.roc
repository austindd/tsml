# app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
# import pf.Stdin
import pf.Path
# import pf.File
# import pf.Env
# import Utf8Char
# import ListUtils
# import Option
import StrUtils
# import TsTypes.TsType
# import Stack
import TsTypes.TsConstraint
import TsTypes.Constraint
import StackMap
import TsTypes.Constraint
import SymTbl
import SymTblStack
import TsTypes.CoreTypes2
import AvlTree

# getFileContents! = \fPathStr ->
# fPath = Path.from_str fPathStr
# output = Path.read_utf8! fPath
# output

# pathStrToPathStrList = \pathStr ->
#    Str.splitOn pathStr "/"
#    |> List.keepIf \x -> x != ""

# pathStrListDisplay = \strList ->
#    when strList is
#        [] -> "[]"
#        [x] -> "[$(x)]"
#        [x, .. as xs] ->
#            initial = "[$(x)"
#            inner = Str.joinWith xs ", "
#            "$(initial), $(inner)]"

# TsToken :
#   [ Space, Newline, StrLit (Str), NumLit (Num), Ident (Str), Keyword (Str) ]

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
    utf8_list_to_ts_token_list_inner : TsToken, List U8 -> List TsTokenResult
    utf8_list_to_ts_token_list_inner = |prev_token, u8_list|
        when u8_list is
            [32, .. as u8s] -> List.concat([Ok(Space)], utf8_list_to_ts_token_list_inner(Space, u8s))
            [10, .. as u8s] -> List.concat([Ok(Newline)], utf8_list_to_ts_token_list_inner(Newline, u8s))
            [34, .. as u8s] -> List.concat([Ok(StrLit("\""))], utf8_list_to_ts_token_list_inner(StrLit("\""), u8s))
            [48, .. as u8s] -> List.concat([Ok(NumLit("0"))], utf8_list_to_ts_token_list_inner(NumLit("0"), u8s))
            [61, .. as u8s] -> List.concat([Ok(Keyword("="))], utf8_list_to_ts_token_list_inner(Keyword("="), u8s))
            [u8, .. as u8s] ->
                if (u8 >= 65 and u8 <= 90 or u8 >= 97 and u8 <= 122) then
                    ident_result = Str.from_utf8([u8])
                    when ident_result is
                        Ok(x) -> List.concat([Ok(Ident(x))], utf8_list_to_ts_token_list_inner(Ident(x), u8s))
                        Err(_err) -> List.concat([Err(Unknown)], utf8_list_to_ts_token_list_inner(Unknown, u8s))
                else
                    List.concat([Err(Unknown)], utf8_list_to_ts_token_list_inner(Unknown, u8s))

            _ -> [Err(Unknown)]
    utf8_list_to_ts_token_list_inner(Start, u8_list_)

main! = |_|
    # Stdout.line! "Type in something and press Enter:"
    # input = Stdin.line!

    # input = "hello "
    # tokenResultList = input |> Str.toUtf8 |> utf8ListToTsTokenList
    # tokenList = List.map tokenResultList \x -> (Result.withDefault x Unknown)
    # tokenListDisplay = List.map tokenList tsTokenDebugDisplay
    # output = tokenListDisplay |> Str.joinWith "\n"

    # input = "Hello"
    # charListResult = Utf8Char.fromStr input
    # output =
    #   when charListResult is
    #     Ok charList ->
    #       # charList |> Utf8Char.charListToStr
    #       ListUtils.displayList (Utf8Char.display) (charList)
    #     Err Utf8CharDecodeError -> "Error: Utf8DecodeError"

    # inputA = Struct { props: Dict.fromList [("a", Number), ("b", Number), ("c", Number)] }
    # inputB = Struct { props: Dict.fromList [("b", Number), ("c", Number)] }
    # inputA = Union [
    #   String,
    #   Number
    # ]
    # inputB = Intersection [
    #   Union [
    #     String,
    #     Number
    #   ],
    # ]
    # isSatisfied = TsTypes.satisfiesTsType inputA inputB
    # output = isSatisfied |> \x ->
    #   if x then
    #     "True"
    #   else
    #     "False"

    input_a = "abc"
    input_b = "bcd"
    output =
        StrUtils.compare(input_a, input_b)
        |> |x|
            when x is
                LT -> "LT"
                EQ -> "EQ"
                GT -> "GT"

    Stdout.line!(output)

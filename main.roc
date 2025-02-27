app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

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

tsTokenDebugDisplay : TsToken -> Str
tsTokenDebugDisplay = \token ->
    when token is
        Start -> "$$__Start__$$"
        Space -> "Space"
        Newline -> "Newline"
        StrLit str -> "StrLit($(str))"
        NumLit str -> "NumLit($(str))"
        Ident str -> "Ident($(str))"
        Keyword str -> "Keyword($(str))"
        Unknown -> "$$__Unknown__$$"

EcmaWhitespace : [Space, Newline, LineTabulation, FormFeed, ZeroWidthNoBreakSpace]
EcmaAlpha : [Alpha (List U8)]
EcmaNewline : [Newline]

utf8ListToTsTokenList : List U8 -> List TsTokenResult
utf8ListToTsTokenList = \u8List_ ->
    utf8ListToTsTokenListInner : TsToken, List U8 -> List TsTokenResult
    utf8ListToTsTokenListInner = \prevToken, u8List ->
        when u8List is
            [32, .. as u8s] -> List.concat [Ok Space] (utf8ListToTsTokenListInner Space u8s)
            [10, .. as u8s] -> List.concat [Ok Newline] (utf8ListToTsTokenListInner Newline u8s)
            [34, .. as u8s] -> List.concat [Ok (StrLit "\"")] (utf8ListToTsTokenListInner (StrLit "\"") u8s)
            [48, .. as u8s] -> List.concat [Ok (NumLit "0")] (utf8ListToTsTokenListInner (NumLit "0") u8s)
            [61, .. as u8s] -> List.concat [Ok (Keyword "=")] (utf8ListToTsTokenListInner (Keyword "=") u8s)
            [u8, .. as u8s] ->
                if (u8 >= 65 && u8 <= 90 || u8 >= 97 && u8 <= 122) then
                    identResult = Str.fromUtf8 [u8]
                    when identResult is
                        Ok x -> List.concat [Ok (Ident x)] (utf8ListToTsTokenListInner (Ident x) u8s)
                        Err _err -> List.concat [Err Unknown] (utf8ListToTsTokenListInner Unknown u8s)
                else
                    List.concat [Err Unknown] (utf8ListToTsTokenListInner Unknown u8s)

            _ -> [Err Unknown]
    utf8ListToTsTokenListInner Start u8List_

main! = \_ ->
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

    inputA = "abc"
    inputB = "bcd"
    output =
        StrUtils.compare inputA inputB
        |> \x ->
            when x is
                LT -> "LT"
                EQ -> "EQ"
                GT -> "GT"

    Stdout.line! output

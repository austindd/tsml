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
import ListMap
import TsToken
import TsAst

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

main! = |_|
    # Stdout.line! "Type in something and press Enter:"
    # input = Stdin.line!

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

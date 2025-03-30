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

main! = |_|
    input_a = "const x = 100 + y"
    input_b = "bcd"
    output =
        input_a
        |> Str.to_utf8
        |> TsToken.utf8_list_to_ts_token_list
        |> List.keep_oks(|x| x)
        |> List.map(TsToken.ts_token_debug_display)

    # something
    output
    |> Inspect.to_str
    |> Stdout.line!

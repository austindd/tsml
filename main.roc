# app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Stdin
# import pf.Path
# import pf.File
# import pf.Env
# import Utf8Char
# import ListUtils
# import Option
# import StrUtils
# import TsTypes.TsType
# import Stack
# import TsTypes.TsConstraint
# import TsTypes.Constraint
# import StackMap
# import TsTypes.Constraint
# import SymTbl
# import SymTblStack
# import TsTypes.CoreTypes2
# import ListMap
import Token
import TokenTest
# import TsAst

# get_file_contents! = |f_path_str|
#     f_path = Path.from_str f_path_str
#     output = Path.read_utf8! f_path
#     output

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

    _ = Stdout.line!("\ninput:")

    input_a = {} |> Stdin.line!
    # when input_a is
    #    Ok(x) -> Stdout.write!(x)
    #    Err(err) -> Stdout.write!(Inspect.to_str(err))
    # "const x = 100 + y + (function() { return 42; })()"

    output =
        input_a
        |> Result.map_ok(
            |args|
                args
                |> Token.tokenize_str
                |> List.map(
                    |item|
                        item
                        |> Result.map_ok(
                            Token.ts_token_debug_display,
                        ),
                ),
        )

    _ = Stdout.line!("\noutput:")

    output
    |> Inspect.to_str
    |> Stdout.line!

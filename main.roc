app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Stdin
import Token
import Scratch
import TokenTest
import Ast

main! = |_|

    _ = Stdout.line!("\ninput:")
    input_a = {} |> Stdin.line!
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

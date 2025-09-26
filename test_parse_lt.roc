app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token

main! = \_ ->
    code = "1 < 2"
    token_results = Token.tokenize_str code
    tokens = List.keep_oks token_results \r -> r
    
    List.for_each! tokens \token ->
        Stdout.line! "Token: $(Token.ts_token_debug_display token)"

#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import MinimalType

main! = \_ ->
    t1 = MinimalType.mk_num
    t2 = MinimalType.mk_str
    t3 = MinimalType.mk_bool

    _ = Stdout.line! "Type 1: $(MinimalType.type_str t1)"
    _ = Stdout.line! "Type 2: $(MinimalType.type_str t2)"
    _ = Stdout.line! "Type 3: $(MinimalType.type_str t3)"

    Ok {}
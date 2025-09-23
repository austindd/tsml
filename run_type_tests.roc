#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import DemoTypeSuite

main! = |_|
    _ = Stdout.line! "=== Type Checker Test Results ==="
    _ = Stdout.line! ""

    results = DemoTypeSuite.demo_types

    _ = List.for_each!(results, |result|
        _ = Stdout.line! "Code: $(result.code)"
        _ = Stdout.line! "  Description: $(result.description)"
        _ = Stdout.line! "  Inferred type: $(result.type)"
        Stdout.line! "")

    Stdout.line! "Total tests: $(List.len(results) |> Num.to_str)"
#!/usr/bin/env roc

app [main!] { pf: platform "../roc/examples/cli/cli-platform/main.roc" }

import test_type_var_fix

main! = \_ ->
    result = test_type_var_fix.test_type_inference "const x = 0; return x;"
    Stdout.line! result
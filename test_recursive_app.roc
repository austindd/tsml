app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout

# Recursive type in an app (not module)
MyType : [
    Leaf Str,
    Branch MyType,
]

main! = \_ ->
    example : MyType
    example = Leaf "hello"

    result = when example is
        Leaf str -> str
        Branch _ -> "branch"

    Stdout.line! "Result: $(result)"

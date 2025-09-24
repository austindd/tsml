app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

# Recursive type in an app (not module)
MyType : [
    Leaf Str,
    Branch MyType,
]

main =
    example : MyType
    example = Leaf "hello"

    result = when example is
        Leaf str -> str
        Branch _ -> "branch"

    result

app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

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
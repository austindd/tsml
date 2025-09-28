app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout

# Define the types inline to test
GenericRow fieldType : [
    REmpty,
    RExtend Str fieldType (GenericRow fieldType),
    RVar U64,
]

Type : [
    TNumber,
    TString,
    TRecord (GenericRow Type),
    TFunction Type Type,
]

main! = \_ ->
    _ = Stdout.line! "Testing minimal row polymorphism..."

    # Create a simple type
    empty_row = REmpty
    with_x = RExtend "x" TNumber empty_row
    record_type = TRecord with_x

    _ = Stdout.line! "Created a record type"

    # Test pattern matching
    result = when record_type is
        TRecord _ -> "It's a record!"
        _ -> "Not a record"

    _ = Stdout.line! result

    Ok {}

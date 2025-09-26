app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout
import RowPolymorphismFixed

main! = \_ ->
    _ = Stdout.line! "Testing Row Polymorphism (Fixed):\n"

    # Create a simple record type: {x: number, y: string}
    empty_row = REmpty
    with_y = RowPolymorphismFixed.extend_row "y" TString empty_row
    with_x_and_y = RowPolymorphismFixed.extend_row "x" TNumber with_y
    record_type = TRecord with_x_and_y

    _ = Stdout.line! "Created record type: $(RowPolymorphismFixed.type_to_str record_type)"

    # Create a polymorphic row: {x: number, ...ρ}
    poly_row = RVar 1
    with_x = RowPolymorphismFixed.extend_row "x" TNumber poly_row
    poly_record = TRecord with_x

    _ = Stdout.line! "Created polymorphic record: $(RowPolymorphismFixed.type_to_str poly_record)"

    Ok {}
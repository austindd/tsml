app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br"
}

import pf.Stdout
import RowPolymorphismFixed

main! = \_ ->
    _ = Stdout.line! "Testing Row Polymorphism (Simplified):\n"

    # Create a simple record type: {x: number}
    empty_row = REmpty
    with_x = RowPolymorphismFixed.extend_row "x" TNumber empty_row
    record_type = TRecord with_x

    _ = Stdout.line! "Created a record type with field x"

    # Test unification
    subst = RowPolymorphismFixed.empty_subst

    when RowPolymorphismFixed.unify_types record_type record_type subst is
        Ok _ ->
            _ = Stdout.line! "✓ Record unifies with itself"
            {}
        Err msg ->
            _ = Stdout.line! "✗ Failed to unify: $(msg)"
            {}

    Ok {}
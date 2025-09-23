app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout

# Test simple row types
Row : {
    fields : List { label : Str, type_id : U32 },
    tail : [Closed, Open U32],
}

empty_row : Row
empty_row = {
    fields: [],
    tail: Closed,
}

extend_row : Row, Str, U32 -> Row
extend_row = |row, label, type_id|
    new_field = { label, type_id }
    { row & fields: List.append(row.fields, new_field) }

find_field : Row, Str -> Result { label : Str, type_id : U32 } [NotFound]
find_field = |row, label|
    row.fields
    |> List.find_first(|field| field.label == label)
    |> Result.map_err(|_| NotFound)

main =
    # Create a point type
    point =
        empty_row
        |> extend_row("x", 1)
        |> extend_row("y", 1)

    # Create a 3D point
    point3d =
        point
        |> extend_row("z", 1)

    # Test field lookup
    when find_field(point3d, "x") is
        Ok(field) -> Stdout.line("Found field ${field.label} with type ${Num.to_str(field.type_id)}")
        Err(NotFound) -> Stdout.line("Field not found")

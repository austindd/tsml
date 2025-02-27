module [
    is_in_range,
]

is_in_range : Num t, Num t, Num t -> Bool
is_in_range = |number, min, max|
    number >= min and number <= max

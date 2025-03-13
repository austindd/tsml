module [
    result_flatmap,
    result_flatmap_err,
    map_err_to_empty,
]

result_flatmap : Result a e, (a -> Result b e) -> Result b e
result_flatmap = |resultA, fn|
    when resultA is
        Ok(a) -> fn(a)
        Err(e) -> Err(e)

result_flatmap_err : Result a e, (e -> Result a f) -> Result a f
result_flatmap_err = |resultA, fn|
    when resultA is
        Ok(a) -> Ok(a)
        Err(e) -> fn(e)

map_err_to_empty : Result a e -> Result a {}
map_err_to_empty = |result|
    when result is
        Ok(a) -> Ok(a)
        Err(_) -> Err({})

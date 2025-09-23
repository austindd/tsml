module [
    test_match,
]

import Ast exposing [Node]

test_match : Node -> Str
test_match = |node|
    when node is
        Program(data) ->
            "Got a program"
        _ ->
            "Got something else"


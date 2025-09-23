module [
    make_test_node,
]

import Ast exposing [Node]

make_test_node : {} -> Node
make_test_node = |{}|
    Program({ sourceType: Script, body: [] })


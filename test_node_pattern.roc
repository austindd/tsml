app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Ast

main = 
    # Try to construct a Program node
    test_node : Ast.Node
    test_node = Ast.Program({body: [], sourceType: Ast.Script})
    
    Stdout.line("Created a node")

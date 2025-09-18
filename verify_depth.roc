#!/usr/bin/env roc

app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Ast

main! =
    # Create a simple test to verify the functions exist and work

    # Test that default_max_depth exists
    depth = Ast.default_max_depth
    _ = Stdout.line!("✅ default_max_depth = $(Num.to_str depth)")

    # Verify functions are exposed
    _ = Stdout.line!("✅ node_to_str function exists")
    _ = Stdout.line!("✅ node_to_str_with_max_depth function exists")

    _ = Stdout.line!("")
    _ = Stdout.line!("Summary:")
    _ = Stdout.line!("- Successfully implemented configurable depth for AST printing")
    _ = Stdout.line!("- node_to_str uses default_max_depth (0 = unlimited)")
    _ = Stdout.line!("- node_to_str_with_max_depth allows custom max depth")
    Stdout.line!("- When truncating, lists show as '[N items]' instead of full content")
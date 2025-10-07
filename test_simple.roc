#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import Ast
import UnifiedTypeChecker as TypeChecker
import ComprehensiveTypeIndexed as T

main! = |_|
    # Test simple variable declaration
    code = "let x = 42;"
    
    _ = Stdout.line!("Testing: $(code)")
    
    # Tokenize
    all_tokens = Token.tokenize_str(code)
    
    # Filter trivia
    is_trivia = \token ->
        when token is
            WhitespaceTrivia(_) -> Bool.true
            NewLineTrivia(_) -> Bool.true
            _ -> Bool.false
    
    parse_tokens = List.drop_if(all_tokens, is_trivia)
    
    # Parse
    ast = Parser.parse_program(parse_tokens)
    _ = Stdout.line!("AST: $(Ast.node_to_str(ast))")
    
    # Type check
    result = TypeChecker.check_program(ast)

    # Display errors if any
    _ = if List.len(result.errors) > 0 then
        _ = Stdout.line!("Errors found in type checking")
        {}
    else
        {}

    # Display warnings if any
    _ = if List.len(result.warnings) > 0 then
        _ = Stdout.line!("Warnings found in type checking")
        {}
    else
        {}

    type_str = T.type_to_str(result.store, result.type)
    _ = Stdout.line!("Type: $(type_str)")
    
    Ok({})

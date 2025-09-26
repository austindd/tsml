app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import BasicTypeInfer
import SimpleComprehensiveType as Type
import Ast

main! = \_ ->
    code = "1 < 2"
    token_results = Token.tokenize_str code
    tokens = List.keep_oks token_results \r -> r
    filtered = List.keep_if tokens \token -> Bool.not (is_trivia_token token)
    
    # Debug the tokens
    List.for_each! filtered \token ->
        Stdout.line! "Token: $(Token.ts_token_debug_display token)"
    
    ast = Parser.parse_program filtered
    
    # Check what we get
    when ast is
        Program { body } ->
            List.for_each! body \node ->
                inferred = BasicTypeInfer.infer_type node
                type_str = Type.type_to_str inferred
                Stdout.line! "Node type: $(type_str)"
        _ ->
            Stdout.line! "Not a program"

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

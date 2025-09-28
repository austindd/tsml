app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import Parser exposing [parse_program]
import Token exposing [tokenize_str]

main! = |_args|
    code = "function test(): number { return 42; }"

    tokens = tokenize_str(code)

    # Filter out trivia tokens
    non_trivia_tokens = List.keep_if(tokens, |token|
        when token is
            WhitespaceTrivia(_) | NewLineTrivia(_) | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText(_) ->
                Bool.false
            _ ->
                Bool.true
    )

    (ast, _rest) = parse_program(non_trivia_tokens)
    Stdout.line!("AST: $(Inspect.to_str(ast))")
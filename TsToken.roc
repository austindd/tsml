# module [
#    ts_token_debug_display,
#    EcmaAlpha,
#    EcmaWhitespace,
#    EcmaNewline,
# ]
# TsToken : [Start, Space, Newline, StrLit Str, NumLit Str, Ident Str, Keyword Str, Unknown]
# TsTokenResult : [Ok TsToken, Err [Unknown]]
# ts_token_debug_display : TsToken -> Str
# ts_token_debug_display = |token|
#    when token is
#        Start -> "$$__Start__$$"
#        Space -> "Space"
#        Newline -> "Newline"
#        StrLit(str) -> "StrLit(${str})"
#        NumLit(str) -> "NumLit(${str})"
#        Ident(str) -> "Ident(${str})"
#        Keyword(str) -> "Keyword(${str})"
#        Unknown -> "$$__Unknown__$$"
# EcmaWhitespace : [Space, Newline, LineTabulation, FormFeed, ZeroWidthNoBreakSpace]
# EcmaAlpha : [Alpha (List U8)]
# EcmaNewline : [Newline]
# utf8_list_to_ts_token_list : List U8 -> List TsTokenResult
# utf8_list_to_ts_token_list = |u8_list_|
#    utf8_list_to_ts_token_list_inner : TsToken, List U8, List (Result TsToken [Unknown]) -> List TsTokenResult
#    utf8_list_to_ts_token_list_inner = |prev_token, u8_list, token_list|
#        when u8_list is
#            [32, .. as u8s] -> utf8_list_to_ts_token_list_inner(Space, u8s, List.append(token_list, Ok(Space)))
#            [10, .. as u8s] -> utf8_list_to_ts_token_list_inner(Newline, u8s, List.append(token_list, Ok(Newline)))
#            [34, .. as u8s] -> utf8_list_to_ts_token_list_inner(StrLit("\""), u8s, List.append(token_list, Ok(StrLit("\""))))
#            [48, .. as u8s] -> utf8_list_to_ts_token_list_inner(NumLit("0"), u8s, List.append(token_list, Ok(NumLit("0"))))
#            [61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Keyword("="), u8s, List.append(token_list, Ok(Keyword("="))))
#            [u8, .. as u8s] ->
#                if (u8 >= 65 and u8 <= 90 or u8 >= 97 and u8 <= 122) then
#                    ident_result = Str.from_utf8([u8])
#                    when ident_result is
#                        Ok(x) -> utf8_list_to_ts_token_list_inner(Ident(x), u8s, List.append(token_list, Ok(Ident(x))))
#                        Err(_err) -> utf8_list_to_ts_token_list_inner(Unknown, u8s, List.append(token_list, Err(Unknown)))
#                else
#                    utf8_list_to_ts_token_list_inner(Unknown, u8s, List.append(token_list, Err(Unknown)))
#            _ -> List.append(token_list, Err(Unknown))
#    utf8_list_to_ts_token_list_inner(Start, u8_list_, [Ok(Start)])
# Improved
module [
    ts_token_debug_display,
    utf8_list_to_ts_token_list,
    EcmaAlpha,
    EcmaWhitespace,
    EcmaNewline,
]

TsToken : [
    Start,
    Space,
    Tab,
    Newline,
    StrLit Str,
    NumLit Str,
    Ident Str,
    Keyword Str,
    Operator Str,
    Punctuation Str,
    Comment Str,
    TemplateLitPart Str,
    TemplateLitEnd Str,
    InterpolationPart,
    Unknown,
]

TsTokenResult : [Ok TsToken, Err [Unknown, UnclosedString, UnclosedTemplate, UnclosedInterpolation]]

ts_token_debug_display : TsToken -> Str
ts_token_debug_display = |token|
    when token is
        Start -> "$$__Start__$$"
        Space -> "Space"
        Tab -> "Tab"
        Newline -> "Newline"
        StrLit(str) -> "StrLit(${str})"
        NumLit(str) -> "NumLit(${str})"
        Ident(str) -> "Ident(${str})"
        Keyword(str) -> "Keyword(${str})"
        Operator(str) -> "Operator(${str})"
        Punctuation(str) -> "Punctuation(${str})"
        Comment(str) -> "Comment(${str})"
        TemplateLitPart(str) -> "TemplateLitPart(${str})"
        TemplateLitEnd(str) -> "TemplateLitEnd(${str})"
        InterpolationPart -> "InterpolationPart"
        Unknown -> "$$__Unknown__$$"

EcmaWhitespace : [Space, Tab, Newline, LineTabulation, FormFeed, ZeroWidthNoBreakSpace]
EcmaAlpha : [Alpha (List U8)]
EcmaNewline : [Newline]

utf8_list_to_ts_token_list : List U8 -> List TsTokenResult
utf8_list_to_ts_token_list = |u8_list_|
    # Helper functions

    # Helper processing functions with tail recursion

    # Start the tokenization
    utf8_list_to_ts_token_list_inner(Start, u8_list_, [])

# Main recursive tokenizer function with accumulator
utf8_list_to_ts_token_list_inner : TsToken, List U8, List TsTokenResult -> List TsTokenResult
utf8_list_to_ts_token_list_inner = |prev_token, u8_list, token_list|
    when u8_list is
        # Whitespace
        [32, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Space, u8s, List.append(token_list, Ok(Space)))

        [9, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Tab, u8s, List.append(token_list, Ok(Tab)))

        [10, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Newline, u8s, List.append(token_list, Ok(Newline)))

        [13, 10, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Newline, u8s, List.append(token_list, Ok(Newline)))

        # String literals
        [34, .. as u8s] ->
            process_string_literal(u8s, "\"", [], token_list)

        [39, .. as u8s] ->
            process_string_literal(u8s, "'", [], token_list)

        [96, .. as u8s] ->
            process_template_literal(u8s, [], token_list)

        # Numeric literals
        [u8, .. as u8s] if is_digit(u8) ->
            process_numeric_literal(u8, u8s, token_list)

        # Comments
        [47, 47, .. as u8s] ->
            process_line_comment(u8s, [], token_list)

        [47, 42, .. as u8s] ->
            process_block_comment(u8s, [], Bool.false, token_list)

        # Operators and punctuation - multi-character operators first
        [61, 61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("==="), u8s, List.append(token_list, Ok(Operator("==="))))

        [33, 61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("!=="), u8s, List.append(token_list, Ok(Operator("!=="))))

        [61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("=="), u8s, List.append(token_list, Ok(Operator("=="))))

        [33, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("!="), u8s, List.append(token_list, Ok(Operator("!="))))

        [62, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator(">="), u8s, List.append(token_list, Ok(Operator(">="))))

        [60, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("<="), u8s, List.append(token_list, Ok(Operator("<="))))

        [38, 38, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("&&"), u8s, List.append(token_list, Ok(Operator("&&"))))

        [124, 124, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("||"), u8s, List.append(token_list, Ok(Operator("||"))))

        [43, 43, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("++"), u8s, List.append(token_list, Ok(Operator("++"))))

        [45, 45, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("--"), u8s, List.append(token_list, Ok(Operator("--"))))

        # Single-character operators and punctuation
        [40, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation("("), u8s, List.append(token_list, Ok(Punctuation("("))))

        [41, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation(")"), u8s, List.append(token_list, Ok(Punctuation(")"))))

        [123, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation("{"), u8s, List.append(token_list, Ok(Punctuation("{"))))

        [125, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation("}"), u8s, List.append(token_list, Ok(Punctuation("}"))))

        [91, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation("["), u8s, List.append(token_list, Ok(Punctuation("["))))

        [93, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation("]"), u8s, List.append(token_list, Ok(Punctuation("]"))))

        [59, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation(";"), u8s, List.append(token_list, Ok(Punctuation(";"))))

        [44, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation(","), u8s, List.append(token_list, Ok(Punctuation(","))))

        [46, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation("."), u8s, List.append(token_list, Ok(Punctuation("."))))

        [58, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation(":"), u8s, List.append(token_list, Ok(Punctuation(":"))))

        [63, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("?"), u8s, List.append(token_list, Ok(Operator("?"))))

        [61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("="), u8s, List.append(token_list, Ok(Operator("="))))

        [43, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("+"), u8s, List.append(token_list, Ok(Operator("+"))))

        [45, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("-"), u8s, List.append(token_list, Ok(Operator("-"))))

        [42, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("*"), u8s, List.append(token_list, Ok(Operator("*"))))

        [47, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("/"), u8s, List.append(token_list, Ok(Operator("/"))))

        [37, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("%"), u8s, List.append(token_list, Ok(Operator("%"))))

        [33, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("!"), u8s, List.append(token_list, Ok(Operator("!"))))

        [62, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator(">"), u8s, List.append(token_list, Ok(Operator(">"))))

        [60, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("<"), u8s, List.append(token_list, Ok(Operator("<"))))

        [38, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("&"), u8s, List.append(token_list, Ok(Operator("&"))))

        [124, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("|"), u8s, List.append(token_list, Ok(Operator("|"))))

        [94, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("^"), u8s, List.append(token_list, Ok(Operator("^"))))

        [126, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("~"), u8s, List.append(token_list, Ok(Operator("~"))))

        # Identifiers and keywords
        [u8, .. as u8s] if is_identifier_start(u8) ->
            process_identifier(u8, u8s, token_list)

        # End of input or unknown character
        [] -> token_list
        [_, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Unknown, u8s, List.append(token_list, Err(Unknown)))

process_block_comment : List U8, List U8, Bool, List TsTokenResult -> List TsTokenResult
process_block_comment = |u8s, acc, star_seen, token_list|
    inner_process : List U8, List U8, Bool, List TsTokenResult -> List TsTokenResult
    inner_process = |current_u8s, current_acc, current_star_seen, current_token_list|
        when current_u8s is
            [42, 47, .. as rest] -> # "*/" ends comment
                comment_result = Str.from_utf8(current_acc)
                when comment_result is
                    Ok(comment) ->
                        utf8_list_to_ts_token_list_inner(Comment(comment), rest, List.append(current_token_list, Ok(Comment(comment))))

                    Err(_) ->
                        utf8_list_to_ts_token_list_inner(Unknown, current_u8s, List.append(current_token_list, Err(Unknown)))

            [42, .. as rest] ->
                inner_process(rest, List.append(current_acc, 42), Bool.true, current_token_list)

            [47, .. as rest] if current_star_seen ->
                inner_process(rest, List.append(current_acc, 47), Bool.false, current_token_list)

            [u8, .. as rest] ->
                inner_process(rest, List.append(current_acc, u8), Bool.false, current_token_list)

            [] -> # Unclosed block comment
                comment_result = Str.from_utf8(current_acc)
                when comment_result is
                    Ok(comment) ->
                        List.append(current_token_list, Ok(Comment(comment)))

                    Err(_) ->
                        List.append(current_token_list, Err(Unknown))
    inner_process(u8s, acc, star_seen, token_list)

is_identifier_start : U8 -> Bool
is_identifier_start = |u8|
    (u8 >= 65 and u8 <= 90)
    or # A-Z
    (u8 >= 97 and u8 <= 122)
    or # a-z
    u8
    == 95
    or # underscore
    u8
    == 36 # dollar sign

is_identifier_part : U8 -> Bool
is_identifier_part = |u8|
    is_identifier_start(u8) or is_digit(u8)

is_digit : U8 -> Bool
is_digit = |u8| u8 >= 48 and u8 <= 57

process_identifier : U8, List U8, List TsTokenResult -> List TsTokenResult
process_identifier = |first_char, rest, token_list|
    collect_identifier_chars : List U8, List U8 -> (List U8, List U8)
    collect_identifier_chars = |acc, remaining|
        inner_collect : List U8, List U8, List U8 -> (List U8, List U8)
        inner_collect = |current_acc, current_remaining, final_acc|
            when current_remaining is
                [u8, .. as rest_chars] if is_identifier_part(u8) ->
                    inner_collect(List.append(current_acc, u8), rest_chars, final_acc)

                _ -> (List.concat(final_acc, current_acc), current_remaining)
        inner_collect(acc, remaining, [])

    (ident_chars, new_remaining) = collect_identifier_chars([first_char], rest)
    ident_result = Str.from_utf8(ident_chars)

    when ident_result is
        Ok(ident) ->
            token = if is_keyword(ident) then Keyword(ident) else Ident(ident)
            utf8_list_to_ts_token_list_inner(token, new_remaining, List.append(token_list, Ok(token)))

        Err(_) ->
            utf8_list_to_ts_token_list_inner(Unknown, rest, List.append(token_list, Err(Unknown)))

process_numeric_literal : U8, List U8, List TsTokenResult -> List TsTokenResult
process_numeric_literal = |first_digit, rest, token_list|
    collect_numeric_chars : List U8, List U8, Bool, Bool -> (List U8, List U8)
    collect_numeric_chars = |acc, remaining, has_decimal, has_exp|
        inner_collect : List U8, List U8, Bool, Bool, List U8 -> (List U8, List U8)
        inner_collect = |current_acc, current_remaining, current_decimal, current_exp, final_acc|
            when current_remaining is
                [46, .. as rest_chars] if !current_decimal -> # decimal point
                    inner_collect(List.append(current_acc, 46), rest_chars, Bool.true, current_exp, final_acc)

                [101, .. as rest_chars] if !current_exp -> # 'e' for exponent
                    inner_collect(List.append(current_acc, 101), rest_chars, current_decimal, Bool.true, final_acc)

                [69, .. as rest_chars] if !current_exp -> # 'E' for exponent
                    inner_collect(List.append(current_acc, 69), rest_chars, current_decimal, Bool.true, final_acc)

                [43, .. as rest_chars] if current_exp and (List.last(current_acc) == Ok(101) or List.last(current_acc) == Ok(69)) -> # '+' after exponent
                    inner_collect(List.append(current_acc, 43), rest_chars, current_decimal, current_exp, final_acc)

                [45, .. as rest_chars] if current_exp and (List.last(current_acc) == Ok(101) or List.last(current_acc) == Ok(69)) -> # '-' after exponent
                    inner_collect(List.append(current_acc, 45), rest_chars, current_decimal, current_exp, final_acc)

                [u8, .. as rest_chars] if is_digit(u8) ->
                    inner_collect(List.append(current_acc, u8), rest_chars, current_decimal, current_exp, final_acc)

                _ -> (List.concat(final_acc, current_acc), current_remaining)
        inner_collect(acc, remaining, has_decimal, has_exp, [])

    (num_chars, new_remaining) = collect_numeric_chars([first_digit], rest, Bool.false, Bool.false)
    num_result = Str.from_utf8(num_chars)

    when num_result is
        Ok(num_str) ->
            utf8_list_to_ts_token_list_inner(NumLit(num_str), new_remaining, List.append(token_list, Ok(NumLit(num_str))))

        Err(_) ->
            utf8_list_to_ts_token_list_inner(Unknown, rest, List.append(token_list, Err(Unknown)))

process_string_literal : List U8, Str, List U8, List TsTokenResult -> List TsTokenResult
process_string_literal = |u8s, quote_type, acc, token_list|
    inner_process : List U8, Str, List U8, List TsTokenResult -> List TsTokenResult
    inner_process = |current_u8s, current_quote_type, current_acc, current_token_list|
        when current_u8s is
            # Handle escape sequences
            [92, next, .. as rest] -> # backslash followed by any character
                inner_process(
                    rest,
                    current_quote_type,
                    current_acc
                    |> List.append(92)
                    |> List.append(next),
                    current_token_list,
                )

            # End of string based on quote type
            [34, .. as rest] if current_quote_type == "\"" -> # double quote
                str_result = Str.from_utf8(current_acc)
                when str_result is
                    Ok(str) ->
                        utf8_list_to_ts_token_list_inner(StrLit(str), rest, List.append(current_token_list, Ok(StrLit(str))))

                    Err(_) ->
                        utf8_list_to_ts_token_list_inner(Unknown, rest, List.append(current_token_list, Err(Unknown)))

            [39, .. as rest] if current_quote_type == "'" -> # single quote
                str_result = Str.from_utf8(current_acc)
                when str_result is
                    Ok(str) ->
                        utf8_list_to_ts_token_list_inner(StrLit(str), rest, List.append(current_token_list, Ok(StrLit(str))))

                    Err(_) ->
                        utf8_list_to_ts_token_list_inner(Unknown, rest, List.append(current_token_list, Err(Unknown)))

            # Collect string content
            [u8, .. as rest] ->
                inner_process(rest, current_quote_type, List.append(current_acc, u8), current_token_list)

            # Unclosed string
            [] ->
                List.append(current_token_list, Err(UnclosedString))
    inner_process(u8s, quote_type, acc, token_list)

process_line_comment : List U8, List U8, List TsTokenResult -> List TsTokenResult
process_line_comment = |u8s, acc, token_list|
    inner_process : List U8, List U8, List TsTokenResult -> List TsTokenResult
    inner_process = |current_u8s, current_acc, current_token_list|
        when current_u8s is
            [10, .. as rest] -> # Newline ends comment
                comment_result = Str.from_utf8(current_acc)
                when comment_result is
                    Ok(comment) ->
                        utf8_list_to_ts_token_list_inner(Comment(comment), rest, List.append(current_token_list, Ok(Comment(comment))))

                    Err(_) ->
                        utf8_list_to_ts_token_list_inner(Unknown, current_u8s, List.append(current_token_list, Err(Unknown)))

            [u8, .. as rest] ->
                inner_process(rest, List.append(current_acc, u8), current_token_list)

            [] -> # End of input
                comment_result = Str.from_utf8(current_acc)
                when comment_result is
                    Ok(comment) ->
                        List.append(current_token_list, Ok(Comment(comment)))

                    Err(_) ->
                        List.append(current_token_list, Err(Unknown))
    inner_process(u8s, acc, token_list)

process_template_literal : List U8, List U8, List TsTokenResult -> List TsTokenResult
process_template_literal = |u8s, acc, token_list|
    inner_process : List U8, List U8, List TsTokenResult -> List TsTokenResult
    inner_process = |current_u8s, current_acc, current_token_list|
        when current_u8s is
            # Handle escape sequences
            [92, next, .. as rest] -> # backslash followed by any character
                inner_process(
                    rest,
                    current_acc
                    |> List.append(92)
                    |> List.append(next),
                    current_token_list,
                )

            # Handle expression interpolation - ${...}
            [36, 123, .. as rest] ->
                str_result = Str.from_utf8(current_acc)
                when str_result is
                    Ok(str) ->
                        template_token = TemplateLitPart(str)
                        updated_token_list = List.append(current_token_list, Ok(template_token))
                        # Handle interpolation
                        process_interpolation_wrapper(rest, 1, updated_token_list)

                    Err(_) ->
                        utf8_list_to_ts_token_list_inner(Unknown, rest, List.append(current_token_list, Err(Unknown)))

            # End of template
            [96, .. as rest] -> # backtick
                str_result = Str.from_utf8(current_acc)
                when str_result is
                    Ok(str) ->
                        utf8_list_to_ts_token_list_inner(TemplateLitEnd(str), rest, List.append(current_token_list, Ok(TemplateLitEnd(str))))

                    Err(_) ->
                        utf8_list_to_ts_token_list_inner(Unknown, rest, List.append(current_token_list, Err(Unknown)))

            # Collect template content
            [u8, .. as rest] ->
                inner_process(rest, List.append(current_acc, u8), current_token_list)

            # Unclosed template
            [] ->
                List.append(current_token_list, Err(UnclosedTemplate))
    inner_process(u8s, acc, token_list)

process_interpolation_wrapper : List U8, Num _, List TsTokenResult -> List TsTokenResult
process_interpolation_wrapper = |u8s, brace_depth, token_list|
    process_interpolation : List U8, Num _, List TsTokenResult -> (List U8, List TsTokenResult)
    process_interpolation = |current_u8s, current_depth, current_token_list|
        when current_u8s is
            [123, .. as rest] -> # Opening brace increases depth
                process_interpolation(rest, current_depth + 1, current_token_list)

            [125, .. as rest] if current_depth == 1 -> # Closing brace at depth 1 ends interpolation
                (rest, List.append(current_token_list, Ok(InterpolationPart)))

            [125, .. as rest] -> # Closing brace decreases depth
                process_interpolation(rest, current_depth - 1, current_token_list)

            [_, .. as rest] ->
                process_interpolation(rest, current_depth, current_token_list)

            [] -> # Unclosed interpolation
                ([], List.append(current_token_list, Err(UnclosedInterpolation)))

    (new_remaining, updated_token_list) = process_interpolation(u8s, brace_depth, token_list)
    # Continue processing the template
    process_template_literal(new_remaining, [], updated_token_list)

is_keyword : Str -> Bool
is_keyword = |s|
    List.contains(keywords, s)

keywords = [
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "as",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield",
    "any",
    "boolean",
    "constructor",
    "declare",
    "get",
    "module",
    "require",
    "number",
    "set",
    "string",
    "symbol",
    "type",
    "from",
    "of",
    "async",
    "await",
]

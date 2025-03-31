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
    # Operators
    Operator Str,
    TripleEqual,
    NotTripleEqual,
    DoubleEqual,
    NotDoubleEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equal,
    QuestionMark,
    Plus,
    Minus,
    LogicalAnd,
    LogicalOr,
    PlusPlus,
    MinusMinus,
    # Punctuation
    Punctuation Str,
    OpenParen,
    CloseParen,
    OpenCurlyBracket,
    CloseCurlyBracket,
    OpenSquareBracket,
    CloseSquareBracket,
    Comment Str,
    TemplateLitPart Str,
    TemplateLitEnd Str,
    InterpolationPart,
    FunctionArrow,
    Unknown,
]

TsTokenResult : [
    Ok TsToken,
    Err [Unknown, UnclosedString, UnclosedTemplate, UnclosedInterpolation, InvalidNumericSeparator],
]

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
        # Operators
        Operator(str) -> "Operator(${str})"
        TripleEqual -> "TripleEqual"
        NotTripleEqual -> "NotTripleEqual"
        DoubleEqual -> "DoubleEqual"
        NotDoubleEqual -> "NotDoubleEqual"
        GreaterThan -> "GreaterThan"
        LessThan -> "LessThan"
        GreaterThanOrEqual -> "GreaterThanOrEqual"
        LessThanOrEqual -> "LessThanOrEqual"
        Equal -> "Equal"
        QuestionMark -> "QuestionMark"
        Plus -> "Plus"
        Minus -> "Minus"
        LogicalAnd -> "LogicalAnd"
        LogicalOr -> "LogicalOr"
        PlusPlus -> "PlusPlus"
        MinusMinus -> "MinusMinus"
        # Punctuation
        Punctuation(str) -> "Punctuation(${str})"
        OpenParen -> "OpenParen"
        CloseParen -> "CloseParen"
        OpenCurlyBracket -> "OpenCurlyBracket"
        CloseCurlyBracket -> "CloseCurlyBracket"
        OpenSquareBracket -> "OpenSquareBracket"
        CloseSquareBracket -> "CloseSquareBracket"
        Comment(str) -> "Comment(${str})"
        TemplateLitPart(str) -> "TemplateLitPart(${str})"
        TemplateLitEnd(str) -> "TemplateLitEnd(${str})"
        InterpolationPart -> "InterpolationPart"
        FunctionArrow -> "FunctionArrow"
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
            utf8_list_to_ts_token_list_inner(TripleEqual, u8s, List.append(token_list, Ok(TripleEqual)))

        [33, 61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(NotTripleEqual, u8s, List.append(token_list, Ok(NotTripleEqual)))

        [61, 62, .. as u8s] -> # => (Function Arrow)
            utf8_list_to_ts_token_list_inner(FunctionArrow, u8s, List.append(token_list, Ok(FunctionArrow)))

        [61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(DoubleEqual, u8s, List.append(token_list, Ok(DoubleEqual)))

        [33, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(NotDoubleEqual, u8s, List.append(token_list, Ok(NotDoubleEqual)))

        [62, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(GreaterThanOrEqual, u8s, List.append(token_list, Ok(GreaterThanOrEqual)))

        [60, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(LessThanOrEqual, u8s, List.append(token_list, Ok(LessThanOrEqual)))

        [38, 38, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(LogicalAnd, u8s, List.append(token_list, Ok(LogicalAnd)))

        [124, 124, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(LogicalOr, u8s, List.append(token_list, Ok(LogicalOr)))

        [43, 43, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(PlusPlus, u8s, List.append(token_list, Ok(PlusPlus)))

        [45, 45, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(MinusMinus, u8s, List.append(token_list, Ok(MinusMinus)))

        # Single-character operators and punctuation
        [40, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(OpenParen, u8s, List.append(token_list, Ok(OpenParen)))

        [41, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(CloseParen, u8s, List.append(token_list, Ok(CloseParen)))

        [123, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(OpenCurlyBracket, u8s, List.append(token_list, Ok(OpenCurlyBracket)))

        [125, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(CloseCurlyBracket, u8s, List.append(token_list, Ok(CloseCurlyBracket)))

        [91, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(OpenSquareBracket, u8s, List.append(token_list, Ok(OpenSquareBracket)))

        [93, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(CloseSquareBracket, u8s, List.append(token_list, Ok(CloseSquareBracket)))

        [59, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation(";"), u8s, List.append(token_list, Ok(Punctuation(";"))))

        [44, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation(","), u8s, List.append(token_list, Ok(Punctuation(","))))

        [46, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation("."), u8s, List.append(token_list, Ok(Punctuation("."))))

        [58, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Punctuation(":"), u8s, List.append(token_list, Ok(Punctuation(":"))))

        [63, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(QuestionMark, u8s, List.append(token_list, Ok(QuestionMark)))

        [61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Equal, u8s, List.append(token_list, Ok(Equal)))

        [43, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Plus, u8s, List.append(token_list, Ok(Plus)))

        [45, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Minus, u8s, List.append(token_list, Ok(Minus)))

        [42, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("*"), u8s, List.append(token_list, Ok(Operator("*"))))

        [47, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("/"), u8s, List.append(token_list, Ok(Operator("/"))))

        [37, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("%"), u8s, List.append(token_list, Ok(Operator("%"))))

        [33, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(Operator("!"), u8s, List.append(token_list, Ok(Operator("!"))))

        [62, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(GreaterThan, u8s, List.append(token_list, Ok(GreaterThan)))

        [60, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(LessThan, u8s, List.append(token_list, Ok(LessThan)))

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
    # Assuming is_digit is defined elsewhere as: |u8| u8 >= 48 && u8 <= 57

    # Return type changed to Result
    collect_numeric_chars : List U8, List U8, Bool, Bool -> Result (List U8, List U8) [InvalidNumericSeparator]
    collect_numeric_chars = |acc, remaining, has_decimal, has_exp|
        # Tail-recursive inner helper - Return type changed to Result
        inner_collect : List U8, List U8, Bool, Bool, List U8 -> Result (List U8, List U8) [InvalidNumericSeparator]
        inner_collect = |current_acc, current_remaining, current_decimal, current_exp, final_acc|
            when current_remaining is
                # --- Underscore Separator Handling ---
                [95, .. as rest_after_underscore] -> # Encountered an underscore '_'
                    # Check for immediate double underscore: __
                    when List.first(rest_after_underscore) is
                        Ok(95) -> Err(InvalidNumericSeparator) # Explicit Error for __
                        _ -> # Not a double underscore, check normal rules
                            # Rule 1 & 2 & 3 (preceding): Last char added MUST be a digit.
                            prev_is_digit =
                                when List.last(current_acc) is
                                    Ok(prev_u8) -> is_digit(prev_u8)
                                    Err(_) -> Bool.false

                            # Rule 1 & 3 & 4 (succeeding): Next char MUST be a digit.
                            # Note: We already know next is not '_' from the outer when.
                            next_is_digit =
                                when List.first(rest_after_underscore) is
                                    Ok(next_u8) -> is_digit(next_u8)
                                    Err(_) -> Bool.false # Underscore at the end

                            if prev_is_digit and next_is_digit then
                                # Valid separator: Append underscore and continue
                                # Wrap recursive call in Ok as it returns Result
                                inner_collect(List.append(current_acc, 95), rest_after_underscore, current_decimal, current_exp, final_acc)
                            else
                                # Invalid separator placement (e.g., _ at end, 1_e, ._): Report Error
                                Err(InvalidNumericSeparator)

                # --- Decimal point '.' (ASCII 46) ---
                [46, .. as rest_chars] if !current_decimal and !current_exp ->
                    prev_is_underscore =
                        when List.last(current_acc) is
                            Ok(95) -> Bool.true
                            _ -> Bool.false
                    next_is_underscore =
                        when List.first(rest_chars) is
                            Ok(95) -> Bool.true
                            _ -> Bool.false

                    if prev_is_underscore or next_is_underscore then
                        Err(InvalidNumericSeparator) # Invalid: '_.' or '._'
                    else
                        inner_collect(List.append(current_acc, 46), rest_chars, Bool.true, current_exp, final_acc)

                # --- Exponent 'e'/'E' (ASCII 101/69) ---
                [exp_char, .. as rest_chars] if (exp_char == 101 or exp_char == 69) and !current_exp ->
                    prev_is_underscore =
                        when List.last(current_acc) is
                            Ok(95) -> Bool.true
                            _ -> Bool.false
                    next_pattern_is_invalid = # Check e_, E_, e+_, E+_, e-_, E-_
                        when List.first(rest_chars) is
                            Ok(95) -> Bool.true
                            Ok(x) if x == 43 or x == 45 ->
                                when List.get(rest_chars, 1) is
                                    Ok(95) -> Bool.true
                                    _ -> Bool.false

                            _ -> Bool.false

                    if prev_is_underscore or next_pattern_is_invalid then
                        Err(InvalidNumericSeparator) # Invalid: '_e', 'e_', 'e+_' etc.
                    else
                        inner_collect(List.append(current_acc, exp_char), rest_chars, current_decimal, Bool.true, final_acc)

                # --- Exponent sign '+' / '-' (ASCII 43/45) ---
                [sign_char, .. as rest_chars] if (sign_char == 43 or sign_char == 45) and current_exp ->
                    is_after_exp_indicator =
                        when List.last(current_acc) is
                            Ok(prev_u8) -> prev_u8 == 101 or prev_u8 == 69
                            Err(_) -> Bool.false
                    next_is_underscore =
                        when List.first(rest_chars) is
                            Ok(95) -> Bool.true
                            _ -> Bool.false

                    if is_after_exp_indicator and !next_is_underscore then
                        inner_collect(List.append(current_acc, sign_char), rest_chars, current_decimal, current_exp, final_acc)
                    else
                        Err(InvalidNumericSeparator) # Invalid sign placement or e+_
                # --- Digits '0'-'9' ---

                [u8, .. as rest_chars] if is_digit(u8) ->
                    inner_collect(List.append(current_acc, u8), rest_chars, current_decimal, current_exp, final_acc)

                # --- End of numeric literal (any other character or end of input) ---
                # Successful termination of collection
                _ -> Ok (List.concat(final_acc, current_acc), current_remaining)

        # Initial call to the inner helper
        inner_collect(acc, remaining, has_decimal, has_exp, [])

    # --- Function Body ---
    # Handle the Result from collect_numeric_chars
    when collect_numeric_chars([first_digit], rest, Bool.false, Bool.false) is
        Ok (num_chars, new_remaining) ->
            # Success case: Proceed mostly as before
            final_num_chars =
                (
                    when List.last(num_chars) is
                        # Final check for trailing underscore (should be caught earlier, but good practice)
                        Ok(95) -> Err(InvalidNumericSeparator) # Trigger error path if found
                        _ -> Ok(num_chars)
                )
                |> Result.with_default(num_chars) # Use original if Ok, fallback shouldn't hit if logic sound

            if List.is_empty(final_num_chars) then
                # This case might happen if e.g., the input was just "1_" and '_' was invalid
                # We already reported the error if collect_numeric_chars returned Err.
                # If it returned Ok with empty list (e.g. from final validation), treat as Unknown?
                # Or maybe this path shouldn't be reachable if errors are caught properly.
                # For safety, let's treat empty as Unknown/Errornous scenario.
                utf8_list_to_ts_token_list_inner(Unknown, new_remaining, List.append(token_list, Err(Unknown))) # Or InvalidNumericSeparator?
            else
                num_result = Str.from_utf8(final_num_chars)
                when num_result is
                    Ok(num_str) ->
                        utf8_list_to_ts_token_list_inner(NumLit(num_str), new_remaining, List.append(token_list, Ok(NumLit(num_str))))

                    Err(_) -> # UTF8 error, shouldn't happen with digits/'.'/'e'/'_'/'+'/'-'
                        utf8_list_to_ts_token_list_inner(Unknown, new_remaining, List.append(token_list, Err(Unknown)))

        Err(InvalidNumericSeparator) ->
            # Error detected during numeric collection!
            # Append the specific error token.
            # For recovery, we'll continue tokenizing *after* the first digit,
            # effectively discarding the problematic numeric literal attempt.
            # 'rest' is the list of U8s immediately following the 'first_digit'.
            utf8_list_to_ts_token_list_inner(Unknown, rest, List.append(token_list, Err(InvalidNumericSeparator)))

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
process_interpolation_wrapper = |u8s, curly_bracket_depth, token_list|
    process_interpolation : List U8, Num _, List TsTokenResult -> (List U8, List TsTokenResult)
    process_interpolation = |current_u8s, current_depth, current_token_list|
        when current_u8s is
            [123, .. as rest] -> # Opening CurlyBracket increases depth
                process_interpolation(rest, current_depth + 1, current_token_list)

            [125, .. as rest] if current_depth == 1 -> # Closing CurlyBracket at depth 1 ends interpolation
                (rest, List.append(current_token_list, Ok(InterpolationPart)))

            [125, .. as rest] -> # Closing CurlyBracket decreases depth
                process_interpolation(rest, current_depth - 1, current_token_list)

            [_, .. as rest] ->
                process_interpolation(rest, current_depth, current_token_list)

            [] -> # Unclosed interpolation
                ([], List.append(current_token_list, Err(UnclosedInterpolation)))

    (new_remaining, updated_token_list) = process_interpolation(u8s, curly_bracket_depth, token_list)
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

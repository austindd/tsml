module []

import Token exposing [tokenize_str]

expect
    ts_string = ""
    token_list = tokenize_str(ts_string)
    token_list == [Ok(EndOfFileToken)]

expect
    ts_string = "\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(NewLineTrivia), Ok(EndOfFileToken)]

expect
    ts_string = "\r\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(NewLineTrivia), Ok(EndOfFileToken)]

expect
    ts_string = "\r"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(Unknown), Ok(EndOfFileToken)]

expect
    ts_string = "const"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(EndOfFileToken)]

expect
    ts_string = "const "
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(WhitespaceTrivia), Ok(EndOfFileToken)]

expect
    ts_string = "const\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(NewLineTrivia), Ok(EndOfFileToken)]

expect
    ts_string = "const\r\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(NewLineTrivia), Ok(EndOfFileToken)]

expect
    ts_string = "let x = 1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(LetKeyword),
        Ok(WhitespaceTrivia),
        Ok(Identifier("x")),
        Ok(WhitespaceTrivia),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia),
        Ok(NumericLiteral("1")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "var x = 23;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(VarKeyword),
        Ok(WhitespaceTrivia),
        Ok(Identifier("x")),
        Ok(WhitespaceTrivia),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia),
        Ok(NumericLiteral("23")),
        Ok(SemicolonToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "-1.890e-1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(MinusToken),
        Ok(NumericLiteral("1.890e-1")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "18.90e21"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumericLiteral("18.90e21")),
        Ok(EndOfFileToken),
    ]

# TODO: Support hex literals
# expect
#     ts_string = "0x1A"
#     token_list = tokenize_str(ts_string)
#     token_list
#     == [
#         Ok(NumericLiteral("0x1A")),
#         Ok(EndOfFileToken),
#     ]

expect
    ts_string = "1_000_123"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumericLiteral("1_000_123")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "1_000_123.456_789"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumericLiteral("1_000_123.456_789")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "1_000_123.456_789e-1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumericLiteral("1_000_123.456_789e-1")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "\"Hello, world!\""
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(StringLiteral("\"Hello, world!\"")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "'Hello, world!'"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(StringLiteral("'Hello, world!'")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "\"Hello, world!\\n\""
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(StringLiteral("\"Hello, world!\\n\"")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "for (let i = 0; i < 10; i++) {}"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(ForKeyword),
        Ok(WhitespaceTrivia),
        Ok(OpenParenToken),
        Ok(LetKeyword),
        Ok(WhitespaceTrivia),
        Ok(Identifier("i")),
        Ok(WhitespaceTrivia),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia),
        Ok(NumericLiteral("0")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia),
        Ok(Identifier("i")),
        Ok(WhitespaceTrivia),
        Ok(LessThanToken),
        Ok(WhitespaceTrivia),
        Ok(NumericLiteral("10")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia),
        Ok(Identifier("i")),
        Ok(PlusPlusToken),
        Ok(CloseParenToken),
        Ok(WhitespaceTrivia),
        Ok(OpenBraceToken),
        Ok(CloseBraceToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "const x = 100 + y + (function() { return 42; })()"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(ConstKeyword),
        Ok(WhitespaceTrivia),
        Ok(Identifier("x")),
        Ok(WhitespaceTrivia),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia),
        Ok(NumericLiteral("100")),
        Ok(WhitespaceTrivia),
        Ok(PlusToken),
        Ok(WhitespaceTrivia),
        Ok(Identifier("y")),
        Ok(WhitespaceTrivia),
        Ok(PlusToken),
        Ok(WhitespaceTrivia),
        Ok(OpenParenToken),
        Ok(FunctionKeyword),
        Ok(OpenParenToken),
        Ok(CloseParenToken),
        Ok(WhitespaceTrivia),
        Ok(OpenBraceToken),
        Ok(WhitespaceTrivia),
        Ok(ReturnKeyword),
        Ok(WhitespaceTrivia),
        Ok(NumericLiteral("42")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia),
        Ok(CloseBraceToken),
        Ok(CloseParenToken),
        Ok(OpenParenToken),
        Ok(CloseParenToken),
        Ok(EndOfFileToken),
    ]

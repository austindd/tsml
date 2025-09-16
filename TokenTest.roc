module []

import Token exposing [tokenize_str]

expect
    ts_string = ""
    token_list = tokenize_str(ts_string)
    token_list == [Ok(EndOfFileToken)]

expect
    ts_string = "\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(NewLineTrivia(1)), Ok(EndOfFileToken)]

expect
    ts_string = "\r\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(NewLineTrivia(1)), Ok(EndOfFileToken)]

expect
    ts_string = "\r"
    token_list = tokenize_str(ts_string)
    token_list == [Err(UnknownToken([])), Ok(EndOfFileToken)]

expect
    ts_string = "const"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(EndOfFileToken)]

expect
    ts_string = "const "
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(WhitespaceTrivia(1)), Ok(EndOfFileToken)]

expect
    ts_string = "const\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(NewLineTrivia(1)), Ok(EndOfFileToken)]

expect
    ts_string = "const\r\n"
    token_list = tokenize_str(ts_string)
    token_list == [Ok(ConstKeyword), Ok(NewLineTrivia(1)), Ok(EndOfFileToken)]

expect
    ts_string = "let x = 1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(LetKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("x")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("1")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "var x = 23;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(VarKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("x")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("23")),
        Ok(SemicolonToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "-1.890e-1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(MinusToken),
        Ok(NumberLiteralToken("1.890e-1")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "18.90e21"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumberLiteralToken("18.90e21")),
        Ok(EndOfFileToken),
    ]

# TODO: Support hex literals
# expect
#     ts_string = "0x1A"
#     token_list = tokenize_str(ts_string)
#     token_list
#     == [
#         Ok(NumberLiteralToken("0x1A")),
#         Ok(EndOfFileToken),
#     ]

expect
    ts_string = "1_000_123"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumberLiteralToken("1_000_123")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "1_000_123.456_789"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumberLiteralToken("1_000_123.456_789")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "1_000_123.456_789e-1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(NumberLiteralToken("1_000_123.456_789e-1")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "\"Hello, world!\""
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(StringLiteralToken("\"Hello, world!\"")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "'Hello, world!'"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(StringLiteralToken("'Hello, world!'")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "\"Hello, world!\\n\""
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(StringLiteralToken("\"Hello, world!\\n\"")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "for (let i = 0; i < 10; i++) {}"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(ForKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenParenToken),
        Ok(LetKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("i")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("0")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("i")),
        Ok(WhitespaceTrivia(1)),
        Ok(LessThanToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("10")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("i")),
        Ok(PlusPlusToken),
        Ok(CloseParenToken),
        Ok(WhitespaceTrivia(1)),
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
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("x")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("100")),
        Ok(WhitespaceTrivia(1)),
        Ok(PlusToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("y")),
        Ok(WhitespaceTrivia(1)),
        Ok(PlusToken),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenParenToken),
        Ok(FunctionKeyword),
        Ok(OpenParenToken),
        Ok(CloseParenToken),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(WhitespaceTrivia(1)),
        Ok(ReturnKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("42")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(CloseBraceToken),
        Ok(CloseParenToken),
        Ok(OpenParenToken),
        Ok(CloseParenToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "interface A {}"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(InterfaceKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("A")),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(CloseBraceToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "const x: number = 42;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(ConstKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("x")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("42")),
        Ok(SemicolonToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "let message: string = \"Hello, World!\";"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(LetKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("message")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringLiteralToken("\"Hello, World!\"")),
        Ok(SemicolonToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "const pi: number = 3.14159;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(ConstKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("pi")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("3.14159")),
        Ok(SemicolonToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "let names: string[] = [\"Alice\", \"Bob\", \"Charlie\"];"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(LetKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("names")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringKeyword),
        Ok(OpenBracketToken),
        Ok(CloseBracketToken),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBracketToken),
        Ok(StringLiteralToken("\"Alice\"")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringLiteralToken("\"Bob\"")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringLiteralToken("\"Charlie\"")),
        Ok(CloseBracketToken),
        Ok(SemicolonToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "enum Color { Red, Green, Blue }"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(EnumKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Color")),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Red")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Green")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Blue")),
        Ok(WhitespaceTrivia(1)),
        Ok(CloseBraceToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "enum Status { Active = 1, Inactive = 0 }"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(EnumKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Status")),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Active")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("1")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Inactive")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberLiteralToken("0")),
        Ok(WhitespaceTrivia(1)),
        Ok(CloseBraceToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "interface Person {\n  name: string;\n  age: number;\n}"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(InterfaceKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("Person")),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(NewLineTrivia(1)),
        Ok(WhitespaceTrivia(2)),
        Ok(IdentifierToken("name")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringKeyword),
        Ok(SemicolonToken),
        Ok(NewLineTrivia(1)),
        Ok(WhitespaceTrivia(2)),
        Ok(IdentifierToken("age")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberKeyword),
        Ok(SemicolonToken),
        Ok(NewLineTrivia(1)),
        Ok(CloseBraceToken),
        Ok(EndOfFileToken),
    ]

# This makes sure that identifiers are not tokenized as keywords when they begin with keyword characters.
expect
    ts_string = "any_ as_ async_ await_ boolean_ break_ case_ catch_ class_ const_ constructor_ continue_ debugger_ declare_ default_ delete_ do_ else_ enum_ export_ extends_ false_ finally_ for_ function_ get_ if_ implements_ import_ in_ instanceof_ interface_ let_ module_ new_ null_ number_ of_ package_ private_ protected_ public_ require_ return_ set_ static_ string_ super_ switch_ symbol_ this_ throw_ true_ try_ type_ typeof_ var_ void_ while_ with_ yield_"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(IdentifierToken("any_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("as_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("async_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("await_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("boolean_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("break_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("case_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("catch_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("class_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("const_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("constructor_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("continue_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("debugger_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("declare_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("default_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("delete_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("do_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("else_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("enum_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("export_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("extends_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("false_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("finally_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("for_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("function_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("get_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("if_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("implements_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("import_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("in_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("instanceof_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("interface_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("let_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("module_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("new_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("null_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("number_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("of_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("package_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("private_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("protected_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("public_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("require_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("return_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("set_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("static_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("string_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("super_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("switch_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("symbol_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("this_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("throw_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("true_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("try_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("type_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("typeof_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("var_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("void_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("while_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("with_")),
        Ok(WhitespaceTrivia(1)),
        Ok(IdentifierToken("yield_")),
        Ok(EndOfFileToken),
    ]

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
    token_list == [Err(Unknown), Ok(EndOfFileToken)]

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
        Ok(Identifier("x")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("1")),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "var x = 23;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(VarKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("x")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
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
        Ok(WhitespaceTrivia(1)),
        Ok(OpenParenToken),
        Ok(LetKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("i")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("0")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("i")),
        Ok(WhitespaceTrivia(1)),
        Ok(LessThanToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("10")),
        Ok(SemicolonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("i")),
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
        Ok(Identifier("x")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("100")),
        Ok(WhitespaceTrivia(1)),
        Ok(PlusToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("y")),
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
        Ok(NumericLiteral("42")),
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
        Ok(Identifier("A")),
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
        Ok(Identifier("x")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("42")),
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
        Ok(Identifier("message")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringLiteral("\"Hello, World!\"")),
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
        Ok(Identifier("pi")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberKeyword),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("3.14159")),
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
        Ok(Identifier("names")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringKeyword),
        Ok(OpenBracketToken),
        Ok(CloseBracketToken),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBracketToken),
        Ok(StringLiteral("\"Alice\"")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringLiteral("\"Bob\"")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringLiteral("\"Charlie\"")),
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
        Ok(Identifier("Color")),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("Red")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("Green")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("Blue")),
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
        Ok(Identifier("Status")),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("Active")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("1")),
        Ok(CommaToken),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("Inactive")),
        Ok(WhitespaceTrivia(1)),
        Ok(EqualsToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumericLiteral("0")),
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
        Ok(Identifier("Person")),
        Ok(WhitespaceTrivia(1)),
        Ok(OpenBraceToken),
        Ok(NewLineTrivia(1)),
        Ok(WhitespaceTrivia(2)),
        Ok(Identifier("name")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(StringKeyword),
        Ok(SemicolonToken),
        Ok(NewLineTrivia(1)),
        Ok(WhitespaceTrivia(2)),
        Ok(Identifier("age")),
        Ok(ColonToken),
        Ok(WhitespaceTrivia(1)),
        Ok(NumberKeyword),
        Ok(SemicolonToken),
        Ok(NewLineTrivia(1)),
        Ok(CloseBraceToken),
        Ok(EndOfFileToken),
    ]

expect
    ts_string = "any_ as_ async_ await_ boolean_ break_ case_ catch_ class_ const_ constructor_ continue_ debugger_ declare_ default_ delete_ do_ else_ enum_ export_ extends_ false_ finally_ for_ function_ get_ if_ implements_ import_ in_ instanceof_ interface_ let_ module_ new_ null_ number_ of_ package_ private_ protected_ public_ require_ return_ set_ static_ string_ super_ switch_ symbol_ this_ throw_ true_ try_ type_ typeof_ var_ void_ while_ with_ yield_"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        Ok(Identifier("any_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("as_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("async_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("await_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("boolean_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("break_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("case_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("catch_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("class_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("const_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("constructor_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("continue_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("debugger_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("declare_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("default_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("delete_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("do_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("else_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("enum_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("export_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("extends_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("false_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("finally_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("for_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("function_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("get_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("if_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("implements_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("import_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("in_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("instanceof_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("interface_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("let_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("module_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("new_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("null_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("number_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("of_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("package_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("private_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("protected_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("public_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("require_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("return_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("set_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("static_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("string_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("super_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("switch_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("symbol_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("this_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("throw_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("true_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("try_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("type_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("typeof_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("var_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("void_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("while_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("with_")),
        Ok(WhitespaceTrivia(1)),
        Ok(Identifier("yield_")),
        Ok(EndOfFileToken),
    ]

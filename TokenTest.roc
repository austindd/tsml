module []

import Token exposing [tokenize_str]

expect
    ts_string = ""
    token_list = tokenize_str(ts_string)
    token_list == [EndOfFileToken]

expect
    ts_string = "\n"
    token_list = tokenize_str(ts_string)
    token_list == [NewLineTrivia(1), EndOfFileToken]

expect
    ts_string = "\r\n"
    token_list = tokenize_str(ts_string)
    token_list == [NewLineTrivia(1), EndOfFileToken]

expect
    ts_string = "\r"
    token_list = tokenize_str(ts_string)
    token_list == [UnknownToken([]), EndOfFileToken]

expect
    ts_string = "const"
    token_list = tokenize_str(ts_string)
    token_list == [ConstKeyword, EndOfFileToken]

expect
    ts_string = "const "
    token_list = tokenize_str(ts_string)
    token_list == [ConstKeyword, WhitespaceTrivia(1), EndOfFileToken]

expect
    ts_string = "const\n"
    token_list = tokenize_str(ts_string)
    token_list == [ConstKeyword, NewLineTrivia(1), EndOfFileToken]

expect
    ts_string = "const\r\n"
    token_list = tokenize_str(ts_string)
    token_list == [ConstKeyword, NewLineTrivia(1), EndOfFileToken]

expect
    ts_string = "let x = 1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        LetKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("x"),
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("1"),
        EndOfFileToken,
    ]

expect
    ts_string = "var x = 23;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        VarKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("x"),
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("23"),
        SemicolonToken,
        EndOfFileToken,
    ]

expect
    ts_string = "-1.890e-1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        MinusToken,
        NumberLiteralToken("1.890e-1"),
        EndOfFileToken,
    ]

expect
    ts_string = "18.90e21"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        NumberLiteralToken("18.90e21"),
        EndOfFileToken,
    ]

# TODO: Support hex literals
# expect
#     ts_string = "0x1A"
#     token_list = tokenize_str(ts_string)
#     token_list
#     == [
#         NumberLiteralToken("0x1A"),
#         EndOfFileToken,
#     ]

expect
    ts_string = "1_000_123"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        NumberLiteralToken("1_000_123"),
        EndOfFileToken,
    ]

expect
    ts_string = "1_000_123.456_789"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        NumberLiteralToken("1_000_123.456_789"),
        EndOfFileToken,
    ]

expect
    ts_string = "1_000_123.456_789e-1"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        NumberLiteralToken("1_000_123.456_789e-1"),
        EndOfFileToken,
    ]

expect
    ts_string = "\"Hello, world!\""
    token_list = tokenize_str(ts_string)
    token_list
    == [
        StringLiteralToken("\"Hello, world!\""),
        EndOfFileToken,
    ]

expect
    ts_string = "'Hello, world!'"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        StringLiteralToken("'Hello, world!'"),
        EndOfFileToken,
    ]

expect
    ts_string = "\"Hello, world!\\n\""
    token_list = tokenize_str(ts_string)
    token_list
    == [
        StringLiteralToken("\"Hello, world!\\n\""),
        EndOfFileToken,
    ]

expect
    ts_string = "for (let i = 0; i < 10; i++) {}"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        ForKeyword,
        WhitespaceTrivia(1),
        OpenParenToken,
        LetKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("i"),
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("0"),
        SemicolonToken,
        WhitespaceTrivia(1),
        IdentifierToken("i"),
        WhitespaceTrivia(1),
        LessThanToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("10"),
        SemicolonToken,
        WhitespaceTrivia(1),
        IdentifierToken("i"),
        PlusPlusToken,
        CloseParenToken,
        WhitespaceTrivia(1),
        OpenBraceToken,
        CloseBraceToken,
        EndOfFileToken,
    ]

expect
    ts_string = "const x = 100 + y + (function() { return 42; })()"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        ConstKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("x"),
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("100"),
        WhitespaceTrivia(1),
        PlusToken,
        WhitespaceTrivia(1),
        IdentifierToken("y"),
        WhitespaceTrivia(1),
        PlusToken,
        WhitespaceTrivia(1),
        OpenParenToken,
        FunctionKeyword,
        OpenParenToken,
        CloseParenToken,
        WhitespaceTrivia(1),
        OpenBraceToken,
        WhitespaceTrivia(1),
        ReturnKeyword,
        WhitespaceTrivia(1),
        NumberLiteralToken("42"),
        SemicolonToken,
        WhitespaceTrivia(1),
        CloseBraceToken,
        CloseParenToken,
        OpenParenToken,
        CloseParenToken,
        EndOfFileToken,
    ]

expect
    ts_string = "interface A {}"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        InterfaceKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("A"),
        WhitespaceTrivia(1),
        OpenBraceToken,
        CloseBraceToken,
        EndOfFileToken,
    ]

expect
    ts_string = "const x: number = 42;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        ConstKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("x"),
        ColonToken,
        WhitespaceTrivia(1),
        NumberKeyword,
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("42"),
        SemicolonToken,
        EndOfFileToken,
    ]

expect
    ts_string = "let message: string = \"Hello, World!\";"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        LetKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("message"),
        ColonToken,
        WhitespaceTrivia(1),
        StringKeyword,
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        StringLiteralToken("\"Hello, World!\""),
        SemicolonToken,
        EndOfFileToken,
    ]

expect
    ts_string = "const pi: number = 3.14159;"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        ConstKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("pi"),
        ColonToken,
        WhitespaceTrivia(1),
        NumberKeyword,
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("3.14159"),
        SemicolonToken,
        EndOfFileToken,
    ]

expect
    ts_string = "let names: string[] = [\"Alice\", \"Bob\", \"Charlie\"];"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        LetKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("names"),
        ColonToken,
        WhitespaceTrivia(1),
        StringKeyword,
        OpenBracketToken,
        CloseBracketToken,
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        OpenBracketToken,
        StringLiteralToken("\"Alice\""),
        CommaToken,
        WhitespaceTrivia(1),
        StringLiteralToken("\"Bob\""),
        CommaToken,
        WhitespaceTrivia(1),
        StringLiteralToken("\"Charlie\""),
        CloseBracketToken,
        SemicolonToken,
        EndOfFileToken,
    ]

expect
    ts_string = "enum Color { Red, Green, Blue }"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        EnumKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("Color"),
        WhitespaceTrivia(1),
        OpenBraceToken,
        WhitespaceTrivia(1),
        IdentifierToken("Red"),
        CommaToken,
        WhitespaceTrivia(1),
        IdentifierToken("Green"),
        CommaToken,
        WhitespaceTrivia(1),
        IdentifierToken("Blue"),
        WhitespaceTrivia(1),
        CloseBraceToken,
        EndOfFileToken,
    ]

expect
    ts_string = "enum Status { Active = 1, Inactive = 0 }"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        EnumKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("Status"),
        WhitespaceTrivia(1),
        OpenBraceToken,
        WhitespaceTrivia(1),
        IdentifierToken("Active"),
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("1"),
        CommaToken,
        WhitespaceTrivia(1),
        IdentifierToken("Inactive"),
        WhitespaceTrivia(1),
        EqualsToken,
        WhitespaceTrivia(1),
        NumberLiteralToken("0"),
        WhitespaceTrivia(1),
        CloseBraceToken,
        EndOfFileToken,
    ]

expect
    ts_string = "interface Person {\n  name: string;\n  age: number;\n}"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        InterfaceKeyword,
        WhitespaceTrivia(1),
        IdentifierToken("Person"),
        WhitespaceTrivia(1),
        OpenBraceToken,
        NewLineTrivia(1),
        WhitespaceTrivia(2),
        IdentifierToken("name"),
        ColonToken,
        WhitespaceTrivia(1),
        StringKeyword,
        SemicolonToken,
        NewLineTrivia(1),
        WhitespaceTrivia(2),
        IdentifierToken("age"),
        ColonToken,
        WhitespaceTrivia(1),
        NumberKeyword,
        SemicolonToken,
        NewLineTrivia(1),
        CloseBraceToken,
        EndOfFileToken,
    ]

# This makes sure that identifiers are not tokenized as keywords when they begin with keyword characters.
expect
    ts_string = "any_ as_ async_ await_ boolean_ break_ case_ catch_ class_ const_ constructor_ continue_ debugger_ declare_ default_ delete_ do_ else_ enum_ export_ extends_ false_ finally_ for_ function_ get_ if_ implements_ import_ in_ instanceof_ interface_ let_ module_ new_ null_ number_ of_ package_ private_ protected_ public_ require_ return_ set_ static_ string_ super_ switch_ symbol_ this_ throw_ true_ try_ type_ typeof_ var_ void_ while_ with_ yield_"
    token_list = tokenize_str(ts_string)
    token_list
    == [
        IdentifierToken("any_"),
        WhitespaceTrivia(1),
        IdentifierToken("as_"),
        WhitespaceTrivia(1),
        IdentifierToken("async_"),
        WhitespaceTrivia(1),
        IdentifierToken("await_"),
        WhitespaceTrivia(1),
        IdentifierToken("boolean_"),
        WhitespaceTrivia(1),
        IdentifierToken("break_"),
        WhitespaceTrivia(1),
        IdentifierToken("case_"),
        WhitespaceTrivia(1),
        IdentifierToken("catch_"),
        WhitespaceTrivia(1),
        IdentifierToken("class_"),
        WhitespaceTrivia(1),
        IdentifierToken("const_"),
        WhitespaceTrivia(1),
        IdentifierToken("constructor_"),
        WhitespaceTrivia(1),
        IdentifierToken("continue_"),
        WhitespaceTrivia(1),
        IdentifierToken("debugger_"),
        WhitespaceTrivia(1),
        IdentifierToken("declare_"),
        WhitespaceTrivia(1),
        IdentifierToken("default_"),
        WhitespaceTrivia(1),
        IdentifierToken("delete_"),
        WhitespaceTrivia(1),
        IdentifierToken("do_"),
        WhitespaceTrivia(1),
        IdentifierToken("else_"),
        WhitespaceTrivia(1),
        IdentifierToken("enum_"),
        WhitespaceTrivia(1),
        IdentifierToken("export_"),
        WhitespaceTrivia(1),
        IdentifierToken("extends_"),
        WhitespaceTrivia(1),
        IdentifierToken("false_"),
        WhitespaceTrivia(1),
        IdentifierToken("finally_"),
        WhitespaceTrivia(1),
        IdentifierToken("for_"),
        WhitespaceTrivia(1),
        IdentifierToken("function_"),
        WhitespaceTrivia(1),
        IdentifierToken("get_"),
        WhitespaceTrivia(1),
        IdentifierToken("if_"),
        WhitespaceTrivia(1),
        IdentifierToken("implements_"),
        WhitespaceTrivia(1),
        IdentifierToken("import_"),
        WhitespaceTrivia(1),
        IdentifierToken("in_"),
        WhitespaceTrivia(1),
        IdentifierToken("instanceof_"),
        WhitespaceTrivia(1),
        IdentifierToken("interface_"),
        WhitespaceTrivia(1),
        IdentifierToken("let_"),
        WhitespaceTrivia(1),
        IdentifierToken("module_"),
        WhitespaceTrivia(1),
        IdentifierToken("new_"),
        WhitespaceTrivia(1),
        IdentifierToken("null_"),
        WhitespaceTrivia(1),
        IdentifierToken("number_"),
        WhitespaceTrivia(1),
        IdentifierToken("of_"),
        WhitespaceTrivia(1),
        IdentifierToken("package_"),
        WhitespaceTrivia(1),
        IdentifierToken("private_"),
        WhitespaceTrivia(1),
        IdentifierToken("protected_"),
        WhitespaceTrivia(1),
        IdentifierToken("public_"),
        WhitespaceTrivia(1),
        IdentifierToken("require_"),
        WhitespaceTrivia(1),
        IdentifierToken("return_"),
        WhitespaceTrivia(1),
        IdentifierToken("set_"),
        WhitespaceTrivia(1),
        IdentifierToken("static_"),
        WhitespaceTrivia(1),
        IdentifierToken("string_"),
        WhitespaceTrivia(1),
        IdentifierToken("super_"),
        WhitespaceTrivia(1),
        IdentifierToken("switch_"),
        WhitespaceTrivia(1),
        IdentifierToken("symbol_"),
        WhitespaceTrivia(1),
        IdentifierToken("this_"),
        WhitespaceTrivia(1),
        IdentifierToken("throw_"),
        WhitespaceTrivia(1),
        IdentifierToken("true_"),
        WhitespaceTrivia(1),
        IdentifierToken("try_"),
        WhitespaceTrivia(1),
        IdentifierToken("type_"),
        WhitespaceTrivia(1),
        IdentifierToken("typeof_"),
        WhitespaceTrivia(1),
        IdentifierToken("var_"),
        WhitespaceTrivia(1),
        IdentifierToken("void_"),
        WhitespaceTrivia(1),
        IdentifierToken("while_"),
        WhitespaceTrivia(1),
        IdentifierToken("with_"),
        WhitespaceTrivia(1),
        IdentifierToken("yield_"),
        EndOfFileToken,
    ]

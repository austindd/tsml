module [
    ts_token_debug_display,
    tokenize_str,
    tokenize_utf8_bytes,
    Token,
]

Token : [
    TokenError TokenErrorKind,
    UnknownToken (List U8),
    EndOfFileToken,
    LineCommentStart,
    BlockCommentStart,
    BlockCommentEnd,
    CommentText Str,
    # SingleLineCommentTrivia,
    # MultiLineCommentTrivia,
    NewLineTrivia U64,
    WhitespaceTrivia U64,
    ShebangTrivia,
    ConflictMarkerTrivia,
    NonTextFileMarkerTrivia,
    # Literals
    NumberLiteralToken Str,
    BigIntLiteralToken Str,
    StringLiteralToken Str,
    JsxText Str,
    JsxTextAllWhiteSpaces Str,
    RegularExpressionLiteralToken Str,
    NoSubstitutionTemplateLiteralToken Str,
    # Template literal parts
    TemplateHead Str,
    TemplateMiddle Str,
    TemplateTail Str,
    # Template literal interpolation
    DollarBraceToken, # ${
    # Punctuation
    OpenBraceToken,
    CloseBraceToken,
    OpenParenToken,
    CloseParenToken,
    OpenBracketToken,
    CloseBracketToken,
    DotToken,
    DotDotDotToken,
    SemicolonToken,
    CommaToken,
    QuestionDotToken,
    LessThanToken,
    LessThanSlashToken,
    GreaterThanToken,
    LessThanEqualsToken,
    GreaterThanEqualsToken,
    EqualsEqualsToken,
    ExclamationEqualsToken,
    EqualsEqualsEqualsToken,
    ExclamationEqualsEqualsToken,
    EqualsGreaterThanToken,
    PlusToken,
    MinusToken,
    AsteriskToken,
    AsteriskAsteriskToken,
    SlashToken,
    PercentToken,
    PlusPlusToken,
    MinusMinusToken,
    LessThanLessThanToken,
    GreaterThanGreaterThanToken,
    GreaterThanGreaterThanGreaterThanToken,
    AmpersandToken,
    BarToken,
    CaretToken,
    ExclamationToken,
    TildeToken,
    AmpersandAmpersandToken,
    BarBarToken,
    QuestionToken,
    ColonToken,
    AtToken,
    QuestionQuestionToken,
    #
    # /** Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteralToken and related kinds. */
    BacktickToken,
    # /** Only the JSDoc scanner produces HashToken. The normal scanner produces PrivateIdentifierToken. */
    HashToken,
    #
    # Assignments
    EqualsToken,
    PlusEqualsToken,
    MinusEqualsToken,
    AsteriskEqualsToken,
    AsteriskAsteriskEqualsToken,
    SlashEqualsToken,
    PercentEqualsToken,
    LessThanLessThanEqualsToken,
    GreaterThanGreaterThanEqualsToken,
    GreaterThanGreaterThanGreaterThanEqualsToken,
    AmpersandEqualsToken,
    BarEqualsToken,
    BarBarEqualsToken,
    AmpersandAmpersandEqualsToken,
    QuestionQuestionEqualsToken,
    CaretEqualsToken,
    # IdentifierTokens and PrivateIdentifierTokens
    IdentifierToken Str,
    PrivateIdentifierToken Str,
    #
    # /**
    # * Only the special JSDoc comment text scanner produces JSDocCommentTextTokes. One of these tokens spans all text after a tag comment's start and before the next @
    # * @internal
    # */
    # JSDocCommentTextToken,
    #
    # Reserved words
    BreakKeyword,
    CaseKeyword,
    CatchKeyword,
    ClassKeyword,
    ConstKeyword,
    ContinueKeyword,
    DebuggerKeyword,
    DefaultKeyword,
    DeleteKeyword,
    DoKeyword,
    ElseKeyword,
    EnumKeyword,
    ExportKeyword,
    ExtendsKeyword,
    FalseKeyword,
    FinallyKeyword,
    ForKeyword,
    FunctionKeyword,
    IfKeyword,
    ImportKeyword,
    InKeyword,
    InstanceofKeyword,
    NewKeyword,
    NullKeyword,
    ReturnKeyword,
    SuperKeyword,
    SwitchKeyword,
    ThisKeyword,
    ThrowKeyword,
    TrueKeyword,
    TryKeyword,
    TypeofKeyword,
    KeyofKeyword,
    VarKeyword,
    VoidKeyword,
    WhileKeyword,
    WithKeyword,
    # Strict mode reserved words
    ImplementsKeyword,
    InterfaceKeyword,
    LetKeyword,
    PackageKeyword,
    PrivateKeyword,
    ProtectedKeyword,
    PublicKeyword,
    StaticKeyword,
    YieldKeyword,
    # Contextual keywords
    AbstractKeyword,
    AccessorKeyword,
    AsKeyword,
    AssertsKeyword,
    AssertKeyword,
    AnyKeyword,
    AsyncKeyword,
    AwaitKeyword,
    BooleanKeyword,
    ConstructorKeyword,
    DeclareKeyword,
    GetKeyword,
    InferKeyword,
    IntrinsicKeyword,
    IsKeyword,
    KeyOfKeyword,
    ModuleKeyword,
    NamespaceKeyword,
    NeverKeyword,
    OutKeyword,
    ReadonlyKeyword,
    RequireKeyword,
    NumberKeyword,
    ObjectKeyword,
    SatisfiesKeyword,
    SetKeyword,
    StringKeyword,
    SymbolKeyword,
    TypeKeyword,
    UndefinedKeyword,
    UniqueKeyword,
    UnknownKeyword,
    UsingKeyword,
    FromKeyword,
    GlobalKeyword,
    BigIntKeyword,
    OverrideKeyword,
    OfKeyword, # LastKeyword and LastToken and LastContextualKeyword
]

TokenErrorKind : [
    UnknownToken(List U8),
    UnclosedString,
    UnclosedTemplate,
    UnclosedInterpolation,
    InvalidNumberSeparator,
]

ts_token_debug_display : Token -> Str
ts_token_debug_display = |token|
    when token is
        TokenError(kind) -> "TokenError(${Inspect.to_str(kind)})"
        UnknownToken(u8s) -> "UnknownToken(${Str.from_utf8_lossy(u8s)})"
        EndOfFileToken -> "EndOfFileToken"
        LineCommentStart -> "LineCommentStart"
        BlockCommentStart -> "BlockCommentStart"
        BlockCommentEnd -> "BlockCommentBlockEnd"
        CommentText(str) -> "CommentText(${str})"
        # SingleLineCommentTrivia -> "SingleLineCommentTrivia"
        # MultiLineCommentTrivia -> "MultiLineCommentTrivia"
        NewLineTrivia(count) -> "NewLineTrivia (${Num.to_str(count)})"
        WhitespaceTrivia(count) -> "WhitespaceTrivia(${Num.to_str(count)})"
        ShebangTrivia -> "ShebangTrivia"
        ConflictMarkerTrivia -> "ConflictMarkerTrivia"
        NonTextFileMarkerTrivia -> "NonTextFileMarkerTrivia"
        # Literals
        NumberLiteralToken(str) -> "NumberLiteralToken(${str})"
        BigIntLiteralToken(str) -> "BigIntLiteralToken(${str})"
        StringLiteralToken(str) -> "StringLiteralToken(${str})"
        JsxText(str) -> "JsxText(${str})"
        JsxTextAllWhiteSpaces(str) -> "JsxTextAllWhiteSpaces(${str})"
        RegularExpressionLiteralToken(str) -> "RegularExpressionLiteralToken(${str})"
        NoSubstitutionTemplateLiteralToken(str) -> "NoSubstitutionTemplateLiteralToken(${str})"
        TemplateHead(str) -> "TemplateHead(${str})"
        TemplateMiddle(str) -> "TemplateMiddle(${str})"
        TemplateTail(str) -> "TemplateTail(${str})"
        DollarBraceToken -> "DollarBraceToken"
        # Punctuation
        OpenBraceToken -> "OpenBraceToken"
        CloseBraceToken -> "CloseBraceToken"
        OpenParenToken -> "OpenParenToken"
        CloseParenToken -> "CloseParenToken"
        OpenBracketToken -> "OpenBracketToken"
        CloseBracketToken -> "CloseBracketToken"
        DotToken -> "DotToken"
        DotDotDotToken -> "DotDotDotToken"
        SemicolonToken -> "SemicolonToken"
        CommaToken -> "CommaToken"
        QuestionDotToken -> "QuestionDotToken"
        LessThanToken -> "LessThanToken"
        LessThanSlashToken -> "LessThanSlashToken"
        GreaterThanToken -> "GreaterThanToken"
        LessThanEqualsToken -> "LessThanEqualsToken"
        GreaterThanEqualsToken -> "GreaterThanEqualsToken"
        EqualsEqualsToken -> "EqualsEqualsToken"
        ExclamationEqualsToken -> "ExclamationEqualsToken"
        EqualsEqualsEqualsToken -> "EqualsEqualsEqualsToken"
        ExclamationEqualsEqualsToken -> "ExclamationEqualsEqualsToken"
        EqualsGreaterThanToken -> "EqualsGreaterThanToken"
        PlusToken -> "PlusToken"
        MinusToken -> "MinusToken"
        AsteriskToken -> "AsteriskToken"
        AsteriskAsteriskToken -> "AsteriskAsteriskToken"
        SlashToken -> "SlashToken"
        PercentToken -> "PercentToken"
        PlusPlusToken -> "PlusPlusToken"
        MinusMinusToken -> "MinusMinusToken"
        LessThanLessThanToken -> "LessThanLessThanToken"
        GreaterThanGreaterThanToken -> "GreaterThanGreaterThanToken"
        GreaterThanGreaterThanGreaterThanToken -> "GreaterThanGreaterThanGreaterThanToken"
        AmpersandToken -> "AmpersandToken"
        BarToken -> "BarToken"
        CaretToken -> "CaretToken"
        ExclamationToken -> "ExclamationToken"
        TildeToken -> "TildeToken"
        AmpersandAmpersandToken -> "AmpersandAmpersandToken"
        BarBarToken -> "BarBarToken"
        QuestionToken -> "QuestionToken"
        ColonToken -> "ColonToken"
        AtToken -> "AtToken"
        QuestionQuestionToken -> "QuestionQuestionToken"
        #
        # /** Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteralToken and related kinds. */
        BacktickToken -> "BacktickToken"
        # /** Only the JSDoc scanner produces HashToken. The normal scanner produces PrivateIdentifierToken. */
        HashToken -> "HashToken"
        #
        # Assignments
        EqualsToken -> "EqualsToken"
        PlusEqualsToken -> "PlusEqualsToken"
        MinusEqualsToken -> "MinusEqualsToken"
        AsteriskEqualsToken -> "AsteriskEqualsToken"
        AsteriskAsteriskEqualsToken -> "AsteriskAsteriskEqualsToken"
        SlashEqualsToken -> "SlashEqualsToken"
        PercentEqualsToken -> "PercentEqualsToken"
        LessThanLessThanEqualsToken -> "LessThanLessThanEqualsToken"
        GreaterThanGreaterThanEqualsToken -> "GreaterThanGreaterThanEqualsToken"
        GreaterThanGreaterThanGreaterThanEqualsToken -> "GreaterThanGreaterThanGreaterThanEqualsToken"
        AmpersandEqualsToken -> "AmpersandEqualsToken"
        BarEqualsToken -> "BarEqualsToken"
        BarBarEqualsToken -> "BarBarEqualsToken"
        AmpersandAmpersandEqualsToken -> "AmpersandAmpersandEqualsToken"
        QuestionQuestionEqualsToken -> "QuestionQuestionEqualsToken"
        CaretEqualsToken -> "CaretEqualsToken"
        # IdentifierTokens and PrivateIdentifierTokens
        IdentifierToken(str) -> "IdentifierToken(${str})"
        PrivateIdentifierToken(str) -> "PrivateIdentifierToken(${str})"
        #
        # /**
        # * Only the special JSDoc comment text scanner produces JSDocCommentTextTokes. One of these tokens spans all text after a tag comment's start and before the next @
        # * @internal
        # */
        # JSDocCommentTextToken ->
        #
        # Reserved words
        BreakKeyword -> "BreakKeyword"
        CaseKeyword -> "CaseKeyword"
        CatchKeyword -> "CatchKeyword"
        ClassKeyword -> "ClassKeyword"
        ConstKeyword -> "ConstKeyword"
        ContinueKeyword -> "ContinueKeyword"
        DebuggerKeyword -> "DebuggerKeyword"
        DefaultKeyword -> "DefaultKeyword"
        DeleteKeyword -> "DeleteKeyword"
        DoKeyword -> "DoKeyword"
        ElseKeyword -> "ElseKeyword"
        EnumKeyword -> "EnumKeyword"
        ExportKeyword -> "ExportKeyword"
        ExtendsKeyword -> "ExtendsKeyword"
        FalseKeyword -> "FalseKeyword"
        FinallyKeyword -> "FinallyKeyword"
        ForKeyword -> "ForKeyword"
        FunctionKeyword -> "FunctionKeyword"
        IfKeyword -> "IfKeyword"
        ImportKeyword -> "ImportKeyword"
        InKeyword -> "InKeyword"
        InstanceofKeyword -> "InstanceofKeyword"
        NewKeyword -> "NewKeyword"
        NullKeyword -> "NullKeyword"
        ReturnKeyword -> "ReturnKeyword"
        SuperKeyword -> "SuperKeyword"
        SwitchKeyword -> "SwitchKeyword"
        ThisKeyword -> "ThisKeyword"
        ThrowKeyword -> "ThrowKeyword"
        TrueKeyword -> "TrueKeyword"
        TryKeyword -> "TryKeyword"
        TypeofKeyword -> "TypeofKeyword"
        KeyofKeyword -> "KeyofKeyword"
        VarKeyword -> "VarKeyword"
        VoidKeyword -> "VoidKeyword"
        WhileKeyword -> "WhileKeyword"
        WithKeyword -> "WithKeyword"
        # Strict mode reserved words
        ImplementsKeyword -> "ImplementsKeyword"
        InterfaceKeyword -> "InterfaceKeyword"
        LetKeyword -> "LetKeyword"
        PackageKeyword -> "PackageKeyword"
        PrivateKeyword -> "PrivateKeyword"
        ProtectedKeyword -> "ProtectedKeyword"
        PublicKeyword -> "PublicKeyword"
        StaticKeyword -> "StaticKeyword"
        YieldKeyword -> "YieldKeyword"
        # Contextual keywords
        AbstractKeyword -> "AbstractKeyword"
        AccessorKeyword -> "AccessorKeyword"
        AsKeyword -> "AsKeyword"
        AssertsKeyword -> "AssertsKeyword"
        AssertKeyword -> "AssertKeyword"
        AnyKeyword -> "AnyKeyword"
        AsyncKeyword -> "AsyncKeyword"
        AwaitKeyword -> "AwaitKeyword"
        BooleanKeyword -> "BooleanKeyword"
        ConstructorKeyword -> "ConstructorKeyword"
        DeclareKeyword -> "DeclareKeyword"
        GetKeyword -> "GetKeyword"
        InferKeyword -> "InferKeyword"
        IntrinsicKeyword -> "IntrinsicKeyword"
        IsKeyword -> "IsKeyword"
        KeyOfKeyword -> "KeyOfKeyword"
        ModuleKeyword -> "ModuleKeyword"
        NamespaceKeyword -> "NamespaceKeyword"
        NeverKeyword -> "NeverKeyword"
        OutKeyword -> "OutKeyword"
        ReadonlyKeyword -> "ReadonlyKeyword"
        RequireKeyword -> "RequireKeyword"
        NumberKeyword -> "NumberKeyword"
        ObjectKeyword -> "ObjectKeyword"
        SatisfiesKeyword -> "SatisfiesKeyword"
        SetKeyword -> "SetKeyword"
        StringKeyword -> "StringKeyword"
        SymbolKeyword -> "SymbolKeyword"
        TypeKeyword -> "TypeKeyword"
        UndefinedKeyword -> "UndefinedKeyword"
        UniqueKeyword -> "UniqueKeyword"
        UnknownKeyword -> "UnknownKeyword"
        UsingKeyword -> "UsingKeyword"
        FromKeyword -> "FromKeyword"
        GlobalKeyword -> "GlobalKeyword"
        BigIntKeyword -> "BigIntKeyword"
        OverrideKeyword -> "OverrideKeyword"
        OfKeyword -> "OfKeyword"

tokenize_str : Str -> List Token
tokenize_str = |str|
    str
    |> Str.to_utf8
    |> utf8_list_to_ts_token_list

tokenize_utf8_bytes : List U8 -> List Token
tokenize_utf8_bytes = |u8_list|
    u8_list
    |> utf8_list_to_ts_token_list

utf8_list_to_ts_token_list : List U8 -> List Token
utf8_list_to_ts_token_list = |u8_list_|
    # Helper functions

    # Helper processing functions with tail recursion

    # Start the tokenization
    utf8_list_to_ts_token_list_inner(u8_list_, [])

handle_possible_keyword : Token, List U8, List U8 -> (Token, List U8)
handle_possible_keyword = |keyword_token, current_bytes, trailing_bytes|
    consume_identifier = |current, trailing|
        when trailing is
            [next, .. as rest] ->
                if is_identifier_part(next) then
                    consume_identifier(
                        List.append(current, next),
                        rest,
                    )
                else
                    (IdentifierToken(Str.from_utf8_lossy(current)), trailing)

            [] -> (IdentifierToken(Str.from_utf8_lossy(current)), trailing)
    when trailing_bytes is
        [next, .. as rest] ->
            if is_identifier_part(next) then
                consume_identifier(
                    List.append(
                        current_bytes,
                        next,
                    ),
                    rest,
                )
            else
                (keyword_token, trailing_bytes)

        [] -> (keyword_token, trailing_bytes)

# Main recursive tokenizer function with accumulator
utf8_list_to_ts_token_list_inner : List U8, List Token -> List Token
utf8_list_to_ts_token_list_inner = |u8_list, token_list|
    when u8_list is
        [] -> List.append(token_list, EndOfFileToken)
        [099, 111, 110, 115, 116, 114, 117, 099, 116, 111, 114, .. as u8s] -> # constructor
            (token, rest) = handle_possible_keyword(
                ConstructorKeyword,
                [099, 111, 110, 115, 116, 114, 117, 099, 116, 111, 114],
                u8s,
            )
            utf8_list_to_ts_token_list_inner(
                rest,
                List.append(token_list, token),
            )

        [097, 115, 121, 110, 099, .. as u8s] -> # async
            (token, rest) = handle_possible_keyword(
                AsyncKeyword,
                [097, 115, 121, 110, 099],
                u8s,
            )
            utf8_list_to_ts_token_list_inner(
                rest,
                List.append(token_list, token),
            )

        [47, 47, .. as rest] -> # Line comment (//)
            { token, remaining_u8s } = process_line_comment_text(rest)
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                token_list
                |> List.append(LineCommentStart)
                |> List.append(token),
            )

        [47, 42, .. as rest] -> # Block comment start (/*)
            { token, remaining_u8s } = process_block_comment_text(rest)
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                token_list
                |> List.append(BlockCommentStart)
                |> List.append(token)
                |> List.append(BlockCommentEnd),
            )

        [35, 33, .. as rest] -> # Shebang #!
            # Consume until newline or EOF
            consume_shebang = |bytes|
                when bytes is
                    [10, .. as rest_2] -> rest_2 # Stop at \n
                    [13, 10, .. as rest_2] -> rest_2 # Stop at \r\n
                    [_, .. as rest_2] -> consume_shebang(rest_2)
                    [] -> []
            rest_after_shebang = consume_shebang(rest)
            utf8_list_to_ts_token_list_inner(
                rest_after_shebang,
                token_list |> List.append(ShebangTrivia),
            )

        [105, 110, 116, 101, 114, 102, 097, 099, 101, .. as u8s] -> # interface
            (token, rest) = handle_possible_keyword(InterfaceKeyword, [105, 110, 116, 101, 114, 102, 097, 099, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [098, 114, 101, 097, 107, .. as u8s] -> # break
            (token, rest) = handle_possible_keyword(BreakKeyword, [098, 114, 101, 097, 107], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [099, 097, 115, 101, .. as u8s] -> # case
            (token, rest) = handle_possible_keyword(CaseKeyword, [099, 097, 115, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [099, 097, 116, 099, 104, .. as u8s] -> # catch
            (token, rest) = handle_possible_keyword(CatchKeyword, [099, 097, 116, 099, 104], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [099, 108, 097, 115, 115, .. as u8s] -> # class
            (token, rest) = handle_possible_keyword(ClassKeyword, [099, 108, 097, 115, 115], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [099, 111, 110, 115, 116, .. as u8s] -> # const
            (token, rest) = handle_possible_keyword(ConstKeyword, [099, 111, 110, 115, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [099, 111, 110, 116, 105, 110, 117, 101, .. as u8s] -> # continue
            (token, rest) = handle_possible_keyword(ContinueKeyword, [099, 111, 110, 116, 105, 110, 117, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [100, 101, 098, 117, 103, 103, 101, 114, .. as u8s] -> # debugger
            (token, rest) = handle_possible_keyword(DebuggerKeyword, [100, 101, 098, 117, 103, 103, 101, 114], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [100, 101, 102, 097, 117, 108, 116, .. as u8s] -> # default
            (token, rest) = handle_possible_keyword(DefaultKeyword, [100, 101, 102, 097, 117, 108, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [100, 101, 108, 101, 116, 101, .. as u8s] -> # delete
            (token, rest) = handle_possible_keyword(DeleteKeyword, [100, 101, 108, 101, 116, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [100, 111, .. as u8s] -> # do
            (token, rest) = handle_possible_keyword(DoKeyword, [100, 111], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [101, 108, 115, 101, .. as u8s] -> # else
            (token, rest) = handle_possible_keyword(ElseKeyword, [101, 108, 115, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [101, 110, 117, 109, .. as u8s] -> # enum
            (token, rest) = handle_possible_keyword(EnumKeyword, [101, 110, 117, 109], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [101, 120, 112, 111, 114, 116, .. as u8s] -> # export
            (token, rest) = handle_possible_keyword(ExportKeyword, [101, 120, 112, 111, 114, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [101, 120, 116, 101, 110, 100, 115, .. as u8s] -> # extends
            (token, rest) = handle_possible_keyword(ExtendsKeyword, [101, 120, 116, 101, 110, 100, 115], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [102, 097, 108, 115, 101, .. as u8s] -> # false
            (token, rest) = handle_possible_keyword(FalseKeyword, [102, 097, 108, 115, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [102, 105, 110, 097, 108, 108, 121, .. as u8s] -> # finally
            (token, rest) = handle_possible_keyword(FinallyKeyword, [102, 105, 110, 097, 108, 108, 121], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [102, 111, 114, .. as u8s] -> # for
            (token, rest) = handle_possible_keyword(ForKeyword, [102, 111, 114], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [102, 117, 110, 099, 116, 105, 111, 110, .. as u8s] -> # function
            (token, rest) = handle_possible_keyword(FunctionKeyword, [102, 117, 110, 099, 116, 105, 111, 110], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [105, 102, .. as u8s] -> # if
            (token, rest) = handle_possible_keyword(IfKeyword, [105, 102], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [105, 109, 112, 111, 114, 116, .. as u8s] -> # import
            (token, rest) = handle_possible_keyword(ImportKeyword, [105, 109, 112, 111, 114, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [105, 110, 115, 116, 097, 110, 099, 101, 111, 102, .. as u8s] -> # Instanceof
            (token, rest) = handle_possible_keyword(InstanceofKeyword, [105, 110, 115, 116, 097, 110, 099, 101, 111, 102], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [105, 110, .. as u8s] -> # in
            (token, rest) = handle_possible_keyword(InKeyword, [105, 110], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [110, 101, 119, .. as u8s] -> # new
            (token, rest) = handle_possible_keyword(NewKeyword, [110, 101, 119], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [110, 117, 108, 108, .. as u8s] -> # null
            (token, rest) = handle_possible_keyword(NullKeyword, [110, 117, 108, 108], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [114, 101, 116, 117, 114, 110, .. as u8s] -> # return
            (token, rest) = handle_possible_keyword(ReturnKeyword, [114, 101, 116, 117, 114, 110], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [115, 117, 112, 101, 114, .. as u8s] -> # super
            (token, rest) = handle_possible_keyword(SuperKeyword, [115, 117, 112, 101, 114], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [115, 119, 105, 116, 099, 104, .. as u8s] -> # switch
            (token, rest) = handle_possible_keyword(SwitchKeyword, [115, 119, 105, 116, 099, 104], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [116, 104, 105, 115, .. as u8s] -> # this
            (token, rest) = handle_possible_keyword(ThisKeyword, [116, 104, 105, 115], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [116, 104, 114, 111, 119, .. as u8s] -> # throw
            (token, rest) = handle_possible_keyword(ThrowKeyword, [116, 104, 114, 111, 119], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [116, 114, 117, 101, .. as u8s] -> # true
            (token, rest) = handle_possible_keyword(TrueKeyword, [116, 114, 117, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [116, 114, 121, .. as u8s] -> # try
            (token, rest) = handle_possible_keyword(TryKeyword, [116, 114, 121], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [116, 121, 112, 101, 111, 102, .. as u8s] -> # typeof
            (token, rest) = handle_possible_keyword(TypeofKeyword, [116, 121, 112, 101, 111, 102], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [118, 097, 114, .. as u8s] -> # var
            (token, rest) = handle_possible_keyword(VarKeyword, [118, 097, 114], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [118, 111, 105, 100, .. as u8s] -> # void
            (token, rest) = handle_possible_keyword(VoidKeyword, [118, 111, 105, 100], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [119, 104, 105, 108, 101, .. as u8s] -> # while
            (token, rest) = handle_possible_keyword(WhileKeyword, [119, 104, 105, 108, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [119, 105, 116, 104, .. as u8s] -> # with
            (token, rest) = handle_possible_keyword(WithKeyword, [119, 105, 116, 104], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [097, 115, .. as u8s] -> # as
            (token, rest) = handle_possible_keyword(AsKeyword, [097, 115], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [105, 109, 112, 108, 101, 109, 101, 110, 116, 115, .. as u8s] -> # implements
            (token, rest) = handle_possible_keyword(ImplementsKeyword, [105, 109, 112, 108, 101, 109, 101, 110, 116, 115], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [107, 101, 121, 111, 102, .. as u8s] -> # keyof
            (token, rest) = handle_possible_keyword(KeyofKeyword, [107, 101, 121, 111, 102], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [108, 101, 116, .. as u8s] -> # let
            (token, rest) = handle_possible_keyword(LetKeyword, [108, 101, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [112, 097, 099, 107, 097, 103, 101, .. as u8s] -> # package
            (token, rest) = handle_possible_keyword(PackageKeyword, [112, 097, 099, 107, 097, 103, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [112, 114, 105, 118, 097, 116, 101, .. as u8s] -> # private
            (token, rest) = handle_possible_keyword(PrivateKeyword, [112, 114, 105, 118, 097, 116, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [112, 114, 111, 116, 101, 099, 116, 101, 100, .. as u8s] -> # protected
            (token, rest) = handle_possible_keyword(ProtectedKeyword, [112, 114, 111, 116, 101, 099, 116, 101, 100], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [112, 117, 098, 108, 105, 099, .. as u8s] -> # public
            (token, rest) = handle_possible_keyword(PublicKeyword, [112, 117, 098, 108, 105, 099], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [115, 116, 097, 116, 105, 099, .. as u8s] -> # static
            (token, rest) = handle_possible_keyword(StaticKeyword, [115, 116, 097, 116, 105, 099], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [121, 105, 101, 108, 100, .. as u8s] -> # yield
            (token, rest) = handle_possible_keyword(YieldKeyword, [121, 105, 101, 108, 100], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [097, 110, 121, .. as u8s] -> # any
            (token, rest) = handle_possible_keyword(AnyKeyword, [097, 110, 121], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [098, 111, 111, 108, 101, 097, 110, .. as u8s] -> # boolean
            (token, rest) = handle_possible_keyword(BooleanKeyword, [098, 111, 111, 108, 101, 097, 110], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [100, 101, 099, 108, 097, 114, 101, .. as u8s] -> # declare
            (token, rest) = handle_possible_keyword(DeclareKeyword, [100, 101, 099, 108, 097, 114, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [103, 101, 116, .. as u8s] -> # get
            (token, rest) = handle_possible_keyword(GetKeyword, [103, 101, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [109, 111, 100, 117, 108, 101, .. as u8s] -> # module
            (token, rest) = handle_possible_keyword(ModuleKeyword, [109, 111, 100, 117, 108, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [114, 101, 113, 117, 105, 114, 101, .. as u8s] -> # require
            (token, rest) = handle_possible_keyword(RequireKeyword, [114, 101, 113, 117, 105, 114, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [110, 117, 109, 098, 101, 114, .. as u8s] -> # number
            (token, rest) = handle_possible_keyword(NumberKeyword, [110, 117, 109, 098, 101, 114], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [115, 101, 116, .. as u8s] -> # set
            (token, rest) = handle_possible_keyword(SetKeyword, [115, 101, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [115, 116, 114, 105, 110, 103, .. as u8s] -> # string
            (token, rest) = handle_possible_keyword(StringKeyword, [115, 116, 114, 105, 110, 103], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [115, 121, 109, 098, 111, 108, .. as u8s] -> # symbol
            (token, rest) = handle_possible_keyword(SymbolKeyword, [115, 121, 109, 098, 111, 108], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [116, 121, 112, 101, .. as u8s] -> # type
            (token, rest) = handle_possible_keyword(TypeKeyword, [116, 121, 112, 101], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [102, 114, 111, 109, .. as u8s] -> # from
            (token, rest) = handle_possible_keyword(FromKeyword, [102, 114, 111, 109], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [111, 102, .. as u8s] -> # of
            (token, rest) = handle_possible_keyword(OfKeyword, [111, 102], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        [097, 119, 097, 105, 116, .. as u8s] -> # await
            (token, rest) = handle_possible_keyword(AwaitKeyword, [097, 119, 097, 105, 116], u8s)
            utf8_list_to_ts_token_list_inner(rest, List.append(token_list, token))

        # --- Trivia: Newlines and Whitespace ---
        [10, .. as u8s] -> # \n
            consume_new_lines = |count, bytes|
                when bytes is
                    [10, .. as rest] -> consume_new_lines(count + 1, rest)
                    _ -> (count, bytes)
            (nl_count, rest_after_newline) = consume_new_lines(1, u8s)
            utf8_list_to_ts_token_list_inner(
                rest_after_newline,
                List.append(token_list, NewLineTrivia(nl_count)),
            )

        [13, 10, .. as u8s] -> # \r\n
            consume_new_lines = |count, bytes|
                when bytes is
                    [13, 10, .. as rest] -> consume_new_lines(count + 1, rest)
                    _ -> (count, bytes)
            (nl_count, rest_after_newline) = consume_new_lines(1, u8s)
            utf8_list_to_ts_token_list_inner(
                rest_after_newline,
                List.append(token_list, NewLineTrivia(nl_count)),
            )

        [u8, .. as u8s] if u8 == 32 or u8 == 9 or u8 == 11 or u8 == 12 -> # Space, Tab, VT, FF
            # Consume all contiguous whitespace
            consume_whitespace = |count, bytes|
                when bytes is
                    [b, .. as rest] if b == 32 or b == 9 or b == 11 or b == 12 -> consume_whitespace(count + 1, rest)
                    # Check for non-breaking space (U+00A0 = C2 A0) - Optional
                    [194, 160, .. as rest] -> consume_whitespace(count + 1, rest)
                    _ -> (count, bytes)
            (ws_count, rest_after_whitespace) = consume_whitespace(1, u8s)
            tok = WhitespaceTrivia(ws_count)
            utf8_list_to_ts_token_list_inner(
                rest_after_whitespace,
                List.append(token_list, tok),
            )

        # --- Literals ---
        [34, .. as rest] -> # " String Literal
            { token, remaining_u8s } = process_string_literal(rest, "\"") # Pass quote char
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                List.append(token_list, token),
            ) # Placeholder prev_token

        [39, .. as rest] -> # ' String Literal
            { token, remaining_u8s } = process_string_literal(rest, "'") # Pass quote char
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                List.append(token_list, token),
            ) # Placeholder prev_token

        [96, .. as rest] -> # ` Template Literal
            { tokens, remaining_u8s } = process_template_literal(rest)
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                List.concat(token_list, tokens),
            )

        [46, 46, 46, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, DotDotDotToken),
            )

        [63, 46, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, QuestionDotToken),
            )

        [42, 42, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AsteriskAsteriskEqualsToken),
            )

        [42, 42, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AsteriskAsteriskToken),
            )

        [61, 61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, EqualsEqualsEqualsToken),
            )

        [33, 61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, ExclamationEqualsEqualsToken),
            )

        [61, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, EqualsEqualsToken),
            )

        [33, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, ExclamationEqualsToken),
            )

        [61, 62, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, EqualsGreaterThanToken),
            )

        [43, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, PlusEqualsToken),
            )

        [45, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, MinusEqualsToken),
            )

        [42, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AsteriskEqualsToken),
            )

        [47, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, SlashEqualsToken),
            )

        [37, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, PercentEqualsToken),
            )

        [60, 60, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, LessThanLessThanEqualsToken),
            )

        [62, 62, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, GreaterThanGreaterThanEqualsToken),
            )

        [62, 62, 62, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, GreaterThanGreaterThanGreaterThanEqualsToken),
            )

        [38, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AmpersandEqualsToken),
            )

        [124, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, BarEqualsToken),
            )

        [94, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, CaretEqualsToken),
            )

        [38, 38, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AmpersandAmpersandEqualsToken),
            )

        [124, 124, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, BarBarEqualsToken),
            )

        [63, 63, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, QuestionQuestionEqualsToken),
            )

        [60, 60, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, LessThanLessThanToken),
            )

        [62, 62, 62, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, GreaterThanGreaterThanGreaterThanToken),
            )

        [62, 62, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, GreaterThanGreaterThanToken),
            )

        [60, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, LessThanEqualsToken),
            )

        [62, 61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, GreaterThanEqualsToken),
            )

        [38, 38, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AmpersandAmpersandToken),
            )

        [124, 124, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, BarBarToken),
            )

        [63, 63, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, QuestionQuestionToken),
            )

        [43, 43, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, PlusPlusToken),
            )

        [45, 45, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, MinusMinusToken),
            )

        [60, 47, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, LessThanSlashToken),
            ) # For JSX
        # --- Single Character Punctuation and Operators ---

        [123, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, OpenBraceToken),
            )

        [125, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, CloseBraceToken),
            )

        [40, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, OpenParenToken),
            )

        [41, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, CloseParenToken),
            )

        [91, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, OpenBracketToken),
            )

        [93, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, CloseBracketToken),
            )

        [46, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, DotToken),
            )

        [59, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, SemicolonToken),
            )

        [44, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, CommaToken),
            )

        [60, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, LessThanToken),
            )

        [62, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, GreaterThanToken),
            )

        [61, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, EqualsToken),
            )

        [43, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, PlusToken),
            )

        [45, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, MinusToken),
            )

        [42, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AsteriskToken),
            )

        [47, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, SlashToken),
            ) # Needs context for Regex

        [37, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, PercentToken),
            )

        [38, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AmpersandToken),
            )

        [124, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, BarToken),
            )

        [94, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, CaretToken),
            )

        [33, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, ExclamationToken),
            )

        [126, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, TildeToken),
            )

        [63, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, QuestionToken),
            )

        [58, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, ColonToken),
            )

        [64, .. as u8s] ->
            utf8_list_to_ts_token_list_inner(
                u8s,
                List.append(token_list, AtToken),
            )

        # Backtick and Hash are primarily JSDoc or handled differently (template literal start, private identifier start)
        # [96, .. as u8s] -> ... handled by template literal case ...
        # [35, .. as u8s] -> ... handled by identifier case ...
        # --- Number Literal ---
        [u8, .. as rest] if is_digit(u8) ->
            { token, remaining_u8s } = process_numeric_literal(u8, rest)
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                List.append(token_list, token),
            )

        # --- IdentifierToken or Keyword or PrivateIdentifierToken ---
        # Check for # first for PrivateIdentifierToken
        [35, first_char, .. as rest] ->
            # Assuming process_identifier handles the # prefix case
            { token, remaining_u8s } = process_identifier(first_char, rest)
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                List.append(token_list, token),
            )

        [first_char, .. as rest] if is_identifier_start(first_char) ->
            { token, remaining_u8s } = process_identifier(first_char, rest)
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                List.append(token_list, token),
            )

        # ConflictMarkerTrivia and NonTextFileMarkerTrivia are more complex/specific,
        # omitted for brevity here but would follow similar patterns if needed.
        # --- Unknown Character ---
        [u8, .. as u8s] -> # Catch-all for unrecognized bytes
            consume_until_whitespace_or_newline_or_eof : List U8, List U8 -> { unknown_bytes : List U8, remaining_u8s : List U8 }
            consume_until_whitespace_or_newline_or_eof = |current_u8s, acc|
                when current_u8s is
                    [byte, .. as rest] if byte == 32 or byte == 9 or byte == 11 or byte == 12 ->
                        consume_until_whitespace_or_newline_or_eof(rest, acc)

                    [byte, .. as rest] if byte == 10 or byte == 13 ->
                        consume_until_whitespace_or_newline_or_eof(rest, acc)

                    _ ->
                        { unknown_bytes: acc, remaining_u8s: current_u8s }

            { unknown_bytes, remaining_u8s } = consume_until_whitespace_or_newline_or_eof(u8_list, [])
            utf8_list_to_ts_token_list_inner(
                remaining_u8s,
                List.append(token_list, TokenError(UnknownToken(unknown_bytes))),
            )

# Function to parse line comments (//)
process_line_comment_text :
    List U8
    -> {
        token : Token,
        remaining_u8s : List U8,
    }
process_line_comment_text = |u8s|
    consume_until_newline = |comment_text_u8s, next_u8s|
        when next_u8s is
            [10, .. as rest] ->
                (
                    comment_text_u8s |> List.append(10),
                    rest,
                ) # Stop at newline (\n)

            [13, 10, .. as rest] ->
                (
                    comment_text_u8s |> List.append(13) |> List.append(10),
                    rest,
                ) # Stop at \r\n

            [next_u8, .. as rest] ->
                consume_until_newline(
                    comment_text_u8s |> List.append(next_u8),
                    rest,
                ) # Continue

            [] -> (comment_text_u8s, []) # End of input
    (comment_text, remaining_u8s) = consume_until_newline([], u8s)
    token : Token
    token = CommentText(Str.from_utf8_lossy(comment_text))
    {
        token,
        remaining_u8s,
    }

# Process entire block comment after `BlockCommentStart` (`/*`) token until
# `BlockCommentBlockEnd` (`*/`), with `CommentText` in the middle
process_block_comment_text :
    List U8
    ->
    {
        token : Token,
        remaining_u8s : List U8,
    }
process_block_comment_text = |u8s|
    inner_process : List U8, List U8 -> { token : Token, remaining_u8s : List U8 }
    inner_process = |current_u8s, acc|
        when current_u8s is
            [42, 47, .. as rest] -> # "*/" ends comment
                comment_result = Str.from_utf8(acc)
                when comment_result is
                    Ok(comment) ->
                        {
                            token: CommentText(comment),
                            remaining_u8s: rest,
                        }

                    Err(_) ->
                        {
                            token: TokenError(UnknownToken(acc)),
                            remaining_u8s: current_u8s,
                        }

            [u8, .. as rest] ->
                inner_process(rest, List.append(acc, u8))

            [] -> # Unclosed block comment
                comment_result = Str.from_utf8(acc)
                when comment_result is
                    Ok(comment) ->
                        {
                            token: CommentText(comment),
                            remaining_u8s: current_u8s,
                        }

                    Err(_) ->
                        {
                            token: TokenError(UnknownToken(acc)),
                            remaining_u8s: current_u8s,
                        }
    inner_process(u8s, [])

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

process_identifier :
    U8,
    List U8
    -> {
        token : Token,
        remaining_u8s : List U8,
    }
process_identifier = |first_char, rest|
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

    token : Token
    token =
        when ident_result is
            Ok(ident) -> IdentifierToken(ident)
            Err(_) -> TokenError(UnknownToken(ident_chars))
    { token, remaining_u8s: new_remaining }

# Changed return type: returns the consumed sequence, the rest of the input,
# AND a Result indicating if the consumed sequence was valid.

collect_numeric_chars :
    List U8,
    List U8,
    Bool,
    Bool
    -> {
        consumed : List U8,
        rest : List U8,
        status : Result {} [InvalidNumberSeparator],
    }
collect_numeric_chars = |acc, remaining, has_decimal, has_exp|
    # Tail-recursive inner helper
    # Keeps track of validity status internally
    inner_collect :
        List U8,
        List U8,
        Bool,
        Bool,
        Result {} [InvalidNumberSeparator]
        -> {
            consumed : List U8,
            rest : List U8,
            status : Result {} [InvalidNumberSeparator],
        }
    inner_collect = |current_acc, current_remaining, current_decimal, current_exp, current_status|
        when current_remaining is
            # --- Underscore Separator Handling ---
            [95, .. as rest_after_underscore] -> # Encountered an underscore '_'
                # Check for immediate double underscore: __
                double_underscore =
                    when List.first(rest_after_underscore) is
                        Ok(95) -> Bool.true
                        _ -> Bool.false

                # Rule 1 & 2 & 3 (preceding): Last char added MUST be a digit.
                prev_is_digit =
                    when List.last(current_acc) is
                        Ok(prev_u8) -> is_digit(prev_u8)
                        Err(_) -> Bool.false # Handles initial '_' if allowed (not here)

                # Rule 1 & 3 & 4 (succeeding): Next char MUST be a digit.
                next_is_digit =
                    when List.first(rest_after_underscore) is
                        Ok(next_u8) -> is_digit(next_u8)
                        Err(_) -> Bool.false # Underscore at the end

                # Determine validity
                is_valid_separator = prev_is_digit and next_is_digit and !double_underscore
                new_status = if current_status == Ok {} and !is_valid_separator then
                    Err(InvalidNumberSeparator)
                else
                    current_status

                # Consume the underscore regardless of validity and continue
                inner_collect(
                    current_acc |> List.append(95),
                    rest_after_underscore,
                    current_decimal,
                    current_exp,
                    new_status,
                )

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

                # Determine validity
                is_valid_decimal = !prev_is_underscore and !next_is_underscore
                new_status = if current_status == Ok {} and !is_valid_decimal then
                    Err(InvalidNumberSeparator)
                else
                    current_status

                # Consume the decimal point regardless and continue
                inner_collect(
                    current_acc |> List.append(46),
                    rest_chars,
                    Bool.true,
                    current_exp,
                    new_status,
                )

            # --- Exponent 'e'/'E' (ASCII 101/69) ---
            [exp_char, .. as rest_chars] if (exp_char == 101 or exp_char == 69) and !current_exp ->
                prev_is_underscore =
                    when List.last(current_acc) is
                        Ok(95) -> Bool.true
                        _ -> Bool.false
                next_pattern_is_invalid = # Check e_, E_, e+_, E+_, e-_, E-_
                    when List.first(rest_chars) is
                        Ok(95) -> Bool.true # e_ or E_
                        Ok(x) if x == 43 or x == 45 -> # e+ or e- or E+ or E-
                            when List.get(rest_chars, 1) is
                                Ok(95) -> Bool.true # e+_ or e-_ etc.
                                _ -> Bool.false

                        _ -> Bool.false

                # Determine validity
                is_valid_exponent = !prev_is_underscore and !next_pattern_is_invalid
                new_status = if current_status == Ok {} and !is_valid_exponent then
                    Err(InvalidNumberSeparator)
                else
                    current_status

                # Consume the exponent char regardless and continue
                inner_collect(
                    current_acc |> List.append(exp_char),
                    rest_chars,
                    current_decimal,
                    Bool.true,
                    new_status,
                )

            # --- Exponent sign '+' / '-' (ASCII 43/45) ---
            [sign_char, .. as rest_chars] if (sign_char == 43 or sign_char == 45) and current_exp ->
                # Check if it immediately follows 'e' or 'E'
                is_after_exp_indicator =
                    when List.last(current_acc) is
                        Ok(prev_u8) -> prev_u8 == 101 or prev_u8 == 69
                        Err(_) -> Bool.false
                # Check if followed by underscore
                next_is_underscore =
                    when List.first(rest_chars) is
                        Ok(95) -> Bool.true
                        _ -> Bool.false

                # Determine validity
                is_valid_sign = is_after_exp_indicator and !next_is_underscore
                new_status = if current_status == Ok {} and !is_valid_sign then
                    Err(InvalidNumberSeparator)
                else
                    current_status

                # Consume the sign char regardless and continue
                inner_collect(
                    current_acc |> List.append(sign_char),
                    rest_chars,
                    current_decimal,
                    current_exp,
                    new_status,
                )

            # --- Digits '0'-'9' ---
            [u8, .. as rest_chars] if is_digit(u8) ->
                # Just consume and continue, pass status along
                inner_collect(
                    current_acc |> List.append(u8),
                    rest_chars,
                    current_decimal,
                    current_exp,
                    current_status,
                )

            # --- End of numeric literal (any other character or end of input) ---
            _ ->
                # Final check: last character cannot be an underscore
                final_status =
                    when List.last(current_acc) is
                        Ok(95) if current_status == Ok {} ->
                            Err(InvalidNumberSeparator) # Mark error if trailing underscore found AND no prior error

                        _ -> current_status # Keep existing status (Ok or prior Err)

                # Return the fully consumed sequence, the actual remaining input, and the final validity status
                {
                    consumed: current_acc,
                    rest: current_remaining,
                    status: final_status,
                }

    # Initial call to the inner helper, starting with Ok status
    inner_collect(acc, remaining, has_decimal, has_exp, Ok({}))

# Refactored function to process numeric literals using the modified collect_numeric_chars
process_numeric_literal :
    U8,
    List U8
    -> {
        token : Token,
        remaining_u8s : List U8,
    }
process_numeric_literal = |first_digit, rest|
    # Collect all potential numeric characters and determine validity
    collection_result = collect_numeric_chars([first_digit], rest, Bool.false, Bool.false)

    # Decide token based on validity status
    when collection_result.status is
        Ok {} ->
            # Valid number sequence
            num_chars = collection_result.consumed
            new_remaining = collection_result.rest

            # It should theoretically not be empty if first_digit was
            # valid, but check anyway
            if List.is_empty(num_chars) then
                {
                    token: TokenError(UnknownToken([])),
                    remaining_u8s: new_remaining,
                }
            else
                # Try converting the valid sequence to string
                num_result = Str.from_utf8(num_chars)
                when num_result is
                    Ok(num_str) ->
                        # Successfully created NumberLiteralToken token
                        {
                            token: NumberLiteralToken(num_str),
                            remaining_u8s: new_remaining,
                        }

                    Err(_) ->
                        # UTF8 error unlikely but possible? Treat as unknown.
                        {
                            token: UnknownToken(num_chars),
                            remaining_u8s: new_remaining,
                        }

        Err(InvalidNumberSeparator) ->
            # Invalid number sequence due to separator rules
            {
                token: TokenError(InvalidNumberSeparator),
                remaining_u8s: collection_result.rest,
            }

process_string_literal :
    List U8,
    Str
    -> {
        token : Token,
        remaining_u8s : List U8,
    }
process_string_literal = |u8s, quote_type|
    inner_process :
        List U8,
        Str,
        List U8
        -> {
            token : Token,
            remaining_u8s : List U8,
        }
    inner_process = |current_u8s, current_quote_type, acc|
        when current_u8s is
            # Handle escape sequences
            [92, next, .. as rest] -> # backslash followed by any character
                inner_process(
                    rest,
                    current_quote_type,
                    acc
                    |> List.append(92)
                    |> List.append(next),
                )

            # End of string based on quote type
            [34, .. as rest] if current_quote_type == "\"" -> # double quote
                str_result = acc |> List.append(34) |> Str.from_utf8
                when str_result is
                    Ok(str) ->
                        { token: StringLiteralToken(str), remaining_u8s: rest }

                    Err(_) ->
                        { token: UnknownToken(acc), remaining_u8s: rest }

            [39, .. as rest] if current_quote_type == "'" -> # single quote
                str_result = acc |> List.append(39) |> Str.from_utf8
                when str_result is
                    Ok(str) ->
                        { token: StringLiteralToken(str), remaining_u8s: rest }

                    Err(_) ->
                        { token: TokenError(UnknownToken(acc)), remaining_u8s: rest }

            # Collect string content
            [u8, .. as rest] ->
                inner_process(rest, current_quote_type, List.append(acc, u8))

            # Unclosed string
            [] ->
                { token: TokenError(UnclosedString), remaining_u8s: current_u8s }
    inner_process(u8s, quote_type, Str.to_utf8(quote_type))

# Process template literal with expression interpolation support
process_template_literal : List U8 -> { tokens : List Token, remaining_u8s : List U8 }
process_template_literal = |u8s|
    # Check if this is a simple template literal (no interpolation)
    if !(has_interpolation(u8s)) then
        # Simple case: `hello world` (no ${})
        process_simple_template_literal(u8s)
    else
        # Complex case: `hello ${name}!` (has ${} expressions)
        process_interpolated_template_literal(u8s)

# Check if template literal contains ${} interpolation
has_interpolation : List U8 -> Bool
has_interpolation = |u8s|
    check_for_dollar_brace(u8s)

check_for_dollar_brace : List U8 -> Bool
check_for_dollar_brace = |u8s|
    when u8s is
        [96, ..] -> Bool.false # End of template - no interpolation found
        [36, 123, ..] -> Bool.true # Found ${ - has interpolation
        [92, _, .. as rest] -> check_for_dollar_brace(rest) # Skip escaped chars
        [_, .. as rest] -> check_for_dollar_brace(rest) # Continue scanning
        [] -> Bool.false # End of input - no interpolation

# Process simple template literal without interpolation
process_simple_template_literal : List U8 -> { tokens : List Token, remaining_u8s : List U8 }
process_simple_template_literal = |u8s|
    collect_template_content(u8s, [])

collect_template_content : List U8, List U8 -> { tokens : List Token, remaining_u8s : List U8 }
collect_template_content = |u8s, acc|
    when u8s is
        [96, .. as rest] -> # End backtick found
            content_result = Str.from_utf8(acc)
            when content_result is
                Ok(content) ->
                    { tokens: [NoSubstitutionTemplateLiteralToken(content)], remaining_u8s: rest }
                Err(_) ->
                    { tokens: [TokenError(UnknownToken(acc))], remaining_u8s: rest }

        [92, next, .. as rest] -> # Escaped character
            collect_template_content(rest, acc |> List.append(92) |> List.append(next))

        [u8, .. as rest] -> # Regular character
            collect_template_content(rest, List.append(acc, u8))

        [] -> # Unclosed template
            { tokens: [TokenError(UnclosedString)], remaining_u8s: [] }

# Process template literal with interpolation expressions
process_interpolated_template_literal : List U8 -> { tokens : List Token, remaining_u8s : List U8 }
process_interpolated_template_literal = |u8s|
    process_template_parts(u8s, [], Bool.true)

# Process template parts, tracking whether we're at the head/middle/tail
process_template_parts : List U8, List Token, Bool -> { tokens : List Token, remaining_u8s : List U8 }
process_template_parts = |u8s, tokens, is_first_part|
    { content, remaining } = collect_template_text(u8s, [])

    when remaining is
        [36, 123, .. as expr_start] -> # Found ${
            # Add template text token (Head/Middle)
            text_token = if is_first_part then
                TemplateHead(content)
            else
                TemplateMiddle(content)

            updated_tokens = tokens |> List.append(text_token) |> List.append(DollarBraceToken)

            # Process the expression inside ${}
            { expr_tokens, remaining: remaining_after_expr } = process_template_expression(expr_start)

            tokens_with_expr = List.concat(updated_tokens, expr_tokens)

            # Continue processing remaining template parts
            process_template_parts(remaining_after_expr, tokens_with_expr, Bool.false)

        [96, .. as rest] -> # End of template
            # Add final template text token (Tail)
            text_token = if is_first_part then
                NoSubstitutionTemplateLiteralToken(content)
            else
                TemplateTail(content)

            final_tokens = List.append(tokens, text_token)
            { tokens: final_tokens, remaining_u8s: rest }

        [] -> # Unclosed template
            error_tokens = List.append(tokens, TokenError(UnclosedString))
            { tokens: error_tokens, remaining_u8s: [] }

        _ -> # Unexpected character - shouldn't happen
            error_tokens = List.append(tokens, TokenError(UnknownToken([])))
            { tokens: error_tokens, remaining_u8s: remaining }

# Collect template text content until we hit ${ or `
collect_template_text : List U8, List U8 -> { content : Str, remaining : List U8 }
collect_template_text = |u8s, acc|
    when u8s is
        [96, ..] -> # End backtick - return content
            content_result = Str.from_utf8(acc)
            when content_result is
                Ok(content) -> { content, remaining: u8s }
                Err(_) -> { content: "", remaining: u8s }

        [36, 123, ..] -> # Start of expression ${
            content_result = Str.from_utf8(acc)
            when content_result is
                Ok(content) -> { content, remaining: u8s }
                Err(_) -> { content: "", remaining: u8s }

        [92, next, .. as rest] -> # Escaped character
            collect_template_text(rest, acc |> List.append(92) |> List.append(next))

        [u8, .. as rest] -> # Regular character
            collect_template_text(rest, List.append(acc, u8))

        [] -> # End of input
            content_result = Str.from_utf8(acc)
            when content_result is
                Ok(content) -> { content, remaining: [] }
                Err(_) -> { content: "", remaining: [] }

# Process expression inside ${...}
process_template_expression : List U8 -> { expr_tokens : List Token, remaining : List U8 }
process_template_expression = |u8s|
    # Find the matching closing brace, accounting for nested braces
    { expr_content, remaining } = extract_expression_content(u8s, [], 0)

    # Tokenize the expression content recursively
    all_expr_tokens = utf8_list_to_ts_token_list_inner(expr_content, [])

    # Filter out EndOfFileToken since we're in the middle of a template literal
    expr_tokens = List.drop_if(all_expr_tokens, |token_result|
        when token_result is
            EndOfFileToken -> Bool.true
            _ -> Bool.false
    )

    # Add closing brace token
    tokens_with_close = List.append(expr_tokens, CloseBraceToken)

    { expr_tokens: tokens_with_close, remaining: remaining }

# Extract content between ${ and matching }, handling nested braces
extract_expression_content : List U8, List U8, U64 -> { expr_content : List U8, remaining : List U8 }
extract_expression_content = |u8s, acc, brace_count|
    when u8s is
        [123, .. as rest] -> # Open brace {
            extract_expression_content(rest, List.append(acc, 123), brace_count + 1)

        [125, .. as rest] -> # Close brace }
            if brace_count == 0 then
                # This is the matching close brace for our ${
                { expr_content: acc, remaining: rest }
            else
                # This is a nested close brace
                extract_expression_content(rest, List.append(acc, 125), brace_count - 1)

        [u8, .. as rest] -> # Any other character
            extract_expression_content(rest, List.append(acc, u8), brace_count)

        [] -> # Unclosed expression
            { expr_content: acc, remaining: [] }

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
    "Instanceof",
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

# cc = {
#     slash: 47,
#     asterisk: 42,
#     backslash: 92,
#     lf: 10,
#     cr: 13,
#     space: 32,
#     tab: 9,
#     underscore: 95,
#     dollar: 36,
#     zero: 48,
#     nine: 57,
#     a: 97,
#     z: 122,
#     cap_A: 65,
#     cap_Z: 90,
#     openParen: 40,
#     closeParen: 41,
#     openBrace: 123,
#     closeBrace: 125,
#     openBracket: 91,
#     closeBracket: 93,
#     lessThan: 60,
#     greaterThan: 62,
#     equals: 61,
#     plus: 43,
#     minus: 45,
#     percent: 37,
#     ampersand: 38,
#     bar: 124,
#     caret: 94,
#     tilde: 126,
#     exclamation: 33,
#     question: 63,
#     comma: 44,
#     dot: 46,
#     semicolon: 59,
#     colon: 58,
#     singleQuote: 39,
#     doubleQuote: 34,
#     backtick: 96,
#     hash: 35,
#     at: 64,
# }

# any
# as
# async
# await
# boolean
# break
# case
# catch
# class
# const
# constructor
# continue
# debugger
# declare
# default
# delete
# do
# else
# enum
# export
# extends
# false
# finally
# for
# from
# function
# get
# if
# implements
# import
# in
# Instanceof
# interface
# let
# module
# new
# null
# number
# of
# package
# private
# protected
# public
# require
# return
# set
# static
# string
# super
# switch
# symbol
# this
# throw
# true
# try
# type
# typeof
# var
# void
# while
# with
# yield


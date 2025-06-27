module [
    ts_token_debug_display,
    utf8_list_to_ts_token_list,
    EcmaAlpha,
    EcmaWhitespace,
    EcmaNewline,
]

TsToken : [
    Start, # Not used for AST. Just marking the start of the token stream.
    Unknown,
    EndOfFileToken,
    LineCommentStart,
    BlockCommentStart,
    BlockCommentEnd,
    CommentText Str,
    # SingleLineCommentTrivia,
    # MultiLineCommentTrivia,
    NewLineTrivia,
    WhitespaceTrivia,
    ShebangTrivia,
    ConflictMarkerTrivia,
    NonTextFileMarkerTrivia,
    # Literals
    NumericLiteral Str,
    BigIntLiteral Str,
    StringLiteral Str,
    JsxText Str,
    JsxTextAllWhiteSpaces Str,
    RegularExpressionLiteral Str,
    # NoSubstitutionTemplateLiteral Str,
    # # Pseudo-literals
    # TemplateHead,
    # TemplateMiddle,
    # TemplateTail,
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
    # /** Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteral and related kinds. */
    BacktickToken,
    # /** Only the JSDoc scanner produces HashToken. The normal scanner produces PrivateIdentifier. */
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
    # Identifiers and PrivateIdentifiers
    Identifier Str,
    PrivateIdentifier Str,
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
    InstanceOfKeyword,
    NewKeyword,
    NullKeyword,
    ReturnKeyword,
    SuperKeyword,
    SwitchKeyword,
    ThisKeyword,
    ThrowKeyword,
    TrueKeyword,
    TryKeyword,
    TypeOfKeyword,
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

TsTokenResult : [
    Ok TsToken,
    Err [Unknown, UnclosedString, UnclosedTemplate, UnclosedInterpolation, InvalidNumericSeparator],
]

ts_token_debug_display : TsToken -> Str
ts_token_debug_display = |token|
    when token is
        Start -> "Start"
        Unknown -> "Unknown"
        EndOfFileToken -> "EndOfFileToken"
        LineCommentStart -> "LineCommentStart"
        BlockCommentStart -> "BlockCommentStart"
        BlockCommentEnd -> "BlockCommentBlockEnd"
        CommentText(str) -> "CommentText(${str})"
        # SingleLineCommentTrivia -> "SingleLineCommentTrivia"
        # MultiLineCommentTrivia -> "MultiLineCommentTrivia"
        NewLineTrivia -> "NewLineTrivia"
        WhitespaceTrivia -> "WhitespaceTrivia"
        ShebangTrivia -> "ShebangTrivia"
        ConflictMarkerTrivia -> "ConflictMarkerTrivia"
        NonTextFileMarkerTrivia -> "NonTextFileMarkerTrivia"
        # Literals
        NumericLiteral(str) -> "NumericLiteral(${str})"
        BigIntLiteral(str) -> "BigIntLiteral(${str})"
        StringLiteral(str) -> "StringLiteral(${str})"
        JsxText(str) -> "JsxText(${str})"
        JsxTextAllWhiteSpaces(str) -> "JsxTextAllWhiteSpaces(${str})"
        RegularExpressionLiteral(str) -> "RegularExpressionLiteral(${str})"
        # NoSubstitutionTemplateLiteral(str) -> "NoSubstitutionTemplateLiteral(${str})"
        # # Pseudo-literals
        # TemplateHead(str) -> "TemplateHead(${str})"
        # TemplateMiddle(str) -> "TemplateMiddle(${str})"
        # TemplateTail(str) -> "TemplateTail(${str})"
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
        # /** Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteral and related kinds. */
        BacktickToken -> "BacktickToken"
        # /** Only the JSDoc scanner produces HashToken. The normal scanner produces PrivateIdentifier. */
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
        # Identifiers and PrivateIdentifiers
        Identifier(str) -> "Identifier(${str})"
        PrivateIdentifier(str) -> "PrivateIdentifier(${str})"
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
        InstanceOfKeyword -> "InstanceOfKeyword"
        NewKeyword -> "NewKeyword"
        NullKeyword -> "NullKeyword"
        ReturnKeyword -> "ReturnKeyword"
        SuperKeyword -> "SuperKeyword"
        SwitchKeyword -> "SwitchKeyword"
        ThisKeyword -> "ThisKeyword"
        ThrowKeyword -> "ThrowKeyword"
        TrueKeyword -> "TrueKeyword"
        TryKeyword -> "TryKeyword"
        TypeOfKeyword -> "TypeOfKeyword"
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
        DebuggerKeyword -> "DebuggerKeyword"
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

EcmaWhitespace : [Space, Tab, Newline, LineTabulation, FormFeed, ZeroWidthNoBreakSpace]
EcmaAlpha : [Alpha (List U8)]
EcmaNewline : [Newline]

utf8_list_to_ts_token_list : List U8 -> List TsTokenResult
utf8_list_to_ts_token_list = |u8_list_|
    # Helper functions

    # Helper processing functions with tail recursion

    # Start the tokenization
    utf8_list_to_ts_token_list_inner(Ok(Start), u8_list_, [])

# Main recursive tokenizer function with accumulator
utf8_list_to_ts_token_list_inner : TsTokenResult, List U8, List TsTokenResult -> List TsTokenResult
utf8_list_to_ts_token_list_inner = |_prev_token, u8_list, token_list| # prev_token often not needed in this style
    when u8_list is
        # --- End of File ---
        [] -> List.append(token_list, Ok(EndOfFileToken))
        # --- Keywords ---
        [098, 114, 101, 097, 107, .. as u8s] -> # break
            utf8_list_to_ts_token_list_inner(Ok(BreakKeyword), u8s, List.append(token_list, Ok(BreakKeyword)))

        [099, 097, 115, 101, .. as u8s] -> # case
            utf8_list_to_ts_token_list_inner(Ok(CaseKeyword), u8s, List.append(token_list, Ok(CaseKeyword)))

        [099, 097, 116, 099, 104, .. as u8s] -> # catch
            utf8_list_to_ts_token_list_inner(Ok(CatchKeyword), u8s, List.append(token_list, Ok(CatchKeyword)))

        [099, 108, 097, 115, 115, .. as u8s] -> # class
            utf8_list_to_ts_token_list_inner(Ok(ClassKeyword), u8s, List.append(token_list, Ok(ClassKeyword)))

        [099, 111, 110, 115, 116, .. as u8s] -> # const
            utf8_list_to_ts_token_list_inner(Ok(ConstKeyword), u8s, List.append(token_list, Ok(ConstKeyword)))

        [099, 111, 110, 116, 105, 110, 117, 101, .. as u8s] -> # continue
            utf8_list_to_ts_token_list_inner(Ok(ContinueKeyword), u8s, List.append(token_list, Ok(ContinueKeyword)))

        [100, 101, 098, 117, 103, 103, 101, 114, .. as u8s] -> # debugger
            utf8_list_to_ts_token_list_inner(Ok(DebuggerKeyword), u8s, List.append(token_list, Ok(DebuggerKeyword)))

        [100, 101, 102, 097, 117, 108, 116, .. as u8s] -> # default
            utf8_list_to_ts_token_list_inner(Ok(DefaultKeyword), u8s, List.append(token_list, Ok(DefaultKeyword)))

        [100, 101, 108, 101, 116, 101, .. as u8s] -> # delete
            utf8_list_to_ts_token_list_inner(Ok(DeleteKeyword), u8s, List.append(token_list, Ok(DeleteKeyword)))

        [100, 111, .. as u8s] -> # do
            utf8_list_to_ts_token_list_inner(Ok(DoKeyword), u8s, List.append(token_list, Ok(DoKeyword)))

        [101, 108, 115, 101, .. as u8s] -> # else
            utf8_list_to_ts_token_list_inner(Ok(ElseKeyword), u8s, List.append(token_list, Ok(ElseKeyword)))

        [101, 110, 117, 109, .. as u8s] -> # enum
            utf8_list_to_ts_token_list_inner(Ok(EnumKeyword), u8s, List.append(token_list, Ok(EnumKeyword)))

        [101, 120, 112, 111, 114, 116, .. as u8s] -> # export
            utf8_list_to_ts_token_list_inner(Ok(ExportKeyword), u8s, List.append(token_list, Ok(ExportKeyword)))

        [101, 120, 116, 101, 110, 100, 115, .. as u8s] -> # extends
            utf8_list_to_ts_token_list_inner(Ok(ExtendsKeyword), u8s, List.append(token_list, Ok(ExtendsKeyword)))

        [102, 097, 108, 115, 101, .. as u8s] -> # false
            utf8_list_to_ts_token_list_inner(Ok(FalseKeyword), u8s, List.append(token_list, Ok(FalseKeyword)))

        [102, 105, 110, 097, 108, 108, 121, .. as u8s] -> # finally
            utf8_list_to_ts_token_list_inner(Ok(FinallyKeyword), u8s, List.append(token_list, Ok(FinallyKeyword)))

        [102, 111, 114, .. as u8s] -> # for
            utf8_list_to_ts_token_list_inner(Ok(ForKeyword), u8s, List.append(token_list, Ok(ForKeyword)))

        [102, 117, 110, 099, 116, 105, 111, 110, .. as u8s] -> # function
            utf8_list_to_ts_token_list_inner(Ok(FunctionKeyword), u8s, List.append(token_list, Ok(FunctionKeyword)))

        [105, 102, .. as u8s] -> # if
            utf8_list_to_ts_token_list_inner(Ok(IfKeyword), u8s, List.append(token_list, Ok(IfKeyword)))

        [105, 109, 112, 111, 114, 116, .. as u8s] -> # import
            utf8_list_to_ts_token_list_inner(Ok(ImportKeyword), u8s, List.append(token_list, Ok(ImportKeyword)))

        [105, 110, 115, 116, 097, 110, 099, 101, 111, 102, .. as u8s] -> # instanceof
            utf8_list_to_ts_token_list_inner(Ok(InstanceOfKeyword), u8s, List.append(token_list, Ok(InstanceOfKeyword)))

        [105, 110, .. as u8s] -> # in
            utf8_list_to_ts_token_list_inner(Ok(InKeyword), u8s, List.append(token_list, Ok(InKeyword)))

        [110, 101, 119, .. as u8s] -> # new
            utf8_list_to_ts_token_list_inner(Ok(NewKeyword), u8s, List.append(token_list, Ok(NewKeyword)))

        [110, 117, 108, 108, .. as u8s] -> # null
            utf8_list_to_ts_token_list_inner(Ok(NullKeyword), u8s, List.append(token_list, Ok(NullKeyword)))

        [114, 101, 116, 117, 114, 110, .. as u8s] -> # return
            utf8_list_to_ts_token_list_inner(Ok(ReturnKeyword), u8s, List.append(token_list, Ok(ReturnKeyword)))

        [115, 117, 112, 101, 114, .. as u8s] -> # super
            utf8_list_to_ts_token_list_inner(Ok(SuperKeyword), u8s, List.append(token_list, Ok(SuperKeyword)))

        [115, 119, 105, 116, 099, 104, .. as u8s] -> # switch
            utf8_list_to_ts_token_list_inner(Ok(SwitchKeyword), u8s, List.append(token_list, Ok(SwitchKeyword)))

        [116, 104, 105, 115, .. as u8s] -> # this
            utf8_list_to_ts_token_list_inner(Ok(ThisKeyword), u8s, List.append(token_list, Ok(ThisKeyword)))

        [116, 104, 114, 111, 119, .. as u8s] -> # throw
            utf8_list_to_ts_token_list_inner(Ok(ThrowKeyword), u8s, List.append(token_list, Ok(ThrowKeyword)))

        [116, 114, 117, 101, .. as u8s] -> # true
            utf8_list_to_ts_token_list_inner(Ok(TrueKeyword), u8s, List.append(token_list, Ok(TrueKeyword)))

        [116, 114, 121, .. as u8s] -> # try
            utf8_list_to_ts_token_list_inner(Ok(TryKeyword), u8s, List.append(token_list, Ok(TryKeyword)))

        [116, 121, 112, 101, 111, 102, .. as u8s] -> # typeof
            utf8_list_to_ts_token_list_inner(Ok(TypeOfKeyword), u8s, List.append(token_list, Ok(TypeOfKeyword)))

        [118, 097, 114, .. as u8s] -> # var
            utf8_list_to_ts_token_list_inner(Ok(VarKeyword), u8s, List.append(token_list, Ok(VarKeyword)))

        [118, 111, 105, 100, .. as u8s] -> # void
            utf8_list_to_ts_token_list_inner(Ok(VoidKeyword), u8s, List.append(token_list, Ok(VoidKeyword)))

        [119, 104, 105, 108, 101, .. as u8s] -> # while
            utf8_list_to_ts_token_list_inner(Ok(WhileKeyword), u8s, List.append(token_list, Ok(WhileKeyword)))

        [119, 105, 116, 104, .. as u8s] -> # with
            utf8_list_to_ts_token_list_inner(Ok(WithKeyword), u8s, List.append(token_list, Ok(WithKeyword)))

        [097, 115, .. as u8s] -> # as
            utf8_list_to_ts_token_list_inner(Ok(AsKeyword), u8s, List.append(token_list, Ok(AsKeyword)))

        [105, 109, 112, 108, 101, 109, 101, 110, 116, 115, .. as u8s] -> # implements
            utf8_list_to_ts_token_list_inner(Ok(ImplementsKeyword), u8s, List.append(token_list, Ok(ImplementsKeyword)))

        [105, 110, 116, 101, 114, 102, 097, 099, 101, .. as u8s] -> # interface
            utf8_list_to_ts_token_list_inner(Ok(InterfaceKeyword), u8s, List.append(token_list, Ok(InterfaceKeyword)))

        [108, 101, 116, .. as u8s] -> # let
            utf8_list_to_ts_token_list_inner(Ok(LetKeyword), u8s, List.append(token_list, Ok(LetKeyword)))

        [112, 097, 099, 107, 097, 103, 101, .. as u8s] -> # package
            utf8_list_to_ts_token_list_inner(Ok(PackageKeyword), u8s, List.append(token_list, Ok(PackageKeyword)))

        [112, 114, 105, 118, 097, 116, 101, .. as u8s] -> # private
            utf8_list_to_ts_token_list_inner(Ok(PrivateKeyword), u8s, List.append(token_list, Ok(PrivateKeyword)))

        [112, 114, 111, 116, 101, 099, 116, 101, 100, .. as u8s] -> # protected
            utf8_list_to_ts_token_list_inner(Ok(ProtectedKeyword), u8s, List.append(token_list, Ok(ProtectedKeyword)))

        [112, 117, 098, 108, 105, 099, .. as u8s] -> # public
            utf8_list_to_ts_token_list_inner(Ok(PublicKeyword), u8s, List.append(token_list, Ok(PublicKeyword)))

        [115, 116, 097, 116, 105, 099, .. as u8s] -> # static
            utf8_list_to_ts_token_list_inner(Ok(StaticKeyword), u8s, List.append(token_list, Ok(StaticKeyword)))

        [121, 105, 101, 108, 100, .. as u8s] -> # yield
            utf8_list_to_ts_token_list_inner(Ok(YieldKeyword), u8s, List.append(token_list, Ok(YieldKeyword)))

        [097, 110, 121, .. as u8s] -> # any
            utf8_list_to_ts_token_list_inner(Ok(AnyKeyword), u8s, List.append(token_list, Ok(AnyKeyword)))

        [098, 111, 111, 108, 101, 097, 110, .. as u8s] -> # boolean
            utf8_list_to_ts_token_list_inner(Ok(BooleanKeyword), u8s, List.append(token_list, Ok(BooleanKeyword)))

        [099, 111, 110, 115, 116, 114, 117, 099, 116, 111, 114, .. as u8s] -> # constructor
            utf8_list_to_ts_token_list_inner(Ok(ConstructorKeyword), u8s, List.append(token_list, Ok(ConstructorKeyword)))

        [100, 101, 099, 108, 097, 114, 101, .. as u8s] -> # declare
            utf8_list_to_ts_token_list_inner(Ok(DeclareKeyword), u8s, List.append(token_list, Ok(DeclareKeyword)))

        [103, 101, 116, .. as u8s] -> # get
            utf8_list_to_ts_token_list_inner(Ok(GetKeyword), u8s, List.append(token_list, Ok(GetKeyword)))

        [109, 111, 100, 117, 108, 101, .. as u8s] -> # module
            utf8_list_to_ts_token_list_inner(Ok(ModuleKeyword), u8s, List.append(token_list, Ok(ModuleKeyword)))

        [114, 101, 113, 117, 105, 114, 101, .. as u8s] -> # require
            utf8_list_to_ts_token_list_inner(Ok(RequireKeyword), u8s, List.append(token_list, Ok(RequireKeyword)))

        [110, 117, 109, 098, 101, 114, .. as u8s] -> # number
            utf8_list_to_ts_token_list_inner(Ok(NumberKeyword), u8s, List.append(token_list, Ok(NumberKeyword)))

        [115, 101, 116, .. as u8s] -> # set
            utf8_list_to_ts_token_list_inner(Ok(SetKeyword), u8s, List.append(token_list, Ok(SetKeyword)))

        [115, 116, 114, 105, 110, 103, .. as u8s] -> # string
            utf8_list_to_ts_token_list_inner(Ok(StringKeyword), u8s, List.append(token_list, Ok(StringKeyword)))

        [115, 121, 109, 098, 111, 108, .. as u8s] -> # symbol
            utf8_list_to_ts_token_list_inner(Ok(SymbolKeyword), u8s, List.append(token_list, Ok(SymbolKeyword)))

        [116, 121, 112, 101, .. as u8s] -> # type
            utf8_list_to_ts_token_list_inner(Ok(TypeKeyword), u8s, List.append(token_list, Ok(TypeKeyword)))

        [102, 114, 111, 109, .. as u8s] -> # from
            utf8_list_to_ts_token_list_inner(Ok(FromKeyword), u8s, List.append(token_list, Ok(FromKeyword)))

        [111, 102, .. as u8s] -> # of
            utf8_list_to_ts_token_list_inner(Ok(OfKeyword), u8s, List.append(token_list, Ok(OfKeyword)))

        [097, 115, 121, 110, 099, .. as u8s] -> # async
            utf8_list_to_ts_token_list_inner(Ok(AsyncKeyword), u8s, List.append(token_list, Ok(AsyncKeyword)))

        [097, 119, 097, 105, 116, .. as u8s] -> # await
            utf8_list_to_ts_token_list_inner(Ok(AwaitKeyword), u8s, List.append(token_list, Ok(AwaitKeyword)))

        # --- Trivia: Newlines and Whitespace ---
        [10, .. as u8s] -> # \n
            utf8_list_to_ts_token_list_inner(Ok(NewLineTrivia), u8s, List.append(token_list, Ok(NewLineTrivia)))

        [13, 10, .. as u8s] -> # \r\n
            utf8_list_to_ts_token_list_inner(Ok(NewLineTrivia), u8s, List.append(token_list, Ok(NewLineTrivia)))

        [u8, .. as u8s] if u8 == 32 or u8 == 9 or u8 == 11 or u8 == 12 -> # Space, Tab, VT, FF
            # Consume all contiguous whitespace
            consume_whitespace = |bytes|
                when bytes is
                    [b, .. as rest] if b == 32 or b == 9 or b == 11 or b == 12 -> consume_whitespace(rest)
                    # Check for non-breaking space (U+00A0 = C2 A0) - Optional
                    [194, 160, .. as rest] -> consume_whitespace(rest)
                    _ -> bytes
            rest_after_whitespace = consume_whitespace(u8s)
            utf8_list_to_ts_token_list_inner(
                Ok(WhitespaceTrivia),
                rest_after_whitespace,
                List.append(token_list, Ok(WhitespaceTrivia)),
            )

        # --- Trivia: Comments ---
        [47, 47, .. as u8s_after_slash_slash] -> # // Single Line Comment
            # Assume process_line_comment_text consumes until newline and returns the rest
            { token_result, u8s_after_comment } = process_line_comment_text(u8s_after_slash_slash)
            utf8_list_to_ts_token_list_inner(
                Ok(LineCommentStart),
                u8s_after_comment,
                List.append(token_list, token_result),
            ) # Should return Ok(SingleLineCommentTrivia)

        [47, 42, .. as u8s_after_slash_star] -> # /* Multi Line Comment */
            # Assume process_block_comment_text consumes until */ and returns the rest
            { token_result, u8s_after_comment } = process_block_comment_text(u8s_after_slash_star)
            utf8_list_to_ts_token_list_inner(
                token_result,
                u8s_after_comment,
                List.append(token_list, token_result),
            ) # Should return Ok(MultiLineCommentTrivia)
        # --- Literals ---

        [34, .. as u8s_after_quote] -> # " String Literal
            { token_result, u8s_after_str_literal } = process_string_literal(u8s_after_quote, "\"") # Pass quote char
            utf8_list_to_ts_token_list_inner(
                token_result,
                u8s_after_str_literal,
                List.append(token_list, token_result),
            ) # Placeholder prev_token

        [39, .. as u8s_after_quote] -> # ' String Literal
            { token_result, u8s_after_str_literal } = process_string_literal(u8s_after_quote, "'") # Pass quote char
            utf8_list_to_ts_token_list_inner(
                token_result,
                u8s_after_str_literal,
                List.append(token_list, token_result),
            ) # Placeholder prev_token

        [96, .. as u8s_after_backtick] -> # ` Template Literal (TODO: Fix this. We need to support template literals)
            { token_result, u8s_after_str_literal } = process_string_literal(u8s_after_backtick, "`") # Pass quote char
            utf8_list_to_ts_token_list_inner(
                token_result,
                u8s_after_str_literal,
                List.append(token_list, token_result),
            ) # Placeholder prev_token
        # { token: template_token_res, rest: rest_after_template } = process_template_literal(u8s_after_backtick)
        # # prev_token needs to be the actual token variant here
        # current_token =
        #     when template_token_res is
        #         Ok(tok) -> tok
        #         Err(_) -> Unknown # Or handle error appropriately
        # utf8_list_to_ts_token_list_inner(current_token, rest_after_template, List.append(token_list, template_token_res))
        # --- Punctuation and Operators (Longest First) ---

        [46, 46, 46, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(DotDotDotToken), u8s, List.append(token_list, Ok(DotDotDotToken)))
        [63, 46, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(QuestionDotToken), u8s, List.append(token_list, Ok(QuestionDotToken)))
        [42, 42, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AsteriskAsteriskEqualsToken), u8s, List.append(token_list, Ok(AsteriskAsteriskEqualsToken)))
        [42, 42, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AsteriskAsteriskToken), u8s, List.append(token_list, Ok(AsteriskAsteriskToken)))
        [61, 61, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(EqualsEqualsEqualsToken), u8s, List.append(token_list, Ok(EqualsEqualsEqualsToken)))
        [33, 61, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(ExclamationEqualsEqualsToken), u8s, List.append(token_list, Ok(ExclamationEqualsEqualsToken)))
        [61, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(EqualsEqualsToken), u8s, List.append(token_list, Ok(EqualsEqualsToken)))
        [33, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(ExclamationEqualsToken), u8s, List.append(token_list, Ok(ExclamationEqualsToken)))
        [61, 62, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(EqualsGreaterThanToken), u8s, List.append(token_list, Ok(EqualsGreaterThanToken)))
        [43, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(PlusEqualsToken), u8s, List.append(token_list, Ok(PlusEqualsToken)))
        [45, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(MinusEqualsToken), u8s, List.append(token_list, Ok(MinusEqualsToken)))
        [42, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AsteriskEqualsToken), u8s, List.append(token_list, Ok(AsteriskEqualsToken)))
        [47, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(SlashEqualsToken), u8s, List.append(token_list, Ok(SlashEqualsToken)))
        [37, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(PercentEqualsToken), u8s, List.append(token_list, Ok(PercentEqualsToken)))
        [60, 60, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(LessThanLessThanEqualsToken), u8s, List.append(token_list, Ok(LessThanLessThanEqualsToken)))
        [62, 62, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(GreaterThanGreaterThanEqualsToken), u8s, List.append(token_list, Ok(GreaterThanGreaterThanEqualsToken)))
        [62, 62, 62, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(GreaterThanGreaterThanGreaterThanEqualsToken), u8s, List.append(token_list, Ok(GreaterThanGreaterThanGreaterThanEqualsToken)))
        [38, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AmpersandEqualsToken), u8s, List.append(token_list, Ok(AmpersandEqualsToken)))
        [124, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(BarEqualsToken), u8s, List.append(token_list, Ok(BarEqualsToken)))
        [94, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(CaretEqualsToken), u8s, List.append(token_list, Ok(CaretEqualsToken)))
        [38, 38, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AmpersandAmpersandEqualsToken), u8s, List.append(token_list, Ok(AmpersandAmpersandEqualsToken)))
        [124, 124, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(BarBarEqualsToken), u8s, List.append(token_list, Ok(BarBarEqualsToken)))
        [63, 63, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(QuestionQuestionEqualsToken), u8s, List.append(token_list, Ok(QuestionQuestionEqualsToken)))
        [60, 60, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(LessThanLessThanToken), u8s, List.append(token_list, Ok(LessThanLessThanToken)))
        [62, 62, 62, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(GreaterThanGreaterThanGreaterThanToken), u8s, List.append(token_list, Ok(GreaterThanGreaterThanGreaterThanToken)))
        [62, 62, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(GreaterThanGreaterThanToken), u8s, List.append(token_list, Ok(GreaterThanGreaterThanToken)))
        [60, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(LessThanEqualsToken), u8s, List.append(token_list, Ok(LessThanEqualsToken)))
        [62, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(GreaterThanEqualsToken), u8s, List.append(token_list, Ok(GreaterThanEqualsToken)))
        [38, 38, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AmpersandAmpersandToken), u8s, List.append(token_list, Ok(AmpersandAmpersandToken)))
        [124, 124, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(BarBarToken), u8s, List.append(token_list, Ok(BarBarToken)))
        [63, 63, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(QuestionQuestionToken), u8s, List.append(token_list, Ok(QuestionQuestionToken)))
        [43, 43, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(PlusPlusToken), u8s, List.append(token_list, Ok(PlusPlusToken)))
        [45, 45, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(MinusMinusToken), u8s, List.append(token_list, Ok(MinusMinusToken)))
        [60, 47, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(LessThanSlashToken), u8s, List.append(token_list, Ok(LessThanSlashToken))) # For JSX
        # --- Single Character Punctuation and Operators ---
        [123, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(OpenBraceToken), u8s, List.append(token_list, Ok(OpenBraceToken)))
        [125, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(CloseBraceToken), u8s, List.append(token_list, Ok(CloseBraceToken)))
        [40, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(OpenParenToken), u8s, List.append(token_list, Ok(OpenParenToken)))
        [41, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(CloseParenToken), u8s, List.append(token_list, Ok(CloseParenToken)))
        [91, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(OpenBracketToken), u8s, List.append(token_list, Ok(OpenBracketToken)))
        [93, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(CloseBracketToken), u8s, List.append(token_list, Ok(CloseBracketToken)))
        [46, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(DotToken), u8s, List.append(token_list, Ok(DotToken)))
        [59, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(SemicolonToken), u8s, List.append(token_list, Ok(SemicolonToken)))
        [44, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(CommaToken), u8s, List.append(token_list, Ok(CommaToken)))
        [60, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(LessThanToken), u8s, List.append(token_list, Ok(LessThanToken)))
        [62, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(GreaterThanToken), u8s, List.append(token_list, Ok(GreaterThanToken)))
        [61, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(EqualsToken), u8s, List.append(token_list, Ok(EqualsToken)))
        [43, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(PlusToken), u8s, List.append(token_list, Ok(PlusToken)))
        [45, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(MinusToken), u8s, List.append(token_list, Ok(MinusToken)))
        [42, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AsteriskToken), u8s, List.append(token_list, Ok(AsteriskToken)))
        [47, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(SlashToken), u8s, List.append(token_list, Ok(SlashToken))) # Needs context for Regex
        [37, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(PercentToken), u8s, List.append(token_list, Ok(PercentToken)))
        [38, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AmpersandToken), u8s, List.append(token_list, Ok(AmpersandToken)))
        [124, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(BarToken), u8s, List.append(token_list, Ok(BarToken)))
        [94, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(CaretToken), u8s, List.append(token_list, Ok(CaretToken)))
        [33, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(ExclamationToken), u8s, List.append(token_list, Ok(ExclamationToken)))
        [126, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(TildeToken), u8s, List.append(token_list, Ok(TildeToken)))
        [63, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(QuestionToken), u8s, List.append(token_list, Ok(QuestionToken)))
        [58, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(ColonToken), u8s, List.append(token_list, Ok(ColonToken)))
        [64, .. as u8s] -> utf8_list_to_ts_token_list_inner(Ok(AtToken), u8s, List.append(token_list, Ok(AtToken)))
        # Backtick and Hash are primarily JSDoc or handled differently (template literal start, private identifier start)
        # [96, .. as u8s] -> ... handled by template literal case ...
        # [35, .. as u8s] -> ... handled by identifier case ...
        # --- Numeric Literal ---
        [u8, .. as ignored] if is_digit(u8) ->
            { token_result, u8s_after_num_literal } = process_numeric_literal(u8, u8_list)
            utf8_list_to_ts_token_list_inner(
                token_result,
                u8s_after_num_literal,
                List.append(token_list, token_result),
            )

        # --- Identifier or Keyword or PrivateIdentifier ---
        # Check for # first for PrivateIdentifier
        [35, first_char, .. as u8s_after_first_char] ->
            # Assuming process_identifier handles the # prefix case
            { token_result, u8s_after_ident } = process_identifier(first_char, u8s_after_first_char)
            utf8_list_to_ts_token_list_inner(token_result, u8s_after_ident, List.append(token_list, token_result))

        [first_char, .. as u8s_after_first_char] if is_identifier_start(first_char) ->
            { token_result, u8s_after_ident } = process_identifier(first_char, u8s_after_first_char)
            utf8_list_to_ts_token_list_inner(token_result, u8s_after_ident, List.append(token_list, token_result))

        # --- Comments ---
        [47, 47, .. as rest] -> # Line comment (//)
            { token_result, u8s_after_comment } = process_line_comment_text(rest)
            utf8_list_to_ts_token_list_inner(
                token_result,
                u8s_after_comment,
                token_list |> List.append(Ok(LineCommentStart)),
            )

        [47, 42, .. as rest] -> # Block comment start (/*)
            { token_result, u8s_after_comment } = process_block_comment_text(rest)
            utf8_list_to_ts_token_list_inner(
                token_result,
                u8s_after_comment,
                token_list
                |> List.append(Ok(BlockCommentStart))
                |> List.append(token_result)
                |> List.append(Ok(BlockCommentEnd)),
            )

        # --- Other Trivia ---
        [35, 33, .. as u8s_after_shebang] -> # Shebang #!
            # Consume until newline or EOF
            consume_shebang = |bytes|
                when bytes is
                    [10, .. as rest] -> rest # Stop at \n
                    [13, 10, .. as rest] -> rest # Stop at \r\n
                    [_, .. as rest] -> consume_shebang(rest)
                    [] -> []
            rest_after_shebang = consume_shebang(u8s_after_shebang)
            utf8_list_to_ts_token_list_inner(Ok(ShebangTrivia), rest_after_shebang, List.append(token_list, Ok(ShebangTrivia)))

        # ConflictMarkerTrivia and NonTextFileMarkerTrivia are more complex/specific, omitted for brevity here
        # but would follow similar patterns if needed.
        # --- Unknown Character ---
        [_, .. as u8s] -> # Catch-all for unrecognized bytes
            utf8_list_to_ts_token_list_inner(Err(Unknown), u8s, List.append(token_list, Ok(Unknown)))

# Function to parse line comments (//)
process_line_comment_text :
    List U8
    -> {
        token_result : TsTokenResult,
        u8s_after_comment : List U8,
    }
process_line_comment_text = |u8s|
    consume_until_newline = |comment_text_u8s, remaining_u8s|
        when remaining_u8s is
            [10, .. as rest] -> (List.append(comment_text_u8s, 10), rest) # Stop at newline (\n)
            [13, 10, .. as rest] -> (comment_text_u8s |> List.append(13) |> List.append(10), rest) # Stop at \r\n
            [next_u8, .. as rest] -> consume_until_newline(List.append(comment_text_u8s, next_u8), rest) # Continue
            [] -> (comment_text_u8s, []) # End of input
    (comment_text, u8s_after_comment) = consume_until_newline([], u8s)
    token_result : TsTokenResult
    token_result = Ok(CommentText(Str.from_utf8_lossy(comment_text)))
    {
        token_result,
        u8s_after_comment,
    }

# Process entire block comment after `BlockCommentStart` (`/*`) token until `BlockCommentBlockEnd` (`*/`), with `CommentText` in the middle
process_block_comment_text :
    List U8
    ->
    {
        token_result : TsTokenResult,
        u8s_after_comment : List U8,
    }
process_block_comment_text = |u8s|
    # inner_process : List U8, List U8 -> { token_result : TsTokenResult, u8s_after_comment : List U8 }
    inner_process = |current_u8s, acc|
        when current_u8s is
            [42, 47, .. as rest] -> # "*/" ends comment
                comment_result = Str.from_utf8(acc)
                when comment_result is
                    Ok(comment) ->
                        token_result : TsTokenResult
                        token_result = Ok(CommentText(comment))
                        {
                            token_result,
                            u8s_after_comment: rest,
                        }

                    Err(_) ->
                        {
                            token_result: Err(Unknown),
                            u8s_after_comment: current_u8s,
                        }

            [u8, .. as rest] ->
                inner_process(rest, List.append(acc, u8))

            [] -> # Unclosed block comment
                comment_result = Str.from_utf8(acc)
                when comment_result is
                    Ok(comment) ->
                        token_result : TsTokenResult
                        token_result = Ok(CommentText(comment))
                        {
                            token_result,
                            u8s_after_comment: current_u8s,
                        }

                    Err(_) ->
                        {
                            token_result: Err(Unknown),
                            u8s_after_comment: current_u8s,
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
        token_result : TsTokenResult,
        u8s_after_ident : List U8,
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

    token_result : TsTokenResult
    token_result =
        when ident_result is
            Ok(ident) -> Ok(Identifier(ident))
            Err(_) -> Err(Unknown)
    { token_result, u8s_after_ident: new_remaining }

# Changed return type: returns the consumed sequence, the rest of the input,
# AND a Result indicating if the consumed sequence was valid.

collect_numeric_chars : List U8, List U8, Bool, Bool -> { consumed : List U8, rest : List U8, status : Result {} [InvalidNumericSeparator] }
collect_numeric_chars = |acc, remaining, has_decimal, has_exp|
    # Tail-recursive inner helper
    # Keeps track of validity status internally
    inner_collect : List U8, List U8, Bool, Bool, List U8, Result {} [InvalidNumericSeparator] -> { consumed : List U8, rest : List U8, status : Result {} [InvalidNumericSeparator] }
    inner_collect = |current_acc, current_remaining, current_decimal, current_exp, final_acc, current_status|
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
                new_status = if current_status == Ok {} and !is_valid_separator then Err(InvalidNumericSeparator) else current_status

                # Consume the underscore regardless of validity and continue
                inner_collect(List.append(current_acc, 95), rest_after_underscore, current_decimal, current_exp, final_acc, new_status)

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
                new_status = if current_status == Ok {} and !is_valid_decimal then Err(InvalidNumericSeparator) else current_status

                # Consume the decimal point regardless and continue
                inner_collect(List.append(current_acc, 46), rest_chars, Bool.true, current_exp, final_acc, new_status)

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
                new_status = if current_status == Ok {} and !is_valid_exponent then Err(InvalidNumericSeparator) else current_status

                # Consume the exponent char regardless and continue
                inner_collect(List.append(current_acc, exp_char), rest_chars, current_decimal, Bool.true, final_acc, new_status)

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
                new_status = if current_status == Ok {} and !is_valid_sign then Err(InvalidNumericSeparator) else current_status

                # Consume the sign char regardless and continue
                inner_collect(List.append(current_acc, sign_char), rest_chars, current_decimal, current_exp, final_acc, new_status)

            # --- Digits '0'-'9' ---
            [u8, .. as rest_chars] if is_digit(u8) ->
                # Just consume and continue, pass status along
                inner_collect(List.append(current_acc, u8), rest_chars, current_decimal, current_exp, final_acc, current_status)

            # --- End of numeric literal (any other character or end of input) ---
            _ ->
                # Final check: last character cannot be an underscore
                final_status =
                    when List.last(current_acc) is
                        Ok(95) if current_status == Ok {} -> Err(InvalidNumericSeparator) # Mark error if trailing underscore found AND no prior error
                        _ -> current_status # Keep existing status (Ok or prior Err)

                # Return the fully consumed sequence, the actual remaining input, and the final validity status
                { consumed: List.concat(final_acc, current_acc), rest: current_remaining, status: final_status }

    # Initial call to the inner helper, starting with Ok status
    inner_collect(acc, remaining, has_decimal, has_exp, [], Ok({}))

# Refactored function to process numeric literals using the modified collect_numeric_chars
process_numeric_literal :
    U8,
    List U8
    -> {
        token_result : TsTokenResult,
        u8s_after_num_literal : List U8,
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

            # It should theoretically not be empty if first_digit was valid, but check anyway
            if List.is_empty(num_chars) then
                { token_result: Err(Unknown), u8s_after_num_literal: new_remaining }
            else
                # Try converting the valid sequence to string
                num_result = Str.from_utf8(num_chars)
                when num_result is
                    Ok(num_str) ->
                        # Successfully created NumericLiteral token
                        { token_result: Ok(NumericLiteral(num_str)), u8s_after_num_literal: new_remaining }

                    Err(_) ->
                        # UTF8 error unlikely but possible? Treat as unknown.
                        { token_result: Err(Unknown), u8s_after_num_literal: new_remaining }

        Err(InvalidNumericSeparator) ->
            # Invalid number sequence due to separator rules
            { token_result: Err(InvalidNumericSeparator), u8s_after_num_literal: collection_result.rest }

process_string_literal : List U8, Str -> { token_result : TsTokenResult, u8s_after_str_literal : List U8 }
process_string_literal = |u8s, quote_type|
    inner_process : List U8, Str, List U8 -> { token_result : TsTokenResult, u8s_after_str_literal : List U8 }
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
                str_result = Str.from_utf8(acc)
                when str_result is
                    Ok(str) ->
                        { token_result: Ok(StringLiteral(str)), u8s_after_str_literal: rest }

                    Err(_) ->
                        { token_result: Err(Unknown), u8s_after_str_literal: rest }

            [39, .. as rest] if current_quote_type == "'" -> # single quote
                str_result = Str.from_utf8(acc)
                when str_result is
                    Ok(str) ->
                        { token_result: Ok(StringLiteral(str)), u8s_after_str_literal: rest }

                    Err(_) ->
                        { token_result: Err(Unknown), u8s_after_str_literal: rest }

            # Collect string content
            [u8, .. as rest] ->
                inner_process(rest, current_quote_type, List.append(acc, u8))

            # Unclosed string
            [] ->
                { token_result: Err(UnclosedString), u8s_after_str_literal: current_u8s }
    inner_process(u8s, quote_type, [])

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

cc = {
    slash: 47,
    asterisk: 42,
    backslash: 92,
    lf: 10,
    cr: 13,
    space: 32,
    tab: 9,
    underscore: 95,
    dollar: 36,
    zero: 48,
    nine: 57,
    a: 97,
    z: 122,
    cap_A: 65,
    cap_Z: 90,
    openParen: 40,
    closeParen: 41,
    openBrace: 123,
    closeBrace: 125,
    openBracket: 91,
    closeBracket: 93,
    lessThan: 60,
    greaterThan: 62,
    equals: 61,
    plus: 43,
    minus: 45,
    percent: 37,
    ampersand: 38,
    bar: 124,
    caret: 94,
    tilde: 126,
    exclamation: 33,
    question: 63,
    comma: 44,
    dot: 46,
    semicolon: 59,
    colon: 58,
    singleQuote: 39,
    doubleQuote: 34,
    backtick: 96,
    hash: 35,
    at: 64,
}

# break
# case
# catch
# class
# const
# continue
# debugger
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
# function
# if
# import
# in
# instanceof
# new
# null
# return
# super
# switch
# this
# throw
# true
# try
# typeof
# var
# void
# while
# with
# as
# implements
# interface
# let
# package
# private
# protected
# public
# static
# yield
# any
# boolean
# constructor
# declare
# get
# module
# require
# number
# set
# string
# symbol
# type
# from
# of
# async
# await
#

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
    SingleLineCommentTrivia,
    MultiLineCommentTrivia,
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
    NoSubstitutionTemplateLiteral Str,
    # Pseudo-literals
    TemplateHead,
    TemplateMiddle,
    TemplateTail,
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
        SingleLineCommentTrivia -> "SingleLineCommentTrivia"
        MultiLineCommentTrivia -> "MultiLineCommentTrivia"
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
        NoSubstitutionTemplateLiteral(str) -> "NoSubstitutionTemplateLiteral(${str})"
        # Pseudo-literals
        TemplateHead(str) -> "TemplateHead(${str})"
        TemplateMiddle(str) -> "TemplateMiddle(${str})"
        TemplateTail(str) -> "TemplateTail(${str})"
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
        Identifier Str -> "Identifier"
        PrivateIdentifier Str -> "PrivateIdentifier"
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
    utf8_list_to_ts_token_list_inner(Start, u8_list_, [])

# Main recursive tokenizer function with accumulator
utf8_list_to_ts_token_list_inner : TsToken, List U8, List TsTokenResult -> List TsTokenResult
utf8_list_to_ts_token_list_inner = |_prev_token, u8_list, token_list| # prev_token often not needed in this style
    when u8_list is
        # --- End of File ---
        [] -> List.append(token_list, Ok(EndOfFileToken))
        # --- Trivia: Newlines and Whitespace ---
        [10, .. as u8s] -> # \n
            utf8_list_to_ts_token_list_inner(NewLineTrivia, u8s, List.append(token_list, Ok(NewLineTrivia)))

        [13, 10, .. as u8s] -> # \r\n
            utf8_list_to_ts_token_list_inner(NewLineTrivia, u8s, List.append(token_list, Ok(NewLineTrivia)))

        [u8, .. as u8s] if u8 == 32 or u8 == 9 or u8 == 11 or u8 == 12 -> # Space, Tab, VT, FF
            # Consume all contiguous whitespace
            consume_whitespace = |bytes|
                when bytes is
                    [b, .. as rest] if b == 32 or b == 9 or b == 11 or b == 12 -> consume_whitespace(rest)
                    # Check for non-breaking space (U+00A0 = C2 A0) - Optional
                    [194, 160, .. as rest] -> consume_whitespace(rest)
                    _ -> bytes
            rest_after_whitespace = consume_whitespace(u8s)
            utf8_list_to_ts_token_list_inner(WhitespaceTrivia, rest_after_whitespace, List.append(token_list, Ok(WhitespaceTrivia)))

        # --- Trivia: Comments ---
        [47, 47, .. as u8s_after_slash_slash] -> # // Single Line Comment
            # Assume process_line_comment consumes until newline and returns the rest
            { token: comment_token_res, rest: rest_after_comment } = process_line_comment(u8s_after_slash_slash)
            utf8_list_to_ts_token_list_inner(SingleLineCommentTrivia, rest_after_comment, List.append(token_list, comment_token_res)) # Should return Ok(SingleLineCommentTrivia)

        [47, 42, .. as u8s_after_slash_star] -> # /* Multi Line Comment */
            # Assume process_block_comment consumes until */ and returns the rest
            { token: comment_token_res, rest: rest_after_comment } = process_block_comment(u8s_after_slash_star)
            utf8_list_to_ts_token_list_inner(MultiLineCommentTrivia, rest_after_comment, List.append(token_list, comment_token_res)) # Should return Ok(MultiLineCommentTrivia)
        # --- Literals ---

        [34, .. as u8s_after_quote] -> # " String Literal
            { token: str_token_res, rest: rest_after_str } = process_string_literal(u8s_after_quote, 34) # Pass quote char
            utf8_list_to_ts_token_list_inner(StringLiteral(""), rest_after_str, List.append(token_list, str_token_res)) # Placeholder prev_token

        [39, .. as u8s_after_quote] -> # ' String Literal
            { token: str_token_res, rest: rest_after_str } = process_string_literal(u8s_after_quote, 39) # Pass quote char
            utf8_list_to_ts_token_list_inner(StringLiteral(""), rest_after_str, List.append(token_list, str_token_res)) # Placeholder prev_token

        [96, .. as u8s_after_backtick] -> # ` Template Literal
            { token: template_token_res, rest: rest_after_template } = process_template_literal(u8s_after_backtick)
            # prev_token needs to be the actual token variant here
            current_token =
                when template_token_res is
                    Ok(tok) -> tok
                    Err(_) -> Unknown # Or handle error appropriately
            utf8_list_to_ts_token_list_inner(current_token, rest_after_template, List.append(token_list, template_token_res))

        # --- Punctuation and Operators (Longest First) ---
        [46, 46, 46, .. as u8s] -> utf8_list_to_ts_token_list_inner(DotDotDotToken, u8s, List.append(token_list, Ok(DotDotDotToken)))
        [63, 46, .. as u8s] -> utf8_list_to_ts_token_list_inner(QuestionDotToken, u8s, List.append(token_list, Ok(QuestionDotToken)))
        [42, 42, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(AsteriskAsteriskEqualsToken, u8s, List.append(token_list, Ok(AsteriskAsteriskEqualsToken)))
        [42, 42, .. as u8s] -> utf8_list_to_ts_token_list_inner(AsteriskAsteriskToken, u8s, List.append(token_list, Ok(AsteriskAsteriskToken)))
        [61, 61, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(EqualsEqualsEqualsToken, u8s, List.append(token_list, Ok(EqualsEqualsEqualsToken)))
        [33, 61, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(ExclamationEqualsEqualsToken, u8s, List.append(token_list, Ok(ExclamationEqualsEqualsToken)))
        [61, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(EqualsEqualsToken, u8s, List.append(token_list, Ok(EqualsEqualsToken)))
        [33, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(ExclamationEqualsToken, u8s, List.append(token_list, Ok(ExclamationEqualsToken)))
        [61, 62, .. as u8s] -> utf8_list_to_ts_token_list_inner(EqualsGreaterThanToken, u8s, List.append(token_list, Ok(EqualsGreaterThanToken)))
        [43, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(PlusEqualsToken, u8s, List.append(token_list, Ok(PlusEqualsToken)))
        [45, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(MinusEqualsToken, u8s, List.append(token_list, Ok(MinusEqualsToken)))
        [42, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(AsteriskEqualsToken, u8s, List.append(token_list, Ok(AsteriskEqualsToken)))
        [47, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(SlashEqualsToken, u8s, List.append(token_list, Ok(SlashEqualsToken)))
        [37, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(PercentEqualsToken, u8s, List.append(token_list, Ok(PercentEqualsToken)))
        [60, 60, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(LessThanLessThanEqualsToken, u8s, List.append(token_list, Ok(LessThanLessThanEqualsToken)))
        [62, 62, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(GreaterThanGreaterThanEqualsToken, u8s, List.append(token_list, Ok(GreaterThanGreaterThanEqualsToken)))
        [62, 62, 62, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(GreaterThanGreaterThanGreaterThanEqualsToken, u8s, List.append(token_list, Ok(GreaterThanGreaterThanGreaterThanEqualsToken)))
        [38, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(AmpersandEqualsToken, u8s, List.append(token_list, Ok(AmpersandEqualsToken)))
        [124, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(BarEqualsToken, u8s, List.append(token_list, Ok(BarEqualsToken)))
        [94, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(CaretEqualsToken, u8s, List.append(token_list, Ok(CaretEqualsToken)))
        [38, 38, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(AmpersandAmpersandEqualsToken, u8s, List.append(token_list, Ok(AmpersandAmpersandEqualsToken)))
        [124, 124, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(BarBarEqualsToken, u8s, List.append(token_list, Ok(BarBarEqualsToken)))
        [63, 63, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(QuestionQuestionEqualsToken, u8s, List.append(token_list, Ok(QuestionQuestionEqualsToken)))
        [60, 60, .. as u8s] -> utf8_list_to_ts_token_list_inner(LessThanLessThanToken, u8s, List.append(token_list, Ok(LessThanLessThanToken)))
        [62, 62, 62, .. as u8s] -> utf8_list_to_ts_token_list_inner(GreaterThanGreaterThanGreaterThanToken, u8s, List.append(token_list, Ok(GreaterThanGreaterThanGreaterThanToken)))
        [62, 62, .. as u8s] -> utf8_list_to_ts_token_list_inner(GreaterThanGreaterThanToken, u8s, List.append(token_list, Ok(GreaterThanGreaterThanToken)))
        [60, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(LessThanEqualsToken, u8s, List.append(token_list, Ok(LessThanEqualsToken)))
        [62, 61, .. as u8s] -> utf8_list_to_ts_token_list_inner(GreaterThanEqualsToken, u8s, List.append(token_list, Ok(GreaterThanEqualsToken)))
        [38, 38, .. as u8s] -> utf8_list_to_ts_token_list_inner(AmpersandAmpersandToken, u8s, List.append(token_list, Ok(AmpersandAmpersandToken)))
        [124, 124, .. as u8s] -> utf8_list_to_ts_token_list_inner(BarBarToken, u8s, List.append(token_list, Ok(BarBarToken)))
        [63, 63, .. as u8s] -> utf8_list_to_ts_token_list_inner(QuestionQuestionToken, u8s, List.append(token_list, Ok(QuestionQuestionToken)))
        [43, 43, .. as u8s] -> utf8_list_to_ts_token_list_inner(PlusPlusToken, u8s, List.append(token_list, Ok(PlusPlusToken)))
        [45, 45, .. as u8s] -> utf8_list_to_ts_token_list_inner(MinusMinusToken, u8s, List.append(token_list, Ok(MinusMinusToken)))
        [60, 47, .. as u8s] -> utf8_list_to_ts_token_list_inner(LessThanSlashToken, u8s, List.append(token_list, Ok(LessThanSlashToken))) # For JSX
        # --- Single Character Punctuation and Operators ---
        [123, .. as u8s] -> utf8_list_to_ts_token_list_inner(OpenBraceToken, u8s, List.append(token_list, Ok(OpenBraceToken)))
        [125, .. as u8s] -> utf8_list_to_ts_token_list_inner(CloseBraceToken, u8s, List.append(token_list, Ok(CloseBraceToken)))
        [40, .. as u8s] -> utf8_list_to_ts_token_list_inner(OpenParenToken, u8s, List.append(token_list, Ok(OpenParenToken)))
        [41, .. as u8s] -> utf8_list_to_ts_token_list_inner(CloseParenToken, u8s, List.append(token_list, Ok(CloseParenToken)))
        [91, .. as u8s] -> utf8_list_to_ts_token_list_inner(OpenBracketToken, u8s, List.append(token_list, Ok(OpenBracketToken)))
        [93, .. as u8s] -> utf8_list_to_ts_token_list_inner(CloseBracketToken, u8s, List.append(token_list, Ok(CloseBracketToken)))
        [46, .. as u8s] -> utf8_list_to_ts_token_list_inner(DotToken, u8s, List.append(token_list, Ok(DotToken)))
        [59, .. as u8s] -> utf8_list_to_ts_token_list_inner(SemicolonToken, u8s, List.append(token_list, Ok(SemicolonToken)))
        [44, .. as u8s] -> utf8_list_to_ts_token_list_inner(CommaToken, u8s, List.append(token_list, Ok(CommaToken)))
        [60, .. as u8s] -> utf8_list_to_ts_token_list_inner(LessThanToken, u8s, List.append(token_list, Ok(LessThanToken)))
        [62, .. as u8s] -> utf8_list_to_ts_token_list_inner(GreaterThanToken, u8s, List.append(token_list, Ok(GreaterThanToken)))
        [61, .. as u8s] -> utf8_list_to_ts_token_list_inner(EqualsToken, u8s, List.append(token_list, Ok(EqualsToken)))
        [43, .. as u8s] -> utf8_list_to_ts_token_list_inner(PlusToken, u8s, List.append(token_list, Ok(PlusToken)))
        [45, .. as u8s] -> utf8_list_to_ts_token_list_inner(MinusToken, u8s, List.append(token_list, Ok(MinusToken)))
        [42, .. as u8s] -> utf8_list_to_ts_token_list_inner(AsteriskToken, u8s, List.append(token_list, Ok(AsteriskToken)))
        [47, .. as u8s] -> utf8_list_to_ts_token_list_inner(SlashToken, u8s, List.append(token_list, Ok(SlashToken))) # Needs context for Regex
        [37, .. as u8s] -> utf8_list_to_ts_token_list_inner(PercentToken, u8s, List.append(token_list, Ok(PercentToken)))
        [38, .. as u8s] -> utf8_list_to_ts_token_list_inner(AmpersandToken, u8s, List.append(token_list, Ok(AmpersandToken)))
        [124, .. as u8s] -> utf8_list_to_ts_token_list_inner(BarToken, u8s, List.append(token_list, Ok(BarToken)))
        [94, .. as u8s] -> utf8_list_to_ts_token_list_inner(CaretToken, u8s, List.append(token_list, Ok(CaretToken)))
        [33, .. as u8s] -> utf8_list_to_ts_token_list_inner(ExclamationToken, u8s, List.append(token_list, Ok(ExclamationToken)))
        [126, .. as u8s] -> utf8_list_to_ts_token_list_inner(TildeToken, u8s, List.append(token_list, Ok(TildeToken)))
        [63, .. as u8s] -> utf8_list_to_ts_token_list_inner(QuestionToken, u8s, List.append(token_list, Ok(QuestionToken)))
        [58, .. as u8s] -> utf8_list_to_ts_token_list_inner(ColonToken, u8s, List.append(token_list, Ok(ColonToken)))
        [64, .. as u8s] -> utf8_list_to_ts_token_list_inner(AtToken, u8s, List.append(token_list, Ok(AtToken)))
        # Backtick and Hash are primarily JSDoc or handled differently (template literal start, private identifier start)
        # [96, .. as u8s] -> ... handled by template literal case ...
        # [35, .. as u8s] -> ... handled by identifier case ...
        # --- Numeric Literal ---
        [u8, .. as ignored] if is_digit(u8) ->
            { token: num_token_res, rest: rest_after_num } = process_numeric_literal(u8_list)
            current_token =
                when num_token_res is
                    # Get token for prev_token arg
                    Ok(tok) -> tok
                    Err(_) -> Unknown
            utf8_list_to_ts_token_list_inner(current_token, rest_after_num, List.append(token_list, num_token_res))

        # --- Identifier or Keyword or PrivateIdentifier ---
        # Check for # first for PrivateIdentifier
        [35, .. as u8s_after_hash] ->
            # Assuming process_identifier handles the # prefix case
            { token: ident_token_res, rest: rest_after_ident } = process_identifier(u8_list)
            current_token =
                when ident_token_res is
                    Ok(tok) -> tok
                    Err(_) -> Unknown
            utf8_list_to_ts_token_list_inner(current_token, rest_after_ident, List.append(token_list, ident_token_res))

        [u8, .. as ignored] if is_identifier_start(u8) ->
            { token: ident_token_res, rest: rest_after_ident } = process_identifier(u8_list)
            current_token =
                when ident_token_res is
                    Ok(tok) -> tok
                    Err(_) -> Unknown
            utf8_list_to_ts_token_list_inner(current_token, rest_after_ident, List.append(token_list, ident_token_res))

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
            utf8_list_to_ts_token_list_inner(ShebangTrivia, rest_after_shebang, List.append(token_list, Ok(ShebangTrivia)))

        # ConflictMarkerTrivia and NonTextFileMarkerTrivia are more complex/specific, omitted for brevity here
        # but would follow similar patterns if needed.
        # --- Unknown Character ---
        [_, .. as u8s] -> # Catch-all for unrecognized bytes
            utf8_list_to_ts_token_list_inner(Unknown, u8s, List.append(token_list, Ok(Unknown)))

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
    inner_collect(acc, remaining, has_decimal, has_exp, [], Ok {})

# Refactored function to process numeric literals using the modified collect_numeric_chars
process_numeric_literal : U8, List U8, List TsTokenResult -> List TsTokenResult
process_numeric_literal = |first_digit, rest, token_list|
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
                utf8_list_to_ts_token_list_inner(Unknown, new_remaining, List.append(token_list, Err(Unknown)))
            else
                # Try converting the valid sequence to string
                num_result = Str.from_utf8(num_chars)
                when num_result is
                    Ok(num_str) ->
                        # Successfully created NumLit token
                        utf8_list_to_ts_token_list_inner(NumLit(num_str), new_remaining, List.append(token_list, Ok(NumLit(num_str))))

                    Err(_) ->
                        # UTF8 error unlikely but possible? Treat as unknown.
                        utf8_list_to_ts_token_list_inner(Unknown, new_remaining, List.append(token_list, Err(Unknown)))

        Err(InvalidNumericSeparator) ->
            # Invalid number sequence due to separator rules
            # Add the error token
            updated_token_list = List.append(token_list, Err(InvalidNumericSeparator))
            # Continue tokenizing AFTER the consumed invalid sequence
            utf8_list_to_ts_token_list_inner(Unknown, collection_result.rest, updated_token_list)

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

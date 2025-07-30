module []

Logical a : [
    Not a,
    And List a,
    Or List a,
    Xor List a,
]

TypeFacts a : [
    IsBoolean,
    IsNumber,
    IsString,
    IsNull,
    IsUndefined,
    IsRegExp,
    IsBigInt,
    IsSymbol,
    IsObject,
    IsFunction,
    IsArray,
    IsTypedArray,
    IsConcrete,
    IsAny,
    IsUnknown,
    HasProps (Dict Str a),
]

Ast : [
    Node {
            kind : [
                # Program structure
                Program,
                ModuleDeclaration,
                # Statements
                ExpressionStatement,
                BlockStatement,
                IfStatement (Ast, Ast),
                ForStatement,
                WhileStatement,
                ReturnStatement,
                FunctionDeclaration,
                VariableDeclaration,
                #
                # Expressions
                BinaryExpression,
                UnaryExpression,
                CallExpression,
                MemberExpression,
                ConditionalExpression,

                # Literals
                Identifier,
                NumericLiteral,
                StringLiteral,
                BooleanLiteral,
                NullLiteral,

                # TypeScript specific
                TypeAnnotation,
                InterfaceDeclaration,

                # Other
                Error,
            ],
            # loc : Location,
            id : U64,
            type : Logical TypeFacts Ast,

        },
]


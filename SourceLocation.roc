module [
    WithSourceLocation,
    SourceLocation,
    WithPosition,
    Position,
]

WithPosition x : { line : U64, column : U64 }x

Position : WithPosition {}

WithSourceLocation x : { source : Str, start : Position, end : Position, byte_index : U32 }x

SourceLocation : WithSourceLocation {}

module [
    WithSourceLocation,
    SourceLocation,
    WithPosition,
    Position,
]

WithPosition x : { line : U32, column : U32 }x

Position : WithPosition {}

WithSourceLocation x : { source : Str, start : Position, end : Position, byte_index : U32 }x

SourceLocation : WithSourceLocation {}

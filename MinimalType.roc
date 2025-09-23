module [
    TType,
    mk_num,
    mk_str,
    mk_bool,
    type_str,
]

TType : [
    TNum,
    TStr,
    TBool,
    TUnknown,
]

mk_num : TType
mk_num = TNum

mk_str : TType
mk_str = TStr

mk_bool : TType
mk_bool = TBool

type_str : TType -> Str
type_str = \t ->
    when t is
        TNum -> "number"
        TStr -> "string"
        TBool -> "boolean"
        TUnknown -> "unknown"
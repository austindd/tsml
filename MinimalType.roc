module [
    TType,
    mk_num,
    mk_str,
    mk_bool,
    mk_unknown,
    type_str,
    is_num,
    is_str,
    is_bool,
    is_unknown,
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

mk_unknown : TType
mk_unknown = TUnknown

type_str : TType -> Str
type_str = \t ->
    when t is
        TNum -> "number"
        TStr -> "string"
        TBool -> "boolean"
        TUnknown -> "unknown"

is_num : TType -> Bool
is_num = \t ->
    when t is
        TNum -> Bool.true
        _ -> Bool.false

is_str : TType -> Bool
is_str = \t ->
    when t is
        TStr -> Bool.true
        _ -> Bool.false

is_bool : TType -> Bool
is_bool = \t ->
    when t is
        TBool -> Bool.true
        _ -> Bool.false

is_unknown : TType -> Bool
is_unknown = \t ->
    when t is
        TUnknown -> Bool.true
        _ -> Bool.false
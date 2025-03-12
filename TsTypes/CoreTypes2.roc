module [
    TConstraint,
    Property,
    TvarStore,
]

import Stack exposing [Stack]
import SymTbl exposing [SymTbl, SymKey]
import SymTblStack exposing [SymTblStack]

TConstraint : [
    # TypeOfTerm Str,
    Tvar SymKey,
    DeclaredType Str,
    Boolean,
    Number,
    Struct (Dict Str Property),
    Union (List TConstraint),
    Intersection (List TConstraint),
    Unknown,
    Never,
]

TSubtype : [
    Tvar SymKey,
]

TStruct : [
    Struct (Dict Str Property),
]

TUnion : [
    Union (List TConstraint),
]

TIntersection : [
    Intersection (List TConstraint),
]

TIntersection2 : [
    Intersection {
            merged_struct : Result TStruct {},
            constraints : List TConstraint,
        },
]

Property : {
    name : Str,
    type : SymKey,
}

TvarStore := SymTblStack TConstraint

TypeMap := Dict Str TConstraint

Env := {
    tvar_store : TvarStore,
    type_map : TypeMap,
    may_throw : Result TConstraint {},
}

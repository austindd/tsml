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

type_map_add_type : TypeMap, Str, TConstraint -> TypeMap
type_map_add_type = |@TypeMap(type_map), type_name, t_constraint|
    type_map |> Dict.insert(type_name, t_constraint) |> @TypeMap

type_map_remove_type : TypeMap, Str -> TypeMap
type_map_remove_type = |@TypeMap(type_map), type_name|
    type_map |> Dict.remove(type_name) |> @TypeMap

Env := {
    tvar_store : TvarStore,
    type_map : TypeMap,
    may_throw : Result TConstraint {},
}

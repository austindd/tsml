module [
    TypeId,
    TConstraint,
    Kind,
    Property,
    TypeScope,
    TypeCtx,
]

import Stack exposing [Stack]
import Option exposing [Option, Some, None]

TypeId := U64
    implements [
        Eq,
        Hash,
    ]

TermId := U64
    implements [
        Eq,
        Hash,
    ]

TConstraint : [
    TypeOfTerm TermId Kind, # A type that is the type of a term with a specific kind
    SubtypeOf TypeId Kind, # A type that is a subtype of another type with a specific kind
    Boolean Kind, # Boolean type with its kind
    Number Kind, # Number type with its kind
    Struct (Dict Str Property) Kind, # Record type with properties and its kind
    Union (List TConstraint) Kind, # Union type with its kind
    Intersection (List TConstraint) Kind, # Intersection type with its kind
    TypeConstructor (List TypeId) Kind, # Type constructor with parameters and its kind
    TypeVariable Str Kind, # Type variable with its kind
    Unknown Kind, # Unknown type with its kind
    Never Kind, # Never type with its kind
]

Kind : [
    KStar, # Kind of all types
    KArrow Kind Kind, # Kind of type constructors
]

TSubtype : [
    SubtypeOf TypeId Kind,
]

TStruct : [
    Struct (Dict Str Property) Kind,
]

TUnion : [
    Union (List TConstraint) Kind,
]

TIntersection : [
    Intersection (List TConstraint) Kind,
]

TIntersection2 : [
    Intersection {
            mergedStruct : Option TStruct,
            constraints : List TConstraint,
        } Kind,
]

Property : {
    name : Str,
    typeId : TypeId,
}

TypeScope : Dict TypeId TConstraint

TermScope : Dict TermId TConstraint

TypeCtx := {
    lastId : TypeId,
    scopeStack : Stack.Stack TypeScope,
}

TermCtx := {
    lastId : TermId,
    scopeStack : Stack.Stack TermScope,
}

getTypeFromScope : TypeScope, TypeId -> Result TConstraint [TypeNotFoundInScope]
getTypeFromScope = \typeScope, typeId ->
    when Dict.get typeScope typeId is
        Ok constraint -> Ok constraint
        Err KeyNotFound -> Err TypeNotFoundInScope

getTypeFromCtx : TypeCtx, TypeId -> Result TConstraint [TypeNotFoundInCtx]
getTypeFromCtx = \@TypeCtx { scopeStack }, typeId ->
    initialState : Result TConstraint [NotFound]
    initialState = Err NotFound
    res = Stack.walkFromTopUntil
        scopeStack
        initialState
        (\state, typeScope ->
            typeResult = getTypeFromScope typeScope typeId
            when typeResult is
                Ok type -> Break (Ok type)
                Err _ -> Continue (Err NotFound)
        )
    Result.mapErr res (\_ -> TypeNotFoundInCtx)

getTermFromScope : TermScope, TermId -> Result TConstraint [TermNotFoundInScope]
getTermFromScope = \termScope, termId ->
    when Dict.get termScope termId is
        Ok constraint -> Ok constraint
        Err KeyNotFound -> Err TermNotFoundInScope

getTermFromCtx : TermCtx, TermId -> Result TConstraint [TermNotFoundInCtx]
getTermFromCtx = \@TermCtx { scopeStack }, termId ->
    initialState : Result TConstraint [NotFound]
    initialState = Err NotFound
    res = Stack.walkFromTopUntil
        scopeStack
        initialState
        (\state, termScope ->
            termResult = getTermFromScope termScope termId
            when termResult is
                Ok term -> Break (Ok term)
                Err _ -> Continue (Err NotFound)
        )
    Result.mapErr res (\_ -> TermNotFoundInCtx)

constraintIsSubtypeOf : TermCtx, TypeCtx, TConstraint, TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
constraintIsSubtypeOf = \termCtx, typeCtx, constraintA, constraintB ->
    pair : (TConstraint, TConstraint)
    pair = (constraintA, constraintB)
    when pair is
        (cA, Boolean _) -> isSubtypeOfBoolean termCtx typeCtx cA
        (cA, Number _) -> isSubtypeOfNumber termCtx typeCtx cA
        (cA, SubtypeOf typeIdB _) -> isSubtypeOfTypeId termCtx typeCtx cA typeIdB
        (cA, Struct structB kindB) -> isSubtypeOfStruct termCtx typeCtx cA (Struct structB kindB)
        (Union constraintsA _, Union constraintsB _) -> Err SubtypeMismatch # TODO: Implement
        (Intersection constraintsA _, Intersection constraintsB _) -> Err SubtypeMismatch # TODO: Implement
        (TypeConstructor paramsA _, TypeConstructor paramsB _) -> Err SubtypeMismatch # TODO: Implement
        (TypeVariable nameA _, TypeVariable nameB _) ->
            when nameA == nameB is
                true -> Ok {}
                false -> Err SubtypeMismatch

        (Unknown _, Unknown _) -> Ok {}
        _ -> Err SubtypeMismatch

isSubtypeOfBoolean : TermCtx, TypeCtx, TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isSubtypeOfBoolean = \termCtx, typeCtx, constraint ->
    when constraint is
        Boolean _ -> Ok {}
        SubtypeOf typeId _ ->
            innerResult = getTypeFromCtx typeCtx typeId
            when innerResult is
                Ok inner -> isSubtypeOfBoolean termCtx typeCtx inner
                Err _ -> Err TypeNotFoundInCtx

        _ -> Err SubtypeMismatch

isSubtypeOfNumber : TermCtx, TypeCtx, TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isSubtypeOfNumber = \termCtx, typeCtx, constraint ->
    when constraint is
        Number _ -> Ok {}
        SubtypeOf typeId _ ->
            innerResult = getTypeFromCtx typeCtx typeId
            when innerResult is
                Ok inner -> isSubtypeOfNumber termCtx typeCtx inner
                Err _ -> Err TypeNotFoundInCtx

        _ -> Err SubtypeMismatch

isSubtypeOfTypeId : TermCtx, TypeCtx, TConstraint, TypeId -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isSubtypeOfTypeId = \termCtx, typeCtx, constraint, typeId ->
    when constraint is
        SubtypeOf innerId _ ->
            innerResult = getTypeFromCtx typeCtx innerId
            when innerResult is
                Ok inner -> constraintIsSubtypeOf termCtx typeCtx inner (SubtypeOf typeId KStar)
                Err _ -> Err TypeNotFoundInCtx

        _ -> Err SubtypeMismatch

isTypeIdSubtypeOfTypeId : TermCtx, TypeCtx, TypeId, TypeId -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isTypeIdSubtypeOfTypeId = \termCtx, typeCtx, typeIdA, typeIdB ->
    typeAResult = getTypeFromCtx typeCtx typeIdA
    typeBResult = getTypeFromCtx typeCtx typeIdB
    when typeAResult is
        Ok typeA ->
            when typeBResult is
                Ok typeB -> constraintIsSubtypeOf termCtx typeCtx typeA typeB
                Err _ -> Err TypeNotFoundInCtx

        Err _ -> Err TypeNotFoundInCtx

isSubtypeOfStruct : TermCtx, TypeCtx, TConstraint, TStruct -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isSubtypeOfStruct = \termCtx, typeCtx, constraint, Struct structB _ ->
    when constraint is
        Struct structA _ ->
            Dict.walk
                structB
                (Ok {})
                (\prevResult, keyB, propB ->
                    propAResult = Dict.get structA keyB
                    when propAResult is
                        Ok propA ->
                            propATypeId = propA.typeId
                            propATypeResult = getTypeFromCtx typeCtx propATypeId
                            when propATypeResult is
                                Err _ -> Err TypeNotFoundInCtx
                                Ok propAType ->
                                    isSubtypeOfTypeId termCtx typeCtx propAType propB.typeId

                        Err _ -> Err SubtypeMismatch
                )

        _ -> Err SubtypeMismatch

isSubtypeOfSubtype : TermCtx, TypeCtx, TConstraint, TSubtype -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isSubtypeOfSubtype = \termCtx, typeCtx, constraint, SubtypeOf typeIdB _ ->
    isSubtypeOfTypeId termCtx typeCtx constraint typeIdB

isSubtypeOfUnion : TermCtx, TypeCtx, TConstraint, List TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isSubtypeOfUnion = \termCtx, typeCtx, constraintA, unionB ->
    when constraintA is
        Union unionA _ ->
            isSatisfied = List.any unionB \cB ->
                List.all unionA \cA ->
                    when constraintIsSubtypeOf termCtx typeCtx cA cB is
                        Ok _ -> Bool.true
                        Err _ -> Bool.false

            when isSatisfied is
                true -> Ok {}
                false -> Err SubtypeMismatch

        _ -> Ok {}

# A union is satisfied by a constraint if the constraint is a subtype of any of the constraints in the union
# An intersection is satisfied by a constraint if the constraint is a subtype of all of the constraints in the intersection

isSubtypeOfIntersection : TermCtx, TypeCtx, TConstraint, List TConstraint -> Result {} [TypeNotFoundInCtx, TermNotFoundInCtx, SubtypeMismatch]
isSubtypeOfIntersection = \termCtx, typeCtx, constraintA, intersectionB ->
    when constraintA is
        Union unionA _ ->
            isSatisfied = List.all intersectionB \cB ->
                List.any unionA \cA ->
                    when constraintIsSubtypeOf termCtx typeCtx cA cB is
                        Ok _ -> Bool.true
                        Err _ -> Bool.false
            when isSatisfied is
                true -> Ok {}
                false -> Err SubtypeMismatch

        _ -> Err SubtypeMismatch

mergeStructs :
    TermCtx,
    TypeCtx,
    TStruct,
    TStruct
    -> {
        struct : TStruct,
        typeCtx : TypeCtx,
        termCtx : TermCtx,
    }
mergeStructs = \termCtx, typeCtx, Struct structA kindA, Struct structB kindB ->
    if (kindA == kindB) || (kindA == KStar) || (kindB == KStar) then
        highestKind =
            when kindA == KStar is
                true -> kindB
                false -> kindA
        largestSize = ((Dict.len structA) + (Dict.len structB))
        initialState = Ok {
            struct: structA,
            typeCtx: typeCtx,
        }
        finalStateResult = Dict.walk structB initialState \stateResult, key, propB ->
            propAResult = Dict.get structA key
            when stateResult is
                Err _ -> stateResult
                Ok state ->
                    when propAResult is
                        Ok propA ->
                            propATypeResult = getTypeFromCtx typeCtx propA.typeId
                            when propATypeResult is
                                Err _ -> Ok state
                                Ok propAType ->
                                    propBTypeResult = getTypeFromCtx typeCtx propB.typeId
                                    when propBTypeResult is
                                        Err _ -> Ok state
                                        Ok propBType ->
                                            when constraintIsSubtypeOf termCtx typeCtx propAType propBType is
                                                Ok _ ->
                                                    newType = Union [propAType, propBType] highestKind
                                                    (newTypeCtx, freshTypeId) = registerType typeCtx newType
                                                    newProp = {
                                                        name: key,
                                                        typeId: freshTypeId,
                                                    }
                                                    newStruct = Dict.insert state.struct key newProp
                                                    Ok {
                                                        struct: newStruct,
                                                        typeCtx: newTypeCtx,
                                                    }

                                                Err _ -> Ok state

                        Err _ ->
                            Ok {
                                struct: Dict.insert state.struct key propB,
                                typeCtx: state.typeCtx,
                            }
        when finalStateResult is
            Ok finalState ->
                finalStruct = {
                    struct: Struct finalState.struct highestKind,
                    typeCtx: finalState.typeCtx,
                    termCtx: termCtx,
                }
                finalStruct

            Err _ ->
                crash "Failed to merge structs"
    else
        crash "Mismatched kinds"
# if (kindA == kindB) || (kindA == KStar) || (kindB == KStar) then
#    largestSize = ((Dict.len structA) + (Dict.len structB))
#    newStruct = Dict.walk structB structA \result, key, propB ->
#        Dict.update result key \propAResult ->
#            when propAResult is
#                Ok propA ->
#                    when isTypeIdSubtypeOfTypeId termCtx typeCtx propA.typeId propB.typeId is
#                        Ok _ ->
#                            Ok (Union [propA, propB])

#                        Err _ -> propAResult

#                Err _ -> Ok propB
#    Struct newStruct kindB
# else
#    crash "Mismatched kinds"

registerType : TypeCtx, TConstraint -> (TypeCtx, TypeId)
registerType = \@TypeCtx { lastId: @TypeId lastId_, scopeStack }, constraint ->
    nextId = @TypeId (lastId_ + 1)
    nearestScope = Stack.peek scopeStack
    newScopeStack =
        when nearestScope is
            Ok scope ->
                newScope = Dict.insert scope nextId constraint
                (Stack.pop scopeStack) |> Stack.push newScope

            Err _ -> Stack.push scopeStack (Dict.single nextId constraint)

    (@TypeCtx { lastId: nextId, scopeStack: newScopeStack }, nextId)

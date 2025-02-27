module []

import Stack
import ListMap

TypeId := U64
    implements [
        Eq,
        Hash,
    ]

TypeTag : [
    Boolean,
    Number,
    Struct { props : Dict Str TypeId },
    ConstraintList (List ConstraintTag),
    Existential,
]

StructTag : [
    Struct {
            props : Dict Str TypeId,
        },
]

ConstraintTag : [
    SubtypeOf TypeId,
    EqualTo TypeId,
]

TypeScope : Dict TypeId TypeTag

TypeEnv := {
    lastId : TypeId,
    scopeStack : Stack.Stack TypeScope,
}

TypeEvalResult : {
    typeEnv : TypeEnv,
    typeId : TypeId,
    type : TypeTag,
}

TypeError : {
    actual : TypeTag,
    expected : TypeTag,
    message : Str,
}

registerType : TypeEnv, TypeTag -> (TypeEnv, TypeId)
registerType = \@TypeEnv { lastId: @TypeId lastId_, scopeStack }, typeTag ->
    nextId = @TypeId (lastId_ + 1)
    nearestScope = Stack.peek scopeStack
    newScopeStack =
        when nearestScope is
            Ok scope ->
                newScope = Dict.insert scope nextId typeTag
                (Stack.pop scopeStack) |> Stack.push newScope

            Err _ -> Stack.push scopeStack (Dict.single nextId typeTag)

    (@TypeEnv { lastId: nextId, scopeStack: newScopeStack }, nextId)

getTypeFromScope : TypeScope, TypeId -> Result TypeTag [NotFound]
getTypeFromScope = \typeScope, typeId ->
    when Dict.get typeScope typeId is
        Ok typeTag -> Ok typeTag
        Err KeyNotFound -> Err NotFound

getTypeFromEnv : TypeEnv, TypeId -> Result TypeTag [NotFound]
getTypeFromEnv = \@TypeEnv { scopeStack }, typeId ->
    initialState : Result TypeTag [NotFound]
    initialState = Err NotFound
    Stack.walkFromTopUntil
        scopeStack
        initialState
        (\state, typeScope ->
            result = getTypeFromScope typeScope typeId
            when result is
                Ok _ -> Break result
                Err _ -> Continue result
        )

pushEmptyScopeToEnv : TypeEnv -> TypeEnv
pushEmptyScopeToEnv = \@TypeEnv { lastId, scopeStack } ->
    @TypeEnv { lastId, scopeStack: Stack.push scopeStack (Dict.empty {}) }

popScopeFromEnv : TypeEnv -> TypeEnv
popScopeFromEnv = \@TypeEnv { lastId, scopeStack } ->
    @TypeEnv { lastId, scopeStack: Stack.pop scopeStack }

# Constraint Solving
typeIdSatisfiesTypeId : TypeEnv, TypeId, TypeId -> Result {} {}
typeIdSatisfiesTypeId = \typeEnv, typeIdA, typeIdB ->
    if typeIdA == typeIdB then
        Ok {}
    else
        crash "Not Implemented: typeIdSatisfiesTypeId"

typeTagSatisfiesBoolean : TypeTag, [Boolean] -> Result {} {}
typeTagSatisfiesBoolean = \typeA, typeB ->
    when typeA is
        Boolean -> Ok {}
        _ -> Err {}

typeIdSatisfiesBoolean : TypeEnv, TypeId, [Boolean] -> Result {} {}
typeIdSatisfiesBoolean = \typeEnv, typeIdA, typeB ->
    when getTypeFromEnv typeEnv typeIdA is
        Ok typeA -> typeTagSatisfiesBoolean typeA typeB
        Err err -> Err {}

typeTagSatisfiesNumber : TypeTag, [Number] -> Result {} {}
typeTagSatisfiesNumber = \typeA, typeB ->
    when typeA is
        Number -> Ok {}
        _ -> Err {}

typeIdSatisfiesNumber : TypeEnv, TypeId, [Number] -> Result {} {}
typeIdSatisfiesNumber = \typeEnv, typeIdA, typeB ->
    when getTypeFromEnv typeEnv typeIdA is
        Ok typeA -> typeTagSatisfiesNumber typeA typeB
        Err err -> Err {}

structTagSatisfiesStruct : TypeEnv, StructTag, StructTag -> Result {} {}
structTagSatisfiesStruct = \typeEnv, Struct { props: propsA }, Struct { props: propsB } ->
    Dict.walk
        propsB
        (Ok {})
        (\prevResult, keyB, propB ->
            currentResult =
                when Dict.get propsA keyB is
                    Ok propA ->
                        typeIdSatisfiesTypeId typeEnv propA propB

                    Err err -> Err {}
            when currentResult is
                Ok _ -> prevResult
                Err err ->
                    when prevResult is
                        Ok _ -> Err {}
                        Err prevErrors -> Err {})

typeIdSatisfiesStruct : TypeEnv, TypeId, StructTag -> Result {} {}
typeIdSatisfiesStruct = \typeEnv, typeIdA, typeB ->
    when getTypeFromEnv typeEnv typeIdA is
        Ok typeA ->
            when typeA is
                Struct structA -> structTagSatisfiesStruct typeEnv (Struct structA) typeB
                _ -> Err {}

        Err err -> Err {}

typeTagSatisfiesExistential : TypeTag, [Existential] -> Result {} {}
typeTagSatisfiesExistential = \typeA, typeB ->
    Ok {}

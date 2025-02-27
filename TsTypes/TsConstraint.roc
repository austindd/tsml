module [
]

import Stack exposing [Stack]

TypeId := U64
    implements [
        Eq,
        Hash,
    ]

# Constraint : [
#    SubtypeOf TypeId,
#    Boolean,
#    Number,
#    HasProperties List Property,
#    Union (List Constraint),
#    Intersection (List Constraint),
#    Unknown,
# ]

Constraint : [
    SubtypeOf TypeId Kind, # A type that is a subtype of another type with a specific kind
    Boolean Kind, # Boolean type with its kind
    Number Kind, # Number type with its kind
    HasProperties (List Property) Kind, # Record type with properties and its kind
    Union (List Constraint) Kind, # Union type with its kind
    Intersection (List Constraint) Kind, # Intersection type with its kind
    TypeConstructor (List TypeId) Kind, # Type constructor with parameters and its kind
    TypeVariable Str Kind, # Type variable with its kind
    Unknown Kind, # Unknown type with its kind
]

Kind : [
    KStar, # Kind of all types
    KArrow Kind Kind, # Kind of type constructors
]

Property : {
    name : Str,
    type : TypeId,
}

ConstraintSet := List Constraint

TypeScope : Dict TypeId Constraint

TypeCtx := {
    lastId : TypeId,
    scopeStack : Stack.Stack TypeScope,
}

getTypeFromScope : TypeScope, TypeId -> Result Constraint [NotFound]
getTypeFromScope = \typeScope, typeId ->
    when Dict.get typeScope typeId is
        Ok constraint -> Ok constraint
        Err KeyNotFound -> Err NotFound

getTypeFromCtx : TypeCtx, TypeId -> Result Constraint [NotFound]
getTypeFromCtx = \@TypeCtx { scopeStack }, typeId ->
    initialState : Result Constraint [NotFound]
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

pushEmptyScopeToCtx : TypeCtx -> TypeCtx
pushEmptyScopeToCtx = \@TypeCtx { lastId, scopeStack } ->
    @TypeCtx { lastId, scopeStack: Stack.push scopeStack (Dict.empty {}) }

popScopeFromCtx : TypeCtx -> TypeCtx
popScopeFromCtx = \@TypeCtx { lastId, scopeStack } ->
    @TypeCtx { lastId, scopeStack: Stack.pop scopeStack }

# inferKind : TypeCtx, Constraint -> Result Kind [KindError]
# inferKind = \ctx, constraint ->
#    when constraint is
#        Boolean -> Ok KStar
#        Number -> Ok KStar
#        TypeConstructor kind _ -> Ok kind
#        TypeVariable _ kind -> Ok kind
#        TypeApplication typeId args ->
#            when getTypeFromCtx ctx typeId is
#                Ok (TypeConstructor constructorKind _) ->
#                    validateApplication constructorKind args

#                _ -> Err KindError

#        _ -> Ok KStar

# validateApplication : Kind, List TypeId -> Result Kind [KindError]
# validateApplication = \kind, args ->
#    walk = \remainingKind, remainingArgs ->
#        when (remainingKind, remainingArgs) is
#            (KStar, []) -> Ok KStar
#            _ -> Err KindMismatch
# (KArrow (k1 k2), [arg, ...rest]) ->
#    # Verify the argument kind matches k1
#    when validateArgumentKind arg k1 is
#        Ok _ -> walk k2 rest
#        Err e -> Err e
# _ -> Err KindMismatch

# walk kind args

# addBooleanConstraintToConstraintSet : ConstraintSet -> ConstraintSet
# addBooleanConstraintToConstraintSet = \@ConstraintSet constraintSet ->
#    indexResult = List.findFirstIndex
#        constraintSet
#        (\x ->
#            when x is
#                Boolean -> Bool.true
#                _ -> Bool.false)
#    when indexResult is
#        Ok index -> constraintSet
#        Err _ -> List.append constraintSet Boolean

# addNumberConstraintToConstraintSet : ConstraintSet -> ConstraintSet
# addNumberConstraintToConstraintSet = \@ConstraintSet constraintSet ->
#    indexResult = List.findFirstIndex
#        constraintSet
#        (\x ->
#            when x is
#                Number -> Bool.true
#                _ -> Bool.false)
#    when indexResult is
#        Ok index -> constraintSet
#        Err _ -> List.append constraintSet Number

# addSubtypeConstraintToConstraintSet : ConstraintSet, TypeId -> ConstraintSet
# addSubtypeConstraintToConstraintSet = \@ConstraintSet constraintSet, typeId ->
#    indexResult = List.findFirstIndex
#        constraintSet
#        (\x ->
#            when x is
#                SubtypeOf y -> y == typeId
#                _ -> Bool.false)
#    when indexResult is
#        Ok index -> List.set (constraintSet index (SubtypeOf typeId))
#        Err _ -> List.append (constraintSet (SubtypeOf typeId))

# addUnionConstraintToConstraintSet : ConstraintSet, List TypeId -> ConstraintSet
# addUnionConstraintToConstraintSet = \@ConstraintSet constraintSet, typeIds ->
#    crash "Not Implemented: addUnionConstraintToConstraintSet"

# addUnionConstraintToConstraintSet : ConstraintSet, List TypeId -> ConstraintSet
# addUnionConstraintToConstraintSet = \@ConstraintSet constraintSet, typeIds ->
#    indexResult = List.findFirstIndex
#        constraintSet
#        (\x ->
#            when x is
#                Union existingTypes ->
#                    # Maybe merge with existing union if there's overlap
#                    List.all typeIds (\t -> List.contains (existingTypes t))

#                _ -> Bool.false)

#    when indexResult is
#        Ok index -> constraintSet
#        Err _ -> List.append constraintSet (Union typeIds)

# addIntersectionConstraintToConstraintSet : ConstraintSet, List TypeId -> ConstraintSet
# addIntersectionConstraintToConstraintSet = \@ConstraintSet constraintSet, typeIds ->
#    crash "Not Implemented: addIntersectionConstraintToConstraintSet"

# addIntersectionConstraintToConstraintSet : ConstraintSet, List TypeId -> ConstraintSet
# addIntersectionConstraintToConstraintSet = \@ConstraintSet constraintSet, typeIds ->
#    indexResult = List.findFirstIndex
#        constraintSet
#        (\x ->
#            when x is
#                Intersection existingTypes ->
#                    List.all typeIds (\t -> List.contains existingTypes t)

#                _ -> Bool.false)

#    when indexResult is
#        Ok index -> constraintSet
#        Err _ -> List.append constraintSet (Intersection typeIds)

# resolveSubtypeConstraints : TypeCtx, ConstraintSet -> Result ConstraintSet [NotFound]
# resolveSubtypeConstraints = \typeCtx, constraintSet ->
#    List.foldl
#        constraintSet
#        (\state, constraint ->
#            when state is
#                Ok _ ->
#                    when constraint is
#                        SubtypeOf typeId -> addSubtypeConstraintToConstraintSet state typeId
#                        Boolean -> addBooleanConstraintToConstraintSet state
#                        Number -> addNumberConstraintToConstraintSet state
#                        # Union typeIds -> addUnionConstraintToConstraintSet state typeIds
#                        # Intersection typeIds -> addIntersectionConstraintToConstraintSet state typeIds
#                Err _ -> state
#        )
#        (Ok constraintSet)

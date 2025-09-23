module [
    unify,
    solve_constraints,
    Substitution,
    UnificationError,
]

import SimpleComprehensiveType as Type
import Option exposing [Option]

# Type variable
TypeVar : U32

# Extended type with variables for unification
UnifiableType : [
    # Concrete types
    UNumber,
    UString,
    UBoolean,
    UNull,
    UUndefined,
    UAny,
    UUnknown,
    # Type variable
    UVar TypeVar,
    # Compound types
    UArray UnifiableType,
    UObject (List { key: Str, value: UnifiableType }),
    UFunction (List UnifiableType) UnifiableType,
    UUnion (List UnifiableType),
]

# Substitution mapping variables to types
Substitution : List { var: TypeVar, type: UnifiableType }

# Unification error
UnificationError : [
    TypeMismatch UnifiableType UnifiableType,
    OccursCheck TypeVar UnifiableType,
    UnknownVariable TypeVar,
]

# Constraint for unification
Constraint : [
    # t1 = t2
    Equals UnifiableType UnifiableType,
    # t1 is instance of t2
    Instance UnifiableType UnifiableType,
]

# Empty substitution
empty_subst : Substitution
empty_subst = []

# Apply substitution to a type
apply_subst : UnifiableType, Substitution -> UnifiableType
apply_subst = |type, subst|
    when type is
        UVar var ->
            when List.find_first(subst, |s| s.var == var) is
                Ok(s) -> apply_subst(s.type, subst)  # Follow chains
                Err(_) -> UVar var

        UArray elem_type ->
            UArray (apply_subst(elem_type, subst))

        UObject props ->
            UObject (List.map(props, |prop|
                { prop & value: apply_subst(prop.value, subst) }))

        UFunction params return_type ->
            UFunction
                (List.map(params, |p| apply_subst(p, subst)))
                (apply_subst(return_type, subst))

        UUnion types ->
            UUnion (List.map(types, |t| apply_subst(t, subst)))

        _ -> type

# Compose two substitutions
compose_subst : Substitution, Substitution -> Substitution
compose_subst = |s1, s2|
    # Apply s2 to all types in s1
    applied_s1 = List.map(s1, |entry|
        { entry & type: apply_subst(entry.type, s2) })

    # Add entries from s2 that aren't in s1
    new_entries = List.keep_if(s2, |s2_entry|
        Bool.not(List.any(s1, |s1_entry| s1_entry.var == s2_entry.var)))

    List.concat(applied_s1, new_entries)

# Unify two types
unify : UnifiableType, UnifiableType -> Result Substitution UnificationError
unify = |t1, t2|
    unify_with_subst(t1, t2, empty_subst)

# Unify with existing substitution
unify_with_subst : UnifiableType, UnifiableType, Substitution -> Result Substitution UnificationError
unify_with_subst = |t1_orig, t2_orig, subst|
    # Apply current substitution
    t1 = apply_subst(t1_orig, subst)
    t2 = apply_subst(t2_orig, subst)

    when (t1, t2) is
        # Same type
        (UNumber, UNumber) -> Ok(subst)
        (UString, UString) -> Ok(subst)
        (UBoolean, UBoolean) -> Ok(subst)
        (UNull, UNull) -> Ok(subst)
        (UUndefined, UUndefined) -> Ok(subst)
        (UAny, _) -> Ok(subst)
        (_, UAny) -> Ok(subst)
        (UUnknown, _) -> Ok(subst)
        (_, UUnknown) -> Ok(subst)

        # Variable cases
        (UVar v1, UVar v2) ->
            if v1 == v2 then
                Ok(subst)
            else
                Ok(List.append(subst, { var: v1, type: UVar v2 }))

        (UVar v, t) ->
            if occurs_check(v, t) then
                Err(OccursCheck v t)
            else
                Ok(List.append(subst, { var: v, type: t }))

        (t, UVar v) ->
            if occurs_check(v, t) then
                Err(OccursCheck v t)
            else
                Ok(List.append(subst, { var: v, type: t }))

        # Array types
        (UArray elem1, UArray elem2) ->
            unify_with_subst(elem1, elem2, subst)

        # Function types
        (UFunction params1 ret1, UFunction params2 ret2) ->
            if List.len(params1) != List.len(params2) then
                Err(TypeMismatch t1 t2)
            else
                # Unify parameters
                params_result = List.walk(
                    List.map2(params1, params2, |p1, p2| (p1, p2)),
                    Ok(subst),
                    |acc_result, param_pair|
                        when acc_result is
                            Ok(acc_subst) ->
                                (p1, p2) = param_pair
                                unify_with_subst(p1, p2, acc_subst)
                            Err(e) -> Err(e))

                # Unify return types
                when params_result is
                    Ok(params_subst) ->
                        unify_with_subst(ret1, ret2, params_subst)
                    Err(e) -> Err(e)

        # Object types
        (UObject props1, UObject props2) ->
            # For simplicity, require exact match of properties
            unify_object_props(props1, props2, subst)

        # Union types
        (UUnion types, t) ->
            # Try to unify with each type in the union
            # This is simplified - real implementation would be more complex
            if List.any(types, |union_type|
                when unify_with_subst(union_type, t, subst) is
                    Ok(_) -> Bool.true
                    Err(_) -> Bool.false) then
                Ok(subst)
            else
                Err(TypeMismatch t1 t2)

        (t, UUnion types) ->
            if List.any(types, |union_type|
                when unify_with_subst(t, union_type, subst) is
                    Ok(_) -> Bool.true
                    Err(_) -> Bool.false) then
                Ok(subst)
            else
                Err(TypeMismatch t1 t2)

        # Mismatch
        _ -> Err(TypeMismatch t1 t2)

# Check if variable occurs in type (for infinite type detection)
occurs_check : TypeVar, UnifiableType -> Bool
occurs_check = |var, type|
    when type is
        UVar v -> v == var
        UArray elem -> occurs_check(var, elem)
        UFunction params ret ->
            List.any(params, |p| occurs_check(var, p)) ||
            occurs_check(var, ret)
        UObject props ->
            List.any(props, |prop| occurs_check(var, prop.value))
        UUnion types ->
            List.any(types, |t| occurs_check(var, t))
        _ -> Bool.false

# Unify object properties
unify_object_props : List { key: Str, value: UnifiableType }, List { key: Str, value: UnifiableType }, Substitution -> Result Substitution UnificationError
unify_object_props = |props1, props2, subst|
    # Check that all properties match
    if List.len(props1) != List.len(props2) then
        Err(TypeMismatch (UObject props1) (UObject props2))
    else
        List.walk(props1, Ok(subst), |acc_result, prop1|
            when acc_result is
                Ok(acc_subst) ->
                    when List.find_first(props2, |p2| p2.key == prop1.key) is
                        Ok(prop2) ->
                            unify_with_subst(prop1.value, prop2.value, acc_subst)
                        Err(_) ->
                            Err(TypeMismatch (UObject props1) (UObject props2))
                Err(e) -> Err(e))

# Solve a list of constraints
solve_constraints : List Constraint -> Result Substitution UnificationError
solve_constraints = |constraints|
    List.walk(constraints, Ok(empty_subst), |acc_result, constraint|
        when acc_result is
            Ok(acc_subst) ->
                when constraint is
                    Equals t1 t2 ->
                        unify_with_subst(t1, t2, acc_subst)
                    Instance t1 t2 ->
                        # For now, treat instance like equality
                        # Real implementation would handle subtyping
                        unify_with_subst(t1, t2, acc_subst)
            Err(e) -> Err(e))

# Convert from our unifiable type to SimpleComprehensiveType
to_simple_type : UnifiableType -> Type.Type
to_simple_type = |type|
    when type is
        UNumber -> Type.mk_number
        UString -> Type.mk_string
        UBoolean -> Type.mk_boolean
        UNull -> Type.mk_null
        UUndefined -> Type.mk_undefined
        UAny -> Type.mk_any
        UUnknown -> Type.mk_unknown
        UVar _ -> Type.mk_unknown  # Unsolved variable
        UArray elem -> Type.mk_array(to_simple_type(elem))
        UObject props ->
            Type.mk_object(List.map(props, |p|
                { key: p.key, value_type: to_simple_type(p.value) }))
        UFunction _ _ -> Type.mk_unknown  # No function type in SimpleComprehensiveType
        UUnion types -> Type.mk_union(List.map(types, to_simple_type))

# Convert from SimpleComprehensiveType to unifiable type
from_simple_type : Type.Type -> UnifiableType
from_simple_type = |type|
    if Type.is_assignable_to(type, Type.mk_number) then
        UNumber
    else if Type.is_assignable_to(type, Type.mk_string) then
        UString
    else if Type.is_assignable_to(type, Type.mk_boolean) then
        UBoolean
    else if Type.is_assignable_to(type, Type.mk_null) then
        UNull
    else if Type.is_assignable_to(type, Type.mk_undefined) then
        UUndefined
    else if Type.is_assignable_to(type, Type.mk_any) then
        UAny
    else
        UUnknown
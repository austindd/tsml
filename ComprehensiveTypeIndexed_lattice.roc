# Lattice Operations for Type System
# This extends ComprehensiveTypeIndexed with lattice operations

# Check if a type is the top of the lattice (unknown)
is_top : TypeStore, TypeId -> Bool
is_top = \store, type_id ->
    when get_type store type_id is
        Ok TUnknown -> Bool.true
        _ -> Bool.false

# Check if a type is the bottom of the lattice (never)
is_bottom : TypeStore, TypeId -> Bool
is_bottom = \store, type_id ->
    when get_type store type_id is
        Ok TNever -> Bool.true
        _ -> Bool.false

# Subtype relation (partial order for the lattice)
# A <: B means A is a subtype of B
is_subtype_of : TypeStore, TypeId, TypeId -> Bool
is_subtype_of = \store, sub_type, super_type ->
    if sub_type == super_type then
        Bool.true
    else
        when (get_type store sub_type, get_type store super_type) is
            (Ok sub_def, Ok super_def) ->
                is_subtype_of_helper store sub_def super_def
            _ -> Bool.false

is_subtype_of_helper : TypeStore, TypeDef, TypeDef -> Bool
is_subtype_of_helper = \store, sub_def, super_def ->
    when (sub_def, super_def) is
        # Never is subtype of everything
        (TNever, _) -> Bool.true

        # Everything is subtype of unknown
        (_, TUnknown) -> Bool.true

        # Any is supertype of everything except unknown
        (_, TAny) -> Bool.true
        (TAny, _) -> Bool.false

        # Primitive subtyping
        (TNumber, TNumber) -> Bool.true
        (TString, TString) -> Bool.true
        (TBoolean, TBoolean) -> Bool.true
        (TBigInt, TBigInt) -> Bool.true
        (TSymbol, TSymbol) -> Bool.true
        (TNull, TNull) -> Bool.true
        (TUndefined, TUndefined) -> Bool.true
        (TVoid, TVoid) -> Bool.true

        # Literals are subtypes of their base types
        (TLiteral (NumLit _), TNumber) -> Bool.true
        (TLiteral (StrLit _), TString) -> Bool.true
        (TLiteral (BoolLit _), TBoolean) -> Bool.true
        (TLiteral (BigIntLit _), TBigInt) -> Bool.true

        # Array subtyping (covariant)
        (TArray elem_sub, TArray elem_super) ->
            is_subtype_of store elem_sub elem_super

        # Tuple subtyping (width and depth)
        (TTuple sub_types, TTuple super_types) ->
            if List.len(super_types) > List.len(sub_types) then
                Bool.false
            else
                List.map2(super_types, sub_types, \super_t, sub_t ->
                    is_subtype_of store sub_t super_t
                )
                |> List.all(\x -> x)

        # Union subtyping: A <: (B | C) if A <: B or A <: C
        (_, TUnion super_types) ->
            List.any(super_types, \super_t ->
                is_subtype_of store sub_type super_t
            )

        # Union subtyping: (A | B) <: C if A <: C and B <: C
        (TUnion sub_types, _) ->
            List.all(sub_types, \sub_t ->
                is_subtype_of store sub_t super_type
            )

        # Intersection subtyping: (A & B) <: C if A <: C or B <: C
        (TIntersection sub_types, _) ->
            List.any(sub_types, \sub_t ->
                is_subtype_of store sub_t super_type
            )

        # Intersection subtyping: A <: (B & C) if A <: B and A <: C
        (_, TIntersection super_types) ->
            List.all(super_types, \super_t ->
                is_subtype_of store sub_type super_t
            )

        # Function subtyping (contravariant in params, covariant in return)
        (TFunction sub_fn, TFunction super_fn) ->
            # Check return type (covariant)
            return_ok = is_subtype_of store sub_fn.return_type super_fn.return_type

            # Check parameters (contravariant)
            params_ok =
                if List.len(sub_fn.params) != List.len(super_fn.params) then
                    Bool.false
                else
                    List.map2(sub_fn.params, super_fn.params, \sub_p, super_p ->
                        # Note: contravariance - super param type must be subtype of sub param type
                        is_subtype_of store super_p sub_p
                    )
                    |> List.all(\x -> x)

            return_ok && params_ok

        # Object/Row subtyping (width subtyping)
        (TObject sub_row, TObject super_row) ->
            is_row_subtype_of store sub_row super_row

        _ -> Bool.false

# Row subtyping (structural, width subtyping)
is_row_subtype_of : TypeStore, RowId, RowId -> Bool
is_row_subtype_of = \store, sub_row, super_row ->
    when get_row store super_row is
        Ok super_def ->
            when super_def is
                REmpty -> Bool.true  # Everything has at least empty fields

                RExtend super_extend ->
                    # Check if sub_row has this field with compatible type
                    has_compatible_field store sub_row super_extend.field_name super_extend.field_type
                    && is_row_subtype_of store sub_row super_extend.rest

                _ -> Bool.false
        Err _ -> Bool.false

has_compatible_field : TypeStore, RowId, Str, TypeId -> Bool
has_compatible_field = \store, row_id, field_name, field_type ->
    when get_row store row_id is
        Ok row_def ->
            when row_def is
                RExtend extend ->
                    if extend.field_name == field_name then
                        is_subtype_of store extend.field_type field_type
                    else
                        has_compatible_field store extend.rest field_name field_type
                _ -> Bool.false
        Err _ -> Bool.false

# Join operation (least upper bound)
# Returns the most specific common supertype
join : TypeStore, TypeId, TypeId -> (TypeStore, TypeId)
join = \store, type1, type2 ->
    if type1 == type2 then
        (store, type1)
    else
        when (get_type store type1, get_type store type2) is
            (Ok def1, Ok def2) ->
                join_helper store def1 def2 type1 type2
            _ ->
                # If we can't get the types, default to unknown
                make_unknown store

join_helper : TypeStore, TypeDef, TypeDef, TypeId, TypeId -> (TypeStore, TypeId)
join_helper = \store, def1, def2, id1, id2 ->
    when (def1, def2) is
        # Never joins to the other type
        (TNever, _) -> (store, id2)
        (_, TNever) -> (store, id1)

        # Unknown absorbs everything
        (TUnknown, _) -> (store, id1)
        (_, TUnknown) -> (store, id2)

        # Any absorbs everything except unknown
        (TAny, _) -> (store, id1)
        (_, TAny) -> (store, id2)

        # Same primitive types
        (TNumber, TNumber) -> (store, id1)
        (TString, TString) -> (store, id1)
        (TBoolean, TBoolean) -> (store, id1)
        (TBigInt, TBigInt) -> (store, id1)
        (TSymbol, TSymbol) -> (store, id1)
        (TNull, TNull) -> (store, id1)
        (TUndefined, TUndefined) -> (store, id1)
        (TVoid, TVoid) -> (store, id1)

        # Literal to base type
        (TLiteral (NumLit _), TNumber) -> (store, id2)
        (TNumber, TLiteral (NumLit _)) -> (store, id1)
        (TLiteral (StrLit _), TString) -> (store, id2)
        (TString, TLiteral (StrLit _)) -> (store, id1)
        (TLiteral (BoolLit _), TBoolean) -> (store, id2)
        (TBoolean, TLiteral (BoolLit _)) -> (store, id1)

        # Different literals of same type
        (TLiteral (NumLit _), TLiteral (NumLit _)) ->
            make_primitive store TNumber
        (TLiteral (StrLit _), TLiteral (StrLit _)) ->
            make_primitive store TString
        (TLiteral (BoolLit _), TLiteral (BoolLit _)) ->
            make_primitive store TBoolean

        # Arrays - join element types
        (TArray elem1, TArray elem2) ->
            (store1, joined_elem) = join store elem1 elem2
            make_array store1 joined_elem

        # Tuples - join elementwise if same length, otherwise unknown
        (TTuple types1, TTuple types2) ->
            if List.len(types1) == List.len(types2) then
                result = List.walk2(types1, types2, (store, []), \(acc_store, acc_types), t1, t2 ->
                    (new_store, joined) = join acc_store t1 t2
                    (new_store, List.append(acc_types, joined))
                )
                when result is
                    (final_store, joined_types) ->
                        make_tuple final_store joined_types
            else
                make_unknown store

        # Create union for different types
        _ ->
            # Check if either is already a union we can extend
            when (def1, def2) is
                (TUnion types1, TUnion types2) ->
                    make_union store (List.concat(types1, types2))
                (TUnion types1, _) ->
                    if List.contains(types1, id2) then
                        (store, id1)
                    else
                        make_union store (List.append(types1, id2))
                (_, TUnion types2) ->
                    if List.contains(types2, id1) then
                        (store, id2)
                    else
                        make_union store (List.append(types2, id1))
                _ ->
                    make_union store [id1, id2]

# Meet operation (greatest lower bound)
# Returns the most general common subtype
meet : TypeStore, TypeId, TypeId -> (TypeStore, TypeId)
meet = \store, type1, type2 ->
    if type1 == type2 then
        (store, type1)
    else
        when (get_type store type1, get_type store type2) is
            (Ok def1, Ok def2) ->
                meet_helper store def1 def2 type1 type2
            _ ->
                # If we can't get the types, default to never
                make_never store

meet_helper : TypeStore, TypeDef, TypeDef, TypeId, TypeId -> (TypeStore, TypeId)
meet_helper = \store, def1, def2, id1, id2 ->
    when (def1, def2) is
        # Never meets to never
        (TNever, _) -> (store, id1)
        (_, TNever) -> (store, id2)

        # Unknown meets to the other type
        (TUnknown, _) -> (store, id2)
        (_, TUnknown) -> (store, id1)

        # Any meets to the other type (except unknown)
        (TAny, _) -> (store, id2)
        (_, TAny) -> (store, id1)

        # Same primitive types
        (TNumber, TNumber) -> (store, id1)
        (TString, TString) -> (store, id1)
        (TBoolean, TBoolean) -> (store, id1)
        (TBigInt, TBigInt) -> (store, id1)
        (TSymbol, TSymbol) -> (store, id1)
        (TNull, TNull) -> (store, id1)
        (TUndefined, TUndefined) -> (store, id1)
        (TVoid, TVoid) -> (store, id1)

        # Literal with base type
        (TLiteral lit, TNumber) if is_num_literal(lit) -> (store, id1)
        (TNumber, TLiteral lit) if is_num_literal(lit) -> (store, id2)
        (TLiteral lit, TString) if is_str_literal(lit) -> (store, id1)
        (TString, TLiteral lit) if is_str_literal(lit) -> (store, id2)
        (TLiteral lit, TBoolean) if is_bool_literal(lit) -> (store, id1)
        (TBoolean, TLiteral lit) if is_bool_literal(lit) -> (store, id2)

        # Arrays - meet element types
        (TArray elem1, TArray elem2) ->
            (store1, met_elem) = meet store elem1 elem2
            make_array store1 met_elem

        # Tuples - meet elementwise if same length
        (TTuple types1, TTuple types2) ->
            if List.len(types1) == List.len(types2) then
                result = List.walk2(types1, types2, (store, []), \(acc_store, acc_types), t1, t2 ->
                    (new_store, met) = meet acc_store t1 t2
                    (new_store, List.append(acc_types, met))
                )
                when result is
                    (final_store, met_types) ->
                        make_tuple final_store met_types
            else
                make_never store

        # Create intersection for compatible types
        _ ->
            # Check if either is already an intersection we can extend
            when (def1, def2) is
                (TIntersection types1, TIntersection types2) ->
                    make_intersection store (List.concat(types1, types2))
                (TIntersection types1, _) ->
                    if List.contains(types1, id2) then
                        (store, id1)
                    else
                        make_intersection store (List.append(types1, id2))
                (_, TIntersection types2) ->
                    if List.contains(types2, id1) then
                        (store, id2)
                    else
                        make_intersection store (List.append(types2, id1))

                # Check if types are compatible for intersection
                _ ->
                    if types_are_compatible store def1 def2 then
                        make_intersection store [id1, id2]
                    else
                        make_never store

# Helper functions
is_num_literal : LiteralValue -> Bool
is_num_literal = \lit ->
    when lit is
        NumLit _ -> Bool.true
        _ -> Bool.false

is_str_literal : LiteralValue -> Bool
is_str_literal = \lit ->
    when lit is
        StrLit _ -> Bool.true
        _ -> Bool.false

is_bool_literal : LiteralValue -> Bool
is_bool_literal = \lit ->
    when lit is
        BoolLit _ -> Bool.true
        _ -> Bool.false

# Check if two types can form a valid intersection
types_are_compatible : TypeStore, TypeDef, TypeDef -> Bool
types_are_compatible = \_store, def1, def2 ->
    when (def1, def2) is
        # Incompatible primitive types
        (TNumber, TString) | (TString, TNumber) -> Bool.false
        (TNumber, TBoolean) | (TBoolean, TNumber) -> Bool.false
        (TString, TBoolean) | (TBoolean, TString) -> Bool.false
        (TNull, TUndefined) | (TUndefined, TNull) -> Bool.false

        # Objects can always intersect (structural typing)
        (TObject _, TObject _) -> Bool.true
        (TObject _, _) | (_, TObject _) -> Bool.true

        # Functions might be compatible
        (TFunction _, TFunction _) -> Bool.true

        _ -> Bool.true  # Optimistic - let type checking catch real errors
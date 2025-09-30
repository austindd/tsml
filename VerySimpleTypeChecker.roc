module [
    check_program,
    TypeCheckResult,
]

import ComprehensiveTypeIndexed as T

# Result of type checking
TypeCheckResult : {
    type : T.TypeId,
    store : T.TypeStore,
    info : Str,
}

# Very simple type checker - just returns unknown type
check_program : _ -> TypeCheckResult
check_program = \_ ->
    store0 = T.empty_store
    (store1, unknown) = T.make_unknown(store0)

    {
        type: unknown,
        store: store1,
        info: "Type checking not yet implemented - returning unknown type",
    }
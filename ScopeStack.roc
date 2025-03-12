module []

import Stack exposing [Stack]
import SymTbl exposing [SymTbl, SymKey]
import ResultUtils exposing [result_flatmap]

ScopeStack a := Stack (SymTbl a)

ItemId := {
    level : U64, # zero-indexed
    key : SymKey,
}

get : ScopeStack a, ItemId -> Result a {}
get = |@ScopeStack(tree), @ItemId(st_key)|
    Stack.peek_at(tree, st_key.level)
    |> Result.map_err(|_| {})
    |> result_flatmap(
        |level|
            SymTbl.get(level, st_key.key),
    )

set : ScopeStack a, ItemId, a -> ScopeStack a
set = |@ScopeStack(scope_stack), @ItemId(item_id), value|
    Stack.update(
        scope_stack,
        item_id.level,
        |scope|
            SymTbl.set(scope, item_id.key, value),
    )
    |> @ScopeStack

module [SymTblStack, get, set]

import Stack exposing [Stack]
import SymTbl exposing [SymTbl, SymTblId]
import ResultUtils exposing [result_flatmap]

SymTblStack a := Stack (SymTbl a)

SymTblStackId := {
    level : U64, # zero-indexed
    key : SymTblId,
}

get : SymTblStack a, SymTblStackId -> Result a {}
get = |@SymTblStack(tree), @SymTblStackId(st_key)|
    Stack.peek_at(tree, st_key.level)
    |> Result.map_err(|_| {})
    |> result_flatmap(
        |level|
            SymTbl.get(level, st_key.key),
    )

set : SymTblStack a, SymTblStackId, a -> SymTblStack a
set = |@SymTblStack(sym_tbl_stack), @SymTblStackId(item_id), value|
    Stack.update(
        sym_tbl_stack,
        item_id.level,
        |sym_tbl|
            SymTbl.set(sym_tbl, item_id.key, value),
    )
    |> @SymTblStack

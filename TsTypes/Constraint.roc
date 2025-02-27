module [
]

import TsTypes.CoreTypes exposing [
    TypeId,
    TConstraint,
    Kind,
    Property,
    TypeScope,
    TypeCtx,
]

satisfies_boolean : TConstraint -> Result {} {}
satisfies_boolean = |constraint|
    when constraint is
        Boolean(_) -> Ok({})
        _ -> Err({})

satisfies_number : TConstraint -> Result {} {}
satisfies_number = |constraint|
    when constraint is
        Number(_) -> Ok({})
        _ -> Err({})

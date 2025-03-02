module [
]

import TsTypes.CoreTypes exposing [
    TypeId,
    TConstraint,
    Property,
    TypeScope,
    TypeCtx,
]

satisfies_boolean : TConstraint -> Result {} {}
satisfies_boolean = |constraint|
    when constraint is
        Boolean -> Ok({})
        _ -> Err({})

satisfies_number : TConstraint -> Result {} {}
satisfies_number = |constraint|
    when constraint is
        Number -> Ok({})
        _ -> Err({})

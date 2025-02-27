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

satisfiesBoolean : TConstraint -> Result {} {}
satisfiesBoolean = \constraint ->
    when constraint is
        Boolean _ -> Ok {}
        _ -> Err {}

satisfiesNumber : TConstraint -> Result {} {}
satisfiesNumber = \constraint ->
    when constraint is
        Number _ -> Ok {}
        _ -> Err {}

module [
    AsyncType,
    PromiseType,
    check_async_function,
    check_await_expression,
    infer_promise_type,
]

# Async/Await and Promise Type Support for TypeScript/JavaScript

TypeId : U64

# Core async type representations
AsyncType : [
    # Promise<T> type
    TPromise TypeId,

    # Async function return type (always Promise<T>)
    TAsyncFunction (List TypeId) TypeId,  # params -> Promise<return>

    # Awaited type T from Promise<T>
    TAwaited TypeId,

    # Promise states for advanced typing
    TPromiseState [Pending, Fulfilled TypeId, Rejected TypeId],

    # Basic types that can be in promises
    TBasic BasicType,

    # Type variable
    TVar TypeId,

    # Unknown
    TUnknown,
]

BasicType : [
    BNum,
    BStr,
    BBool,
    BNull,
    BUndefined,
    BObject (List { key: Str, type: TypeId }),
    BArray TypeId,
]

# Promise type with resolved and rejected types
PromiseType : {
    resolved: TypeId,
    rejected: TypeId,  # Usually Error or any
}

# === Core Operations ===

# Create a Promise type
make_promise : TypeId -> AsyncType
make_promise = \resolved_type ->
    TPromise resolved_type

# Get the resolved type from a Promise
unwrap_promise : AsyncType -> Result TypeId [NotAPromise]
unwrap_promise = \typ ->
    when typ is
        TPromise t -> Ok t
        TAsyncFunction _ ret -> Ok ret  # Async functions return Promise<T>
        _ -> Err NotAPromise

# Check if a type is a Promise
is_promise : AsyncType -> Bool
is_promise = \typ ->
    when typ is
        TPromise _ -> Bool.true
        TAsyncFunction _ _ -> Bool.true  # Async functions return promises
        _ -> Bool.false

# === Async Function Checking ===

# Check an async function declaration
check_async_function : List TypeId, TypeId, Bool -> AsyncType
check_async_function = \params, return_type, is_async ->
    if is_async then
        # Async function always returns Promise<T>
        TAsyncFunction params return_type
    else
        # Regular function (simplified)
        TBasic (BArray 0)  # Placeholder

# Infer the return type of an async function
infer_async_return : AsyncType -> Result TypeId [NotAsync]
infer_async_return = \func_type ->
    when func_type is
        TAsyncFunction _ ret -> Ok ret
        _ -> Err NotAsync

# === Await Expression Checking ===

# Check an await expression
check_await_expression : AsyncType, Bool -> Result AsyncType [AwaitError Str]
check_await_expression = \expr_type, in_async_context ->
    if Bool.not in_async_context then
        Err (AwaitError "await can only be used in async functions")
    else
        when expr_type is
            TPromise t ->
                # await Promise<T> -> T
                Ok (TVar t)

            TAsyncFunction _ ret ->
                # Awaiting an async function call
                Ok (TVar ret)

            TAwaited t ->
                # Already awaited
                Ok (TVar t)

            _ ->
                # Can await non-promises (they're wrapped in Promise.resolve)
                Ok expr_type

# === Promise Combinators ===

# Promise.all type inference
# Promise.all<T>([Promise<T1>, Promise<T2>, ...]) -> Promise<[T1, T2, ...]>
infer_promise_all : List AsyncType -> AsyncType
infer_promise_all = \promises ->
    resolved_types = List.keep_oks promises \p ->
        when p is
            TPromise t -> Ok t
            _ -> Err NotPromise

    # Return Promise of tuple of resolved types
    TPromise 999  # Simplified: would be tuple type

# Promise.race type inference
# Promise.race<T>([Promise<T1>, Promise<T2>, ...]) -> Promise<T1 | T2 | ...>
infer_promise_race : List AsyncType -> AsyncType
infer_promise_race = \promises ->
    resolved_types = List.keep_oks promises \p ->
        when p is
            TPromise t -> Ok t
            _ -> Err NotPromise

    # Return Promise of union of resolved types
    TPromise 998  # Simplified: would be union type

# Promise.allSettled result type
# Promise.allSettled<T>([...]) -> Promise<PromiseSettledResult<T>[]>
infer_promise_all_settled : List AsyncType -> AsyncType
infer_promise_all_settled = \promises ->
    # Returns array of { status: 'fulfilled' | 'rejected', value?: T, reason?: any }
    TPromise 997  # Simplified: would be array of settled results

# === Type Inference for Async Patterns ===

# Infer type of .then() chain
# promise.then(onFulfilled, onRejected)
infer_then_chain : AsyncType, TypeId, TypeId -> AsyncType
infer_then_chain = \promise_type, on_fulfilled_return, on_rejected_return ->
    when promise_type is
        TPromise _ ->
            # .then() returns a new Promise with the callback's return type
            TPromise on_fulfilled_return
        _ ->
            TUnknown

# Infer type of .catch() chain
# promise.catch(onRejected)
infer_catch_chain : AsyncType, TypeId -> AsyncType
infer_catch_chain = \promise_type, handler_return ->
    when promise_type is
        TPromise resolved ->
            # .catch() returns Promise<T | HandlerReturn>
            TPromise resolved  # Simplified: would be union
        _ ->
            TUnknown

# Infer type of .finally() chain
# promise.finally(onFinally)
infer_finally_chain : AsyncType -> AsyncType
infer_finally_chain = \promise_type ->
    # .finally() preserves the original Promise type
    promise_type

# === Async Generator Support ===

# AsyncIterator<T> type
AsyncIteratorType : {
    yield_type: TypeId,
    return_type: TypeId,
    next_type: AsyncType,  # Promise<{done: boolean, value: T}>
}

# Check async generator function
check_async_generator : List TypeId, TypeId, TypeId -> AsyncIteratorType
check_async_generator = \params, yield_type, return_type ->
    {
        yield_type,
        return_type,
        next_type: TPromise 996,  # Promise<IteratorResult<T>>
    }

# === Common Async Patterns ===

# Type for async error handling
AsyncResult : [
    Success TypeId,
    Error TypeId,
]

# Convert callback-based to Promise-based (promisify pattern)
promisify_type : TypeId -> AsyncType
promisify_type = \callback_return_type ->
    TPromise callback_return_type

# Check Promise constructor
# new Promise<T>((resolve: (value: T) => void, reject: (reason: any) => void) => void)
check_promise_constructor : TypeId -> AsyncType
check_promise_constructor = \resolved_type ->
    TPromise resolved_type

# === Examples ===

# async function fetchData(): Promise<Data> {
#     const result = await fetch('/api/data');
#     return result.json();
# }
example_fetch_data : AsyncType
example_fetch_data =
    TAsyncFunction [] 100  # [] params -> Promise<Data(id:100)>

# Promise.all([promise1, promise2, promise3])
example_promise_all : List AsyncType -> AsyncType
example_promise_all = \promises ->
    infer_promise_all promises

# async function* asyncGenerator() {
#     yield 1;
#     yield 2;
#     yield 3;
# }
example_async_generator : AsyncIteratorType
example_async_generator =
    check_async_generator [] 101 102  # yields number(101), returns void(102)

# Type inference for async/await chains
# const data = await fetch(url).then(r => r.json());
example_fetch_chain : AsyncType
example_fetch_chain =
    fetch_promise = TPromise 200  # Promise<Response>
    json_promise = infer_then_chain fetch_promise 201 0  # Promise<any>
    json_promise

# === Integration with Main Type System ===

# Convert AsyncType to simplified type for main type checker
async_to_simple : AsyncType -> TypeId
async_to_simple = \async_type ->
    when async_type is
        TPromise t -> t
        TAsyncFunction _ ret -> ret
        TAwaited t -> t
        TVar t -> t
        _ -> 0  # Unknown

# Check if an expression can be awaited
can_await : AsyncType -> Bool
can_await = \typ ->
    # In JavaScript, anything can be awaited (wrapped in Promise.resolve if needed)
    Bool.true

# Infer the type after await
infer_promise_type : AsyncType -> AsyncType
infer_promise_type = \typ ->
    when typ is
        TPromise inner -> TVar inner
        _ -> typ  # Non-promises are wrapped in Promise.resolve

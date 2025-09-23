// Test Async/Promise Types

// Basic Promise
const promise1: Promise<number> = Promise.resolve(42);

// Async function
async function fetchUser(id: number): Promise<User> {
    const response = await fetch(`/api/users/${id}`);
    return response.json();
}

// Await expression
async function processUser() {
    const user = await fetchUser(1);  // Type should be User, not Promise<User>
    console.log(user.name);
}

// Promise chaining
function chainedPromise(): Promise<string> {
    return Promise.resolve(1)
        .then(n => n * 2)
        .then(n => n.toString())
        .then(s => `Result: ${s}`);
}

// Promise.all
async function fetchMultiple() {
    const [user, posts, comments] = await Promise.all([
        fetchUser(1),
        fetchPosts(),
        fetchComments()
    ]);
    // Types should be: User, Post[], Comment[]
}

// Promise.race
async function timeout<T>(promise: Promise<T>, ms: number): Promise<T> {
    return Promise.race([
        promise,
        new Promise<never>((_, reject) =>
            setTimeout(() => reject(new Error("Timeout")), ms)
        )
    ]);
}

// Promise.allSettled
async function tryMultiple() {
    const results = await Promise.allSettled([
        fetchUser(1),
        fetchUser(2),
        fetchUser(3)
    ]);

    results.forEach(result => {
        if (result.status === 'fulfilled') {
            console.log(result.value);  // Type: User
        } else {
            console.log(result.reason);  // Type: any
        }
    });
}

// Async generator
async function* asyncGenerator(): AsyncGenerator<number> {
    yield 1;
    yield 2;
    yield 3;
}

// Async iteration
async function consumeAsyncIterator() {
    for await (const value of asyncGenerator()) {
        console.log(value);  // Type: number
    }
}

// Error handling in async
async function withErrorHandling(): Promise<string | null> {
    try {
        const data = await fetchData();
        return processData(data);
    } catch (error) {
        console.error(error);
        return null;
    }
}

// Conditional async
async function conditionalAsync<T>(
    condition: boolean,
    asyncFn: () => Promise<T>,
    defaultValue: T
): Promise<T> {
    if (condition) {
        return await asyncFn();
    }
    return defaultValue;
}

// Promise with type parameter
function delay<T>(value: T, ms: number): Promise<T> {
    return new Promise(resolve => {
        setTimeout(() => resolve(value), ms);
    });
}

// Nested promises (should be flattened)
async function nestedPromises(): Promise<number> {
    const p1: Promise<Promise<number>> = Promise.resolve(Promise.resolve(42));
    const result = await p1;  // Should be number, not Promise<number>
    return result;
}

// Type definitions used
interface User {
    id: number;
    name: string;
}

declare function fetchPosts(): Promise<Post[]>;
declare function fetchComments(): Promise<Comment[]>;
declare function fetchData(): Promise<any>;
declare function processData(data: any): string;

interface Post {
    id: number;
    title: string;
}

interface Comment {
    id: number;
    text: string;
}
// Test Union and Intersection Types

// Basic union
type StringOrNumber = string | number;
let value: StringOrNumber = "hello";
value = 42;  // Also valid

// Basic intersection
type Named = { name: string };
type Aged = { age: number };
type Person = Named & Aged;

const person: Person = {
    name: "Alice",
    age: 30
};

// Discriminated union
type Result<T, E> =
    | { success: true; value: T }
    | { success: false; error: E };

function processResult<T>(result: Result<T, string>): T | null {
    if (result.success) {
        return result.value;  // Narrowed to success case
    } else {
        console.error(result.error);  // Narrowed to error case
        return null;
    }
}

// Tagged union with multiple cases
type Shape =
    | { kind: "circle"; radius: number }
    | { kind: "rectangle"; width: number; height: number }
    | { kind: "triangle"; base: number; height: number };

function area(shape: Shape): number {
    switch (shape.kind) {
        case "circle":
            return Math.PI * shape.radius ** 2;
        case "rectangle":
            return shape.width * shape.height;
        case "triangle":
            return 0.5 * shape.base * shape.height;
    }
}

// Union of literals
type Direction = "north" | "south" | "east" | "west";
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;

// Intersection of functions
type AsyncFunc<T> = (callback: (result: T) => void) => void;
type Cancelable = { cancel(): void };
type CancelableAsync<T> = AsyncFunc<T> & Cancelable;

// Complex union with type guards
type NetworkState =
    | { state: "idle" }
    | { state: "loading"; progress: number }
    | { state: "success"; response: any }
    | { state: "error"; code: number; message: string };

function handleState(state: NetworkState) {
    switch (state.state) {
        case "idle":
            console.log("Waiting...");
            break;
        case "loading":
            console.log(`Loading: ${state.progress}%`);
            break;
        case "success":
            console.log("Data:", state.response);
            break;
        case "error":
            console.log(`Error ${state.code}: ${state.message}`);
            break;
    }
}

// Distribution over unions
type Boxed<T> = { value: T };
type BoxedStringOrNumber = Boxed<string | number>;
// Equivalent to: Boxed<string> | Boxed<number>

// Intersection merging objects
type A = { a: string; b: number };
type B = { b: string; c: boolean };
type C = A & B;  // { a: string; b: never; c: boolean }
// Note: b is never because number & string = never

// Union normalization
type Normalized = string | number | string | boolean;
// Should be normalized to: string | number | boolean

// Extracting from unions
type ExtractString<T> = T extends string ? T : never;
type OnlyStrings = ExtractString<string | number | boolean>;  // string

// Excluding from unions
type ExcludeString<T> = T extends string ? never : T;
type NoStrings = ExcludeString<string | number | boolean>;  // number | boolean

// Complex discriminated union
type ApiResponse =
    | { status: 200; data: any }
    | { status: 404; message: "Not Found" }
    | { status: 500; message: "Internal Server Error"; details?: string };

function handleResponse(response: ApiResponse) {
    if (response.status === 200) {
        return response.data;
    } else if (response.status === 404) {
        throw new Error(response.message);
    } else {
        throw new Error(`${response.message}: ${response.details || "Unknown"}`);
    }
}

// Union with shared properties
type Cat = { species: "cat"; meow(): void; paws: 4 };
type Dog = { species: "dog"; bark(): void; paws: 4 };
type Animal = Cat | Dog;

function getPaws(animal: Animal): number {
    return animal.paws;  // OK - shared property
}

// Distributive conditional types
type IsUnion<T> = T extends any ? T : never;
type Distributed = IsUnion<string | number>;  // string | number
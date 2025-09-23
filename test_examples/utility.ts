// Test TypeScript Utility Types

// Original types for testing
interface User {
    id: number;
    name: string;
    email: string;
    age: number;
    isActive: boolean;
}

interface Config {
    apiUrl?: string;
    timeout?: number;
    retries?: number;
}

// Partial<T> - Make all properties optional
type PartialUser = Partial<User>;
const partialUser: PartialUser = {
    name: "Alice"  // Only some properties
};

// Required<T> - Make all properties required
type RequiredConfig = Required<Config>;
const config: RequiredConfig = {
    apiUrl: "https://api.example.com",
    timeout: 5000,
    retries: 3  // All must be present
};

// Readonly<T> - Make all properties readonly
type ReadonlyUser = Readonly<User>;
const readonlyUser: ReadonlyUser = {
    id: 1,
    name: "Bob",
    email: "bob@example.com",
    age: 30,
    isActive: true
};
// readonlyUser.name = "Alice";  // Error: readonly

// Pick<T, K> - Pick specific properties
type UserSummary = Pick<User, "id" | "name">;
const summary: UserSummary = {
    id: 1,
    name: "Charlie"
};

// Omit<T, K> - Omit specific properties
type UserWithoutEmail = Omit<User, "email">;
const userNoEmail: UserWithoutEmail = {
    id: 2,
    name: "David",
    age: 25,
    isActive: true
};

// Record<K, T> - Create object type with keys K and values T
type Roles = "admin" | "user" | "guest";
type RolePermissions = Record<Roles, string[]>;
const permissions: RolePermissions = {
    admin: ["read", "write", "delete"],
    user: ["read", "write"],
    guest: ["read"]
};

// Exclude<T, U> - Exclude U from union T
type Status = "pending" | "active" | "inactive" | "deleted";
type ActiveStatus = Exclude<Status, "deleted" | "inactive">;
// Result: "pending" | "active"

// Extract<T, U> - Extract U from union T
type StringOrNumber = string | number | boolean;
type OnlyString = Extract<StringOrNumber, string>;
// Result: string

// NonNullable<T> - Remove null and undefined
type MaybeString = string | null | undefined;
type DefiniteString = NonNullable<MaybeString>;
// Result: string

// Parameters<T> - Get function parameters as tuple
function greet(name: string, age: number): string {
    return `Hello ${name}, age ${age}`;
}
type GreetParams = Parameters<typeof greet>;
// Result: [string, number]

// ReturnType<T> - Get function return type
type GreetReturn = ReturnType<typeof greet>;
// Result: string

// ConstructorParameters<T> - Get constructor parameters
class Person {
    constructor(public name: string, public age: number) { }
}
type PersonParams = ConstructorParameters<typeof Person>;
// Result: [string, number]

// InstanceType<T> - Get instance type of constructor
type PersonInstance = InstanceType<typeof Person>;
// Result: Person

// Awaited<T> - Unwrap Promise recursively
type PromiseString = Promise<string>;
type NestedPromise = Promise<Promise<number>>;
type UnwrappedString = Awaited<PromiseString>;  // string
type UnwrappedNumber = Awaited<NestedPromise>;  // number

// Uppercase<S> - Convert string literal to uppercase
type Greeting = "hello";
type GREETING = Uppercase<Greeting>;  // "HELLO"

// Lowercase<S> - Convert string literal to lowercase
type SHOUT = "HELLO";
type whisper = Lowercase<SHOUT>;  // "hello"

// Capitalize<S> - Capitalize first letter
type uncapitalized = "hello world";
type Capitalized = Capitalize<uncapitalized>;  // "Hello world"

// Uncapitalize<S> - Uncapitalize first letter
type Title = "Hello World";
type untitled = Uncapitalize<Title>;  // "hello World"

// Combining utility types
type ReadonlyPartialUser = Readonly<Partial<User>>;
type RequiredUserSummary = Required<Pick<User, "id" | "name">>;
type EditableUser = Omit<User, "id"> & Partial<Pick<User, "email">>;

// Custom combinations
type UpdateUser = Partial<Omit<User, "id">>;  // Can update any field except id
type UserDisplay = Readonly<Pick<User, "name" | "email">>;  // Read-only display fields

// Complex example
interface ApiResponse<T> {
    data: T;
    status: number;
    timestamp: Date;
    metadata?: Record<string, any>;
}

type SuccessResponse<T> = Required<Pick<ApiResponse<T>, "data" | "status">> &
                          Partial<Pick<ApiResponse<T>, "timestamp" | "metadata">>;

// String template literal types
type EventName = "click" | "focus" | "blur";
type EventHandler<T extends string> = `on${Capitalize<T>}`;
type ClickHandler = EventHandler<"click">;  // "onClick"

// Deep utility types
type DeepPartial<T> = {
    [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

type DeepReadonly<T> = {
    readonly [P in keyof T]: T[P] extends object ? DeepReadonly<T[P]> : T[P];
};
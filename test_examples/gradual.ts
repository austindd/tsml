// Test Gradual Typing (any and unknown)

// === any type ===
// any disables type checking - can be assigned to/from anything
let anyValue: any = 42;
anyValue = "hello";  // OK
anyValue = { x: 1 };  // OK
anyValue = null;      // OK

// any can be used as anything without checks
anyValue.foo.bar.baz;  // No error (but might crash at runtime)
anyValue();            // No error
anyValue[0];           // No error

// any pollutes type safety
function processAny(value: any) {
    return value.toString();  // No type checking
}

// any spreads through operations
let spreading = anyValue + 5;  // Result is any
let moreSpread = [1, 2, anyValue];  // Result is (number | any)[]

// === unknown type ===
// unknown is the safe any - requires type checking before use
let unknownValue: unknown = 42;
unknownValue = "hello";  // OK - can assign anything
unknownValue = { x: 1 };  // OK
unknownValue = null;      // OK

// unknownValue.foo;  // Error! Can't use without narrowing
// unknownValue();     // Error! Can't call
// unknownValue[0];    // Error! Can't index

// Must narrow unknown before use
function processUnknown(value: unknown) {
    // Type guards required
    if (typeof value === "string") {
        return value.toUpperCase();  // OK - narrowed to string
    } else if (typeof value === "number") {
        return value * 2;  // OK - narrowed to number
    } else if (value !== null && typeof value === "object") {
        // Narrowed to object (but not a specific shape)
        return Object.keys(value);
    }
    return null;
}

// Type assertion with unknown
function assertString(value: unknown): string {
    // Must check or assert
    if (typeof value === "string") {
        return value;
    }
    throw new Error("Not a string");
}

// unknown in unions
type MaybeString = unknown | string;  // unknown (absorbs in union)
type MaybeNumber = unknown & number;  // number (identity in intersection)

// === Migrating from any to unknown ===
// Old unsafe code with any
function unsafeJson(text: string): any {
    return JSON.parse(text);  // Returns any
}

// Safe version with unknown
function safeJson(text: string): unknown {
    return JSON.parse(text);  // Returns unknown
}

// Using the safe version requires validation
function parseConfig(text: string): Config {
    const data = safeJson(text);

    // Must validate the shape
    if (isConfig(data)) {
        return data;  // Now typed as Config
    }
    throw new Error("Invalid config");
}

// Type guard for validation
function isConfig(value: unknown): value is Config {
    return value !== null &&
           typeof value === "object" &&
           "apiKey" in value &&
           "endpoint" in value;
}

// === Gradual typing patterns ===

// Function accepting any (loose)
function looseFunction(callback: (value: any) => any) {
    return callback("test");
}

// Function accepting unknown (strict)
function strictFunction(callback: (value: unknown) => unknown) {
    return callback("test");
}

// Mixed strictness
interface MixedApi {
    strictMethod(value: unknown): void;
    looseMethod(value: any): void;
    process<T>(value: T): T;  // Generic - preserves type
}

// any in object types
type LooseObject = {
    known: string;
    dynamic: any;  // This property can be anything
};

// unknown in object types
type StrictObject = {
    known: string;
    dynamic: unknown;  // Must be checked before use
};

// Casting between any and unknown
let a: any = "hello";
let u: unknown = a;  // OK - any to unknown
// let a2: any = u;  // Error - need assertion
let a2: any = u as any;  // OK with assertion

// Function overloads with any/unknown
function parse(value: string): unknown;
function parse(value: string, reviver: (key: string, value: any) => any): any;
function parse(value: string, reviver?: (key: string, value: any) => any) {
    return JSON.parse(value, reviver);
}

// === Real-world examples ===

// API response handling
async function fetchData<T>(url: string): Promise<T> {
    const response = await fetch(url);
    const data: unknown = await response.json();  // Start with unknown

    // Would validate data here
    return data as T;  // Assert after validation
}

// Event handler with unknown
function handleEvent(event: unknown) {
    if (event && typeof event === "object" && "type" in event) {
        const e = event as { type: string };
        console.log(`Event type: ${e.type}`);
    }
}

// Legacy code integration
declare function legacyFunction(): any;  // Old library returns any
function modernWrapper(): unknown {
    return legacyFunction();  // Wrap any as unknown for safety
}

// Type for Config
interface Config {
    apiKey: string;
    endpoint: string;
    timeout?: number;
}
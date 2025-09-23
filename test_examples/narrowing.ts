// Test Control Flow Type Narrowing

// typeof narrowing
function processValue(value: string | number | boolean) {
    if (typeof value === "string") {
        // value is string here
        return value.toUpperCase();
    } else if (typeof value === "number") {
        // value is number here
        return value * 2;
    } else {
        // value is boolean here
        return !value;
    }
}

// instanceof narrowing
class Car {
    drive() { }
}
class Bike {
    ride() { }
}

function operate(vehicle: Car | Bike) {
    if (vehicle instanceof Car) {
        vehicle.drive();  // OK
    } else {
        vehicle.ride();   // OK
    }
}

// Truthiness narrowing
function processOptional(value: string | null | undefined) {
    if (value) {
        // value is string here (null and undefined are falsy)
        console.log(value.length);
    } else {
        // value is null | undefined here
        console.log("No value");
    }
}

// Equality narrowing
type Action =
    | { type: "INCREMENT"; amount: number }
    | { type: "DECREMENT"; amount: number }
    | { type: "RESET" };

function reducer(action: Action) {
    if (action.type === "INCREMENT") {
        return action.amount;  // Has amount
    } else if (action.type === "DECREMENT") {
        return -action.amount; // Has amount
    } else {
        return 0;  // RESET has no amount
    }
}

// in operator narrowing
type Fish = { swim(): void };
type Bird = { fly(): void };
type Human = { swim(): void; walk(): void };

function move(animal: Fish | Bird | Human) {
    if ("swim" in animal) {
        // animal is Fish | Human
        animal.swim();
    }

    if ("walk" in animal) {
        // animal is Human
        animal.walk();
        animal.swim();  // Human can also swim
    }
}

// Custom type predicates
function isString(value: unknown): value is string {
    return typeof value === "string";
}

function isNumber(value: unknown): value is number {
    return typeof value === "number";
}

function processUnknown(value: unknown) {
    if (isString(value)) {
        console.log(value.toUpperCase());
    } else if (isNumber(value)) {
        console.log(value.toFixed(2));
    }
}

// Null/undefined checking
function printLength(value: string | null | undefined) {
    if (value != null) {
        // value is string (both null and undefined removed)
        console.log(value.length);
    }

    if (value !== null && value !== undefined) {
        // More explicit null/undefined check
        console.log(value.length);
    }
}

// Array type guard
function processArray(value: string | string[]) {
    if (Array.isArray(value)) {
        // value is string[]
        return value.join(", ");
    } else {
        // value is string
        return value;
    }
}

// Exhaustiveness checking
type Color = "red" | "green" | "blue";

function getColorCode(color: Color): number {
    switch (color) {
        case "red":
            return 0xFF0000;
        case "green":
            return 0x00FF00;
        case "blue":
            return 0x0000FF;
        default:
            // This should never be reached
            const _exhaustive: never = color;
            throw new Error(`Unknown color: ${color}`);
    }
}

// Complex narrowing with multiple checks
type User = {
    type: "user";
    name: string;
    email: string;
};

type Admin = {
    type: "admin";
    name: string;
    email: string;
    permissions: string[];
};

type Guest = {
    type: "guest";
    sessionId: string;
};

type Account = User | Admin | Guest;

function processAccount(account: Account) {
    if (account.type === "guest") {
        // account is Guest
        console.log(`Guest session: ${account.sessionId}`);
    } else {
        // account is User | Admin (both have name and email)
        console.log(`Welcome ${account.name}`);

        if (account.type === "admin") {
            // account is Admin
            console.log(`Permissions: ${account.permissions.join(", ")}`);
        }
    }
}

// Assertion functions
function assertDefined<T>(value: T | null | undefined): asserts value is T {
    if (value == null) {
        throw new Error("Value is null or undefined");
    }
}

function useAssertion(value: string | null) {
    assertDefined(value);
    // value is string here
    console.log(value.toUpperCase());
}
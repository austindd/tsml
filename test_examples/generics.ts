// Test Generic Types with Constraints and Variance

// Basic generic function
function identity<T>(x: T): T {
    return x;
}

// Generic with constraint
function getLength<T extends { length: number }>(obj: T): number {
    return obj.length;
}

getLength("hello");     // OK - string has length
getLength([1, 2, 3]);   // OK - array has length
getLength({ length: 5, width: 10 }); // OK - has length property

// Multiple type parameters
function pair<T, U>(first: T, second: U): [T, U] {
    return [first, second];
}

// Generic class
class Container<T> {
    private value: T;

    constructor(value: T) {
        this.value = value;
    }

    getValue(): T {
        return this.value;
    }

    map<U>(fn: (value: T) => U): Container<U> {
        return new Container(fn(this.value));
    }
}

// Generic interface
interface Comparable<T> {
    compareTo(other: T): number;
}

// Generic constraint with keyof
function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
    return obj[key];
}

const person = { name: "Alice", age: 30 };
const name = getProperty(person, "name");  // string
const age = getProperty(person, "age");    // number

// Generic with default type
interface Response<T = any> {
    data: T;
    status: number;
}

// Variance examples

// Covariant position (return type)
interface Producer<out T> {
    produce(): T;
}

// Contravariant position (parameter type)
interface Consumer<in T> {
    consume(item: T): void;
}

// Invariant position (both parameter and return)
interface Processor<T> {
    process(input: T): T;
}

// Conditional types
type IsString<T> = T extends string ? true : false;
type Test1 = IsString<"hello">;  // true
type Test2 = IsString<42>;       // false

// Mapped types
type Readonly<T> = {
    readonly [P in keyof T]: T[P];
};

type Partial<T> = {
    [P in keyof T]?: T[P];
};

// Infer in conditional types
type ReturnType<T> = T extends (...args: any[]) => infer R ? R : never;
type Result = ReturnType<() => string>;  // string

// Generic type alias
type Pair<T, U> = [T, U];
type StringNumberPair = Pair<string, number>;

// Recursive generic type
type DeepReadonly<T> = {
    readonly [P in keyof T]: DeepReadonly<T[P]>;
};

// Higher-order generics
type Functor<F> = {
    map<A, B>(fn: (a: A) => B): (fa: F<A>) => F<B>;
};

// Generic constraints with unions
function processValue<T extends string | number>(value: T): T {
    if (typeof value === "string") {
        return value.toUpperCase() as T;
    }
    return value * 2 as T;
}

// Generic rest parameters
function merge<T extends object[]>(...objects: T): UnionToIntersection<T[number]> {
    return Object.assign({}, ...objects);
}

// Utility type for merge
type UnionToIntersection<U> =
    (U extends any ? (k: U) => void : never) extends ((k: infer I) => void) ? I : never;

// Template literal types with generics
type EventName<T extends string> = `on${Capitalize<T>}`;
type ClickEvent = EventName<"click">;  // "onClick"
// Test Row Polymorphism and Principal Type Inference

// Basic row polymorphism - function works on any record with 'x' field
function getX<T extends {x: number}>(obj: T): number {
    return obj.x;
}

// Test with different objects
const point2d = {x: 10, y: 20};
const point3d = {x: 10, y: 20, z: 30};
const named = {x: 5, y: 10, name: "origin"};

// All should work - row polymorphism preserves extra fields
const x1 = getX(point2d);  // Works with {x, y}
const x2 = getX(point3d);  // Works with {x, y, z}
const x3 = getX(named);    // Works with {x, y, name}

// Principal type inference - most general type
function identity(x) {
    return x;  // Should infer: ∀α. α → α
}

// Row extension
function addZ<T extends {x: number, y: number}>(point: T) {
    return {...point, z: 0};  // Returns T & {z: number}
}

// Width subtyping
function process(obj: {x: number}) {
    return obj.x * 2;
}

// More fields is subtype
const detailed = {x: 1, y: 2, z: 3, w: 4};
process(detailed);  // OK - width subtyping

// Field order doesn't matter (commutation)
const xy = {x: 1, y: 2};
const yx = {y: 2, x: 1};
// Both should have same type

// Function that preserves row variable
function mapPoint<R>(point: {x: number, y: number} & R, fn: (n: number) => number): {x: number, y: number} & R {
    return {
        ...point,
        x: fn(point.x),
        y: fn(point.y)
    };
}

// Extra fields preserved
const colorPoint = {x: 10, y: 20, color: "red"};
const scaled = mapPoint(colorPoint, n => n * 2);
// scaled should still have color field

// Nested row polymorphism
function getNestedX<T extends {pos: {x: number}}>(obj: T) {
    return obj.pos.x;
}

const nested1 = {pos: {x: 1, y: 2}, id: "a"};
const nested2 = {pos: {x: 3, y: 4, z: 5}, name: "b"};
getNestedX(nested1);
getNestedX(nested2);
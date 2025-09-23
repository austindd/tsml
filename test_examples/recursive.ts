// Test Recursive Types

// Simple linked list
type LinkedList<T> = {
    value: T;
    next: LinkedList<T> | null;
};

const list: LinkedList<number> = {
    value: 1,
    next: {
        value: 2,
        next: {
            value: 3,
            next: null
        }
    }
};

// Tree structure
interface TreeNode<T> {
    value: T;
    children: TreeNode<T>[];
}

const tree: TreeNode<string> = {
    value: "root",
    children: [
        {
            value: "child1",
            children: []
        },
        {
            value: "child2",
            children: [
                {
                    value: "grandchild",
                    children: []
                }
            ]
        }
    ]
};

// JSON type (recursive union)
type JSONValue =
    | string
    | number
    | boolean
    | null
    | JSONValue[]
    | { [key: string]: JSONValue };

const json: JSONValue = {
    name: "test",
    count: 42,
    active: true,
    tags: ["a", "b", "c"],
    nested: {
        x: 1,
        y: [2, 3, {z: 4}]
    }
};

// Self-referential class
class Node {
    value: number;
    parent: Node | null;
    children: Node[];

    constructor(value: number) {
        this.value = value;
        this.parent = null;
        this.children = [];
    }

    addChild(child: Node): void {
        child.parent = this;
        this.children.push(child);
    }
}

// Mutually dependent types (through interface)
interface Comment {
    id: string;
    text: string;
    author: User;
    replies: Comment[];
}

interface User {
    name: string;
    comments: Comment[];
}

// Graph node
type GraphNode<T> = {
    value: T;
    edges: GraphNode<T>[];
};

// Recursive function type
type RecursiveFn = (x: number) => RecursiveFn;

// State machine with recursive transitions
type State =
    | { type: "idle" }
    | { type: "loading", next: State }
    | { type: "success", data: any, reset: () => State }
    | { type: "error", retry: () => State };

// Infinite stream
type Stream<T> = {
    head: T;
    tail: () => Stream<T>;
};

// Helper function for streams
function streamFrom(n: number): Stream<number> {
    return {
        head: n,
        tail: () => streamFrom(n + 1)
    };
}
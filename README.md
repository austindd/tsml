# 🚀 tsml

**An experimental TypeScript/JavaScript compiler written in Roc**

[![Built with Roc](https://img.shields.io/badge/Built%20with-Roc-purple.svg)](https://www.roc-lang.org/)
[![TypeScript Support](https://img.shields.io/badge/TypeScript-Supported-blue.svg)](https://www.typescriptlang.org/)
[![ESTree Compatible](https://img.shields.io/badge/ESTree-Compatible-green.svg)](https://github.com/estree/estree)

tsml is a modern compiler pipeline that transforms TypeScript and JavaScript source code into ESTree-compliant Abstract Syntax Trees (AST). Built entirely in Roc, it demonstrates functional programming principles applied to language tooling.

## ✨ Features

### 🎯 **Comprehensive TypeScript Support**
- **Interface Declarations** with inheritance and optional properties
- **Type Aliases** for custom type definitions
- **Generic Types** with type parameters, constraints, and defaults
- **Intersection Types** (`A & B & C`)
- **Union Types** (`string | number | boolean`)
- **Index Signatures** for dynamic property access (`[key: string]: any`)
- **Literal Types** (string, number, boolean, bigint literals)
- **Template Literal Types** with expression interpolation
- **Enum Declarations** with numeric, string, and computed values
- **Const Enums** for compile-time constants
- **Array Types** with unlimited nesting (`string[][]`)
- **Tuple Types** with literal values (`[1, 2, 3]`)
- **Object Type Literals** (`{ x: number; y: number }`)
- **Variable Type Annotations** on all declaration kinds
- **Function Type Annotations** with generic parameters
- **Decorators** on classes and methods (`@sealed`, `@injectable()`)

### 🔧 **JavaScript Language Features**
- **Async/Await** functions and expressions
- **Arrow Functions** with proper scoping
- **Template Literals** with expression interpolation
- **Classes and Methods** with inheritance
- **All Expression Types** (binary, unary, logical, conditional)
- **Variable Declarations** (const, let, var)
- **Control Flow** (if/else, loops, switch)

### 🏗️ **Robust Architecture**
- **ESTree-compliant** AST generation
- **Recursive descent** parser implementation
- **Full tokenization** with trivia preservation
- **Multiple ES versions** supported (ES5-ES2026)
- **Interactive CLI** for real-time exploration

## 🎨 Live Examples

### TypeScript Interfaces
```typescript
interface User {
    name: string;
    age?: number;
}

interface Admin extends User {
  permissions: string[];
}
```

**Generates:**
```
TSInterfaceDeclaration {
  id: Identifier { name: "User" },
  body: TSInterfaceBody {
    body: [
      TSPropertySignature {
        key: Identifier { name: "name" },
        optional: Bool.false,
        readonly: Bool.false,
        typeAnnotation: TSStringKeyword
      },
      TSPropertySignature {
        key: Identifier { name: "age" },
        optional: Bool.true,
        readonly: Bool.false,
        typeAnnotation: TSNumberKeyword
      }
    ]
  }
}

TSInterfaceDeclaration {
  id: Identifier { name: "Admin" },
  body: TSInterfaceBody {
    body: [
      TSPropertySignature {
        key: Identifier { name: "permissions" },
        optional: Bool.false,
        readonly: Bool.false,
        typeAnnotation: TSArrayType {
          elementType: TSStringKeyword
        }
      }
    ]
  },
  extends: [
    TSTypeReference {
      typeName: Identifier { name: "User" },
      typeParameters: [...0 items]
    }
  ]
}
```

### Complex Type Compositions
```typescript
type Matrix = number[][];
type StringOrNumber = string | number;
type MixedArray = (string | number)[];
type Point = [number, number];
```

**Generates:**
```
TSTypeAliasDeclaration {
  id: Identifier { name: "Matrix" },
  typeAnnotation: TSArrayType {
    elementType: TSArrayType {
      elementType: TSNumberKeyword
    }
  }
}

TSTypeAliasDeclaration {
  id: Identifier { name: "StringOrNumber" },
  typeAnnotation: TSUnionType {
    types: [
      TSStringKeyword,
      TSNumberKeyword
    ]
  }
}

TSTypeAliasDeclaration {
  id: Identifier { name: "MixedArray" },
  typeAnnotation: TSArrayType {
    elementType: TSUnionType {
      types: [
        TSStringKeyword,
        TSNumberKeyword
      ]
    }
  }
}

TSTypeAliasDeclaration {
  id: Identifier { name: "Point" },
  typeAnnotation: TSTupleType {
    elementTypes: [
      TSNumberKeyword,
      TSNumberKeyword
    ]
  }
}
```

### Variable Type Annotations
```typescript
const user: { name: string; age: number } = { name: "Alice", age: 30 };
```

**Generates:**
```
VariableDeclaration {
  kind: Const,
  declarations: [
    VariableDeclarator {
      id: Identifier { name: "user" },
      typeAnnotation: TSTypeLiteral {
        members: [
          TSPropertySignature {
            key: Identifier { name: "name" },
            optional: Bool.false,
            readonly: Bool.false,
            typeAnnotation: TSStringKeyword
          },
          TSPropertySignature {
            key: Identifier { name: "age" },
            optional: Bool.false,
            readonly: Bool.false,
            typeAnnotation: TSNumberKeyword
          },
        ]
      },
      init: ObjectExpression {
        properties: [
          Property {
            kind: Init,
            key: Identifier { name: "name" },
            value: StringLiteral { value: ""Alice"" }
          },
          Property {
            kind: Init,
            key: Identifier { name: "age" },
            value: NumberLiteral { value: "30" }
          }
        ]
      }
    },
  ]
}
```

### Modern JavaScript Features
```javascript
async function fetchData(url) {
  const response = await fetch(url);
  return response.json();
}
```

**Generates:**
```
FunctionDeclaration {
  id: Identifier { name: "fetchData" },
  params: [
    Identifier { name: "url" },
  ],
  body: FunctionBody {
    body: [
      VariableDeclaration {
        kind: Const,
        declarations: [
          VariableDeclarator {
            id: Identifier { name: "response" },
            init: AwaitExpression {
              argument: CallExpression {
                callee: Identifier { name: "fetch" },
                arguments: [
                  Identifier { name: "url" },
                ]
              }
            }
          },
        ]
      },
      ReturnStatement {
        CallExpression {
          callee: MemberExpression {
            object: Identifier { name: "response" },
            property: Identifier { name: "json" },
            computed: Bool.false
          },
          arguments: []
        }
      },
    ]
  },
  async: Bool.true,
  generator: Bool.false
}
```

### Generic Types
```typescript
interface Container<T> {
  value: T;
}

type Pair<T, U = string> = [T, U];

interface Constrained<T extends number> {
  data: T;
}

function identity<T>(value: T): T { return value; }
```

**Generates:**
```
TSInterfaceDeclaration {
  id: Identifier { name: "Container" },
  body: TSInterfaceBody {
    body: [
      TSPropertySignature {
        key: Identifier { name: "value" },
        optional: Bool.false,
        readonly: Bool.false,
        typeAnnotation: TSTypeReference {
          typeName: Identifier { name: "T" },
          typeParameters: [...0 items]
        }
      }
    ]
  },
  typeParameters: TSTypeParameterDeclaration {
    params: [
      TSTypeParameter { name: Identifier { name: "T" } }
    ]
  }
}

TSTypeAliasDeclaration {
  id: Identifier { name: "Pair" },
  typeParameters: TSTypeParameterDeclaration {
    params: [
      TSTypeParameter { name: Identifier { name: "T" } },
      TSTypeParameter { name: Identifier { name: "U" } = TSStringKeyword }
    ]
  },
  typeAnnotation: TSTupleType {
    elementTypes: [
      TSTypeReference {
        typeName: Identifier { name: "T" },
        typeParameters: [...0 items]
      },
      TSTypeReference {
        typeName: Identifier { name: "U" },
        typeParameters: [...0 items]
      }
    ]
  }
}

TSInterfaceDeclaration {
  id: Identifier { name: "Constrained" },
  body: TSInterfaceBody {
    body: [
      TSPropertySignature {
        key: Identifier { name: "data" },
        optional: Bool.false,
        readonly: Bool.false,
        typeAnnotation: TSTypeReference {
          typeName: Identifier { name: "T" },
          typeParameters: [...0 items]
        }
      }
    ]
  },
  typeParameters: TSTypeParameterDeclaration {
    params: [
      TSTypeParameter { name: Identifier { name: "T" } extends TSNumberKeyword }
    ]
  }
}

FunctionDeclaration {
  id: Identifier { name: "identity" },
  typeParameters: TSTypeParameterDeclaration {
    params: [
      TSTypeParameter { name: Identifier { name: "T" } }
    ]
  },
  params: [
    Identifier { name: "value" }
  ],
  body: FunctionBody {
    body: [
      ReturnStatement {
        argument: Identifier { name: "value" }
      }
    ]
  },
  async: Bool.false,
  generator: Bool.false
}
```

### Intersection and Literal Types
```typescript
type Person = { name: string } & { age: number };
type Status = "active" | "inactive" | "pending";
type Count = 0 | 1 | 2 | 3;
type Greeting = `Hello, ${string}!`;
```

**Generates:**
```
TSTypeAliasDeclaration {
  id: Identifier { name: "Person" },
  typeAnnotation: TSIntersectionType {
    types: [
      TSTypeLiteral {
        members: [
          TSPropertySignature {
            key: Identifier { name: "name" },
            optional: Bool.false,
            readonly: Bool.false,
            typeAnnotation: TSStringKeyword
          }
        ]
      },
      TSTypeLiteral {
        members: [
          TSPropertySignature {
            key: Identifier { name: "age" },
            optional: Bool.false,
            readonly: Bool.false,
            typeAnnotation: TSNumberKeyword
          }
        ]
      }
    ]
  }
}

TSTypeAliasDeclaration {
  id: Identifier { name: "Status" },
  typeAnnotation: TSUnionType {
    types: [
      TSLiteralType {
        literal: StringLiteral { value: ""active"" }
      },
      TSLiteralType {
        literal: StringLiteral { value: ""inactive"" }
      },
      TSLiteralType {
        literal: StringLiteral { value: ""pending"" }
      }
    ]
  }
}

TSTypeAliasDeclaration {
  id: Identifier { name: "Count" },
  typeAnnotation: TSUnionType {
    types: [
      TSLiteralType {
        literal: NumberLiteral { value: "0" }
      },
      TSLiteralType {
        literal: NumberLiteral { value: "1" }
      },
      TSLiteralType {
        literal: NumberLiteral { value: "2" }
      },
      TSLiteralType {
        literal: NumberLiteral { value: "3" }
      }
    ]
  }
}

TSTypeAliasDeclaration {
  id: Identifier { name: "Greeting" },
  typeAnnotation: TSTemplateLiteralType {
    quasis: [
      TemplateElement {
        value: "Hello, ",
        raw: "Hello, ",
        tail: Bool.false
      },
      TemplateElement {
        value: "!",
        raw: "!",
        tail: Bool.true
      }
    ],
    types: [
      TSStringKeyword
    ]
  }
}
```

### Index Signatures and Decorators
```typescript
type ReadonlyDict = {
  readonly [index: number]: string;
}

@sealed
@component({ selector: 'app-root' })
class AppComponent {
  id = 'app';
}
```

**Generates:**
```
TSTypeAliasDeclaration {
  id: Identifier { name: "ReadonlyDict" },
  typeAnnotation: TSTypeLiteral {
    members: [
      TSIndexSignature {
        parameters: [
          TSPropertySignature {
            key: Identifier { name: "index" },
            optional: Bool.false,
            readonly: Bool.false,
            typeAnnotation: TSNumberKeyword
          }
        ],
        typeAnnotation: TSStringKeyword,
        readonly: Bool.true
      }
    ]
  }
}

ClassDeclaration {
  id: Identifier { name: "AppComponent" },
  superClass: None,
  decorators: [
    Decorator {
      expression: Identifier { name: "sealed" }
    },
    Decorator {
      expression: NewExpression {
        callee: Identifier { name: "component" },
        arguments: [
          ObjectExpression {
            properties: [
              Property {
                kind: Init,
                key: Identifier { name: "selector" },
                value: StringLiteral { value: "'app-root'" }
              }
            ]
          }
        ]
      }
    }
  ],
  body: BlockStatement {
    body: [...0 items]
  },
}
```

### TypeScript Enums
```typescript
enum Color { Red, Green, Blue }

enum Status {
  Active = 1,
  Inactive = 0
}

const enum Direction {
  Up = "UP",
  Down = "DOWN"
}

enum Values {
  A = 1,
  B = A * 2,
  C = B + 1
}
```

**Generates:**
```
TSEnumDeclaration {
  id: Identifier { name: "Color" },
  members: [
    TSEnumMember { id: Identifier { name: "Red" } },
    TSEnumMember { id: Identifier { name: "Green" } },
    TSEnumMember { id: Identifier { name: "Blue" } }
  ]
}

TSEnumDeclaration {
  id: Identifier { name: "Status" },
  members: [
    TSEnumMember { id: Identifier { name: "Active" } = NumberLiteral { value: "1" } },
    TSEnumMember { id: Identifier { name: "Inactive" } = NumberLiteral { value: "0" } }
  ]
}

TSEnumDeclaration {
  const id: Identifier { name: "Direction" },
  members: [
    TSEnumMember { id: Identifier { name: "Up" } = StringLiteral { value: ""UP"" } },
    TSEnumMember { id: Identifier { name: "Down" } = StringLiteral { value: ""DOWN"" } }
  ]
}

TSEnumDeclaration {
  id: Identifier { name: "Values" },
  members: [
    TSEnumMember { id: Identifier { name: "A" } = NumberLiteral { value: "1" } },
    TSEnumMember { id: Identifier { name: "B" } = BinaryExpression {
        operator: *,
        left: Identifier { name: "A" },
        right: NumberLiteral { value: "2" }
      }
    },
    TSEnumMember { id: Identifier { name: "C" } = BinaryExpression {
        operator: +,
        left: Identifier { name: "B" },
        right: NumberLiteral { value: "1" }
      }
    }
  ]
}
```

## 🚀 Quick Start

### Prerequisites
- [Roc](https://www.roc-lang.org/) nightly build (included in repository)

### Installation
```bash
git clone https://github.com/yourusername/tsml.git
cd tsml
export PATH=$PATH:$(pwd)/roc_nightly-macos_apple_silicon-2025-03-22-c47a8e9cdac
```

### Interactive Mode
```bash
roc main.roc
```

Enter any TypeScript or JavaScript code and see the AST output in real-time:

```
Enter TypeScript/JavaScript code (or 'quit' to exit):
> interface Person { name: string; age: number }

✨ AST:
Program {
  sourceType: Module,
  body: [
    TSInterfaceDeclaration {
      id: Identifier { name: "Person" },
      body: TSInterfaceBody { body: [2 items] },
      extends: None
    },
  ]
}
```

## 🏛️ Architecture

### Processing Pipeline
```
Source Code → Tokenization → Trivia Filtering → Parsing → AST Generation
```

### Core Modules
- **`Token.roc`** - Comprehensive tokenizer with trivia support
- **`Parser.roc`** - Recursive descent parser with operator precedence
- **`Ast.roc`** - ESTree-compliant node definitions and utilities
- **`main.roc`** - Interactive CLI application

### Design Principles
- **Functional Programming** - Pure functions and immutable data structures
- **Type Safety** - Leveraging Roc's type system for correctness
- **Composability** - Modular architecture with clear interfaces
- **Standards Compliance** - Following ESTree and TypeScript specifications

## 🎯 Current Status

**✅ Completed Features:**
- Full TypeScript type system parsing including:
  - Generics with type parameters, constraints, and defaults
  - Intersection and union types
  - Literal and template literal types
  - Index signatures and decorators
- Modern JavaScript syntax support
- ESTree-compliant AST generation
- Interactive development environment
- Comprehensive test suite

**🚧 Future Roadmap:**
- Semantic analysis and type checking
- Code generation and optimization
- IDE language server protocol
- Conditional types and mapped types
- Source map generation
- Module resolution and imports/exports

## 🤝 Contributing

This is currently a hobby project, but contributions are welcome! Feel free to:

- **Report issues** or suggest features
- **Submit pull requests** with improvements
- **Add test cases** for edge cases
- **Improve documentation** and examples

## 📚 Learn More

- **Roc Language**: [roc-lang.org](https://www.roc-lang.org/)
- **ESTree Specification**: [github.com/estree/estree](https://github.com/estree/estree)
- **TypeScript Reference**: [typescriptlang.org](https://www.typescriptlang.org/)

---

**Built with ❤️ using [Roc](https://www.roc-lang.org/) - A fast, friendly, functional language.**

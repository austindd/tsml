# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **tsml**, an experimental TypeScript/JavaScript compiler written in Roc. It implements a tokenizer, parser, and AST (Abstract Syntax Tree) representation following the ESTree specification.

## Roc Language important details

- The new syntax for function definitions is as follows:

```roc
function_identifier : Type1, Type2 (List Type3) -> ReturnType
function_identifier = |param1, param2|
    expression_1
    expression_2
    return_expression
```

- The new syntax for function calls is as follows:

```roc
function_identifier_1(param1, param2, param3)

param_1 |> function_identifier_2(param2, param3)
```

- Roc does not support type annotations.

- Roc does not support mutually recursive type definitions. Mutually recursive functions are supported, but not types. We often use indexed access to avoid this issue in general.

- "Tag types" are basically the same thing as OCaml's polymorphic variants. They exist in a global namespace, so they never need to be imported or qualified. They are also structurally typed.

- When you get a "Type Mismatch" error from the compiler related to pattern matching it is often because of the cases has a slightly different structure compared to the type definition. This can be a simple difference in record field names, or accidentally adding an `Option` type wrapper around something that isn't optional. One way to debug this is to comment out portions of the pattern matching code to isolate the specific pattern that is wrong, since the compiler usually doesn't specify which pattern is wrong. To do this, you will need a wildcard case to get it to compile.

- When calling some effectful functions like `pf.Stdout.line!` or `pf.File.read_utf8!`, they return a `Result` type. You can use the `?` operator to thread the error type through the call chain, and it will simply combine all the error types into a union (they are always tag types). However, the current compiler version has a bug that causes code with `?` operators to explode the build time exponentially, so please avoid using them for now. Instead, I typically throw away the result by using the underscore pattern, like `_ = pf.Stdout.line!(arg)`, and only propagate errors that will be helpful to the user.

## Development Environment Setup

The project uses a local Roc installation:
```bash
# The project includes its own Roc nightly build in the repository
export PATH=$PATH:~/dev/tsml/roc_nightly-macos_apple_silicon-2025-03-22-c47a8e9cdac
export RUST_BACKTRACE=full
```

Use `direnv` to automatically load the environment from `.envrc`.

## Common Commands

### Running the Main Application
```bash
# Run the interactive parser
roc main.roc

# Run directly with dev mode (checks and runs if no errors)
roc dev main.roc

# Check for compilation errors without running
roc check main.roc

# Format code
roc format .
```

### Testing
```bash
# Run all tests
roc test

# Test specific modules
roc test TokenTest.roc

# Run individual test programs from tests directory
roc dev tests/test_tuple_types.roc
roc dev tests/test_async_await.roc
roc dev tests/test_typescript.roc
```

### Building
```bash
# Build binary without running
roc build main.roc
```

## Architecture

### Core Components

1. **Token.roc** - Tokenizer module that converts source code into tokens
   - Handles all TypeScript/JavaScript tokens including literals, operators, keywords
   - Supports trivia tokens (whitespace, comments) for full source reconstruction
   - Main functions: `tokenize_str`, `ts_token_debug_display`

2. **Parser.roc** - Recursive descent parser that converts tokens to AST
   - Entry point: `parse_program`
   - Parses statements, expressions, declarations following ESTree spec
   - Filters out trivia tokens before parsing

3. **Ast.roc** - AST node definitions and utilities
   - Implements ESTree-compliant node types
   - ES version support (Es5 through Es2026)
   - Main functions: `node_to_str` for AST display

4. **ComprehensiveTypeIndexed.roc** - Type inference for indexed types
    - Entry point: `infer_type`
    - Uses a type constraint solver to infer types
    - Uses indexed access to avoid mutual recursion, which is not currently supported by Roc

5. **main.roc** - Interactive CLI application
   - Tokenizes input → Filters trivia → Parses → Displays AST
   - Entry point for testing the compiler pipeline

### Utility Modules

- **Option.roc** - Optional value handling
- **ListUtils.roc**, **StrUtils.roc**, **NumUtils.roc** - Common utilities
- **Stack.roc**, **StackMap.roc**, **ListMap.roc** - Data structures
- **SymTbl.roc**, **SymTblStack.roc** - Symbol table management
- **SourceLocation.roc** - Source position tracking
- **Utf8Char.roc** - UTF-8 character handling

### Processing Pipeline

1. **Input** → **Tokenization** (Token.roc)
2. **Tokens** → **Trivia Filtering** (main.roc)
3. **Clean Tokens** → **Parsing** (Parser.roc)
4. **AST Nodes** → **Display** (Ast.roc)

## Key Implementation Details

- Written in Roc language using functional programming paradigms
- Follows ESTree specification for AST node structure
- Supports multiple ECMAScript versions (ES5-ES2026)
- Trivia tokens are preserved during tokenization but filtered before parsing
- Uses recursive descent parsing strategy
- Symbol tables track variable scoping and declarations

## Testing Strategy

Test files are organized in the `tests/` directory using the pattern `test_*.roc` and can be run individually with `roc dev tests/<filename>`. The `TokenTest.roc` module contains unit tests that can be run with `roc test TokenTest.roc`.

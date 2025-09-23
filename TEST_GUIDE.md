# Testing Guide for TypeScript/JavaScript Type Checker

## 🚀 Quick Start Testing

### Step 1: Check Individual Modules
Each module can be checked independently first:

```bash
# Check each module compiles correctly
roc check IntegratedRowTypeChecker.roc
roc check SimpleRecursiveTypes.roc
roc check AsyncTypes.roc
roc check GenericsTypes.roc
roc check UnionIntersectionTypes.roc
roc check ControlFlowNarrowing.roc
roc check TypeScriptModuleSystem.roc
roc check UtilityTypes.roc
roc check GradualTypes.roc
```

### Step 2: Run Test Suite
```bash
# Run the comprehensive test suite
roc dev TypeSystemTests.roc
```

### Step 3: Try the CLI
```bash
# Run the CLI in interactive mode
roc dev TypeCheckerCLI.roc interactive

# Check a test file
roc dev TypeCheckerCLI.roc check test_examples/basic.ts
```

## 📝 Test Examples to Create

Create a `test_examples/` directory with these test files:

### 1. Row Polymorphism Test (`test_examples/row_poly.ts`)
Test principal type inference and structural typing

### 2. Recursive Types Test (`test_examples/recursive.ts`)
Test self-referential structures

### 3. Async/Promise Test (`test_examples/async.ts`)
Test async/await and Promise types

### 4. Generics Test (`test_examples/generics.ts`)
Test type parameters and constraints

### 5. Union/Intersection Test (`test_examples/unions.ts`)
Test union and intersection operations

### 6. Control Flow Test (`test_examples/narrowing.ts`)
Test type guards and narrowing

### 7. Utility Types Test (`test_examples/utility.ts`)
Test Partial, Required, Pick, Omit, etc.

### 8. Gradual Types Test (`test_examples/gradual.ts`)
Test any and unknown handling

## 🧪 Manual Testing Steps

### Test Row Polymorphism
1. Create functions that work on partial record types
2. Verify principal types are inferred
3. Check that extra fields are preserved

### Test Recursive Types
1. Define linked lists, trees
2. Create recursive class definitions
3. Test JSON-like recursive structures

### Test Async Types
1. Write async functions
2. Use Promise.all, Promise.race
3. Test await expressions

### Test Type Narrowing
1. Use typeof guards
2. Test instanceof checks
3. Verify discriminated union narrowing

## 🔍 Debugging Tips

### If modules don't compile:
1. Check for Roc syntax issues (use `|param|` not `\param`)
2. Verify no mutual recursion (Roc limitation)
3. Ensure all imports are correct

### If tests fail:
1. Run individual test functions
2. Add debug output to see actual vs expected
3. Check type unification traces

### Common Issues:
- **"Outstanding references"** - Module dependency issue
- **Stack overflow** - Recursive type too deep
- **Pattern match failure** - Missing case in when expression
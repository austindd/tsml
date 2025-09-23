#!/bin/bash

# TypeScript/JavaScript Type Checker Test Runner

echo "=================================="
echo "TypeScript Type Checker Test Suite"
echo "=================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counter for results
PASSED=0
FAILED=0

# Function to run a test
run_test() {
    local module=$1
    local description=$2

    echo -n "Testing $description... "

    if roc check "$module" 2>/dev/null; then
        echo -e "${GREEN}✓ PASSED${NC}"
        ((PASSED++))
    else
        echo -e "${RED}✗ FAILED${NC}"
        ((FAILED++))
        echo "  Error in $module"
    fi
}

# Function to test compilation
test_compilation() {
    echo "=== Module Compilation Tests ==="
    echo ""

    run_test "IntegratedRowTypeChecker.roc" "Row Polymorphism"
    run_test "SimpleRecursiveTypes.roc" "Recursive Types"
    run_test "AsyncTypes.roc" "Async/Promise Types"
    run_test "GenericsTypes.roc" "Generic Types"
    run_test "UnionIntersectionTypes.roc" "Union/Intersection"
    run_test "ControlFlowNarrowing.roc" "Control Flow Narrowing"
    run_test "TypeScriptModuleSystem.roc" "Module System"
    run_test "UtilityTypes.roc" "Utility Types"
    run_test "GradualTypes.roc" "Gradual Typing"

    echo ""
}

# Function to run the test suite
run_test_suite() {
    echo "=== Running Test Suite ==="
    echo ""

    if roc dev TypeSystemTests.roc 2>/dev/null; then
        echo -e "${GREEN}✓ Test suite completed${NC}"
        ((PASSED++))
    else
        echo -e "${RED}✗ Test suite failed${NC}"
        ((FAILED++))
    fi

    echo ""
}

# Function to test CLI
test_cli() {
    echo "=== CLI Tests ==="
    echo ""

    echo -n "Testing CLI compilation... "
    if roc check TypeCheckerCLI.roc 2>/dev/null; then
        echo -e "${GREEN}✓ PASSED${NC}"
        ((PASSED++))
    else
        echo -e "${RED}✗ FAILED${NC}"
        ((FAILED++))
    fi

    echo ""
}

# Function to test example files
test_examples() {
    echo "=== TypeScript Example Files ==="
    echo ""

    for file in test_examples/*.ts; do
        if [ -f "$file" ]; then
            basename=$(basename "$file")
            echo -e "${YELLOW}📄 $basename${NC}"
        fi
    done

    echo ""
}

# Main execution
main() {
    # Check if Roc is installed
    if ! command -v roc &> /dev/null; then
        echo -e "${RED}Error: Roc is not installed or not in PATH${NC}"
        echo "Please ensure Roc is installed and available"
        exit 1
    fi

    # Run all tests
    test_compilation
    run_test_suite
    test_cli
    test_examples

    # Summary
    echo "=================================="
    echo "Test Results Summary"
    echo "=================================="
    echo -e "Passed: ${GREEN}$PASSED${NC}"
    echo -e "Failed: ${RED}$FAILED${NC}"

    if [ $FAILED -eq 0 ]; then
        echo ""
        echo -e "${GREEN}🎉 All tests passed!${NC}"
        exit 0
    else
        echo ""
        echo -e "${RED}❌ Some tests failed${NC}"
        exit 1
    fi
}

# Run main function
main
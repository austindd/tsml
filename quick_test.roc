app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Task exposing [Task]

main =
    Stdout.line "Testing TypeScript/JavaScript Type Checker Components..."
    |> Task.then \_ ->

        # Test each module compilation
        modules = [
            "UtilityTypes.roc",
            "GradualTypes.roc",
            "SimpleRecursiveTypes.roc",
            "AsyncTypes.roc",
            "GenericsTypes.roc",
            "UnionIntersectionTypes.roc",
            "ControlFlowNarrowing.roc",
            "TypeScriptModuleSystem.roc",
        ]

        Stdout.line ""
        |> Task.then \_ -> Stdout.line "Modules that should compile:"
        |> Task.then \_ ->
            Task.forEach modules \module ->
                Stdout.line "  - $(module)"
        |> Task.then \_ -> Stdout.line ""
        |> Task.then \_ -> Stdout.line "Test TypeScript examples available in test_examples/:"
        |> Task.then \_ ->

            examples = [
                "row_poly.ts - Row polymorphism and principal types",
                "recursive.ts - Recursive and self-referential types",
                "async.ts - Async/await and Promise types",
                "generics.ts - Generic parameters and constraints",
                "unions.ts - Union and intersection types",
                "narrowing.ts - Control flow type narrowing",
                "utility.ts - TypeScript utility types",
                "gradual.ts - any and unknown types",
            ]

            Task.forEach examples \example ->
                Stdout.line "  - $(example)"
        |> Task.then \_ -> Stdout.line ""
        |> Task.then \_ -> Stdout.line "To test individual modules:"
        |> Task.then \_ -> Stdout.line "  roc check <module_name>"
        |> Task.then \_ -> Stdout.line ""
        |> Task.then \_ -> Stdout.line "To run the test script:"
        |> Task.then \_ -> Stdout.line "  ./run_tests.sh"

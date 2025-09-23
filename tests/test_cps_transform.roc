app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.Task

import "../Token" as Token
import "../Parser" as Parser
import "../CpsTransform" as CpsTransform
import "../Ast" as Ast

main : Task.Task {} _
main =
    test_cases = [
        (
            "Simple expression",
            "2 + 3",
        ),
        (
            "Variable declaration",
            "const x = 10;",
        ),
        (
            "Function declaration",
            "function add(a, b) { return a + b; }",
        ),
        (
            "Function call",
            "add(2, 3)",
        ),
        (
            "Conditional expression",
            "x > 5 ? 'yes' : 'no'",
        ),
        (
            "If statement",
            "if (x > 5) { console.log('yes'); } else { console.log('no'); }",
        ),
        (
            "While loop",
            "while (i < 10) { i++; }",
        ),
        (
            "For loop",
            "for (let i = 0; i < 10; i++) { console.log(i); }",
        ),
        (
            "Try-catch",
            "try { risky(); } catch (e) { handleError(e); }",
        ),
        (
            "Array expression",
            "[1, 2, 3]",
        ),
        (
            "Object expression",
            "{ name: 'Alice', age: 30 }",
        ),
        (
            "Arrow function",
            "const fn = (x) => x * 2;",
        ),
        (
            "Member expression",
            "obj.prop",
        ),
        (
            "Computed member expression",
            "obj[key]",
        ),
        (
            "Assignment expression",
            "x = 10",
        ),
        (
            "Complex nested expression",
            "f(g(h(x)))",
        ),
        (
            "Return in function",
            "function test() { if (x) { return 1; } return 2; }",
        ),
        (
            "Async function",
            "async function fetchData() { const result = await fetch('/api'); return result; }",
        ),
    ]

    test_results = List.map(test_cases, run_test_case)

    Stdout.line!(format_test_results(test_results))

run_test_case : (Str, Str) -> (Str, Result Str Str)
run_test_case = |(name, source)|
    result =
        tokens = Token.tokenize_str(source)
        filtered_tokens = List.keep_oks(tokens, |token_result|
            when token_result is
                Ok(token) ->
                    when token is
                        Token.Trivia(_) -> Err(Unit)
                        _ -> Ok(token)
                Err(e) -> Err(e)
        )

        when Parser.parse_program(filtered_tokens) is
            Ok((ast, _remaining)) ->
                cps_ast = CpsTransform.transform_to_cps(ast)
                Ok(Ast.node_to_str_with_max_depth(cps_ast, 5))
            Err(e) ->
                Err("Parse error: $(e)")

    (name, result)

format_test_results : List (Str, Result Str Str) -> Str
format_test_results = |results|
    formatted = List.map(results, |result|
        (name, test_result) = result
        when test_result is
            Ok(cps_output) ->
                """
                Test: $(name)
                CPS Output:
                $(cps_output)
                ✓ Success
                ────────────────────────────────────
                """
            Err(error) ->
                """
                Test: $(name)
                ✗ Error: $(error)
                ────────────────────────────────────
                """
    )

    Str.join_with(formatted, "\n")
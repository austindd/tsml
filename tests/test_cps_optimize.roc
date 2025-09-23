app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.Task

import "../Ast" as Ast
import "../CpsOptimize" as CpsOptimize
import "../Option" as Option

main : Task.Task {} _
main =
    test_cases = [
        test_beta_reduction(),
        test_dead_elimination(),
        test_constant_folding(),
        test_conditional_folding(),
        test_combined_optimizations(),
    ]

    results = List.map(test_cases, format_test_result)
    output = Str.join_with(results, "\n\n")

    Stdout.line!(output)

format_test_result : (Str, Ast.Node, Ast.Node, CpsOptimize.OptimizationStats) -> Str
format_test_result = |(name, input, output, stats)|
    """
    Test: $(name)
    ────────────────────
    Input AST:
    $(Ast.node_to_str(input))

    Optimized AST:
    $(Ast.node_to_str(output))

    Statistics:
    - Beta reductions: $(Num.to_str(stats.beta_reductions))
    - Dead eliminations: $(Num.to_str(stats.dead_eliminations))
    - Constant folds: $(Num.to_str(stats.constant_folds))
    - Tail call optimizations: $(Num.to_str(stats.tail_call_optimizations))
    """

test_beta_reduction : {} -> (Str, Ast.Node, Ast.Node, CpsOptimize.OptimizationStats)
test_beta_reduction = |{}|
    # Test: ((function(x) { return x + 1; })(5))
    # Should reduce to: 5 + 1

    input = Ast.CallExpression({
        callee: Ast.FunctionExpression({
            id: Option.None,
            params: [Ast.Identifier({ name: "x" })],
            body: Ast.BlockStatement({
                body: [
                    Ast.ReturnStatement({
                        argument: Option.Some(
                            Ast.BinaryExpression({
                                left: Ast.Identifier({ name: "x" }),
                                operator: Ast.Plus,
                                right: Ast.NumberLiteral({ value: "1" }),
                            })
                        ),
                    }),
                ],
            }),
            generator: Bool.false,
            async: Bool.false,
            typeParameters: Option.None,
        }),
        arguments: [Ast.NumberLiteral({ value: "5" })],
    })

    (output, stats) = CpsOptimize.optimize_cps(input)

    ("Beta Reduction", input, output, stats)

test_dead_elimination : {} -> (Str, Ast.Node, Ast.Node, CpsOptimize.OptimizationStats)
test_dead_elimination = |{}|
    # Test: var unused = 5; var used = 10; return used;
    # Should eliminate 'unused'

    input = Ast.BlockStatement({
        body: [
            Ast.VariableDeclaration({
                declarations: [
                    Ast.VariableDeclarator({
                        id: Ast.Identifier({ name: "unused" }),
                        init: Option.Some(Ast.NumberLiteral({ value: "5" })),
                        typeAnnotation: Option.None,
                    }),
                ],
                kind: Ast.Const,
            }),
            Ast.VariableDeclaration({
                declarations: [
                    Ast.VariableDeclarator({
                        id: Ast.Identifier({ name: "used" }),
                        init: Option.Some(Ast.NumberLiteral({ value: "10" })),
                        typeAnnotation: Option.None,
                    }),
                ],
                kind: Ast.Const,
            }),
            Ast.ReturnStatement({
                argument: Option.Some(Ast.Identifier({ name: "used" })),
            }),
        ],
    })

    (output, stats) = CpsOptimize.optimize_cps(input)

    ("Dead Code Elimination", input, output, stats)

test_constant_folding : {} -> (Str, Ast.Node, Ast.Node, CpsOptimize.OptimizationStats)
test_constant_folding = |{}|
    # Test: 2 + 3 * 4
    # Should fold to constants

    input = Ast.BinaryExpression({
        left: Ast.NumberLiteral({ value: "2" }),
        operator: Ast.Plus,
        right: Ast.BinaryExpression({
            left: Ast.NumberLiteral({ value: "3" }),
            operator: Ast.Times,
            right: Ast.NumberLiteral({ value: "4" }),
        }),
    })

    (output, stats) = CpsOptimize.optimize_cps(input)

    ("Constant Folding", input, output, stats)

test_conditional_folding : {} -> (Str, Ast.Node, Ast.Node, CpsOptimize.OptimizationStats)
test_conditional_folding = |{}|
    # Test: true ? 'yes' : 'no'
    # Should fold to 'yes'

    input = Ast.ConditionalExpression({
        test: Ast.BooleanLiteral({ value: Bool.true }),
        consequent: Ast.StringLiteral({ value: "yes" }),
        alternate: Ast.StringLiteral({ value: "no" }),
    })

    (output, stats) = CpsOptimize.optimize_cps(input)

    ("Conditional Constant Folding", input, output, stats)

test_combined_optimizations : {} -> (Str, Ast.Node, Ast.Node, CpsOptimize.OptimizationStats)
test_combined_optimizations = |{}|
    # Test: Complex expression with multiple optimization opportunities
    # ((function(x) { return x * 2; })(3 + 4))
    # Should fold 3 + 4 = 7, then beta reduce to 7 * 2, then fold to 14

    input = Ast.CallExpression({
        callee: Ast.FunctionExpression({
            id: Option.None,
            params: [Ast.Identifier({ name: "x" })],
            body: Ast.BlockStatement({
                body: [
                    Ast.ReturnStatement({
                        argument: Option.Some(
                            Ast.BinaryExpression({
                                left: Ast.Identifier({ name: "x" }),
                                operator: Ast.Times,
                                right: Ast.NumberLiteral({ value: "2" }),
                            })
                        ),
                    }),
                ],
            }),
            generator: Bool.false,
            async: Bool.false,
            typeParameters: Option.None,
        }),
        arguments: [
            Ast.BinaryExpression({
                left: Ast.NumberLiteral({ value: "3" }),
                operator: Ast.Plus,
                right: Ast.NumberLiteral({ value: "4" }),
            })
        ],
    })

    (output, stats) = CpsOptimize.optimize_cps(input)

    ("Combined Optimizations", input, output, stats)
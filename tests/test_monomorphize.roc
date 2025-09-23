app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.Task

import "../Ast" as Ast
import "../Monomorphize" as Monomorphize
import "../Option" as Option

main : Task.Task {} _
main =
    # Test case: A generic identity function called with different types
    test_ast = Ast.Program({
        body: [
            # function identity(x) { return x; }
            Ast.FunctionDeclaration({
                id: Ast.Identifier({ name: "identity" }),
                params: [Ast.Identifier({ name: "x" })],
                body: Ast.BlockStatement({
                    body: [
                        Ast.ReturnStatement({
                            argument: Option.Some(Ast.Identifier({ name: "x" })),
                        }),
                    ],
                }),
                generator: Bool.false,
                async: Bool.false,
                typeParameters: Option.None,
            }),

            # identity(5)
            Ast.CallExpression({
                callee: Ast.Identifier({ name: "identity" }),
                arguments: [Ast.NumberLiteral({ value: "5" })],
            }),

            # identity("hello")
            Ast.CallExpression({
                callee: Ast.Identifier({ name: "identity" }),
                arguments: [Ast.StringLiteral({ value: "hello" })],
            }),

            # identity(true)
            Ast.CallExpression({
                callee: Ast.Identifier({ name: "identity" }),
                arguments: [Ast.BooleanLiteral({ value: Bool.true })],
            }),
        ],
        sourceType: Ast.Module,
    })

    # Apply monomorphization
    monomorphized_ast = Monomorphize.monomorphize(test_ast)

    # Display results
    output = """
    Monomorphization Test
    =====================

    Original AST:
    $(Ast.node_to_str(test_ast))

    Monomorphized AST:
    $(Ast.node_to_str(monomorphized_ast))

    Expected: The identity function should be specialized into three versions:
    - identity_num_0 for numbers
    - identity_str_1 for strings
    - identity_bool_2 for booleans
    """

    Stdout.line!(output)
#!/usr/bin/env roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import Token
import Parser
import TypeCore
import SimpleConstraint
import SimpleUnify

main! = \_ ->
    # Test cases
    test_cases = [
        "42",
        "true",
        "'hello'",
        "[1, 2, 3]",
        "{ x: 10, y: 20 }",
        "x + y",
    ]

    _ = List.walk test_cases {} \_, code ->
        _ = Stdout.line! "\n=== Testing: $(code) ==="

        # Tokenize and parse
        tokens = Token.tokenize_str code
        filtered = List.drop_if tokens is_trivia_token
        ast = Parser.parse_program filtered

        # Create type table
        initial_table = TypeCore.new_type_table {}

        # Generate constraints
        (result_tid, constraint_set) = SimpleConstraint.generate_simple_constraints ast initial_table

        _ = Stdout.line! "Generated $(Num.to_str (List.len constraint_set.constraints)) constraints"

        # Solve constraints
        _ = when SimpleUnify.unify_constraints constraint_set is
            Ok (subst, final_table) ->
                # Apply substitution to get final type
                final_tid = SimpleUnify.apply_subst result_tid subst
                type_str = TypeCore.type_to_string final_table final_tid
                Stdout.line! "Type: $(type_str)"
            Err error ->
                error_str = format_unify_error error
                Stdout.line! "Error: $(error_str)"

        {}

    Stdout.line! "\n✅ All tests completed!"
    Ok {}

is_trivia_token : Token.Token -> Bool
is_trivia_token = \token ->
    when token is
        WhitespaceTrivia _ | NewLineTrivia _ | LineCommentStart | BlockCommentStart | BlockCommentEnd | CommentText _ | ShebangTrivia | ConflictMarkerTrivia | NonTextFileMarkerTrivia -> Bool.true
        _ -> Bool.false

format_unify_error : SimpleUnify.UnifyError -> Str
format_unify_error = \error ->
    when error is
        TypeMismatch t1 t2 -> "Type mismatch: $(Num.to_str t1) != $(Num.to_str t2)"
        OccursCheck v t -> "Infinite type: $(Num.to_str v) occurs in $(Num.to_str t)"
        FieldMissing obj field -> "Missing field '$(field)' in object $(Num.to_str obj)"
        NotCallable t -> "Type $(Num.to_str t) is not callable"
module [
    TypeChecker,
    TypeError,
    ErrorKind,
    TypedNode,
    CheckResult,
    check_program,
    check_node,
    create_checker,
]

import Ast exposing [Node]
import SimpleComprehensiveType as Type exposing [Type]
import TypedSymbolTable as TST
import JSGlobals
import SourceLocation exposing [SourceLocation]
import BasicTypeInfer
import Option exposing [Option]

# Type checker state
TypeChecker : {
    inferred : List Type,
    errors : List TypeError,
    warnings : List TypeError,
    symbol_table : TST.SymbolTable,
    current_return_type : Result Type [NoReturn],
    in_loop : Bool,
    in_function : Bool,
    strict_mode : Bool,
    gradual_typing : Bool,
}

# Type error information
TypeError : {
    kind : ErrorKind,
    message : Str,
    location : Result SourceLocation [NoLocation],
    expected : Result Type [NoExpected],
    actual : Result Type [NoActual],
    suggestion : Result Str [NoSuggestion],
}

# Error kinds
ErrorKind : [
    TypeMismatch,
    UnknownVariable,
    UnknownProperty,
    NotCallable,
    WrongArgumentCount,
    ConstReassignment,
    InvalidReturn,
    InvalidBreak,
    InvalidContinue,
    UnreachableCode,
    MissingReturn,
    InvalidOperation,
    PropertyNotFound,
    IndexNotAllowed,
]

# Typed AST node
TypedNode : [
    TypedNode {
        original : Node,
        inferred_type : Type,
        errors : List TypeError,
        children : List TypedNode,
    }
]

# Result of checking
CheckResult : Result TypedNode (List TypeError)

# Create a new type checker
create_checker : Bool -> TypeChecker
create_checker = |strict_mode|
    {
        module_type: ESModule,
        strict_mode,
        gradual_typing: !strict_mode,  # Allow any/unknown in non-strict mode
        inferred: [],
        errors: [],
        warnings: [],
        symbol_table: TST.empty_table({}) |> JSGlobals.add_js_globals,
        current_return_type: Err(NoReturn),
        in_loop: Bool.false,
        in_function: Bool.false,
    }

# Check a complete program
check_program : Node, Bool -> Result TypedNode (List TypeError)
check_program = |program_node, strict_mode|
    checker = create_checker(strict_mode)
    result = check_node_internal(program_node, checker)

    when result is
        Ok((TypedNode(typed_node), final_checker)) ->
            if List.is_empty(final_checker.errors) then
                Ok(TypedNode(typed_node))
            else
                Err(final_checker.errors)
        Err errors ->
            Err(errors)

# Main checking function (internal)
check_node_internal : Node, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_node_internal = |node, checker|
    when node is
        Program { body } ->
            check_statement_list(body, checker)

        # Variable declarations
        VariableDeclaration { declarations, kind } ->
            check_variable_declaration(declarations, kind, checker)

        # Function declarations
        FunctionDeclaration { id, params, body } ->
            check_function_declaration(id, params, body, checker)

        # Expressions
        BinaryExpression { left, right, operator } ->
            check_binary_expression(left, right, operator, checker)

        UnaryExpression { argument, operator, prefix } ->
            check_unary_expression(argument, operator, prefix, checker)

        CallExpression { callee, arguments } ->
            check_call_expression(callee, arguments, checker)

        MemberExpression { object, property, computed } ->
            check_member_expression(object, property, computed, checker)

        Identifier { name } ->
            check_identifier(name, checker)

        NumericLiteral { value } ->
            Ok((
                TypedNode({
                    original: node,
                    inferred_type: Type.mk_number,
                    errors: [],
                    children: [],
                }),
                checker,
            ))

        StringLiteral { value } ->
            Ok((
                TypedNode({
                    original: node,
                    inferred_type: Type.mk_string,
                    errors: [],
                    children: [],
                }),
                checker,
            ))

        BooleanLiteral { value } ->
            Ok((
                TypedNode({
                    original: node,
                    inferred_type: Type.mk_boolean,
                    errors: [],
                    children: [],
                }),
                checker,
            ))

        NullLiteral {} ->
            Ok((
                TypedNode({
                    original: node,
                    inferred_type: Type.mk_null,
                    errors: [],
                    children: [],
                }),
                checker,
            ))

        # Statements
        IfStatement { test, consequent, alternate } ->
            check_if_statement(test, consequent, alternate, checker)

        ReturnStatement { argument } ->
            check_return_statement(argument, checker)

        BlockStatement { body } ->
            check_block_statement(body, checker)

        ExpressionStatement { expression } ->
            check_node_internal(expression, checker)

        # Default case - return unknown type
        _ ->
            Ok((
                TypedNode({
                    original: node,
                    inferred_type: Type.mk_unknown,
                    errors: [],
                    children: [],
                }),
                checker,
            ))

# Public checking function
check_node : Node, TypeChecker -> CheckResult
check_node = |node, checker|
    when check_node_internal(node, checker) is
        Ok (typed_node, _) -> Ok(typed_node)
        Err errors -> Err(errors)

# Check a list of statements
check_statement_list : List Node, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_statement_list = |statements, checker|
    result = List.walk(statements, Ok([], checker), |acc, stmt|
        when acc is
            Ok (typed_nodes, current_checker) ->
                when check_node_internal(stmt, current_checker) is
                    Ok (typed_node, next_checker) ->
                        Ok((List.append(typed_nodes, typed_node), next_checker))
                    Err errors ->
                        Err(errors)
            Err errors ->
                Err(errors)
    )

    when result is
        Ok (typed_children, final_checker) ->
            Ok((
                TypedNode({
                    original: Program({
                        body: statements,
                        sourceType: Module,
                    }),
                    inferred_type: Type.mk_unknown,
                    errors: [],
                    children: typed_children,
                }),
                final_checker,
            ))
        Err errors ->
            Err(errors)

# Check variable declaration
check_variable_declaration : List Node, Ast.VariableDeclarationKind, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_variable_declaration = |declarations, kind, checker|
    is_const = when kind is
        Const -> Bool.true
        _ -> Bool.false

    result = List.walk(declarations, Ok(([], checker)), |acc, decl|
        when acc is
            Ok((typed_decls, current_checker)) ->
                when decl is
                    VariableDeclarator { id, init } ->
                        when id is
                            Identifier { name } ->
                                # Check initializer
                                init_result = when init is
                                    Some init_expr ->
                                        check_node_internal(init_expr, current_checker)
                                    None ->
                                        Ok((
                                            TypedNode({
                                                original: UndefinedLiteral {},
                                                inferred_type: Type.mk_undefined,
                                                errors: [],
                                                children: [],
                                            }),
                                            current_checker,
                                        ))

                                when init_result is
                                    Ok (TypedNode(init_typed), checker_after_init) ->
                                        # Add to symbol table
                                        new_table = when TST.add_symbol(checker_after_init.symbol_table, name, init_typed.inferred_type, is_const) is
                                            Ok table -> table
                                            Err DuplicateSymbol ->
                                                # Add error but continue
                                                checker_after_init.symbol_table

                                        new_checker = { checker_after_init & symbol_table: new_table }
                                        typed_decl = TypedNode({
                                            original: decl,
                                            inferred_type: init_typed.inferred_type,
                                            errors: [],
                                            children: [TypedNode(init_typed)],
                                        })
                                        Ok((List.append(typed_decls, typed_decl), new_checker))
                                    Err errors ->
                                        Err(errors)
                            _ ->
                                # Pattern destructuring not supported yet
                                Ok((typed_decls, current_checker))
                    _ ->
                        Ok((typed_decls, current_checker))
            Err errors ->
                Err(errors)
    )

    when result is
        Ok (typed_decls, final_checker) ->
            Ok((
                TypedNode({
                    original: VariableDeclaration { declarations, kind },
                    inferred_type: Type.mk_unknown,
                    errors: [],
                    children: typed_decls,
                }),
                final_checker,
            ))
        Err errors ->
            Err(errors)

# Check identifier
check_identifier : Str, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_identifier = |name, checker|
    when TST.lookup_symbol(checker.symbol_table, name) is
        Ok symbol ->
            Ok((
                TypedNode({
                    original: Identifier { name },
                    inferred_type: symbol.sym_type,
                    errors: [],
                    children: [],
                }),
                checker,
            ))
        Err _ ->
            error = {
                kind: UnknownVariable,
                message: "Variable '$(name)' is not defined",
                location: Err(NoLocation),
                expected: Err(NoExpected),
                actual: Err(NoActual),
                suggestion: Err(NoSuggestion),
            }
            Ok((
                TypedNode({
                    original: Identifier { name },
                    inferred_type: Type.mk_unknown,
                    errors: [error],
                    children: [],
                }),
                { checker & errors: List.append(checker.errors, error) },
            ))

check_binary_expression : Node, Node, Ast.BinaryOperator, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_binary_expression = |left, right, operator, checker|
    # Check both operands
    left_result = check_node_internal(left, checker)

    when left_result is
        Ok (TypedNode(left_typed), checker_after_left) ->
            right_result = check_node_internal(right, checker_after_left)

            when right_result is
                Ok (TypedNode(right_typed), checker_after_right) ->
                    # Determine result type based on operator
                    result_type = infer_binary_op_type(
                        left_typed.inferred_type,
                        right_typed.inferred_type,
                        operator
                    )

                    # Check if types are compatible for this operator
                    compatibility_errors = check_binary_compatibility(
                        left_typed.inferred_type,
                        right_typed.inferred_type,
                        operator
                    )

                    Ok((
                        TypedNode({
                            original: BinaryExpression { left, right, operator },
                            inferred_type: result_type,
                            errors: compatibility_errors,
                            children: [TypedNode(left_typed), TypedNode(right_typed)],
                        }),
                        { checker_after_right & errors: List.concat(checker_after_right.errors, compatibility_errors) },
                    ))
                Err errors ->
                    Err(errors)
        Err errors ->
            Err(errors)

# Helper to infer binary operation result type
infer_binary_op_type : Type, Type, Ast.BinaryOperator -> Type
infer_binary_op_type = |left_type, right_type, operator|
    when operator is
        # Arithmetic operators
        Plus ->
            # String concatenation or addition
            if Type.is_assignable_to(left_type, Type.mk_string) || Type.is_assignable_to(right_type, Type.mk_string) then
                Type.mk_string
            else
                Type.mk_number
        Minus | Times | Divide | Modulo | Power ->
            Type.mk_number

        # Comparison operators
        LessThan | LessThanEquals | GreaterThan | GreaterThanEquals ->
            Type.mk_boolean

        # Equality operators
        Equals | NotEquals | StrictEquals | StrictNotEquals ->
            Type.mk_boolean

        # Logical operators
        LogicalAnd | LogicalOr ->
            # These return the actual value, not boolean
            Type.mk_union([left_type, right_type])

        # Bitwise operators
        BitwiseAnd | BitwiseOr | BitwiseXor | LeftShift | RightShift | UnsignedRightShift ->
            Type.mk_number

        # Assignment operators
        _ ->
            left_type

# Check compatibility of types for binary operator
check_binary_compatibility : Type, Type, Ast.BinaryOperator -> List TypeError
check_binary_compatibility = |left_type, right_type, operator|
    when operator is
        # Arithmetic (except +) requires numbers
        Minus | Times | Divide | Modulo | Power ->
            errors = []
            errors1 = if !(is_numeric_type(left_type)) then
                List.append(errors, {
                    kind: TypeMismatch,
                    message: "Left operand must be a number",
                    location: Err(NoLocation),
                    expected: Ok(Type.mk_number),
                    actual: Ok(left_type),
                    suggestion: Err(NoSuggestion),
                })
            else
                errors

            if !(is_numeric_type(right_type)) then
                List.append(errors1, {
                    kind: TypeMismatch,
                    message: "Right operand must be a number",
                    location: Err(NoLocation),
                    expected: Ok(Type.mk_number),
                    actual: Ok(right_type),
                    suggestion: Err(NoSuggestion),
                })
            else
                errors1

        # Other operators are more permissive in JavaScript
        _ ->
            []

is_numeric_type : Type -> Bool
is_numeric_type = |t|
    Type.is_assignable_to(t, Type.mk_number)

# Stubs for other checking functions
check_function_declaration : Option Node, List Node, Node, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_function_declaration = |id, params, body, checker|
    # TODO: Implement function checking
    Ok((
        TypedNode({
            original: FunctionDeclaration { id, params, body },
            inferred_type: Type.mk_unknown,
            errors: [],
            children: [],
        }),
        checker,
    ))

check_unary_expression : Node, Ast.UnaryOperator, Bool, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_unary_expression = |argument, operator, prefix, checker|
    # TODO: Implement unary expression checking
    Ok((
        TypedNode({
            original: UnaryExpression { argument, operator, prefix },
            inferred_type: Type.mk_unknown,
            errors: [],
            children: [],
        }),
        checker,
    ))

check_call_expression : Node, List Node, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_call_expression = |callee, arguments, checker|
    # TODO: Implement call expression checking
    Ok((
        TypedNode({
            original: CallExpression { callee, arguments },
            inferred_type: Type.mk_unknown,
            errors: [],
            children: [],
        }),
        checker,
    ))

check_member_expression : Node, Node, Bool, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_member_expression = |object, property, computed, checker|
    # TODO: Implement member expression checking
    Ok((
        TypedNode({
            original: MemberExpression { object, property, computed },
            inferred_type: Type.mk_unknown,
            errors: [],
            children: [],
        }),
        checker,
    ))

check_if_statement : Node, Node, Option Node, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_if_statement = |test, consequent, alternate, checker|
    # TODO: Implement if statement checking
    Ok((
        TypedNode({
            original: IfStatement { test, consequent, alternate },
            inferred_type: Type.mk_unknown,
            errors: [],
            children: [],
        }),
        checker,
    ))

check_return_statement : Option Node, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_return_statement = |argument, checker|
    # TODO: Implement return statement checking
    Ok((
        TypedNode({
            original: ReturnStatement { argument },
            inferred_type: Type.mk_unknown,
            errors: [],
            children: [],
        }),
        checker,
    ))

check_block_statement : List Node, TypeChecker -> Result (TypedNode, TypeChecker) (List TypeError)
check_block_statement = |body, checker|
    # Enter new block scope
    new_table = TST.push_scope(checker.symbol_table, BlockScope)
    checker_with_scope = { checker & symbol_table: new_table }

    # Check statements in block
    result = check_statement_list(body, checker_with_scope)

    when result is
        Ok (TypedNode(typed_block), checker_after_block) ->
            # Exit block scope
            final_table = when TST.pop_scope(checker_after_block.symbol_table) is
                Ok table -> table
                Err _ -> checker_after_block.symbol_table
            
            typed_node_result : TypedNode
            typed_node_result = TypedNode({
                original: BlockStatement { body },
                inferred_type: Type.mk_unknown,
                errors: [],
                children: [TypedNode(typed_block)],
            })

            checker_result : TypeChecker
            checker_result = { checker_after_block & symbol_table: final_table }

            Ok((typed_node_result, checker_result))
        Err errors ->
            Err(errors)

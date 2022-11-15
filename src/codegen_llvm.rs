use crate::ast::{
    Binop, Expression, GoatProgram, IdentifierShape, IdentifierShapeDeclaration, Node, Parameter,
    ParameterPassIndicator, Procedure, Statement, Unop, VariableDeclaration, VariableType,
};
use crate::semantic::{eval_expression_scalar, ProcedureSymbols, SymbolTable, VariableInfo, VariableLocation};

const SPACE_2: &str = "  ";
type ConvertedStringConst = (usize, String);

pub fn generate_code(program: &GoatProgram, symbol_table: &SymbolTable) -> String {
    let mut output = vec![
        "@format.int = private unnamed_addr constant [3 x i8] c\"%d\\00\"".to_owned(),
        "@format.float = private unnamed_addr constant [3 x i8] c\"%f\\00\"".to_owned(),
        "@format.true = private unnamed_addr constant [5 x i8] c\"true\\00\"".to_owned(),
        "@format.false = private unnamed_addr constant [6 x i8] c\"false\\00\"".to_owned(),
        "@format.bool= private unnamed_addr constant [4 x i8] c\"%5s\\00\"".to_owned(),
        "declare void @exit(i32 noundef)".to_owned(),
        "declare i32 @printf(i8* noundef, ...)".to_owned(),
        "declare i32 @__isoc99_scanf(i8* noundef, ...)".to_owned(),
        "declare i32 @strncmp(i8* noundef, i8* noundef, i64 noundef)".to_owned(),
        "declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg)".to_owned(),
        String::new(),
    ];

    let mut string_constants: Vec<ConvertedStringConst> = vec![];

    let mut procedure_outputs = vec![];
    for procedure in &program.procedures {
        procedure_outputs.push(generate_proc(&mut string_constants, procedure, symbol_table));
    }
    output.push(procedure_outputs.join("\n\n"));
    output.push(String::new());

    for (index, str_const) in string_constants.iter().enumerate() {
        output.push(format!(
            "@strconst.{} = private unnamed_addr constant [{} x i8] c\"{}\"",
            index, str_const.0, str_const.1
        ));
    }

    output.join("\n")
}

fn generate_proc(strings: &mut Vec<ConvertedStringConst>, procedure: &Procedure, symbol_table: &SymbolTable) -> String {
    let mut output = vec![];

    let is_main = procedure.identifier.node == "main";
    let return_type = if is_main { "i32" } else { "void" };

    let mut temp_var = 0;

    // Parameters
    let (parameters_declaration, mut parameters_store) =
        generate_formal_parameters(&mut temp_var, &procedure.parameters);
    output.push(format!(
        "define dso_local {} @{}({}) {{ ; proc {}({})",
        return_type,
        print_identifier_name(&procedure.identifier.node),
        parameters_declaration,
        // comment
        procedure.identifier.node,
        procedure
            .parameters
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join(", ")
    ));
    output.append(&mut parameters_store);

    output.append(&mut generate_var_declarations(
        &mut temp_var,
        &procedure.variable_declarations,
    ));
    output.append(&mut generate_body(&mut temp_var, strings, symbol_table, procedure));

    if is_main {
        output.push("  ret i32 0".to_owned());
    } else {
        output.push("  ret void".to_owned());
    }

    output.push("}".to_owned());

    output.join("\n")
}

fn generate_body(
    temp_var: &mut usize,
    strings: &mut Vec<ConvertedStringConst>,
    symbol_table: &SymbolTable,
    procedure: &Procedure,
) -> Vec<String> {
    generate_statements(strings, temp_var, symbol_table, procedure, &procedure.body.statements)
}

fn generate_statements(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    statements: &Vec<Node<Statement>>,
) -> Vec<String> {
    let mut output = vec![];

    for statement in statements {
        output.append(&mut generate_statement(
            strings,
            temp_var,
            symbol_table,
            procedure,
            &statement.node,
        ));
    }

    output
}

fn generate_statement(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    statement: &Statement,
) -> Vec<String> {
    let procedure_symbols = symbol_table
        .get(&procedure.identifier.node)
        .expect("no symbols for procedure");

    let mut output = vec![];

    match statement {
        Statement::If(expr, statements) => {
            // Evaluate boolean expression
            let (conditional_var, generated) = generate_expression(temp_var, procedure_symbols, &expr.node);
            output.append(
                &mut generated
                    .iter()
                    .map(|g| SPACE_2.to_owned() + g)
                    .collect::<Vec<String>>(),
            );

            // %1 = <expr-result>
            // br i1 %1, label %if, label %endif
            // if: ...
            // br label %endif
            // endif:
            let if_label = increment_temp_var(temp_var);
            let mut if_statements_code = generate_statements(strings, temp_var, symbol_table, procedure, statements);
            let endif_label = increment_temp_var(temp_var);

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var, if_label, endif_label
            ));

            // If statements
            output.push(format!("{}:\t\t\t\t; if statements", if_label));
            output.append(&mut if_statements_code);
            output.push(format!("  br label %{}", endif_label));

            // After endif
            output.push(format!("{}:\t\t\t\t; end of if statement", endif_label));
        }
        Statement::IfElse(expr, statements1, statements2) => {
            // Evaluate boolean expression
            let (conditional_var, mut expr_code) = generate_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            // %1 = <expr-result>
            // br i1 %1, label %if, label %else
            // if: ...
            // br label %endif
            // else: ...
            // br label %endif
            // endif:
            let if_label = increment_temp_var(temp_var);
            let mut if_statements_code = generate_statements(strings, temp_var, symbol_table, procedure, statements1);
            let else_label = increment_temp_var(temp_var);
            let mut else_statements_code = generate_statements(strings, temp_var, symbol_table, procedure, statements2);
            let endif_label = increment_temp_var(temp_var);

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var, if_label, else_label
            ));

            // If statements
            output.push(format!("{}:\t\t\t\t; if statements", if_label));
            output.append(&mut if_statements_code);
            output.push(format!("  br label %{}", endif_label));

            // Else statements
            output.push(format!("{}:\t\t\t\t; else statements", else_label));
            output.append(&mut else_statements_code);
            output.push(format!("  br label %{}", endif_label));

            // After endif
            output.push(format!("{}:\t\t\t\t; end ifelse", endif_label));
        }
        Statement::While(expr, statements) => {
            // conditional:
            // ...
            // %1 = <conditional-result>
            // br i1 %1 %while_body %while_end
            // while_body:
            // ...
            // br label %conditional, !llvm.loop !while_body
            // while_end:
            let conditional_label = increment_temp_var(temp_var);
            let (conditional_var, mut expr_code) = generate_expression(temp_var, procedure_symbols, &expr.node);
            let body_label = increment_temp_var(temp_var);
            let mut while_body_code = generate_statements(strings, temp_var, symbol_table, procedure, statements);
            let endwhile_label = increment_temp_var(temp_var);

            // Immediate jump to conditional
            output.push(format!("  br label %{}", conditional_label));

            // Evaluate boolean expression
            output.push(format!("{}:\t\t\t\t; start while conditional", conditional_label));
            output.append(&mut expr_code);

            // Jump on conditional
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var, body_label, endwhile_label
            ));

            // Body
            output.push(format!("{}:\t\t\t\t; body of while", body_label));
            output.append(&mut while_body_code);
            // Back to conditional
            output.push(format!("  br label %{}", conditional_label));

            // After end of while
            output.push(format!("{}:\t\t\t\t; end while", endwhile_label));
        }
        Statement::Assign(identifier_shape, expr) => {
            // First evaluate the expression
            let expr_value_type = eval_expression_scalar(procedure_symbols, expr)
                .expect("generate_assign failed to eval expression type");
            let (expr_value_var, mut expr_value_code) = generate_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut expr_value_code);

            // Then generate the assign code
            output.append(&mut generate_assign_var(
                temp_var,
                procedure_symbols,
                identifier_shape,
                expr_value_type,
                expr_value_var,
            ));
        }
        Statement::Write(expr) => {
            output.append(&mut generate_write(strings, temp_var, procedure_symbols, expr));
        }
        Statement::Read(shape) => {
            let (_, variable_info) = determine_shape_info(procedure_symbols, shape);

            match variable_info.r#type {
                VariableType::Int => {
                    let alloca_var = increment_temp_var(temp_var);
                    let assign_ret_var = increment_temp_var(temp_var);
                    let load_var = increment_temp_var(temp_var);

                    // %1 = alloca i32, align 4
                    output.push(format!("  %{} = alloca i32", alloca_var));
                    // %2 = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds
                    //      ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), i32* noundef %1)
                    output.push(format!(
                        "  %{} = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds \
                            ([3 x i8], [3 x i8]* @format.int, i64 0, i64 0), i32* noundef %{})",
                        assign_ret_var, alloca_var
                    ));
                    // %3 = load i32, i32* %2
                    output.push(format!("  %{} = load i32, i32* %{}", load_var, alloca_var));

                    // Assign
                    let mut code_assign_code =
                        generate_assign_var(temp_var, procedure_symbols, shape, VariableType::Int, load_var);
                    output.append(&mut code_assign_code);
                }
                VariableType::Float => {
                    let alloca_var = increment_temp_var(temp_var);
                    let assign_ret_var = increment_temp_var(temp_var);
                    let load_var = increment_temp_var(temp_var);

                    // %1 = alloca float, align 4
                    output.push(format!("  %{} = alloca float", alloca_var));
                    // %2 = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds
                    //      ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), float* noundef %1)
                    output.push(format!(
                        "  %{} = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds \
                            ([3 x i8], [3 x i8]* @format.float, i64 0, i64 0), float* noundef %{})",
                        assign_ret_var, alloca_var
                    ));
                    // %3 = load float, float* %2
                    output.push(format!("  %{} = load float, float* %{}", load_var, alloca_var));

                    // Assign
                    let mut code_assign_code =
                        generate_assign_var(temp_var, procedure_symbols, shape, VariableType::Float, load_var);
                    output.append(&mut code_assign_code);
                }
                VariableType::Bool => {
                    let alloca_var = increment_temp_var(temp_var);
                    let read_buf_var = increment_temp_var(temp_var);
                    let read_buf_ptr_var = increment_temp_var(temp_var);
                    let scanf_ret_var = increment_temp_var(temp_var);

                    // %1 = alloca i1                                                       ; var 'read'
                    output.push(format!("  %{} = alloca i1", alloca_var));
                    // %2 = alloca [6 x i8]                                                 ; char*
                    output.push(format!("  %{} = alloca [6 x i8]", read_buf_var));
                    // %3 = getelementptr inbounds [6 x i8], [6 x i8]* %2, i64 0, i64 0
                    output.push(format!(
                        "  %{} = getelementptr inbounds [6 x i8], [6 x i8]* %{}, i64 0, i64 0",
                        read_buf_ptr_var, read_buf_var
                    ));
                    // %4 = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds
                    //      ([4 x i8], [4 x i8]* @format.bool, i64 0, i64 0), i8* noundef %3)
                    output.push(format!(
                        "  %{} = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds \
                            ([4 x i8], [4 x i8]* @format.bool, i64 0, i64 0), i8* noundef %{})",
                        scanf_ret_var, read_buf_ptr_var
                    ));

                    // if <scanf-value> == "true" (with strncmp)
                    // %6 = call i32 @strncmp(i8* noundef %5, i8* noundef getelementptr inbounds
                    //      ([5 x i8], [5 x i8]* @.str.1, i64 0, i64 0), i64 noundef 4) #3
                    // %7 = icmp eq i32 %6, 0
                    let strncmp_true_var = increment_temp_var(temp_var);
                    output.push(format!(
                        "  %{} = call i32 @strncmp(i8* noundef %{}, i8* noundef getelementptr inbounds \
                            ([5 x i8], [5 x i8]* @format.true, i64 0, i64 0), i64 noundef 4)",
                        strncmp_true_var, read_buf_ptr_var
                    ));
                    let strncmp_true_compare_var = increment_temp_var(temp_var);
                    output.push(format!(
                        "  %{} = icmp eq i32 %{}, 0",
                        strncmp_true_compare_var, strncmp_true_var
                    ));

                    let if_true_label = increment_temp_var(temp_var);
                    let compare_false_label = increment_temp_var(temp_var);
                    let strncmp_false_var = increment_temp_var(temp_var);
                    let strncmp_false_compare_var = increment_temp_var(temp_var);
                    let if_false_label = increment_temp_var(temp_var);
                    let else_label = increment_temp_var(temp_var);
                    let endif_label = increment_temp_var(temp_var);

                    // Branch
                    // br i1 %7, label %8, label %9
                    output.push(format!(
                        "  br i1 %{}, label %{}, label %{}",
                        strncmp_true_compare_var, if_true_label, compare_false_label
                    ));

                    // if branch - compare "true" succeeded, set 1
                    output.push(format!("{}:", if_true_label));
                    output.push(format!("  store i1 1, i1* %{}", alloca_var));
                    output.push(format!("  br label %{}", endif_label));

                    // Compare "false"
                    output.push(format!("{}:", compare_false_label));
                    output.push(format!(
                        "  %{} = call i32 @strncmp(i8* noundef %{}, i8* noundef getelementptr inbounds \
                                ([6 x i8], [6 x i8]* @format.false, i64 0, i64 0), i64 noundef 5)",
                        strncmp_false_var, read_buf_ptr_var
                    ));
                    output.push(format!(
                        "  %{} = icmp eq i32 %{}, 0",
                        strncmp_false_compare_var, strncmp_false_var
                    ));
                    output.push(format!(
                        "  br i1 %{}, label %{}, label %{}",
                        strncmp_false_compare_var, if_false_label, else_label
                    ));

                    // elseif branch - compare "false" succeeded, set 0
                    output.push(format!("{}:", if_false_label));
                    output.push(format!("  store i1 0, i1* %{}", alloca_var));
                    output.push(format!("  br label %{}", endif_label));

                    // else branch
                    output.push(format!("{}:", else_label));
                    output.push("  call void @exit(i32 noundef 1)".into());
                    output.push("  unreachable".into());

                    // endif
                    output.push(format!("{}:", endif_label));

                    // Load stored variable
                    let load_var = increment_temp_var(temp_var);
                    output.push(format!("  %{} = load i1, i1* %{}", load_var, alloca_var));

                    // Assign
                    let mut code_assign_code =
                        generate_assign_var(temp_var, procedure_symbols, shape, VariableType::Bool, load_var);
                    output.append(&mut code_assign_code);
                }
            }
        }
        Statement::Call(identifier, expressions) => {
            let callee_symbol_table = symbol_table.get(&identifier.node).expect("no symbols for procedure");
            let callee_parameters = callee_symbol_table
                .iter()
                .filter(|a| a.variable_location == VariableLocation::FormalParameter);

            let mut declarations = vec![];
            for (parameter, argument) in callee_parameters.zip(expressions.iter()) {
                let (passing_indicator, var) = match parameter.pass_indicator {
                    Some(ParameterPassIndicator::Val) => {
                        // Evaluate expression
                        let (expr_var, mut expr_code) =
                            generate_expression(temp_var, procedure_symbols, &argument.node);
                        output.append(&mut expr_code);
                        (ParameterPassIndicator::Val, expr_var.to_string())
                    }
                    Some(ParameterPassIndicator::Ref) => {
                        match &argument.node {
                            Expression::IdentifierShape(identifier_shape) => {
                                // Get pointer to variable declaration, and code for generating pointer variable
                                let (ptr_var, mut ptr_code) =
                                    get_identifier_ptr(temp_var, procedure_symbols, identifier_shape);
                                output.append(&mut ptr_code);

                                (ParameterPassIndicator::Ref, ptr_var.to_string())
                            }
                            _ => {
                                panic!("By ref must always be formal parameter or variable");
                            }
                        }
                    }
                    None => {
                        panic!("parameter should have passing indicator")
                    }
                };

                declarations.push(generate_parameter_declaration(
                    parameter.r#type,
                    passing_indicator,
                    &var,
                ));
            }

            // call void @"ident"(i1 noundef zeroext %6, i8* noundef %2, i32 noundef %7, i32* noundef %3, ...)
            output.push(format!(
                "  call void @{}({})",
                print_identifier_name(&identifier.node),
                declarations.join(", ")
            ));
        }
    }

    output
}

fn get_identifier_ptr(
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    identifier_shape: &IdentifierShape,
) -> (usize, Vec<String>) {
    let mut output = vec![];

    // Get variable information
    let (_, variable_info) = determine_shape_info(procedure_symbols, identifier_shape);
    let variable_type = convert_type(variable_info.r#type);

    let ptr_var = match identifier_shape {
        IdentifierShape::Identifier(identifier) => {
            // %9 = load i32*, i32** %identifier    ; load identifier** into ptr
            // store i32 %9, i32* %decl_dest        ; store value to ptr
            let load_var1 = increment_temp_var(temp_var);

            let identifier_escaped = print_identifier_name(&identifier.node);
            output.push(format!(
                "  %{} = load {}*, {}** %{}",
                load_var1, variable_type, variable_type, identifier_escaped
            ));
            load_var1
        }
        IdentifierShape::IdentifierArray(identifier, m_expr) => {
            let (m_expr_var, mut m_expr_code) = generate_expression(temp_var, procedure_symbols, &m_expr.node);
            output.append(&mut m_expr_code);

            // Get the array dimension
            let variable_shape = variable_info.shape.expect("expectd array to have dimension in table");
            let IdentifierShapeDeclaration::IdentifierArray(_, m) = variable_shape else {
                panic!("Expected array");
            };

            // %1 = alloca [2 x i32], align 4                                           ; allocation (previous)
            // %v = sext i32 %<expr> to i64                                             ; index expression value to i64
            // %5 = getelementptr inbounds [2 x i32], [2 x i32]* %1, i64 0, i64 %v      ; addressing
            let convert_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = sext i32 %{} to i64", convert_var, m_expr_var));
            let address_var = increment_temp_var(temp_var);
            let identifier_escaped = print_identifier_name(&identifier.node);
            output.push(format!(
                "  %{} = getelementptr inbounds [{} x {}], [{} x {}]* %{}, i64 0, i64 %{}",
                address_var, m, variable_type, m, variable_type, identifier_escaped, convert_var
            ));
            address_var
        }
        IdentifierShape::IdentifierArray2D(identifier, expr_m, expr_n) => {
            let (m_expr_var, mut m_expr_code) = generate_expression(temp_var, procedure_symbols, &expr_m.node);
            output.append(&mut m_expr_code);

            let (n_expr_var, mut n_expr_code) = generate_expression(temp_var, procedure_symbols, &expr_n.node);
            output.append(&mut n_expr_code);

            // Get the matrix dimension
            let variable_shape = variable_info.shape.expect("expectd matrix to have dimension in table");
            let IdentifierShapeDeclaration::IdentifierArray2D(_, m, n) = variable_shape else {
                panic!("Expected matrix");
            };

            // %m = sext i32 %<expr-m> to i64                                   ; m conversion to i64
            // %n = sext i32 %<expr-n> to i64                                   ; n conversion to i64
            let convert_var_m = increment_temp_var(temp_var);
            output.push(format!("  %{} = sext i32 %{} to i64", convert_var_m, m_expr_var));

            let convert_var_n = increment_temp_var(temp_var);
            output.push(format!("  %{} = sext i32 %{} to i64", convert_var_n, n_expr_var));

            // %7 = getelementptr inbounds [30 x [30 x i32]], [30 x [30 x i32]]* %1, i64 0, i64 %m
            // %8 = getelementptr inbounds [30 x i32], [30 x i32]* %7, i64 0, i64 %n
            let address_var_1 = increment_temp_var(temp_var);
            let address_var_2 = increment_temp_var(temp_var);
            let identifier_escaped = print_identifier_name(&identifier.node);
            output.push(format!(
                "  %{} = getelementptr inbounds [{} x [{} x {}]], [{} x [{} x {}]]* %{}, i64 0, i64 %{}",
                address_var_1, m, n, variable_type, m, n, variable_type, identifier_escaped, convert_var_m
            ));
            output.push(format!(
                "  %{} = getelementptr inbounds [{} x {}], [{} x {}]* %{}, i64 0, i64 %{}",
                address_var_2, n, variable_type, n, variable_type, address_var_1, convert_var_n
            ));
            address_var_2
        }
    };

    (ptr_var, output)
}

fn generate_assign_var(
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    identifier_shape: &IdentifierShape,
    expr_value_type: VariableType,
    expr_value_var: usize,
) -> Vec<String> {
    let mut output = vec![];

    // Get variable information
    let (_, variable_info) = determine_shape_info(procedure_symbols, identifier_shape);
    let variable_type = convert_type(variable_info.r#type);
    let variable_pass_indicator = *variable_info.pass_indicator.unwrap_or(&ParameterPassIndicator::Val);

    // Int to float conversion
    let expr_value_var = if expr_value_type == VariableType::Int && variable_info.r#type == VariableType::Float {
        // %4 = sitofp i32 %3 to float
        let new_converted_var = increment_temp_var(temp_var);
        output.push(format!(
            "  %{} = sitofp i32 %{} to float",
            new_converted_var, expr_value_var
        ));
        new_converted_var
    } else {
        expr_value_var
    };

    if let (ParameterPassIndicator::Val, IdentifierShape::Identifier(identifier)) =
        (variable_pass_indicator, identifier_shape)
    {
        // store i32 %value_source, i32* %decl_dest
        let identifier_escaped = print_identifier_name(&identifier.node);
        output.push(format!(
            "  store {} %{}, {}* %{}",
            variable_type, expr_value_var, variable_type, identifier_escaped
        ));
    } else {
        // Get pointer to variable, and code for generating pointer variable
        let (address_var, mut ptr_code) = get_identifier_ptr(temp_var, procedure_symbols, identifier_shape);
        output.append(&mut ptr_code);

        output.push(format!(
            "  store {} %{}, {}* %{}",
            variable_type, expr_value_var, variable_type, address_var
        ));
    }

    output
}

fn generate_write(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    expr: &Node<Expression>,
) -> Vec<String> {
    let mut output = vec![];

    if let Expression::StringConst(str_const) = &expr.node {
        let print_return_num = increment_temp_var(temp_var);

        // Convert to (number of bytes, string constant representation)
        let converted = convert_string_const(str_const);
        let str_const_len = converted.0;

        let str_const_index = strings.iter().position(|s| s == &converted).map_or_else(
            // Insert if not found
            || {
                strings.push(converted);
                strings.len() - 1
            },
            // Exists
            |pos| pos,
        );

        output.push(format!(
            "  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr \
                inbounds ([{} x i8], [{} x i8]* @strconst.{}, i64 0, i64 0))",
            print_return_num, str_const_len, str_const_len, str_const_index
        ));
        return output;
    }

    let (expr_var, mut expr_code) = generate_expression(temp_var, procedure_symbols, &expr.node);
    output.append(&mut expr_code);

    let expr_type = eval_expression_scalar(procedure_symbols, expr).expect("type to be well-formed");
    match expr_type {
        VariableType::Bool => {
            let if_label = increment_temp_var(temp_var);
            let print_return_num1 = increment_temp_var(temp_var);
            let else_label = increment_temp_var(temp_var);
            let print_return_num2 = increment_temp_var(temp_var);
            let endif_label = increment_temp_var(temp_var);

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                expr_var, if_label, else_label
            ));

            // If true
            output.push(format!("{}:\t\t\t\t; if bool", if_label));
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds \
                    ([5 x i8], [5 x i8]* @format.true, i64 0, i64 0))",
                print_return_num1
            ));
            output.push(format!("  br label %{}", endif_label));

            // Else false
            output.push(format!("{}:\t\t\t\t; else bool", else_label));
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds \
                    ([6 x i8], [6 x i8]* @format.false, i64 0, i64 0))",
                print_return_num2
            ));
            output.push(format!("  br label %{}", endif_label));

            // After endif
            output.push(format!("{}:\t\t\t\t; end bool", endif_label));
        }
        VariableType::Float => {
            // %6 = fpext float %5 to double
            let convert_double_var = increment_temp_var(temp_var);
            output.push(format!(
                "  %{} = fpext float %{} to double",
                convert_double_var, expr_var
            ));

            let print_return_num = increment_temp_var(temp_var);
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds \
                    ([3 x i8], [3 x i8]* @format.float, i64 0, i64 0), double noundef %{})",
                print_return_num, convert_double_var
            ));
        }
        VariableType::Int => {
            let print_return_num = increment_temp_var(temp_var);
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds \
                    ([3 x i8], [3 x i8]* @format.int, i64 0, i64 0), i32 noundef %{})",
                print_return_num, expr_var
            ));
        }
    }
    output
}

fn generate_expression(
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    expression: &Expression,
) -> (usize, Vec<String>) {
    let mut output = vec![];

    let var_num = match expression {
        Expression::IntConst(n) => {
            // %3 = alloca i32              ; allocate ptr
            // store i32 <const>, i32* %3   ; store const to ptr
            // %4 = load i32, i32* %3       ; load value
            let store_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = alloca i32", store_var));
            output.push(format!("  store i32 {}, i32* %{}", n, store_var));

            let load_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = load i32, i32* %{}", load_var, store_var));
            load_var
        }
        Expression::BoolConst(b) => {
            let store_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = alloca i1", store_var));
            output.push(format!("  store i1 {}, i1* %{}", if *b { "1" } else { "0" }, store_var));

            let load_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = load i1, i1* %{}", load_var, store_var));
            load_var
        }
        Expression::FloatConst(n) => {
            // To IEEE754 double
            let float_parsed: f32 = n.parse::<f32>().expect("failed to parse float");
            let float_double: f64 = float_parsed.into();
            let float_double_str = float_double.to_be_bytes().map(|b| format!("{:02X}", b)).concat();

            let store_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = alloca float", store_var));
            output.push(format!("  store float 0x{}, float* %{}", float_double_str, store_var));

            let load_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = load float, float* %{}", load_var, store_var));
            load_var
        }
        Expression::StringConst(_) => panic!("StringConst is not supported by generate_expression"),
        Expression::IdentifierShape(identifier_shape) => {
            // Get variable information
            let (_, variable_info) = determine_shape_info(procedure_symbols, identifier_shape);
            let variable_type = convert_type(variable_info.r#type);
            let variable_pass_indicator = *variable_info.pass_indicator.unwrap_or(&ParameterPassIndicator::Val);

            if let (ParameterPassIndicator::Val, IdentifierShape::Identifier(identifier)) =
                (variable_pass_indicator, identifier_shape)
            {
                // %8 = load i32, i32* %identifier      ; load value of identifier (ptr) into temp var
                let load_var = increment_temp_var(temp_var);
                let identifier_escaped = print_identifier_name(&identifier.node);
                output.push(format!(
                    "  %{} = load {}, {}* %{}",
                    load_var, variable_type, variable_type, identifier_escaped
                ));
                load_var
            } else {
                // Get pointer to variable, and code for generating pointer variable
                let (address_var, mut ptr_code) = get_identifier_ptr(temp_var, procedure_symbols, identifier_shape);
                output.append(&mut ptr_code);

                let load_var = increment_temp_var(temp_var);
                output.push(format!(
                    "  %{} = load {}, {}* %{}",
                    load_var, variable_type, variable_type, address_var
                ));
                load_var
            }
        }
        Expression::UnopExpr(Unop::Minus, expr) => {
            let expr_value_type =
                eval_expression_scalar(procedure_symbols, expr).expect("unop minus failed to eval expression type");
            let (expr_var, mut expr_code) = generate_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            // %4 = sub nsw i32 0, %3 (0 subtract number)
            let variable_type = convert_type(expr_value_type);
            let sub_var = increment_temp_var(temp_var);
            output.push(format!(
                "  %{} = sub nsw {} {}, %{}",
                sub_var,
                variable_type,
                if expr_value_type == VariableType::Int {
                    "0"
                } else {
                    "0.0"
                },
                expr_var
            ));
            sub_var
        }
        Expression::UnopExpr(Unop::NOT, expr) => {
            let (expr_var, mut expr_code) = generate_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            // %5 = xor i1 %4, true (exclusive or with true)
            let negate_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = xor i1 %{}, true", negate_var, expr_var));
            negate_var
        }
        Expression::BinopExpr(Binop::AND, expr1, expr2) => {
            // alloca
            let alloca_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = alloca i1", alloca_var));

            // lhs
            let (expr1_var, mut expr1_code) = generate_expression(temp_var, procedure_symbols, &expr1.node);
            output.append(&mut expr1_code);
            let lhs_false_label = increment_temp_var(temp_var);

            let rhs_label = increment_temp_var(temp_var);
            let (expr2_var, mut expr2_code) = generate_expression(temp_var, procedure_symbols, &expr2.node);
            let end_label = increment_temp_var(temp_var);

            // If lhs is true, then need to check whether rhs is true
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                expr1_var, rhs_label, lhs_false_label
            ));

            // lhs is false, whole condition is false
            output.push(format!("{}:", lhs_false_label));
            output.push(format!("  store i1 0, i1* %{}", alloca_var));
            output.push(format!("  br label %{}", end_label));

            // otherwise, keep value of rhs
            output.push(format!("{}:", rhs_label));
            output.append(&mut expr2_code);
            output.push(format!("  store i1 %{}, i1* %{}", expr2_var, alloca_var));
            output.push(format!("  br label %{}", end_label));

            // end
            output.push(format!("{}:", end_label));
            let final_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = load i1, i1* %{}", final_var, alloca_var));
            final_var
        }
        Expression::BinopExpr(Binop::OR, expr1, expr2) => {
            // alloca
            let alloca_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = alloca i1", alloca_var));

            // lhs
            let (expr1_var, mut expr1_code) = generate_expression(temp_var, procedure_symbols, &expr1.node);
            output.append(&mut expr1_code);
            let lhs_true_label = increment_temp_var(temp_var);

            let rhs_label = increment_temp_var(temp_var);
            let (expr2_var, mut expr2_code) = generate_expression(temp_var, procedure_symbols, &expr2.node);
            let end_label = increment_temp_var(temp_var);

            // If lhs is true, can return true. Otherwise, need to check rhs.
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                expr1_var, lhs_true_label, rhs_label
            ));

            // lhs is true, while condition is true
            output.push(format!("{}:", lhs_true_label));
            output.push(format!("  store i1 1, i1* %{}", alloca_var));
            output.push(format!("  br label %{}", end_label));

            // rhs
            output.push(format!("{}:", rhs_label));
            output.append(&mut expr2_code);
            output.push(format!("  store i1 %{}, i1* %{}", expr2_var, alloca_var));
            output.push(format!("  br label %{}", end_label));

            // end
            output.push(format!("{}:", end_label));
            let final_var = increment_temp_var(temp_var);
            output.push(format!("  %{} = load i1, i1* %{}", final_var, alloca_var));
            final_var
        }
        Expression::BinopExpr(
            op @ (Binop::Add
            | Binop::Minus
            | Binop::Multiply
            | Binop::Divide
            | Binop::LT
            | Binop::LTE
            | Binop::GT
            | Binop::GTE
            | Binop::EQ
            | Binop::NEQ),
            left,
            right,
        ) => {
            let left_type =
                eval_expression_scalar(procedure_symbols, left).expect("generate_expression failed to evaluate scalar");
            let right_type = eval_expression_scalar(procedure_symbols, right)
                .expect("generate_expression failed to evaluate scalar");

            let operand_type = if left_type == VariableType::Float || right_type == VariableType::Float {
                VariableType::Float
            } else {
                left_type
            };
            let (left_var, mut left_code) = generate_expression(temp_var, procedure_symbols, &left.node);
            let (right_var, mut right_code) = generate_expression(temp_var, procedure_symbols, &right.node);
            output.append(&mut left_code);
            output.append(&mut right_code);

            let (left_var, right_var) = if left_type == VariableType::Float && right_type == VariableType::Int {
                let new_right_var = increment_temp_var(temp_var);
                output.push(format!("  %{} = sitofp i32 %{} to float", new_right_var, right_var));
                (left_var, new_right_var)
            } else if left_type == VariableType::Int && right_type == VariableType::Float {
                let new_left_var = increment_temp_var(temp_var);
                output.push(format!("  %{} = sitofp i32 %{} to float", new_left_var, left_var));
                (new_left_var, right_var)
            } else {
                (left_var, right_var)
            };

            let res_var = increment_temp_var(temp_var);
            match (op, operand_type) {
                // Numeric
                (Binop::Add, VariableType::Int) => {
                    output.push(format!("  %{} = add nsw i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::Add, VariableType::Float) => {
                    output.push(format!("  %{} = fadd float %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::Minus, VariableType::Int) => {
                    output.push(format!("  %{} = sub nsw i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::Minus, VariableType::Float) => {
                    output.push(format!("  %{} = fsub float %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::Multiply, VariableType::Int) => {
                    output.push(format!("  %{} = mul nsw i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::Multiply, VariableType::Float) => {
                    output.push(format!("  %{} = fmul float %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::Divide, VariableType::Int) => {
                    output.push(format!("  %{} = sdiv i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::Divide, VariableType::Float) => {
                    output.push(format!("  %{} = fdiv float %{}, %{}", res_var, left_var, right_var));
                }
                // EQ, NEQ
                (Binop::EQ, VariableType::Bool) => {
                    output.push(format!("  %{} = icmp eq i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::EQ, VariableType::Int) => {
                    output.push(format!("  %{} = icmp eq i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::EQ, VariableType::Float) => {
                    output.push(format!("  %{} = fcmp oeq float %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::NEQ, VariableType::Bool) => {
                    output.push(format!("  %{} = icmp ne i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::NEQ, VariableType::Int) => {
                    output.push(format!("  %{} = icmp ne i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::NEQ, VariableType::Float) => {
                    output.push(format!("  %{} = fcmp une float %{}, %{}", res_var, left_var, right_var));
                }
                // Comparison
                (Binop::LT, VariableType::Bool) => {
                    output.push(format!("  %{} = icmp slt i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::LT, VariableType::Int) => {
                    output.push(format!("  %{} = icmp slt i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::LT, VariableType::Float) => {
                    output.push(format!("  %{} = fcmp olt i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::LTE, VariableType::Bool) => {
                    output.push(format!("  %{} = icmp sle i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::LTE, VariableType::Int) => {
                    output.push(format!("  %{} = icmp sle i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::LTE, VariableType::Float) => {
                    output.push(format!("  %{} = fcmp ole i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::GT, VariableType::Bool) => {
                    output.push(format!("  %{} = icmp sgt i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::GT, VariableType::Int) => {
                    output.push(format!("  %{} = icmp sgt i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::GT, VariableType::Float) => {
                    output.push(format!("  %{} = fcmp ogt i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::GTE, VariableType::Bool) => {
                    output.push(format!("  %{} = icmp sge i1 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::GTE, VariableType::Int) => {
                    output.push(format!("  %{} = icmp sge i32 %{}, %{}", res_var, left_var, right_var));
                }
                (Binop::GTE, VariableType::Float) => {
                    output.push(format!("  %{} = fcmp oge i1 %{}, %{}", res_var, left_var, right_var));
                }
                (_, _) => panic!("Unexpected numeric operation"),
            }
            res_var
        }
    };

    (var_num, output)
}

fn generate_var_declarations(temp_var: &mut usize, declarations: &Vec<VariableDeclaration>) -> Vec<String> {
    let mut output = vec![];

    for declaration in declarations {
        let var_type = convert_type(declaration.r#type);

        match &declaration.identifier_declaration {
            IdentifierShapeDeclaration::Identifier(identifier) => {
                // %1 = alloca i32          ; allocate
                let identifier_escaped = print_identifier_name(&identifier.node);
                output.push(format!("  %{} = alloca {}", identifier_escaped, var_type));
                // store i32 0, i32* %1     ; initialise with value of 0
                output.push(format!(
                    "  store {} {}, {}* %{}",
                    var_type,
                    if declaration.r#type == VariableType::Float {
                        "0.0"
                    } else {
                        "0"
                    },
                    var_type,
                    identifier_escaped,
                ));
            }
            IdentifierShapeDeclaration::IdentifierArray(identifier, n) => {
                // %1 = alloca [n x i32]
                let identifier_escaped = print_identifier_name(&identifier.node);
                output.push(format!("  %{} = alloca [{} x {}]", identifier_escaped, n, var_type));

                let type_size = match declaration.r#type {
                    VariableType::Bool => 1,
                    VariableType::Int | VariableType::Float => 4,
                };

                // %3 = bitcast [10 x i32]* %2 to i8*
                // call void @llvm.memset.p0i8.i64(i8* align 16 %3, i8 0, i64 40, i1 false)
                let bitcast_var = increment_temp_var(temp_var);

                output.push(format!(
                    "  %{} = bitcast [{} x {}]* %{} to i8*",
                    bitcast_var, n, var_type, identifier_escaped,
                ));
                output.push(format!(
                    "  call void @llvm.memset.p0i8.i64(i8* %{}, i8 0, i64 {}, i1 false)",
                    bitcast_var,
                    type_size * n
                ));
            }
            IdentifierShapeDeclaration::IdentifierArray2D(identifier, m, n) => {
                // %1 = alloca [m x [n x i32]]
                let identifier_escaped = print_identifier_name(&identifier.node);
                output.push(format!(
                    "  %{} = alloca [{} x [{} x {}]]",
                    identifier_escaped, m, n, var_type
                ));

                let type_size = match declaration.r#type {
                    VariableType::Bool => 1,
                    VariableType::Int | VariableType::Float => 4,
                };

                // %2 = bitcast [30 x [30 x i32]]* %1 to i8*
                // call void @llvm.memset.p0i8.i64(i8* align 16 %2, i8 0, i64 3600, i1 false)
                let bitcast_var = increment_temp_var(temp_var);
                output.push(format!(
                    "  %{} = bitcast [{} x [{} x {}]]* %{} to i8*",
                    bitcast_var, m, n, var_type, identifier_escaped,
                ));
                output.push(format!(
                    "  call void @llvm.memset.p0i8.i64(i8* %{}, i8 0, i64 {}, i1 false)",
                    bitcast_var,
                    type_size * m * n
                ));
            }
        }
    }

    output
}

/// Get current value of `temp_var` and increment by 1
fn increment_temp_var(temp_var: &mut usize) -> usize {
    *temp_var += 1;
    *temp_var - 1
}

fn generate_formal_parameters(temp_var: &mut usize, parameters: &Vec<Parameter>) -> (String, Vec<String>) {
    let mut declarations: Vec<String> = vec![];
    let mut stores = vec![];

    // Parameter declaration
    for parameter in parameters {
        let var_num = increment_temp_var(temp_var);
        declarations.push(generate_parameter_declaration(
            parameter.r#type,
            parameter.passing_indicator,
            &var_num.to_string(),
        ));
    }

    // For the implicit entry
    *temp_var += 1;

    // Copy to identifier
    for (index, parameter) in parameters.iter().enumerate() {
        let r#type = convert_type(parameter.r#type);
        let passing_indicator = match parameter.passing_indicator {
            ParameterPassIndicator::Ref => "*",
            ParameterPassIndicator::Val => "",
        };
        let identifier_escaped = print_identifier_name(&parameter.identifier.node);

        // %name1 = alloca i32 (val)
        // %name2 = alloca i32* (ref)
        stores.push(format!(
            "  %{} = alloca {}{}",
            identifier_escaped, r#type, passing_indicator
        ));

        // store i32 %1, i32* %name1 (val)
        // store i32* %2, i32** %name2 (ref)
        stores.push(format!(
            "  store {}{} %{}, {}*{} %{}",
            r#type, passing_indicator, index, r#type, passing_indicator, identifier_escaped
        ));
    }

    (declarations.join(", "), stores)
}

fn generate_parameter_declaration(
    r#type: VariableType,
    passing_indicator: ParameterPassIndicator,
    var: &str,
) -> String {
    // i32 noundef %1 (val)
    // i32* noundef %2 (ref)
    let r#type_str = convert_type(r#type);
    let passing_indicator_str = match passing_indicator {
        ParameterPassIndicator::Ref => "*",
        ParameterPassIndicator::Val => "",
    };

    format!(
        "{}{} {}noundef %{}",
        r#type_str,
        passing_indicator_str,
        if passing_indicator == ParameterPassIndicator::Val && r#type == VariableType::Bool {
            "zeroext "
        } else {
            ""
        },
        var
    )
}

fn determine_shape_info<'fromparent>(
    procedure_symbols: &'fromparent ProcedureSymbols,
    shape: &'fromparent IdentifierShape,
) -> (&'fromparent str, &'fromparent VariableInfo<'fromparent>) {
    // Find identifier
    let identifier = match shape {
        IdentifierShape::Identifier(identifier)
        | IdentifierShape::IdentifierArray(identifier, _)
        | IdentifierShape::IdentifierArray2D(identifier, _, _) => identifier,
    };

    // Get information about variable
    let variable_info = procedure_symbols
        .iter()
        .find(|v| v.identifier == &identifier.node)
        .expect("Failed to retrieve variable_info");

    (&identifier.node, variable_info)
}

fn convert_type(r#type: VariableType) -> String {
    match r#type {
        VariableType::Bool => "i1".to_owned(),
        VariableType::Int => "i32".to_owned(),
        VariableType::Float => "float".to_owned(),
    }
}

/// Converts a string const to the required format, alongside number of bytes
fn convert_string_const(string_const: &str) -> ConvertedStringConst {
    let string = string_const
        .replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\r", "\r");
    let bytes = string.bytes();
    let bytes_len = bytes.len();
    let escaped = bytes.map(|b| format!("\\{:02x}", b)).collect::<String>();
    (bytes_len + 1, escaped + "\\00")
}

/// Escapes ' for LLVM variable and function names
fn print_identifier_name(name: &str) -> String {
    "\"".to_owned() + name + "\""
}

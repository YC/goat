use std::collections::HashMap;
use std::string::ToString;

use crate::ast::{
    Binop, Expression, GoatProgram, IdentifierShape, IdentifierShapeDeclaration, Node, Parameter,
    ParameterPassIndicator, Procedure, Statement, Unop, VariableDeclaration, VariableType,
};
use crate::semantic::{eval_expression_scalar, ProcedureSymbols, SymbolTable, VariableInfo, VariableLocation};

type ConvertedStringConst = (usize, String);

struct VarInfo {
    var_num: usize,
}
type VarInfoDict = HashMap<String, VarInfo>;

struct LlvmInfo<'a> {
    symbol_table: &'a SymbolTable<'a>,
    var_info: VarInfoDict,
    bounds_check: bool,
}

pub fn generate_code(program: &GoatProgram, symbol_table: &SymbolTable, bounds_check: bool) -> String {
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
        procedure_outputs.push(generate_proc(
            &mut string_constants,
            procedure,
            symbol_table,
            bounds_check,
        ));
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

fn generate_proc(
    strings: &mut Vec<ConvertedStringConst>,
    procedure: &Procedure,
    symbol_table: &SymbolTable,
    bounds_check: bool,
) -> String {
    let mut output = vec![];
    let mut temp_var = 0;
    let mut vars: VarInfoDict = HashMap::new();

    let is_main = procedure.identifier.node == "main";
    let return_type = if is_main { "i32" } else { "void" };

    // Parameters
    let (parameters_declaration, mut ptr_code) =
        generate_formal_parameters(&mut temp_var, &mut vars, &procedure.parameters);
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
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join(", ")
    ));
    output.append(&mut ptr_code);

    // Variable declarations
    output.append(&mut generate_var_declarations(
        &mut temp_var,
        &mut vars,
        &procedure.variable_declarations,
    ));

    let llvm_info = LlvmInfo {
        bounds_check,
        symbol_table,
        var_info: vars,
    };

    // Body
    output.append(&mut generate_body(&mut temp_var, strings, &llvm_info, procedure));

    // Return
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
    llvm_info: &LlvmInfo,
    procedure: &Procedure,
) -> Vec<String> {
    generate_statements(strings, temp_var, llvm_info, procedure, &procedure.body.statements)
}

fn generate_statements(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    llvm_info: &LlvmInfo,
    procedure: &Procedure,
    statements: &Vec<Node<Statement>>,
) -> Vec<String> {
    let mut output = vec![];

    for statement in statements {
        output.append(&mut generate_statement(
            strings,
            temp_var,
            llvm_info,
            procedure,
            &statement.node,
        ));
    }

    output
}

fn generate_statement(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    llvm_info: &LlvmInfo,
    procedure: &Procedure,
    statement: &Statement,
) -> Vec<String> {
    let procedure_symbols = llvm_info
        .symbol_table
        .get(&procedure.identifier.node)
        .expect("no symbols for procedure");

    let mut output = vec![];

    match statement {
        Statement::If(expr, statements) => {
            // %1 = <expr-result>
            // br i1 %1, label %if, label %endif
            // if: ...
            // br label %endif
            // endif:

            // Evaluate boolean expression
            let (conditional_var, mut generated) =
                generate_expression(temp_var, llvm_info, procedure_symbols, &expr.node);
            output.append(&mut generated);

            let if_label = increment_temp_var(temp_var);
            let mut if_statements_code = generate_statements(strings, temp_var, llvm_info, procedure, statements);
            let endif_label = increment_temp_var(temp_var);

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var, if_label, endif_label
            ));

            // If statements
            output.push(format!("{if_label}:\t\t\t\t; if statements"));
            output.append(&mut if_statements_code);
            output.push(format!("  br label %{endif_label}"));

            // After endif
            output.push(format!("{endif_label}:\t\t\t\t; end of if statement"));
        }
        Statement::IfElse(expr, statements1, statements2) => {
            // %1 = <expr-result>
            // br i1 %1, label %if, label %else
            // if: ...
            // br label %endif
            // else: ...
            // br label %endif
            // endif:

            // Evaluate boolean expression
            let (conditional_var, mut expr_code) =
                generate_expression(temp_var, llvm_info, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            let if_label = increment_temp_var(temp_var);
            let mut if_statements_code = generate_statements(strings, temp_var, llvm_info, procedure, statements1);
            let else_label = increment_temp_var(temp_var);
            let mut else_statements_code = generate_statements(strings, temp_var, llvm_info, procedure, statements2);
            let endif_label = increment_temp_var(temp_var);

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var, if_label, else_label
            ));

            // If statements
            output.push(format!("{if_label}:\t\t\t\t; if statements"));
            output.append(&mut if_statements_code);
            output.push(format!("  br label %{endif_label}"));

            // Else statements
            output.push(format!("{else_label}:\t\t\t\t; else statements"));
            output.append(&mut else_statements_code);
            output.push(format!("  br label %{endif_label}"));

            // After endif
            output.push(format!("{endif_label}:\t\t\t\t; end ifelse"));
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
            let (conditional_var, mut expr_code) =
                generate_expression(temp_var, llvm_info, procedure_symbols, &expr.node);
            let body_label = increment_temp_var(temp_var);
            let mut while_body_code = generate_statements(strings, temp_var, llvm_info, procedure, statements);
            let endwhile_label = increment_temp_var(temp_var);

            // Immediate jump to conditional
            output.push(format!("  br label %{conditional_label}"));

            // Evaluate boolean expression
            output.push(format!("{conditional_label}:\t\t\t\t; start while conditional"));
            output.append(&mut expr_code);

            // Jump on conditional
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var, body_label, endwhile_label
            ));

            // Body
            output.push(format!("{body_label}:\t\t\t\t; body of while"));
            output.append(&mut while_body_code);
            // Back to conditional
            output.push(format!("  br label %{conditional_label}"));

            // After end of while
            output.push(format!("{endwhile_label}:\t\t\t\t; end while"));
        }
        Statement::Assign(identifier_shape, expr) => {
            // First evaluate the expression
            let expr_value_type = eval_expression_scalar(procedure_symbols, expr)
                .expect("generate_assign failed to eval expression type");
            let (expr_value_var, mut expr_value_code) =
                generate_expression(temp_var, llvm_info, procedure_symbols, &expr.node);
            output.append(&mut expr_value_code);

            // Then generate the assign code
            output.append(&mut generate_assign_var(
                temp_var,
                llvm_info,
                procedure_symbols,
                identifier_shape,
                expr_value_type,
                expr_value_var,
            ));
        }
        Statement::Write(expr) => {
            output.append(&mut generate_write(
                strings,
                temp_var,
                llvm_info,
                procedure_symbols,
                expr,
            ));
        }
        Statement::Read(shape) => {
            output.append(&mut generate_read(temp_var, llvm_info, procedure_symbols, shape));
        }
        Statement::Call(identifier, expressions) => {
            let callee_symbol_table = llvm_info
                .symbol_table
                .get(&identifier.node)
                .expect("no symbols for procedure");
            let callee_parameters = callee_symbol_table
                .iter()
                .filter(|a| a.variable_location == VariableLocation::FormalParameter);

            // Generate the arguments within the bracket
            let mut declarations = vec![];
            for (parameter, argument) in callee_parameters.zip(expressions.iter()) {
                let argument_type =
                    eval_expression_scalar(procedure_symbols, argument).expect("cannot evaluate expression type");

                let (passing_indicator, pass_var) = match parameter.pass_indicator {
                    Some(ParameterPassIndicator::Val) => {
                        // Evaluate expression
                        let (expr_var, mut expr_code) =
                            generate_expression(temp_var, llvm_info, procedure_symbols, &argument.node);
                        output.append(&mut expr_code);

                        // Int to float conversion
                        let expr_var = if parameter.r#type == VariableType::Float && argument_type == VariableType::Int
                        {
                            let (converted_var, conversion_code) = generate_int_to_float(temp_var, expr_var);
                            output.push(conversion_code);
                            converted_var
                        } else {
                            expr_var
                        };

                        (ParameterPassIndicator::Val, expr_var)
                    }
                    Some(ParameterPassIndicator::Ref) => {
                        let Expression::IdentifierShape(identifier_shape) = &argument.node else {
                            panic!("By ref must always be formal parameter or variable");
                        };

                        // Get pointer to variable declaration, and code for generating pointer variable
                        let (ptr_var, mut ptr_code) =
                            get_identifier_ptr(temp_var, llvm_info, procedure_symbols, identifier_shape);
                        output.append(&mut ptr_code);

                        (ParameterPassIndicator::Ref, ptr_var)
                    }
                    None => {
                        panic!("parameter should have passing indicator")
                    }
                };

                declarations.push(generate_parameter_declaration(
                    parameter.r#type,
                    passing_indicator,
                    &pass_var.to_string(),
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
    llvm_info: &LlvmInfo,
    procedure_symbols: &ProcedureSymbols,
    identifier_shape: &IdentifierShape,
) -> (usize, Vec<String>) {
    let mut output = vec![];

    // Get variable information
    let (_, shape_info, var_info) = determine_shape_info(procedure_symbols, identifier_shape, llvm_info);
    let variable_type = convert_type(shape_info.r#type);

    let ptr_var = match identifier_shape {
        IdentifierShape::Identifier(_) => var_info.var_num,
        IdentifierShape::IdentifierArray(_, m_expr) => {
            let (m_expr_var, mut m_expr_code) =
                generate_expression(temp_var, llvm_info, procedure_symbols, &m_expr.node);
            output.append(&mut m_expr_code);

            // Get the array dimension
            let variable_shape = shape_info.shape.expect("array should have dimension in table");
            let IdentifierShapeDeclaration::IdentifierArray(_, m) = variable_shape else {
                panic!("Expected array");
            };

            // Index check
            if llvm_info.bounds_check {
                output.append(&mut generate_index_check(temp_var, m_expr_var, *m));
            }

            // %1 = alloca [2 x i32], align 4                                           ; allocation (previous)
            // %v = sext i32 %<expr> to i64                                             ; index expression value to i64
            // %5 = getelementptr inbounds [2 x i32], [2 x i32]* %1, i64 0, i64 %v      ; addressing
            let convert_var = increment_temp_var(temp_var);
            output.push(format!("  %{convert_var} = sext i32 %{m_expr_var} to i64"));
            let address_var = increment_temp_var(temp_var);
            output.push(format!(
                "  %{} = getelementptr inbounds [{} x {}], [{} x {}]* %{}, i64 0, i64 %{}",
                address_var, m, variable_type, m, variable_type, var_info.var_num, convert_var
            ));
            address_var
        }
        IdentifierShape::IdentifierArray2D(_, expr_m, expr_n) => {
            let (m_expr_var, mut m_expr_code) =
                generate_expression(temp_var, llvm_info, procedure_symbols, &expr_m.node);
            output.append(&mut m_expr_code);

            let (n_expr_var, mut n_expr_code) =
                generate_expression(temp_var, llvm_info, procedure_symbols, &expr_n.node);
            output.append(&mut n_expr_code);

            // Get the matrix dimension
            let variable_shape = shape_info.shape.expect("matrix should have dimension in table");
            let IdentifierShapeDeclaration::IdentifierArray2D(_, m, n) = variable_shape else {
                panic!("Expected matrix");
            };

            // Index check
            if llvm_info.bounds_check {
                output.append(&mut generate_index_check(temp_var, m_expr_var, *m));
                output.append(&mut generate_index_check(temp_var, n_expr_var, *n));
            }

            // %m = sext i32 %<expr-m> to i64                                           ; m conversion to i64
            // %n = sext i32 %<expr-n> to i64                                           ; n conversion to i64
            let convert_var_m = increment_temp_var(temp_var);
            output.push(format!("  %{convert_var_m} = sext i32 %{m_expr_var} to i64"));

            let convert_var_n = increment_temp_var(temp_var);
            output.push(format!("  %{convert_var_n} = sext i32 %{n_expr_var} to i64"));

            // %7 = getelementptr inbounds [30 x [30 x i32]], [30 x [30 x i32]]* %1, i64 0, i64 %m
            // %8 = getelementptr inbounds [30 x i32], [30 x i32]* %7, i64 0, i64 %n
            let address_var_1 = increment_temp_var(temp_var);
            let address_var_2 = increment_temp_var(temp_var);
            output.push(format!(
                "  %{} = getelementptr inbounds [{} x [{} x {}]], [{} x [{} x {}]]* %{}, i64 0, i64 %{}",
                address_var_1, m, n, variable_type, m, n, variable_type, var_info.var_num, convert_var_m
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

/// Assignment of value from expression to variable through pointer
fn generate_assign_var(
    temp_var: &mut usize,
    llvm_info: &LlvmInfo,
    procedure_symbols: &ProcedureSymbols,
    identifier_shape: &IdentifierShape,
    expr_value_type: VariableType,
    expr_value_var: usize,
) -> Vec<String> {
    let mut output = vec![];

    // Get variable information
    let (_, variable_info, _) = determine_shape_info(procedure_symbols, identifier_shape, llvm_info);
    let variable_type = convert_type(variable_info.r#type);

    // Int to float conversion
    let expr_value_var = if expr_value_type == VariableType::Int && variable_info.r#type == VariableType::Float {
        let (converted_var, conversion_code) = generate_int_to_float(temp_var, expr_value_var);
        output.push(conversion_code);
        converted_var
    } else {
        expr_value_var
    };

    // Get pointer to variable, and code for generating pointer variable
    let (address_var, mut ptr_code) = get_identifier_ptr(temp_var, llvm_info, procedure_symbols, identifier_shape);
    output.append(&mut ptr_code);

    output.push(format!(
        "  store {} %{}, {}* %{}",
        variable_type, expr_value_var, variable_type, address_var
    ));

    output
}

/// Generates code for read (including assignment of value)
fn generate_read(
    temp_var: &mut usize,
    llvm_info: &LlvmInfo,
    procedure_symbols: &ProcedureSymbols,
    shape: &IdentifierShape,
) -> Vec<String> {
    let mut output = vec![];

    let (_, variable_info, _) = determine_shape_info(procedure_symbols, shape, llvm_info);

    match variable_info.r#type {
        VariableType::Int => {
            let alloca_var = increment_temp_var(temp_var);
            let assign_ret_var = increment_temp_var(temp_var);
            let load_var = increment_temp_var(temp_var);

            // %1 = alloca i32, align 4
            output.push(format!("  %{alloca_var} = alloca i32"));
            // %2 = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds
            //      ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), i32* noundef %1)
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds \
                            ([3 x i8], [3 x i8]* @format.int, i64 0, i64 0), i32* noundef %{})",
                assign_ret_var, alloca_var
            ));
            // %3 = load i32, i32* %2
            output.push(format!("  %{load_var} = load i32, i32* %{alloca_var}"));

            // Assign
            let mut code_assign_code = generate_assign_var(
                temp_var,
                llvm_info,
                procedure_symbols,
                shape,
                VariableType::Int,
                load_var,
            );
            output.append(&mut code_assign_code);
        }
        VariableType::Float => {
            let alloca_var = increment_temp_var(temp_var);
            let assign_ret_var = increment_temp_var(temp_var);
            let load_var = increment_temp_var(temp_var);

            // %1 = alloca float, align 4
            output.push(format!("  %{alloca_var} = alloca float"));
            // %2 = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds
            //      ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), float* noundef %1)
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds \
                            ([3 x i8], [3 x i8]* @format.float, i64 0, i64 0), float* noundef %{})",
                assign_ret_var, alloca_var
            ));
            // %3 = load float, float* %2
            output.push(format!("  %{load_var} = load float, float* %{alloca_var}"));

            // Assign
            let mut code_assign_code = generate_assign_var(
                temp_var,
                llvm_info,
                procedure_symbols,
                shape,
                VariableType::Float,
                load_var,
            );
            output.append(&mut code_assign_code);
        }
        VariableType::Bool => {
            let alloca_var = increment_temp_var(temp_var);
            let read_buf_var = increment_temp_var(temp_var);
            let read_buf_ptr_var = increment_temp_var(temp_var);
            let scanf_ret_var = increment_temp_var(temp_var);

            // %1 = alloca i1                                                       ; var 'read'
            output.push(format!("  %{alloca_var} = alloca i1"));
            // %2 = alloca [6 x i8]                                                 ; char*
            output.push(format!("  %{read_buf_var} = alloca [6 x i8]"));
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
            output.push(format!("{if_true_label}:"));
            output.push(format!("  store i1 1, i1* %{alloca_var}"));
            output.push(format!("  br label %{endif_label}"));

            // Compare "false"
            output.push(format!("{compare_false_label}:"));
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
            output.push(format!("{if_false_label}:"));
            output.push(format!("  store i1 0, i1* %{alloca_var}"));
            output.push(format!("  br label %{endif_label}"));

            // else branch
            output.push(format!("{else_label}:"));
            output.push("  call void @exit(i32 noundef 1)".into());
            output.push("  unreachable".into());

            // endif
            output.push(format!("{endif_label}:"));

            // Load stored variable
            let load_var = increment_temp_var(temp_var);
            output.push(format!("  %{load_var} = load i1, i1* %{alloca_var}"));

            // Assign
            let mut code_assign_code = generate_assign_var(
                temp_var,
                llvm_info,
                procedure_symbols,
                shape,
                VariableType::Bool,
                load_var,
            );
            output.append(&mut code_assign_code);
        }
    }

    output
}

/// Generates code for write from a string constant or expression
fn generate_write(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    llvm_info: &LlvmInfo,
    procedure_symbols: &ProcedureSymbols,
    expr: &Node<Expression>,
) -> Vec<String> {
    let mut output = vec![];

    // String constant
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

    // Int, Float, Bool value
    let (expr_var, mut expr_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &expr.node);
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
            output.push(format!("{if_label}:\t\t\t\t; if bool"));
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds \
                    ([5 x i8], [5 x i8]* @format.true, i64 0, i64 0))",
                print_return_num1
            ));
            output.push(format!("  br label %{endif_label}"));

            // Else false
            output.push(format!("{else_label}:\t\t\t\t; else bool"));
            output.push(format!(
                "  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds \
                    ([6 x i8], [6 x i8]* @format.false, i64 0, i64 0))",
                print_return_num2
            ));
            output.push(format!("  br label %{endif_label}"));

            // After endif
            output.push(format!("{endif_label}:\t\t\t\t; end bool"));
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
    llvm_info: &LlvmInfo,
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
            output.push(format!("  %{store_var} = alloca i32"));
            output.push(format!("  store i32 {n}, i32* %{store_var}"));

            let load_var = increment_temp_var(temp_var);
            output.push(format!("  %{load_var} = load i32, i32* %{store_var}"));
            load_var
        }
        Expression::BoolConst(b) => {
            let store_var = increment_temp_var(temp_var);
            output.push(format!("  %{store_var} = alloca i1"));
            output.push(format!("  store i1 {}, i1* %{store_var}", if *b { "1" } else { "0" }));

            let load_var = increment_temp_var(temp_var);
            output.push(format!("  %{load_var} = load i1, i1* %{store_var}"));
            load_var
        }
        Expression::FloatConst(n) => {
            // To IEEE754 double
            let float_parsed: f32 = n.parse::<f32>().expect("failed to parse float");
            let float_double: f64 = float_parsed.into();
            let float_double_str = float_double.to_be_bytes().map(|b| format!("{b:02X}")).concat();

            let store_var = increment_temp_var(temp_var);
            output.push(format!("  %{store_var} = alloca float"));
            output.push(format!("  store float 0x{float_double_str}, float* %{store_var}"));

            let load_var = increment_temp_var(temp_var);
            output.push(format!("  %{load_var} = load float, float* %{store_var}"));
            load_var
        }
        Expression::StringConst(_) => panic!("StringConst is not supported by generate_expression"),
        Expression::IdentifierShape(identifier_shape) => {
            // Get variable information
            let (_, variable_info, _) = determine_shape_info(procedure_symbols, identifier_shape, llvm_info);
            let variable_type = convert_type(variable_info.r#type);

            // Get pointer to variable, and code for generating pointer variable
            let (address_var, mut ptr_code) =
                get_identifier_ptr(temp_var, llvm_info, procedure_symbols, identifier_shape);
            output.append(&mut ptr_code);

            let load_var = increment_temp_var(temp_var);
            output.push(format!(
                "  %{} = load {}, {}* %{}",
                load_var, variable_type, variable_type, address_var
            ));
            load_var
        }
        Expression::UnopExpr(Unop::Minus, expr) => {
            let expr_value_type =
                eval_expression_scalar(procedure_symbols, expr).expect("unop minus failed to eval expression type");
            let (expr_var, mut expr_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            // %4 = sub nsw i32 0, %3 (0 subtract number)
            // %4 = fneg float %3
            let variable_type = convert_type(expr_value_type);
            let sub_var = increment_temp_var(temp_var);
            if expr_value_type == VariableType::Int {
                output.push(format!("  %{sub_var} = sub nsw {variable_type} 0, %{expr_var}"));
            } else {
                output.push(format!("  %{sub_var} = fneg {variable_type} %{expr_var}"));
            }
            sub_var
        }
        Expression::UnopExpr(Unop::NOT, expr) => {
            let (expr_var, mut expr_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            // %5 = xor i1 %4, true (exclusive or with true)
            let negate_var = increment_temp_var(temp_var);
            output.push(format!("  %{negate_var} = xor i1 %{expr_var}, true"));
            negate_var
        }
        Expression::BinopExpr(Binop::AND, expr1, expr2) => {
            // alloca
            let alloca_var = increment_temp_var(temp_var);
            output.push(format!("  %{alloca_var} = alloca i1"));

            // lhs
            let (expr1_var, mut expr1_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &expr1.node);
            output.append(&mut expr1_code);
            let lhs_false_label = increment_temp_var(temp_var);

            let rhs_label = increment_temp_var(temp_var);
            let (expr2_var, mut expr2_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &expr2.node);
            let end_label = increment_temp_var(temp_var);

            // If lhs is true, then need to check whether rhs is true
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                expr1_var, rhs_label, lhs_false_label
            ));

            // lhs is false, whole condition is false
            output.push(format!("{lhs_false_label}:"));
            output.push(format!("  store i1 0, i1* %{alloca_var}"));
            output.push(format!("  br label %{end_label}"));

            // otherwise, keep value of rhs
            output.push(format!("{rhs_label}:"));
            output.append(&mut expr2_code);
            output.push(format!("  store i1 %{expr2_var}, i1* %{alloca_var}"));
            output.push(format!("  br label %{end_label}"));

            // end
            output.push(format!("{end_label}:"));
            let final_var = increment_temp_var(temp_var);
            output.push(format!("  %{final_var} = load i1, i1* %{alloca_var}"));
            final_var
        }
        Expression::BinopExpr(Binop::OR, expr1, expr2) => {
            // alloca
            let alloca_var = increment_temp_var(temp_var);
            output.push(format!("  %{alloca_var} = alloca i1"));

            // lhs
            let (expr1_var, mut expr1_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &expr1.node);
            output.append(&mut expr1_code);
            let lhs_true_label = increment_temp_var(temp_var);

            let rhs_label = increment_temp_var(temp_var);
            let (expr2_var, mut expr2_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &expr2.node);
            let end_label = increment_temp_var(temp_var);

            // If lhs is true, can return true. Otherwise, need to check rhs.
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                expr1_var, lhs_true_label, rhs_label
            ));

            // lhs is true, while condition is true
            output.push(format!("{lhs_true_label}:"));
            output.push(format!("  store i1 1, i1* %{alloca_var}"));
            output.push(format!("  br label %{end_label}"));

            // rhs
            output.push(format!("{rhs_label}:"));
            output.append(&mut expr2_code);
            output.push(format!("  store i1 %{expr2_var}, i1* %{alloca_var}"));
            output.push(format!("  br label %{end_label}"));

            // end
            output.push(format!("{end_label}:"));
            let final_var = increment_temp_var(temp_var);
            output.push(format!("  %{final_var} = load i1, i1* %{alloca_var}"));
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
            let (left_var, mut left_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &left.node);
            let (right_var, mut right_code) = generate_expression(temp_var, llvm_info, procedure_symbols, &right.node);
            output.append(&mut left_code);
            output.append(&mut right_code);

            let (left_var, right_var) = if left_type == VariableType::Float && right_type == VariableType::Int {
                let (new_right_var, conversion_code) = generate_int_to_float(temp_var, right_var);
                output.push(conversion_code);
                (left_var, new_right_var)
            } else if left_type == VariableType::Int && right_type == VariableType::Float {
                let (new_left_var, conversion_code) = generate_int_to_float(temp_var, left_var);
                output.push(conversion_code);
                (new_left_var, right_var)
            } else {
                (left_var, right_var)
            };

            let res_var = increment_temp_var(temp_var);
            match (op, operand_type) {
                // Numeric
                (Binop::Add, VariableType::Int) => {
                    output.push(format!("  %{res_var} = add nsw i32 %{left_var}, %{right_var}"));
                }
                (Binop::Add, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fadd float %{left_var}, %{right_var}"));
                }
                (Binop::Minus, VariableType::Int) => {
                    output.push(format!("  %{res_var} = sub nsw i32 %{left_var}, %{right_var}"));
                }
                (Binop::Minus, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fsub float %{left_var}, %{right_var}"));
                }
                (Binop::Multiply, VariableType::Int) => {
                    output.push(format!("  %{res_var} = mul nsw i32 %{left_var}, %{right_var}"));
                }
                (Binop::Multiply, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fmul float %{left_var}, %{right_var}"));
                }
                (Binop::Divide, VariableType::Int) => {
                    output.push(format!("  %{res_var} = sdiv i32 %{left_var}, %{right_var}"));
                }
                (Binop::Divide, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fdiv float %{left_var}, %{right_var}"));
                }
                // EQ, NEQ
                (Binop::EQ, VariableType::Bool) => {
                    output.push(format!("  %{res_var} = icmp eq i1 %{left_var}, %{right_var}"));
                }
                (Binop::EQ, VariableType::Int) => {
                    output.push(format!("  %{res_var} = icmp eq i32 %{left_var}, %{right_var}"));
                }
                (Binop::EQ, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fcmp oeq float %{left_var}, %{right_var}"));
                }
                (Binop::NEQ, VariableType::Bool) => {
                    output.push(format!("  %{res_var} = icmp ne i1 %{left_var}, %{right_var}"));
                }
                (Binop::NEQ, VariableType::Int) => {
                    output.push(format!("  %{res_var} = icmp ne i32 %{left_var}, %{right_var}"));
                }
                (Binop::NEQ, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fcmp une float %{left_var}, %{right_var}"));
                }
                // Comparison
                (Binop::LT, VariableType::Bool) => {
                    output.push(format!("  %{res_var} = icmp ult i1 %{left_var}, %{right_var}"));
                }
                (Binop::LT, VariableType::Int) => {
                    output.push(format!("  %{res_var} = icmp slt i32 %{left_var}, %{right_var}"));
                }
                (Binop::LT, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fcmp olt float %{left_var}, %{right_var}"));
                }
                (Binop::LTE, VariableType::Bool) => {
                    output.push(format!("  %{res_var} = icmp ule i1 %{left_var}, %{right_var}"));
                }
                (Binop::LTE, VariableType::Int) => {
                    output.push(format!("  %{res_var} = icmp sle i32 %{left_var}, %{right_var}"));
                }
                (Binop::LTE, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fcmp ole float %{left_var}, %{right_var}"));
                }
                (Binop::GT, VariableType::Bool) => {
                    output.push(format!("  %{res_var} = icmp ugt i1 %{left_var}, %{right_var}"));
                }
                (Binop::GT, VariableType::Int) => {
                    output.push(format!("  %{res_var} = icmp sgt i32 %{left_var}, %{right_var}"));
                }
                (Binop::GT, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fcmp ogt float %{left_var}, %{right_var}"));
                }
                (Binop::GTE, VariableType::Bool) => {
                    output.push(format!("  %{res_var} = icmp uge i1 %{left_var}, %{right_var}"));
                }
                (Binop::GTE, VariableType::Int) => {
                    output.push(format!("  %{res_var} = icmp sge i32 %{left_var}, %{right_var}"));
                }
                (Binop::GTE, VariableType::Float) => {
                    output.push(format!("  %{res_var} = fcmp oge float %{left_var}, %{right_var}"));
                }
                (_, _) => panic!("Unexpected numeric operation"),
            }
            res_var
        }
    };

    (var_num, output)
}

fn generate_int_to_float(temp_var: &mut usize, int_var: usize) -> (usize, String) {
    // %4 = sitofp i32 %3 to float
    let new_converted_var = increment_temp_var(temp_var);
    (
        new_converted_var,
        format!("  %{new_converted_var} = sitofp i32 %{int_var} to float"),
    )
}

fn generate_index_check(temp_var: &mut usize, index_var: usize, bound: u32) -> Vec<String> {
    let mut output = vec![];

    // Ensure that the dimension is not exceeded
    // index < 0 || index >= m
    let comparison_var1 = increment_temp_var(temp_var);
    output.push(format!("  %{comparison_var1} = icmp slt i32 %{index_var}, 0"));
    let comparison_var2 = increment_temp_var(temp_var);
    output.push(format!(
        "  %{} = icmp sge i32 %{}, {}",
        comparison_var2, index_var, bound
    ));
    let or_var = increment_temp_var(temp_var);
    output.push(format!(
        "  %{} = or i1 %{}, %{}",
        or_var, comparison_var1, comparison_var2
    ));

    // Branch from conditional
    let bad_label = increment_temp_var(temp_var);
    let endif_label = increment_temp_var(temp_var);
    output.push(format!(
        "  br i1 %{}, label %{}, label %{}",
        or_var, bad_label, endif_label
    ));

    // Bad, exit program
    // TODO: print stderr message
    output.push(format!("{bad_label}:"));
    output.push("  call void @exit(i32 noundef 1)".into());
    output.push("  unreachable".into());

    // endif
    output.push(format!("{endif_label}:"));

    output
}

fn generate_var_declarations(
    temp_var: &mut usize,
    vars: &mut VarInfoDict,
    declarations: &Vec<VariableDeclaration>,
) -> Vec<String> {
    let mut output = vec![];

    for declaration in declarations {
        let var_type = convert_type(declaration.r#type);
        let var_num = increment_temp_var(temp_var);

        match &declaration.identifier_declaration {
            IdentifierShapeDeclaration::Identifier(identifier) => {
                vars.insert(identifier.node.clone(), VarInfo { var_num });

                // %1 = alloca i32          ; allocate
                output.push(format!("  %{var_num} = alloca {var_type}"));
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
                    var_num,
                ));
            }
            IdentifierShapeDeclaration::IdentifierArray(identifier, n) => {
                vars.insert(identifier.node.clone(), VarInfo { var_num });

                // %1 = alloca [n x i32]
                output.push(format!("  %{var_num} = alloca [{n} x {var_type}]"));

                let type_size = match declaration.r#type {
                    VariableType::Bool => 1,
                    VariableType::Int | VariableType::Float => 4,
                };

                // %3 = bitcast [10 x i32]* %2 to i8*
                // call void @llvm.memset.p0i8.i64(i8* align 16 %3, i8 0, i64 40, i1 false)
                let bitcast_var = increment_temp_var(temp_var);

                output.push(format!(
                    "  %{} = bitcast [{} x {}]* %{} to i8*",
                    bitcast_var, n, var_type, var_num,
                ));
                output.push(format!(
                    "  call void @llvm.memset.p0i8.i64(i8* %{}, i8 0, i64 {}, i1 false)",
                    bitcast_var,
                    type_size * n
                ));
            }
            IdentifierShapeDeclaration::IdentifierArray2D(identifier, m, n) => {
                vars.insert(identifier.node.clone(), VarInfo { var_num });

                // %1 = alloca [m x [n x i32]]
                output.push(format!("  %{var_num} = alloca [{m} x [{n} x {var_type}]]"));

                let type_size = match declaration.r#type {
                    VariableType::Bool => 1,
                    VariableType::Int | VariableType::Float => 4,
                };

                // %2 = bitcast [30 x [30 x i32]]* %1 to i8*
                // call void @llvm.memset.p0i8.i64(i8* align 16 %2, i8 0, i64 3600, i1 false)
                let bitcast_var = increment_temp_var(temp_var);
                output.push(format!(
                    "  %{} = bitcast [{} x [{} x {}]]* %{} to i8*",
                    bitcast_var, m, n, var_type, var_num,
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

fn generate_formal_parameters(
    temp_var: &mut usize,
    vars: &mut VarInfoDict,
    parameters: &Vec<Parameter>,
) -> (String, Vec<String>) {
    let mut declarations: Vec<String> = vec![];
    let mut ptr_code: Vec<String> = vec![];

    // Variable number for body (since the entry of function takes 1)
    let mut temp_var_body = *temp_var + parameters.len() + 1;

    // Parameter declaration
    for parameter in parameters {
        let var_num = increment_temp_var(temp_var);

        declarations.push(generate_parameter_declaration(
            parameter.r#type,
            parameter.passing_indicator,
            &var_num.to_string(),
        ));

        if parameter.passing_indicator == ParameterPassIndicator::Val {
            // Need a pointer for the value
            let var_type = convert_type(parameter.r#type);
            let alloca_var = temp_var_body;
            temp_var_body += 1;

            // Pointer
            ptr_code.push(format!("  %{alloca_var} = alloca {var_type}"));
            // Copy over value
            ptr_code.push(format!(
                "  store {} %{}, {}* %{}",
                var_type, var_num, var_type, alloca_var
            ));

            vars.insert(parameter.identifier.node.clone(), VarInfo { var_num: alloca_var });
        } else {
            vars.insert(parameter.identifier.node.clone(), VarInfo { var_num });
        }
    }

    *temp_var = temp_var_body;
    (declarations.join(", "), ptr_code)
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
    llvm_info: &'fromparent LlvmInfo,
) -> (
    &'fromparent str,
    &'fromparent VariableInfo<'fromparent>,
    &'fromparent VarInfo,
) {
    // Find identifier
    let identifier = shape.get_identifier();

    // Get information about variable
    let variable_info = procedure_symbols
        .iter()
        .find(|v| v.identifier == identifier)
        .expect("failed to retrieve variable_info");
    let var_info = llvm_info
        .var_info
        .get(identifier)
        .expect("expected var_info to be present");

    (identifier, variable_info, var_info)
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
    let escaped = bytes.map(|b| format!("\\{b:02x}")).collect::<String>();
    (bytes_len + 1, escaped + "\\00")
}

/// Escapes ' for LLVM variable and function names
fn print_identifier_name(name: &str) -> String {
    "\"".to_owned() + name + "\""
}

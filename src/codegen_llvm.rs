use crate::ast::{
    Expression, GoatProgram, IdentifierShape, IdentifierShapeDeclaration, Node, Parameter, ParameterPassIndicator,
    Procedure, Statement, VariableDeclaration, VariableType,
};
use crate::semantic::{eval_expression_scalar, ProcedureSymbols, SymbolTable};

const SPACE_2: &str = "  ";
type ConvertedStringConst = (usize, String);

pub fn generate_code(program: &GoatProgram, symbol_table: &SymbolTable) -> String {
    let mut output = vec![
        "@format.int = private unnamed_addr constant [3 x i8] c\"%d\\00\"".to_owned(),
        "@format.float = private unnamed_addr constant [3 x i8] c\"%f\\00\"".to_owned(),
        "@format.true = private unnamed_addr constant [5 x i8] c\"true\\00\"".to_owned(),
        "@format.false = private unnamed_addr constant [6 x i8] c\"false\\00\"".to_owned(),
        "declare i32 @printf(i8* noundef, ...)".to_owned(),
        String::new(),
    ];

    let mut string_constants: Vec<ConvertedStringConst> = vec![];

    let mut procedure_outputs = vec![];
    for procedure in &program.procedures {
        procedure_outputs.push(generate_code_proc(&mut string_constants, procedure, symbol_table));
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

fn generate_code_proc(
    strings: &mut Vec<ConvertedStringConst>,
    procedure: &Procedure,
    symbol_table: &SymbolTable,
) -> String {
    let mut output = vec![];

    let is_main = procedure.identifier.node == "main";
    let return_type = if is_main { "i32" } else { "void" };

    let mut temp_var = 1;

    // Parameters
    let (parameters_declaration, mut parameters_store) =
        generate_code_formal_parameters(&mut temp_var, &procedure.parameters);
    output.push(format!(
        "define dso_local {} @{}({}) {{ ; proc {}({})",
        return_type,
        procedure.identifier.node,
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

    output.append(&mut generate_code_var_declarations(&procedure.variable_declarations));
    output.append(&mut generate_code_body(&mut temp_var, strings, symbol_table, procedure));

    if is_main {
        output.push("  ret i32 0".to_owned());
    } else {
        output.push("  ret void".to_owned());
    }

    output.push("}".to_owned());

    output.join("\n")
}

fn generate_code_body(
    temp_var: &mut usize,
    strings: &mut Vec<ConvertedStringConst>,
    symbol_table: &SymbolTable,
    procedure: &Procedure,
) -> Vec<String> {
    generate_code_statements(strings, temp_var, symbol_table, procedure, &procedure.body.statements)
}

fn generate_code_statements(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    statements: &Vec<Node<Statement>>,
) -> Vec<String> {
    let mut output = vec![];

    for statement in statements {
        output.append(&mut generate_code_statement(
            strings,
            temp_var,
            symbol_table,
            procedure,
            &statement.node,
        ));
    }

    output
}

fn generate_code_statement(
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
            let (temp_var_index, generated) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
            output.append(
                &mut generated
                    .iter()
                    .map(|g| SPACE_2.to_owned() + g)
                    .collect::<Vec<String>>(),
            );

            let if_label = *temp_var;
            *temp_var += 1;

            let mut if_statements_code =
                generate_code_statements(strings, temp_var, symbol_table, procedure, statements);

            let endif_label = *temp_var;
            *temp_var += 1;

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                temp_var_index, if_label, endif_label
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
            let (conditional_var_index, mut expr_code) =
                generate_code_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            let if_label = *temp_var;
            *temp_var += 1;
            let mut if_statements_code =
                generate_code_statements(strings, temp_var, symbol_table, procedure, statements1);
            let else_label = *temp_var;
            *temp_var += 1;
            let mut else_statements_code =
                &mut generate_code_statements(strings, temp_var, symbol_table, procedure, statements2);
            let endif_label = *temp_var;
            *temp_var += 1;

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var_index, if_label, else_label
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
            let conditional_label = *temp_var;
            *temp_var += 1;
            let body_label = *temp_var;
            *temp_var += 1;
            let mut while_body_code = generate_code_statements(strings, temp_var, symbol_table, procedure, statements);
            let endwhile_label = *temp_var;
            *temp_var += 1;

            // Immediate jump to conditional
            output.push(format!("  br label %{}", conditional_label));

            // Evaluate boolean expression
            output.push(format!("{}:\t\t\t\t; start while conditional", conditional_label));
            let (conditional_var_index, mut expr_code) =
                generate_code_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            // Jump on conditional
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                conditional_var_index, body_label, endwhile_label
            ));

            // Body
            output.push(format!("{}:\t\t\t\t; body of while", body_label));
            output.append(&mut while_body_code);
            // Back to conditional
            output.push(format!("  br label %{}, !llvm.loop !{}", conditional_label, body_label));

            // After end of while
            output.push(format!("{}:\t\t\t\t; end while", endwhile_label));
        }
        Statement::Assign(identifier_shape, expr) => {
            output.append(&mut generate_code_assign(
                temp_var,
                procedure_symbols,
                identifier_shape,
                expr,
            ));
        }
        Statement::Write(expr) => {
            output.append(&mut generate_code_write(strings, temp_var, procedure_symbols, expr));
        }
        // TODO
        Statement::Read(_) => {
            output.push("    ; a read statement".to_owned());
        }
        // TODO
        Statement::Call(_, _) => {
            output.push("    ; a call statement".to_owned());
        }
    }

    output
}

fn generate_code_assign(
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    identifier_shape: &IdentifierShape,
    expr: &Node<Expression>,
) -> Vec<String> {
    let mut output = vec![];

    // First evaluate the expression
    let (var_num, mut expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
    output.append(&mut expr_code);

    // Find identifier
    let identifier = match identifier_shape {
        IdentifierShape::Identifier(identifier) => identifier,
        IdentifierShape::IdentifierArray(identifier, _) => identifier,
        IdentifierShape::IdentifierArray2D(identifier, _, _) => identifier,
    };

    // Get information about variable
    let variable_info = procedure_symbols
        .iter()
        .find(|v| v.identifier == &identifier.node)
        .expect("Failed to retrieve variable_info");
    let variable_type = convert_type(variable_info.r#type);
    let variable_pass_indicator = *variable_info.pass_indicator.unwrap_or(&ParameterPassIndicator::Val);

    match identifier_shape {
        // TODO: int/float
        IdentifierShape::Identifier(identifier) => {
            match variable_pass_indicator {
                ParameterPassIndicator::Val => {
                    // store i32 %value_source, i32* %decl_dest
                    output.push(format!(
                        "  store {} %{}, {}* %{}",
                        variable_type, var_num, variable_type, identifier.node
                    ));
                }
                ParameterPassIndicator::Ref => {
                    // TODO
                    panic!("huh?");
                }
            }
        }
        IdentifierShape::IdentifierArray(_identifier, _) => {
            // TODO: no ref
            panic!("huh?");
        }
        IdentifierShape::IdentifierArray2D(_identifier, _, _) => {
            // TODO: no ref
            panic!("huh?");
        }
    }

    output
}

fn generate_code_write(
    strings: &mut Vec<ConvertedStringConst>,
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    expr: &Node<Expression>,
) -> Vec<String> {
    let mut output = vec![];
    let (var_num, mut expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
    output.append(&mut expr_code);

    if let Expression::StringConst(str_const) = &expr.node {
        let print_return_num = *temp_var;
        *temp_var += 1;

        // Convert to (number of bytes, string constant representation)
        let converted = convert_string_const(str_const);
        let str_const_len = converted.0;
        // Get constant index
        let str_const_index = strings.len();
        strings.push(converted);

        output.push(
            format!("  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([{} x i8], [{} x i8]* @strconst.{}, i64 0, i64 0))",
                print_return_num, str_const_len, str_const_len, str_const_index)
        );
        return output;
    }

    let expr_type = eval_expression_scalar(procedure_symbols, expr).expect("type to be well-formed");
    match expr_type {
        VariableType::Bool => {
            let if_label = *temp_var;
            *temp_var += 1;
            let print_return_num1 = *temp_var;
            *temp_var += 1;
            let else_label = *temp_var;
            *temp_var += 1;
            let print_return_num2 = *temp_var;
            *temp_var += 1;
            let endif_label = *temp_var;
            *temp_var += 1;

            // Jump
            output.push(format!(
                "  br i1 %{}, label %{}, label %{}",
                var_num, if_label, else_label
            ));

            // If true
            output.push(format!("{}:\t\t\t\t; if bool", if_label));
            output.push(
                format!("  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([5 x i8], [5 x i8]* @format.true, i64 0, i64 0))",
               print_return_num1)
            );
            output.push(format!("  br label %{}", endif_label));

            // Else false
            output.push(format!("{}:\t\t\t\t; else bool", else_label));
            output.push(
                format!("  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([6 x i8], [6 x i8]* @format.false, i64 0, i64 0))",
                    print_return_num2)
            );
            output.push(format!("  br label %{}", endif_label));

            // After endif
            output.push(format!("{}:\t\t\t\t; end bool", endif_label));
        }
        VariableType::Float => {
            let print_return_num = *temp_var;
            *temp_var += 1;

            output.push(
                format!("  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @format.float, i64 0, i64 0), float noundef %{})",
                    print_return_num, var_num)
            );
        }
        VariableType::Int => {
            let print_return_num = *temp_var;
            *temp_var += 1;

            output.push(
                format!("  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @format.int, i64 0, i64 0), i32 noundef %{})",
                print_return_num, var_num)
            );
        }
    }
    output
}

#[allow(dead_code, unused_variables, unused_mut)]
fn generate_code_expression(
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    expression: &Expression,
) -> (usize, Vec<String>) {
    let mut output = vec![];

    let var_num = match expression {
        Expression::IntConst(n) => {
            let store_var = *temp_var;
            *temp_var += 1;
            output.push(format!("  %{} = alloca i32", store_var));
            output.push(format!("  store i32 {}, i32* %{}", n, store_var));

            let load_var = *temp_var;
            *temp_var += 1;
            output.push(format!("  %{} = load i32, i32* %{}", load_var, store_var));
            load_var
        }
        Expression::BoolConst(b) => {
            let store_var = *temp_var;
            *temp_var += 1;
            output.push(format!("  %{} = alloca i1", store_var));
            output.push(format!("  store i1 {}, i1* %{}", if *b { "1" } else { "0" }, store_var));

            let load_var = *temp_var;
            *temp_var += 1;
            output.push(format!("  %{} = load i1, i1* %{}", load_var, store_var));
            load_var
        }
        // TODO
        Expression::FloatConst(n) => 1001,
        Expression::StringConst(_) => {
            panic!("StringConst is not supported by generate_code_expression");
        }
        Expression::IdentifierShape(shape) => {
            // Find identifier
            let identifier = match shape {
                IdentifierShape::Identifier(identifier) => identifier,
                IdentifierShape::IdentifierArray(identifier, _) => identifier,
                IdentifierShape::IdentifierArray2D(identifier, _, _) => identifier,
            };

            // Get information about variable
            let variable_info = procedure_symbols
                .iter()
                .find(|v| v.identifier == &identifier.node)
                .expect("Failed to retrieve variable_info");
            let variable_type = convert_type(variable_info.r#type);
            let variable_pass_indicator = *variable_info.pass_indicator.unwrap_or(&ParameterPassIndicator::Val);

            match shape {
                IdentifierShape::Identifier(identifier) => {
                    match variable_pass_indicator {
                        ParameterPassIndicator::Val => {
                            // %v = load i32, i32* %identifier
                            let load_var = *temp_var;
                            *temp_var += 1;

                            output.push(format!(
                                "  %{} = load {}, {}* %{}",
                                load_var, variable_type, variable_type, identifier.node
                            ));
                            load_var
                        }
                        ParameterPassIndicator::Ref => {
                            panic!("huh?");
                        }
                    }
                }
                IdentifierShape::IdentifierArray(_, _) => {
                    panic!("huh?");
                }
                IdentifierShape::IdentifierArray2D(_, _, _) => {
                    panic!("huh?");
                }
            }
        }
        // TODO
        Expression::UnopExpr(_, _) => 1004,
        // TODO
        Expression::BinopExpr(_, _, _) => 1005,
    };

    (var_num, output)
}

fn generate_code_var_declarations(declarations: &Vec<VariableDeclaration>) -> Vec<String> {
    let mut output = vec![];

    for declaration in declarations {
        let (identifier, shape) = match &declaration.identifier_declaration {
            // %1 = alloca i32
            IdentifierShapeDeclaration::Identifier(identifier) => {
                (identifier.node.clone(), convert_type(declaration.r#type))
            }
            // %1 = alloca [m x i32]
            IdentifierShapeDeclaration::IdentifierArray(identifier, n) => (
                identifier.node.clone(),
                format!("[{} x {}]", n, convert_type(declaration.r#type)),
            ),
            // %1 = alloca [m x [n x i32]]
            IdentifierShapeDeclaration::IdentifierArray2D(identifier, m, n) => (
                identifier.node.clone(),
                format!("[{} x [{} x {}]]", m, n, convert_type(declaration.r#type)),
            ),
        };
        output.push(format!("  %{} = alloca {}", identifier, shape));
    }

    output
}

fn generate_code_formal_parameters(temp_var: &mut usize, parameters: &Vec<Parameter>) -> (String, Vec<String>) {
    let mut declarations: Vec<String> = vec![];
    let mut stores = vec![];

    for parameter in parameters {
        let r#type = convert_type(parameter.r#type);
        let passing_indicator = match parameter.passing_indicator {
            ParameterPassIndicator::Ref => "*",
            ParameterPassIndicator::Val => "",
        };

        let var_num = *temp_var;
        *temp_var += 1;

        // i32 noundef %1 (val)
        // i32* noundef %2 (ref)
        declarations.push(format!("{}{} noundef %{}", r#type, passing_indicator, var_num));

        // %name1 = alloca i32 (val)
        // %name2 = alloca i32* (ref)
        stores.push(format!(
            "  %{} = alloca {}{}",
            parameter.identifier.node, r#type, passing_indicator
        ));

        // store i32 %1, i32* %name1 (val)
        // store i32* %2, i32** %name2 (ref)
        stores.push(format!(
            "  store {}{} %{}, {}*{} %{}",
            r#type, passing_indicator, temp_var, r#type, passing_indicator, parameter.identifier.node
        ));
    }

    (declarations.join(", "), stores)
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
    let escaped = bytes.map(|b| map_to_escape(b)).collect::<String>();
    (bytes_len + 1, escaped + "\\00")
}

fn map_to_escape(c: u8) -> String {
    format!("\\{:02x}", c)
}

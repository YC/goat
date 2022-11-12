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
        "declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg)".to_owned(),
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

    output.append(&mut generate_code_var_declarations(
        &mut temp_var,
        &procedure.variable_declarations,
    ));
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
            let (conditional_var, generated) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
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
            let if_label = *temp_var;
            *temp_var += 1;

            let mut if_statements_code =
                generate_code_statements(strings, temp_var, symbol_table, procedure, statements);

            let endif_label = *temp_var;
            *temp_var += 1;

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
            let (conditional_var, mut expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut expr_code);

            // %1 = <expr-result>
            // br i1 %1, label %if, label %else
            // if: ...
            // br label %endif
            // else: ...
            // br label %endif
            // endif:
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
            let (conditional_var, mut expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
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
        // TODO: reads
        Statement::Read(_) => {
            output.push("    ; a read statement".to_owned());
        }
        // TODO: calls
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
    let (expr_var, mut expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
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

    // TODO: int to float conversion

    match identifier_shape {
        IdentifierShape::Identifier(identifier) => {
            match variable_pass_indicator {
                ParameterPassIndicator::Val => {
                    // store i32 %value_source, i32* %decl_dest
                    output.push(format!(
                        "  store {} %{}, {}* %{}",
                        variable_type, expr_var, variable_type, identifier.node
                    ));
                }
                ParameterPassIndicator::Ref => {
                    // TODO: ref assign
                    panic!("huh?");
                }
            }
        }
        IdentifierShape::IdentifierArray(identifier, expr) => {
            let (m_expr_var, mut m_expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut m_expr_code);

            // Get the array dimension
            let variable_shape = variable_info.shape.unwrap();
            let IdentifierShapeDeclaration::IdentifierArray(_, m) = variable_shape else {
                panic!("Expected array");
            };

            // %1 = alloca [2 x i32], align 4                                           ; allocation (previous)
            // %v = sext i32 %<expr> to i64                                             ; index expression value to i64
            // %5 = getelementptr inbounds [2 x i32], [2 x i32]* %1, i64 0, i64 %v      ; addressing
            // store i32 3, i32* %5, align 4                                            ; store
            let convert_var = *temp_var;
            *temp_var += 1;
            output.push(format!("  %{} = sext i32 %{} to i64", convert_var, m_expr_var));
            let address_var = *temp_var;
            *temp_var += 1;
            output.push(format!(
                "  %{} = getelementptr inbounds [{} x {}], [{} x {}]* %{}, i64 0, i64 %{}",
                address_var, m, variable_type, m, variable_type, identifier.node, convert_var
            ));
            output.push(format!(
                "  store {} %{}, i32* %{}",
                variable_type, expr_var, address_var
            ));
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

    let (expr_var, mut expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
    output.append(&mut expr_code);

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
                expr_var, if_label, else_label
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
                    print_return_num, expr_var)
            );
        }
        VariableType::Int => {
            let print_return_num = *temp_var;
            *temp_var += 1;

            output.push(
                format!("  %{} = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @format.int, i64 0, i64 0), i32 noundef %{})",
                print_return_num, expr_var)
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
            // %3 = alloca i32              ; allocate ptr
            // store i32 <const>, i32* %3   ; store const to ptr
            // %4 = load i32, i32* %3       ; load value
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
        // TODO: float const expression
        Expression::FloatConst(n) => 1001,
        Expression::StringConst(_) => panic!("StringConst is not supported by generate_code_expression"),
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
                            // %8 = load i32, i32* %identifier      ; load value of identifier (ptr) into temp var
                            let load_var = *temp_var;
                            *temp_var += 1;

                            output.push(format!(
                                "  %{} = load {}, {}* %{}",
                                load_var, variable_type, variable_type, identifier.node
                            ));
                            load_var
                        }
                        ParameterPassIndicator::Ref => {
                            // %9 = load i32*, i32** %identifier    ; load identifier** into ptr
                            // %10 = load i32, i32* %9              ; load value of ptr
                            let load_var1 = *temp_var;
                            *temp_var += 1;
                            let load_var2 = *temp_var;
                            *temp_var += 1;

                            output.push(format!(
                                "  %{} = load {}*, {}** %{}",
                                load_var1, variable_type, variable_type, identifier.node
                            ));
                            output.push(format!(
                                "  %{} = load {}, {}* %{}",
                                load_var2, variable_type, variable_type, load_var1
                            ));
                            load_var2
                        }
                    }
                }
                IdentifierShape::IdentifierArray(identifier, expr) => {
                    // First evaluate the expression
                    let (expr_var, mut expr_code) = generate_code_expression(temp_var, procedure_symbols, &expr.node);
                    output.append(&mut expr_code);

                    // Get the array dimension
                    let variable_shape = variable_info.shape.unwrap();
                    let IdentifierShapeDeclaration::IdentifierArray(_, m) = variable_shape else {
                        panic!("Expected array");
                    };

                    // %1 = alloca [2 x i32]                                                    ; allocate array (not part of this)
                    // %v = sext i32 %<expr> to i64
                    // %3 = getelementptr inbounds [2 x i32], [2 x i32]* %1, i64 0, i64 %v      ; calculate address, second does indexing
                    // %4 = load i32, i32* %3                                                   ; load value
                    let convert_var = *temp_var;
                    *temp_var += 1;
                    let address_var = *temp_var;
                    *temp_var += 1;
                    let load_var = *temp_var;
                    *temp_var += 1;

                    output.push(format!("  %{} = sext i32 %{} to i64", convert_var, expr_var));
                    output.push(format!(
                        "  %{} = getelementptr inbounds [{} x {}], [{} x {}]* %{}, i64 0, i64 %{}",
                        address_var, m, variable_type, m, variable_type, identifier.node, convert_var
                    ));
                    output.push(format!(
                        "  %{} = load {}, {}* %{}",
                        load_var, variable_type, variable_type, address_var
                    ));
                    load_var
                }
                IdentifierShape::IdentifierArray2D(_, _, _) => {
                    panic!("huh?");
                }
            }
        }
        // TODO: unop
        Expression::UnopExpr(_, _) => 1004,
        // TODO: binop
        Expression::BinopExpr(_, _, _) => 1005,
    };

    (var_num, output)
}

fn generate_code_var_declarations(temp_var: &mut usize, declarations: &Vec<VariableDeclaration>) -> Vec<String> {
    let mut output = vec![];

    for declaration in declarations {
        let var_type = convert_type(declaration.r#type);

        match &declaration.identifier_declaration {
            IdentifierShapeDeclaration::Identifier(identifier) => {
                // %1 = alloca i32          ; allocate
                output.push(format!("  %{} = alloca {}", identifier.node, var_type));
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
                    identifier.node
                ));
            }
            IdentifierShapeDeclaration::IdentifierArray(identifier, n) => {
                // %1 = alloca [n x i32]
                output.push(format!("  %{} = alloca [{} x {}]", identifier.node, n, var_type));

                let type_size = match declaration.r#type {
                    VariableType::Bool => 1,
                    VariableType::Int => 4,
                    VariableType::Float => 4,
                };

                // %3 = bitcast [10 x i32]* %2 to i8*
                // call void @llvm.memset.p0i8.i64(i8* align 16 %3, i8 0, i64 40, i1 false)
                let bitcast_var = *temp_var;
                *temp_var += 1;

                output.push(format!(
                    "  %{} = bitcast [{} x {}]* %{} to i8*",
                    bitcast_var, n, var_type, identifier.node
                ));
                output.push(format!(
                    "  call void @llvm.memset.p0i8.i64(i8* %{}, i8 0, i64 {}, i1 false)",
                    bitcast_var,
                    type_size * n
                ));
            }
            IdentifierShapeDeclaration::IdentifierArray2D(identifier, m, n) => {
                // %1 = alloca [m x [n x i32]]
                output.push(format!(
                    "  %{} = alloca [{} x [{} x {}]]",
                    identifier.node, m, n, var_type
                ));

                let type_size = match declaration.r#type {
                    VariableType::Bool => 1,
                    VariableType::Int => 4,
                    VariableType::Float => 4,
                };

                // %2 = bitcast [30 x [30 x i32]]* %1 to i8*
                // call void @llvm.memset.p0i8.i64(i8* align 16 %2, i8 0, i64 3600, i1 false)
                let bitcast_var = *temp_var;
                *temp_var += 1;

                output.push(format!(
                    "  %{} = bitcast [{} x [{} x {}]]* %{} to i8*",
                    bitcast_var, m, n, var_type, identifier.node
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

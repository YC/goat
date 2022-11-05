use crate::ast::{
    Expression, GoatProgram, IdentifierShapeDeclaration, Node, Parameter, ParameterPassIndicator, Procedure, Statement,
    VariableDeclaration, VariableType,
};
use crate::semantic::{ProcedureSymbols, SymbolTable};

pub fn generate_code(program: &GoatProgram, symbol_table: &SymbolTable) -> String {
    let mut output: Vec<String> = vec![];

    for procedure in &program.procedures {
        output.push(generate_code_proc(procedure, symbol_table));
    }

    output.join("\n\n")
}

const SPACE_4: &str = "    ";

fn generate_code_proc(procedure: &Procedure, symbol_table: &SymbolTable) -> String {
    let mut output = vec![];

    let is_main = procedure.identifier.node == "main";
    let return_type = if is_main { "i32" } else { "void" };

    let parameters = generate_code_formal_parameters(&procedure.parameters);
    output.push(format!(
        "define dso_local {} @{}({}) {{ ; proc {}({})",
        return_type,
        procedure.identifier.node,
        parameters,
        // comment
        procedure.identifier.node,
        procedure
            .parameters
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join(", ")
    ));

    output.append(&mut generate_code_var_declarations(&procedure.variable_declarations));
    output.append(&mut generate_code_body(symbol_table, procedure));

    if is_main {
        output.push("    ret i32 0".to_owned());
    } else {
        output.push("    ret void".to_owned());
    }

    output.push("}".to_owned());

    output.join("\n")
}

fn generate_code_body(symbol_table: &SymbolTable, procedure: &Procedure) -> Vec<String> {
    let mut temp_var = 1;
    generate_code_statements(&mut temp_var, symbol_table, procedure, &procedure.body.statements)
}

fn generate_code_statements(
    temp_var: &mut usize,
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    statements: &Vec<Node<Statement>>,
) -> Vec<String> {
    let mut output = vec![];

    for statement in statements {
        output.append(&mut generate_code_statement(
            temp_var,
            symbol_table,
            procedure,
            &statement.node,
        ));
    }

    output
}

fn generate_code_statement(
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
                    .map(|g| SPACE_4.to_owned() + g)
                    .collect::<Vec<String>>(),
            );

            let if_label = *temp_var;
            *temp_var += 1;
            let endif_label = *temp_var;
            *temp_var += 1;

            // Jump
            output.push(format!(
                "{}br i1 %{}, label %{}, label %{}",
                SPACE_4, temp_var_index, if_label, endif_label
            ));

            // If statements
            output.push(format!("{}:\t\t\t\t; if statements", if_label));
            output.append(&mut generate_code_statements(
                temp_var,
                symbol_table,
                procedure,
                statements,
            ));
            output.push(format!("{}br label %{}", SPACE_4, endif_label));

            // After endif
            output.push(format!("{}:\t\t\t\t; end of if statement", endif_label));
        }
        Statement::IfElse(expr, statements1, statements2) => {
            // Evaluate boolean expression
            let (conditional_var_index, mut generated) =
                generate_code_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut generated);

            let if_label = *temp_var;
            *temp_var += 1;
            let else_label = *temp_var;
            *temp_var += 1;
            let endif_label = *temp_var;
            *temp_var += 1;

            // Jump
            output.push(format!(
                "{}br i1 %{}, label %{}, label %{}",
                " ".repeat(4),
                conditional_var_index,
                if_label,
                else_label
            ));

            // If statements
            output.push(format!("{}:\t\t\t\t; if statements", if_label));
            output.append(&mut generate_code_statements(
                temp_var,
                symbol_table,
                procedure,
                statements1,
            ));
            output.push(format!("{}br label %{}", " ".repeat(4), endif_label));

            // Else statements
            output.push(format!("{}:\t\t\t\t; else statements", else_label));
            output.append(&mut generate_code_statements(
                temp_var,
                symbol_table,
                procedure,
                statements2,
            ));
            output.push(format!("{}br label %{}", SPACE_4, endif_label));

            // After endif
            output.push(format!("{}:\t\t\t\t; end ifelse", endif_label));
        }
        Statement::While(expr, statements) => {
            let conditional_label = *temp_var;
            *temp_var += 1;
            let body_label = *temp_var;
            *temp_var += 1;
            let endwhile_label = *temp_var;
            *temp_var += 1;

            // Immediate jump to conditional
            output.push(format!("{}br label %{}", SPACE_4, conditional_label));

            // Evaluate boolean expression
            output.push(format!("{}:\t\t\t\t; start while conditional", conditional_label));
            let (conditional_var_index, mut generated) =
                generate_code_expression(temp_var, procedure_symbols, &expr.node);
            output.append(&mut generated);

            // Jump on conditional
            output.push(format!(
                "{}br i1 %{}, label %{}, label %{}",
                SPACE_4, conditional_var_index, body_label, endwhile_label
            ));

            // Body
            output.push(format!("{}:\t\t\t\t; body of while", body_label));
            output.append(&mut generate_code_statements(
                temp_var,
                symbol_table,
                procedure,
                statements,
            ));
            // Back to conditional
            output.push(format!(
                "{}br label %{}, !llvm.loop !{}",
                SPACE_4, conditional_label, body_label
            ));

            // After end of while
            output.push(format!("{}:\t\t\t\t; end while", endwhile_label));
        }
        // TODO
        Statement::Assign(_, _) => {
            output.push("    ; an assign statement".to_owned());
        }
        // TODO
        Statement::Write(_) => {
            output.push("    ; a write statement".to_owned());
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

#[allow(dead_code, unused_variables, unused_mut)]
fn generate_code_expression(
    temp_var: &mut usize,
    procedure_symbols: &ProcedureSymbols,
    expression: &Expression,
) -> (usize, Vec<String>) {
    let mut output = vec![];

    match expression {
        Expression::IntConst(_) => {}
        Expression::BoolConst(_) => {}
        Expression::FloatConst(_) => {}
        Expression::StringConst(_) => {}
        Expression::IdentifierShape(_) => {}
        Expression::UnopExpr(_, _) => {}
        Expression::BinopExpr(_, _, _) => {}
    }

    (0, output)
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
        output.push(format!("{}%{} = alloca {}", SPACE_4, identifier, shape));
    }

    output
}

fn generate_code_formal_parameters(parameters: &Vec<Parameter>) -> String {
    let mut output: Vec<String> = vec![];

    for parameter in parameters {
        let r#type = convert_type(parameter.r#type);
        let passing_indicator = match parameter.passing_indicator {
            ParameterPassIndicator::Ref => "*",
            ParameterPassIndicator::Val => "",
        };
        output.push(format!(
            "{}{} noundef %{}",
            r#type, passing_indicator, parameter.identifier.node
        ));
    }

    output.join(", ")
}

fn convert_type(r#type: VariableType) -> String {
    match r#type {
        VariableType::Bool => "i1".to_owned(),
        VariableType::Int => "i32".to_owned(),
        VariableType::Float => "float".to_owned(),
    }
}

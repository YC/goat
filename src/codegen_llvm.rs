use crate::ast::{
    pad_space, GoatProgram, IdentifierShapeDeclaration, Parameter, ParameterPassIndicator, ProcBody, Procedure,
    VariableDeclaration, VariableType,
};
use crate::semantic::SymbolTable;

pub fn generate_code(program: &GoatProgram, symbol_table: &SymbolTable) -> String {
    let mut output: Vec<String> = vec![];

    for procedure in &program.procedures {
        output.push(generate_code_proc(procedure, symbol_table));
    }

    output.join("\n\n")
}

fn generate_code_proc(procedure: &Procedure, symbol_table: &SymbolTable) -> String {
    let mut output = vec![];

    let is_main = procedure.identifier.node == "main";
    let return_type = if is_main { "i32" } else { "void" };

    let parameters = generate_code_formal_parameters(&procedure.parameters);
    output.push(format!(
        "define dso_local {} @{}({}) {{",
        return_type, procedure.identifier.node, parameters
    ));

    output.append(&mut generate_code_body(&procedure.body, symbol_table));
    output.append(&mut generate_code_var_declarations(&procedure.variable_declarations));

    if is_main {
        output.push(pad_space("ret i32 0", 1));
    } else {
        output.push(pad_space("ret void", 1));
    }

    output.push("}".to_owned());

    output.join("\n")
}

fn generate_code_body(body: &ProcBody, symbol_table: &SymbolTable) -> Vec<String> {
    let mut output = vec![];

    output
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
        output.push(pad_space(&format!("%{} = alloca {}", identifier, shape), 1));
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

use crate::ast::{
    Binop, Expression, GoatProgram, Identifier, IdentifierShape, IdentifierShapeDeclaration, Procedure, Statement,
    Unop, VariableType,
};
use std::{collections::HashMap, collections::HashSet, error::Error};

type SymbolTable = HashMap<Identifier, Vec<VariableInfo>>;

pub fn semantic_analysis(program: GoatProgram) -> Result<(), Box<dyn Error>> {
    // Distinct names
    let mut names: HashSet<String> = HashSet::new();

    for procedure in &program.procedures {
        if names.contains(&procedure.identifier) {
            Err(format!("There is more than 1 proc with name {}", procedure.identifier))?
        }
        names.insert(procedure.identifier.clone());
    }
    if !names.contains("main") {
        Err("Program does not have main function")?
    }

    let mut symbol_table: SymbolTable = HashMap::new();
    for procedure in &program.procedures {
        let mut vars = vec![];

        if procedure.identifier == "main" && !procedure.parameters.is_empty() {
            // TODO: line number
            Err("main function should not have any parameters")?
        }

        // Formal parameters (for calls)
        for formal_param in &procedure.parameters {
            if vars
                .iter()
                .any(|v: &VariableInfo| v.identifier == formal_param.identifier)
            {
                Err(format!(
                    "There is more than 1 formal parameter with name {} for procedure {}",
                    formal_param.identifier, procedure.identifier
                ))?
            }

            let param = VariableInfo {
                identifier: formal_param.identifier.clone(),
                r#type: formal_param.r#type,
                variable_location: VariableLocation::FormalParameter,
                shape: None,
            };
            vars.push(param);
        }

        // Variable declarations (for each procedure)
        for variable_declaration in &procedure.variable_declarations {
            let identifier = match &variable_declaration.identifier_declaration {
                IdentifierShapeDeclaration::Identifier(identifier) => identifier,
                IdentifierShapeDeclaration::IdentifierArray(identifier, m) => {
                    if *m == 0 {
                        // TODO
                        Err("[m] <= 0")?
                    }
                    identifier
                }
                IdentifierShapeDeclaration::IdentifierArray2D(identifier, m, n) => {
                    if *m == 0 {
                        // TODO
                        Err("[m] <= 0")?
                    }
                    if *n == 0 {
                        // TODO
                        Err("[n] <= 0")?
                    }
                    identifier
                }
            };

            if vars.iter().any(|v: &VariableInfo| v.identifier == *identifier) {
                Err(format!(
                    "There is more than 1 variable/parameter with name {} for procedure {}",
                    identifier, procedure.identifier
                ))?
            }

            let param = VariableInfo {
                identifier: identifier.clone(),
                r#type: variable_declaration.r#type,
                variable_location: VariableLocation::VariableDeclaration,
                shape: Some(variable_declaration.identifier_declaration.clone()),
            };
            vars.push(param);
        }

        symbol_table.insert(procedure.identifier.clone(), vars);
    }

    for procedure in &program.procedures {
        if procedure.body.statements.is_empty() {
            // TODO
            Err("<stmt-list> empty")?
        }

        for statement in &procedure.body.statements {
            analyse_statement(&symbol_table, procedure, statement)?
        }
    }

    Ok(())
}

fn analyse_statement(
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    statement: &Statement,
) -> Result<(), Box<dyn Error>> {
    match statement {
        Statement::If(expr, statements) => {
            // - conditions must be bool
            // - bodies must be sequence of statements
            let expr_type = eval_expression_scalar(symbol_table, procedure, expr)?;
            if expr_type != VariableType::Bool {
                // TODO
                Err("Expression for If statement must be bool")?
            }

            if statements.is_empty() {
                // TODO
                Err("<stmt-list> empty")?
            }
            for statement in statements {
                analyse_statement(symbol_table, procedure, statement)?
            }
        }
        Statement::IfElse(expr, statements1, statements2) => {
            // - conditions must be bool
            // - bodies must be sequence of statements
            let expr_type = eval_expression_scalar(symbol_table, procedure, expr)?;
            if expr_type != VariableType::Bool {
                // TODO
                Err("Expression for If statement must be bool")?
            }

            if statements1.is_empty() {
                // TODO
                Err("<stmt-list> empty")?
            }
            if statements2.is_empty() {
                // TODO
                Err("<stmt-list> empty")?
            }
            for statement in statements1 {
                analyse_statement(symbol_table, procedure, statement)?
            }
            for statement in statements2 {
                analyse_statement(symbol_table, procedure, statement)?
            }
        }
        Statement::While(expr, statements) => {
            // - conditions must be bool
            // - bodies must be sequence of statements
            let expr_type = eval_expression_scalar(symbol_table, procedure, expr)?;
            if expr_type != VariableType::Bool {
                // TODO
                Err("Expression for If statement must be bool")?
            }

            if statements.is_empty() {
                // TODO
                Err("<stmt-list> empty")?
            }
            for statement in statements {
                analyse_statement(symbol_table, procedure, statement)?
            }
        }
        Statement::Assign(shape, expression) => {
            // - left hand side must be of same type as right hand side
            // with exception that int can be assigned to float
            // - arrays and matrices can only be updated selectively
            let left_type = eval_shape_type(symbol_table, procedure, shape)?;

            let right_type = eval_expression_scalar(symbol_table, procedure, expression)?;

            if left_type != right_type && (left_type != VariableType::Float && right_type != VariableType::Int) {
                // TODO
                Err("left right types do not match")?
            }
        }
        Statement::Read(shape) => {
            // read takes scalar
            eval_shape_type(symbol_table, procedure, shape)?;
        }
        Statement::Write(expr) => {
            if let Expression::StringConst(_) = expr {
                // String constant
            } else {
                // Well-typed expression
                eval_expression_scalar(symbol_table, procedure, expr)?;
            }
        }
        Statement::Call(identifier, expressions) => {
            // - parameter must be type of corresponding formal parameter
            // - or int (parameter) to float (formal parameter)

            let vars = match symbol_table.get(identifier) {
                None => {
                    // TODO
                    Err("Cannot call proc {} because it doesn't exist")?
                }
                Some(vars) => vars,
            };

            // List of all formal parameters
            let formal_params: Vec<&VariableInfo> = vars
                .iter()
                .filter(|v| v.variable_location == VariableLocation::FormalParameter)
                .collect();
            if formal_params.len() != expressions.len() {
                // TODO
                Err("Expected call to {} to contain {} arguments, but found {}")?
            }

            for i in 0..formal_params.len() {
                let formal_param = formal_params[i];
                let argument = &expressions[i];
                let argument_type = eval_expression_scalar(symbol_table, procedure, argument)?;

                if formal_param.r#type != argument_type
                    && (formal_param.r#type != VariableType::Float || argument_type != VariableType::Int)
                {
                    // TODO
                    Err("Expected argument {} to be of type {}, but found {}")?
                }
            }
        }
    }
    Ok(())
}

fn eval_expression_scalar(
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    expr: &Expression,
) -> Result<VariableType, Box<dyn Error>> {
    let r#type = match expr {
        Expression::IntConst(_) => VariableType::Int,
        Expression::FloatConst(_) => VariableType::Float,
        Expression::BoolConst(_) => VariableType::Bool,
        Expression::StringConst(_) => Err("String const not supported by parse_expression_scalar")?,
        Expression::IdentifierShape(shape) => eval_shape_type(symbol_table, procedure, shape)?,
        Expression::UnopExpr(op, expr) => {
            match op {
                Unop::Minus => {
                    let expr_type = eval_expression_scalar(symbol_table, procedure, expr)?;
                    if expr_type != VariableType::Int && expr_type != VariableType::Float {
                        // TODO
                        Err("Must be int or float")?
                    }
                    expr_type
                }
                Unop::NOT => {
                    let expr_type = eval_expression_scalar(symbol_table, procedure, expr)?;
                    if expr_type != VariableType::Bool {
                        // TODO
                        Err("Must be bool")?
                    }
                    expr_type
                }
            }
        }
        Expression::BinopExpr(op, left, right) => {
            let left_type = eval_expression_scalar(symbol_table, procedure, left)?;
            let right_type = eval_expression_scalar(symbol_table, procedure, right)?;

            match op {
                Binop::EQ | Binop::NEQ => {
                    if left_type != right_type {
                        // TODO
                        Err("Operands of = or != must be of same type")?
                    }
                    VariableType::Bool
                }
                Binop::AND | Binop::OR => {
                    if left_type != VariableType::Bool {
                        // TODO
                        Err("Operands of && or || must be bool")?
                    }
                    if right_type != VariableType::Bool {
                        // TODO
                        Err("Operands of && or || must be bool")?
                    }
                    VariableType::Bool
                }
                Binop::GT | Binop::GTE | Binop::LT | Binop::LTE => {
                    if left_type == VariableType::Bool && right_type != VariableType::Bool
                        || left_type != VariableType::Bool && right_type == VariableType::Bool
                    {
                        // TODO
                        Err("Cannot compare bool with float/int")?
                    }
                    VariableType::Bool
                }
                Binop::Add | Binop::Minus | Binop::Multiply | Binop::Divide => {
                    if left_type == VariableType::Bool || right_type == VariableType::Bool {
                        // TODO
                        Err("Cannot perform binary arithmetic with bool")?
                    }

                    if left_type == VariableType::Float || right_type == VariableType::Float {
                        VariableType::Float
                    } else {
                        VariableType::Int
                    }
                }
            }
        }
    };
    Ok(r#type)
}

fn eval_shape_type(
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    shape: &IdentifierShape,
) -> Result<VariableType, Box<dyn Error>> {
    let r#type = match shape {
        IdentifierShape::Identifier(identifier) => {
            let procedure_symbols = symbol_table.get(&procedure.identifier).unwrap();
            match procedure_symbols.iter().find(|v| v.identifier == *identifier) {
                None => {
                    // TODO
                    Err(format!("Undeclared variable {}", identifier))?
                }
                // var := <expr>; where var is a scalar formal parameter or declaration
                Some(var) => {
                    if var.variable_location == VariableLocation::FormalParameter {
                        // Scalar to scalar assignment
                        var.r#type
                    } else {
                        let shape = var.shape.as_ref().expect("Variable shape declarations does not exist");
                        if let IdentifierShapeDeclaration::Identifier(_) = shape {
                            var.r#type
                        } else {
                            // TODO
                            Err("Cannot directly assign to array to matrix variable")?
                        }
                    }
                }
            }
        }
        IdentifierShape::IdentifierArray(identifier, expr) => {
            if eval_expression_scalar(symbol_table, procedure, expr)? != VariableType::Int {
                // TODO
                Err("[n] must be int")?
            }

            let procedure_symbols = symbol_table.get(&procedure.identifier).unwrap();
            match procedure_symbols.iter().find(|v| v.identifier == *identifier) {
                None => {
                    // TODO
                    Err(format!("Undeclared variable {}", identifier))?
                }
                // var[<expr>] := <expr>; where var is an array
                Some(var) => {
                    if var.variable_location == VariableLocation::FormalParameter {
                        // TODO
                        Err("Array cannot refer to formal parameter")?
                    }

                    let shape = var.shape.as_ref().expect("Variable shape declarations does not exist");
                    if let IdentifierShapeDeclaration::IdentifierArray(_, _) = shape {
                        var.r#type
                    } else {
                        // TODO
                        Err("Variable declaration mismatch, should be array")?
                    }
                }
            }
        }
        IdentifierShape::IdentifierArray2D(identifier, expr1, expr2) => {
            if eval_expression_scalar(symbol_table, procedure, expr1)? != VariableType::Int {
                // TODO
                Err("[n] must be int")?
            }
            if eval_expression_scalar(symbol_table, procedure, expr2)? != VariableType::Int {
                // TODO
                Err("[m] must be int")?
            }

            let procedure_symbols = symbol_table.get(&procedure.identifier).unwrap();
            match procedure_symbols.iter().find(|v| v.identifier == *identifier) {
                None => {
                    // TODO
                    Err(format!("Undeclared variable {}", identifier))?
                }
                // var[<expr>, <expr>] := <expr>; where var is an array
                Some(var) => {
                    if var.variable_location == VariableLocation::FormalParameter {
                        // TODO
                        Err("Matrix cannot refer to formal parameter")?
                    }

                    let shape = var.shape.as_ref().expect("Variable shape declarations does not exist");
                    if let IdentifierShapeDeclaration::IdentifierArray2D(_, _, _) = shape {
                        var.r#type
                    } else {
                        // TODO
                        Err("Variable declaration mismatch, should be matrix")?
                    }
                }
            }
        }
    };
    Ok(r#type)
}

struct VariableInfo {
    pub identifier: Identifier,
    pub variable_location: VariableLocation,
    pub r#type: VariableType,
    pub shape: Option<IdentifierShapeDeclaration>,
}

#[derive(Debug, Eq, PartialEq)]
enum VariableLocation {
    FormalParameter,
    VariableDeclaration,
}

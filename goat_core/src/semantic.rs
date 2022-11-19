use crate::ast::{
    Binop, Expression, GoatProgram, Identifier, IdentifierShape, IdentifierShapeDeclaration, Node,
    ParameterPassIndicator, Procedure, Statement, Unop, VariableType,
};
use std::{collections::HashMap, collections::HashSet, error::Error};

pub type SymbolTable<'src> = HashMap<&'src String, ProcedureSymbols<'src>>;
pub type ProcedureSymbols<'src> = Vec<VariableInfo<'src>>;

pub struct VariableInfo<'src> {
    pub identifier: &'src Identifier,
    pub variable_location: VariableLocation,
    pub r#type: VariableType,
    pub shape: Option<&'src IdentifierShapeDeclaration>,
    pub pass_indicator: Option<&'src ParameterPassIndicator>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum VariableLocation {
    FormalParameter,
    VariableDeclaration,
}

pub fn analyse(program: &GoatProgram) -> Result<SymbolTable, Box<dyn Error>> {
    // Distinct names
    let mut names: HashSet<&str> = HashSet::new();
    for procedure in &program.procedures {
        if names.contains(procedure.identifier.node.as_str()) {
            return Err(format!(
                "There is more than 1 proc with name {}, second occurrence at {:?}",
                procedure.identifier.node, procedure.identifier.location
            ))?;
        }
        names.insert(&procedure.identifier.node);
    }
    if !names.contains("main") {
        return Err("Program does not have main function")?;
    }

    let mut symbol_table: SymbolTable = HashMap::new();
    for procedure in &program.procedures {
        let mut vars = vec![];

        if procedure.identifier.node == "main" && !procedure.parameters.is_empty() {
            #[allow(clippy::indexing_slicing)]
            return Err(format!(
                "main function should not have any parameters: see line {}",
                procedure.parameters[0].identifier.location.0
            ))?;
        }

        // Formal parameters (for calls)
        for formal_param in &procedure.parameters {
            if vars
                .iter()
                .any(|v: &VariableInfo| v.identifier == &formal_param.identifier.node)
            {
                return Err(format!(
                    "There is more than 1 formal parameter with name {} for procedure {}",
                    formal_param.identifier.node, procedure.identifier.node
                ))?;
            }

            let param = VariableInfo {
                identifier: &formal_param.identifier.node,
                r#type: formal_param.r#type,
                variable_location: VariableLocation::FormalParameter,
                pass_indicator: Some(&formal_param.passing_indicator),
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
                        return Err(format!(
                            "The array {} at {:?} cannot be initialised with [0] elements: {}[{}]",
                            identifier.node, identifier.location, identifier.node, m
                        ))?;
                    }
                    identifier
                }
                IdentifierShapeDeclaration::IdentifierArray2D(identifier, m, n) => {
                    if *m == 0 {
                        return Err(format!(
                            "The array {} at {:?} cannot be initialised with [0, {}] elements: {}[{}, {}]",
                            identifier.node, identifier.location, n, identifier.node, m, n
                        ))?;
                    }
                    if *n == 0 {
                        return Err(format!(
                            "The array {} at {:?} cannot be initialised with [{}, 0] elements: {}[{}, {}]",
                            identifier.node, identifier.location, m, identifier.node, m, n
                        ))?;
                    }
                    identifier
                }
            };

            if vars.iter().any(|v: &VariableInfo| v.identifier == &identifier.node) {
                return Err(format!(
                    "There is more than 1 variable/parameter with name {} for procedure {}",
                    identifier.node, procedure.identifier.node
                ))?;
            }

            let param = VariableInfo {
                identifier: &identifier.node,
                r#type: variable_declaration.r#type,
                variable_location: VariableLocation::VariableDeclaration,
                pass_indicator: None,
                shape: Some(&variable_declaration.identifier_declaration),
            };
            vars.push(param);
        }

        symbol_table.insert(&procedure.identifier.node, vars);
    }

    for procedure in &program.procedures {
        if procedure.body.statements.is_empty() {
            return Err(format!(
                "Procedure {} does not have any statements",
                procedure.identifier.node
            ))?;
        }

        for statement in &procedure.body.statements {
            analyse_statement(&symbol_table, procedure, statement)?;
        }
    }

    Ok(symbol_table)
}

fn analyse_statement(
    symbol_table: &SymbolTable,
    procedure: &Procedure,
    statement: &Node<Statement>,
) -> Result<(), Box<dyn Error>> {
    let procedure_symbols = symbol_table
        .get(&procedure.identifier.node)
        .expect("no symbols for procedure");

    match &statement.node {
        Statement::If(expr, statements) => {
            // - conditions must be bool
            // - bodies must be sequence of statements
            let expr_type = eval_expression_scalar(procedure_symbols, expr)?;
            if expr_type != VariableType::Bool {
                return Err(format!(
                    "Expression for If statement \"{}\" at {:?} must be bool, but is {}",
                    expr.node, expr.location, expr_type
                ))?;
            }

            if statements.is_empty() {
                return Err(format!(
                    "Expected list of statements for if branch of If statement at {:?}",
                    statement.location
                ))?;
            }
            for statement in statements {
                analyse_statement(symbol_table, procedure, statement)?;
            }
        }
        Statement::IfElse(expr, statements1, statements2) => {
            // - conditions must be bool
            // - bodies must be sequence of statements
            let expr_type = eval_expression_scalar(procedure_symbols, expr)?;
            if expr_type != VariableType::Bool {
                return Err(format!(
                    "Expression for IfElse statement \"{}\" at {:?} must be bool, but is {}",
                    expr.node, expr.location, expr_type
                ))?;
            }

            if statements1.is_empty() {
                return Err(format!(
                    "Expected non-empty list of statements for if branch of IfElse statement at {:?}",
                    statement.location
                ))?;
            }
            if statements2.is_empty() {
                return Err(format!(
                    "Expected non-empty list of statements for else branch of IfElse statement at {:?}",
                    statement.location
                ))?;
            }
            for statement in statements1 {
                analyse_statement(symbol_table, procedure, statement)?;
            }
            for statement in statements2 {
                analyse_statement(symbol_table, procedure, statement)?;
            }
        }
        Statement::While(expr, statements) => {
            // - conditions must be bool
            // - bodies must be sequence of statements
            let expr_type = eval_expression_scalar(procedure_symbols, expr)?;
            if expr_type != VariableType::Bool {
                return Err(format!(
                    "Expression for While statement \"{}\" at {:?} must be bool, but is {}",
                    expr.node, expr.location, expr_type
                ))?;
            }

            if statements.is_empty() {
                return Err(format!(
                    "Expected non-empty list of statements for while loop at {:?}",
                    statement.location
                ))?;
            }
            for statement in statements {
                analyse_statement(symbol_table, procedure, statement)?;
            }
        }
        Statement::Assign(shape, expr) => {
            // - left hand side must be of same type as right hand side
            // with exception that int can be assigned to float
            // - arrays and matrices can only be updated selectively
            let left_type = eval_shape_type(procedure_symbols, shape)?;

            let right_type = eval_expression_scalar(procedure_symbols, expr)?;

            if left_type != right_type && (left_type != VariableType::Float && right_type != VariableType::Int) {
                return Err(format!(
                    "Cannot assign \"{}\" (of type {}) to \"{}\" (of type {}) at {:?}",
                    expr.node, right_type, shape, left_type, statement.location
                ))?;
            }
        }
        Statement::Read(shape) => {
            // read takes scalar
            eval_shape_type(procedure_symbols, shape)?;
        }
        Statement::Write(expr) => {
            if let Expression::StringConst(_) = expr.node {
                // String constant
            } else {
                // Well-typed expression
                eval_expression_scalar(procedure_symbols, expr)?;
            }
        }
        Statement::Call(identifier, expressions) => {
            // - parameter must be type of corresponding formal parameter
            // - or int (parameter) to float (formal parameter)

            let Some(vars) = symbol_table.get(&identifier.node) else {
                return Err(format!(
                    "Cannot call proc {} at {:?} because it doesn't exist",
                    identifier.node, identifier.location
                ))?;
            };

            // List of all formal parameters
            let formal_params: Vec<&VariableInfo> = vars
                .iter()
                .filter(|v| v.variable_location == VariableLocation::FormalParameter)
                .collect();
            if formal_params.len() != expressions.len() {
                return Err(format!(
                    "Expected call to {} at {:?} to contain {} arguments, but found {}",
                    identifier.node,
                    identifier.location,
                    formal_params.len(),
                    expressions.len()
                ))?;
            }

            for (i, (formal_param, argument)) in formal_params.iter().zip(expressions.iter()).enumerate() {
                let argument_type = eval_expression_scalar(procedure_symbols, argument)?;

                let passing_indicator = *formal_param
                    .pass_indicator
                    .expect("formal parameter to have passing indicator");

                let can_pass_by_ref = matches!(argument.node, Expression::IdentifierShape(_));
                if passing_indicator == ParameterPassIndicator::Ref && !can_pass_by_ref {
                    return Err(format!(
                        "Expected argument {} \"{}\" to call at {:?} to be able to call by reference",
                        i + 1,
                        argument.node,
                        argument.location
                    ))?;
                }

                if formal_param.r#type != argument_type
                    && (formal_param.r#type != VariableType::Float
                        || argument_type != VariableType::Int
                        || passing_indicator != ParameterPassIndicator::Val)
                {
                    return Err(format!(
                        "Expected argument {} \"{}\" to call at {:?} to be of type {}, but found {}",
                        i + 1,
                        argument.node,
                        argument.location,
                        if passing_indicator == ParameterPassIndicator::Val
                            && formal_param.r#type == VariableType::Float
                        {
                            "Int or Float".to_owned()
                        } else {
                            formal_param.r#type.to_string()
                        },
                        argument_type
                    ))?;
                }
            }
        }
    }
    Ok(())
}

pub fn eval_expression_scalar(
    procedure_symbols: &ProcedureSymbols,
    expr: &Node<Expression>,
) -> Result<VariableType, Box<dyn Error>> {
    let r#type = match &expr.node {
        Expression::IntConst(_) => VariableType::Int,
        Expression::FloatConst(_) => VariableType::Float,
        Expression::BoolConst(_) => VariableType::Bool,
        Expression::StringConst(_) => Err("String const not supported by parse_expression_scalar")?,
        Expression::IdentifierShape(shape) => eval_shape_type(procedure_symbols, shape)?,
        Expression::UnopExpr(Unop::Minus, expr) => {
            let expr_type = eval_expression_scalar(procedure_symbols, expr)?;
            if expr_type != VariableType::Int && expr_type != VariableType::Float {
                return Err(format!(
                    "Expression after unary MINUS '{}' operator (at {:?}) must be int or float, but found \"{}\" ({})",
                    Unop::Minus,
                    expr.location,
                    expr.node,
                    expr_type
                ))?;
            }
            expr_type
        }
        Expression::UnopExpr(Unop::NOT, expr) => {
            let expr_type = eval_expression_scalar(procedure_symbols, expr)?;
            if expr_type != VariableType::Bool {
                return Err(format!(
                    "Expression after unary NOT operator '{}' (at {:?}) must be of type {}, but found \"{}\" ({})",
                    Unop::NOT,
                    expr.location,
                    VariableType::Bool,
                    expr.node,
                    expr_type
                ))?;
            }
            expr_type
        }
        Expression::BinopExpr(op, left, right) => {
            let left_type = eval_expression_scalar(procedure_symbols, left)?;
            let right_type = eval_expression_scalar(procedure_symbols, right)?;

            match *op {
                Binop::EQ | Binop::NEQ => {
                    if left_type != right_type {
                        return Err(format!(
                            "Operands of '{}' at {:?} must be of the same type, but found \"{}\" ({}) and \"{}\" ({})",
                            op, expr.location, left.node, left_type, right.node, right_type
                        ))?;
                    }
                    VariableType::Bool
                }
                Binop::AND | Binop::OR => {
                    if left_type != VariableType::Bool || right_type != VariableType::Bool {
                        return Err(format!(
                            "Operands of '{}' at {:?} must be type {}, but found \"{}\" ({}) and \"{}\" ({})",
                            op,
                            expr.location,
                            VariableType::Bool,
                            left.node,
                            left_type,
                            right.node,
                            right_type
                        ))?;
                    }
                    VariableType::Bool
                }
                Binop::GT | Binop::GTE | Binop::LT | Binop::LTE => {
                    if left_type == VariableType::Bool && right_type != VariableType::Bool
                        || left_type != VariableType::Bool && right_type == VariableType::Bool
                    {
                        return Err(format!(
                                "Operands of '{}' at {:?} must be same type or int/float, but found \"{}\" ({}) and \"{}\" ({})",
                                op,
                                expr.location,
                                left.node,
                                left_type,
                                right.node,
                                right_type
                        ))?;
                    }
                    VariableType::Bool
                }
                Binop::Add | Binop::Minus | Binop::Multiply | Binop::Divide => {
                    if left_type == VariableType::Bool || right_type == VariableType::Bool {
                        return Err(format!(
                            "Operands of '{}' at {:?} must be int/float, but found \"{}\" ({})",
                            op,
                            expr.location,
                            if left_type == VariableType::Bool {
                                &left.node
                            } else {
                                &right.node
                            },
                            if left_type == VariableType::Bool {
                                left_type
                            } else {
                                right_type
                            }
                        ))?;
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
    procedure_symbols: &ProcedureSymbols,
    shape: &IdentifierShape,
) -> Result<VariableType, Box<dyn Error>> {
    let r#type = match shape {
        IdentifierShape::Identifier(identifier) => {
            let Some(var) = procedure_symbols.iter().find(|v| v.identifier == &identifier.node) else {
                return Err(format!(
                    "Undeclared variable \"{}\" at {:?}",
                    identifier.node, identifier.location
                ))?;
            };

            // var := <expr>; where var is a scalar formal parameter or declaration
            if var.variable_location == VariableLocation::FormalParameter {
                // Scalar to scalar assignment
                var.r#type
            } else {
                let shape = var.shape.expect("variable declaration should have shape");
                if let IdentifierShapeDeclaration::Identifier(_) = *shape {
                    var.r#type
                } else {
                    return Err(format!(
                        "Expression \"{}\" at {:?} is expected to be scalar",
                        identifier.node, identifier.location
                    ))?;
                }
            }
        }
        IdentifierShape::IdentifierArray(identifier, expr) => {
            let index_type = eval_expression_scalar(procedure_symbols, expr)?;
            if index_type != VariableType::Int {
                return Err(format!(
                    "Array index \"{}\" at {:?} is expected to be int, but is of type {}",
                    expr.node, expr.location, index_type
                ))?;
            }

            let Some(var) = procedure_symbols.iter().find(|v| *v.identifier == *identifier.node) else {
                return Err(format!(
                    "Undeclared variable \"{}\" at {:?}",
                    identifier.node, identifier.location
                ))?;
            };

            // var[<expr>] := <expr>; where var is an array
            if var.variable_location == VariableLocation::FormalParameter {
                return Err(format!(
                    "Expected {} at {:?} to be an array, but {} is a scalar formal parameter",
                    identifier.node, identifier.location, identifier.node
                ))?;
            }

            let shape = var.shape.expect("variable declaration should have shape");
            if let IdentifierShapeDeclaration::IdentifierArray(_, _) = *shape {
                var.r#type
            } else {
                return Err(format!(
                    "Expected {} at {:?} to be an array variable, but it's not",
                    identifier.node, identifier.location
                ))?;
            }
        }
        IdentifierShape::IdentifierArray2D(identifier, expr1, expr2) => {
            let index_type_m = eval_expression_scalar(procedure_symbols, expr1)?;
            if index_type_m != VariableType::Int {
                return Err(format!(
                    "Matrix index \"{}[{}, _]\" at {:?} is expected to be int, but found {}",
                    identifier.node, expr1.node, expr1.location, index_type_m
                ))?;
            }
            let index_type_n = eval_expression_scalar(procedure_symbols, expr1)?;
            if eval_expression_scalar(procedure_symbols, expr2)? != VariableType::Int {
                return Err(format!(
                    "Matrix index \"{}[_, {}]\" at {:?} is expected to be int, but found {}",
                    identifier.node, expr2.node, expr2.location, index_type_n
                ))?;
            }

            let Some(var) = procedure_symbols.iter().find(|v| *v.identifier == identifier.node) else {
                return Err(format!(
                    "Undeclared variable \"{}\" at {:?}",
                    identifier.node, identifier.location
                ))?;
            };

            // var[<expr>, <expr>] := <expr>; where var is an array
            if var.variable_location == VariableLocation::FormalParameter {
                return Err(format!(
                    "Expected {} at {:?} to be a matrix, but {} is a scalar formal parameter",
                    identifier.node, identifier.location, identifier.node
                ))?;
            }

            let shape = var.shape.expect("variable declaration should have shape");
            if let IdentifierShapeDeclaration::IdentifierArray2D(_, _, _) = *shape {
                var.r#type
            } else {
                return Err(format!(
                    "Expected {} at {:?} to be a matrix variable, but it's not",
                    identifier.node, identifier.location
                ))?;
            }
        }
    };
    Ok(r#type)
}

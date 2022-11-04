use crate::ast::*;
use crate::tokens::{Keyword, Token, TokenInfo};
use std::error::Error;

pub fn parse(tokens: &Vec<TokenInfo>) -> Result<GoatProgram, Box<dyn Error>> {
    let mut index = 0;
    let mut procs = vec![];

    while index < tokens.len() {
        procs.push(parse_proc(tokens, &mut index)?);
    }

    Ok(GoatProgram { procedures: procs })
}

fn match_next(tokens: &Vec<TokenInfo>, token: Token, index: &mut usize) -> Result<TokenLocation, Box<dyn Error>> {
    if *index >= tokens.len() {
        return Err("No more input available")?;
    }
    #[allow(clippy::indexing_slicing)]
    let next_token = &tokens[*index];
    if next_token.0 != token {
        return Err(format!(
            "Expected token {:?}, but found {:?} at {:?}",
            token, next_token.0, next_token.1
        ))?;
    }
    *index += 1;
    Ok(next_token.1)
}

fn peek_next(tokens: &Vec<TokenInfo>, index: usize) -> Result<&TokenInfo, Box<dyn Error>> {
    if index >= tokens.len() {
        return Err("No more input available")?;
    }
    #[allow(clippy::indexing_slicing)]
    Ok(&tokens[index])
}

fn parse_proc(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Procedure, Box<dyn Error>> {
    // proc
    match_next(tokens, Token::Keyword(Keyword::PROC), index)?;

    // Header
    let (identifier, parameters) = parse_header(tokens, index)?;

    // Variable declarations
    let mut variable_declarations = vec![];
    while peek_next(tokens, *index)?.0 != Token::Keyword(Keyword::BEGIN) {
        variable_declarations.push(parse_variable_declaration(tokens, index)?);
    }

    // begin
    match_next(tokens, Token::Keyword(Keyword::BEGIN), index)?;
    // Body
    let body = parse_body(tokens, index)?;
    // end
    match_next(tokens, Token::Keyword(Keyword::END), index)?;

    Ok(Procedure {
        identifier,
        parameters,
        variable_declarations,
        body,
    })
}

fn parse_header(
    tokens: &Vec<TokenInfo>,
    index: &mut usize,
) -> Result<(AstNode<Identifier>, Vec<Parameter>), Box<dyn Error>> {
    // Identifier after proc
    let identifier = parse_identifier(tokens, index)?;

    // (
    match_next(tokens, Token::LPAREN, index)?;

    // 0 or more parameters
    let mut parameters = vec![];
    while peek_next(tokens, *index)?.0 != Token::RPAREN {
        // Parameter
        parameters.push(parse_parameter(tokens, index)?);

        // Comma
        if peek_next(tokens, *index)?.0 != Token::COMMA {
            break;
        }
        *index += 1;
    }

    // )
    match_next(tokens, Token::RPAREN, index)?;

    Ok((identifier, parameters))
}

fn parse_parameter(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Parameter, Box<dyn Error>> {
    let indicator_token = peek_next(tokens, *index)?;
    let indicator = match indicator_token.0 {
        Token::Keyword(Keyword::VAL) => ParameterPassIndicator::Val,
        Token::Keyword(Keyword::REF) => ParameterPassIndicator::Ref,
        _ => Err(format!(
            "Expected ref/val, but found {:?} at {:?}",
            indicator_token.0, indicator_token.1
        ))?,
    };
    *index += 1;

    let r#type = parse_type(tokens, index)?;

    let identifier = parse_identifier(tokens, index)?;

    Ok(Parameter {
        passing_indicator: indicator,
        r#type,
        identifier,
    })
}

fn parse_identifier(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Identifier>, Box<dyn Error>> {
    let ident_token = peek_next(tokens, *index)?;
    let ident = match &ident_token.0 {
        Token::Ident(t) => (t.clone(), ident_token.1),
        _ => Err(format!(
            "Expected identifier, but found {:?} at {:?}",
            ident_token.0, ident_token.1
        ))?,
    };
    *index += 1;
    Ok(AstNode {
        location: ident.1,
        node: ident.0,
    })
}

fn parse_type(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<VariableType, Box<dyn Error>> {
    let type_token = peek_next(tokens, *index)?;
    let r#type = match type_token.0 {
        Token::Keyword(Keyword::BOOL) => VariableType::Bool,
        Token::Keyword(Keyword::INT) => VariableType::Int,
        Token::Keyword(Keyword::FLOAT) => VariableType::Float,
        _ => Err(format!(
            "Expected type, but found {:?} at {:?}",
            type_token.0, type_token.1
        ))?,
    };
    *index += 1;
    Ok(r#type)
}

fn parse_variable_declaration(
    tokens: &Vec<TokenInfo>,
    index: &mut usize,
) -> Result<VariableDeclaration, Box<dyn Error>> {
    // Type
    let r#type = parse_type(tokens, index)?;

    // Identifier Shape
    let identifier_declaration = parse_identifier_shape_declaration(tokens, index)?;

    // ;
    match_next(tokens, Token::SEMI, index)?;

    Ok(VariableDeclaration {
        r#type,
        identifier_declaration,
    })
}

fn parse_identifier_shape_declaration(
    tokens: &Vec<TokenInfo>,
    index: &mut usize,
) -> Result<IdentifierShapeDeclaration, Box<dyn Error>> {
    // Identifier
    let identifier = parse_identifier(tokens, index)?;

    // No left bracket
    if peek_next(tokens, *index)?.0 != Token::LBRACKET {
        return Ok(IdentifierShapeDeclaration::Identifier(identifier));
    }
    // With left bracket
    *index += 1;

    let next_token = peek_next(tokens, *index)?;
    let left = match next_token.0 {
        Token::IntConst(left) => left,
        _ => Err(format!(
            "Expecting IntConst for shape m, but found {:?} at {:?}",
            next_token.0, next_token.1
        ))?,
    };
    // The int constant m
    *index += 1;

    if peek_next(tokens, *index)?.0 != Token::COMMA {
        // Right bracket
        match_next(tokens, Token::RBRACKET, index)?;

        return Ok(IdentifierShapeDeclaration::IdentifierArray(identifier, left));
    }
    // The comma
    *index += 1;

    let next_token = peek_next(tokens, *index)?;
    let right = match next_token.0 {
        Token::IntConst(left) => left,
        _ => Err(format!(
            "Expecting IntConst for shape n, but found {:?} at {:?}",
            next_token.0, next_token.1
        ))?,
    };
    // The int constant n
    *index += 1;

    // Right bracket
    match_next(tokens, Token::RBRACKET, index)?;

    Ok(IdentifierShapeDeclaration::IdentifierArray2D(identifier, left, right))
}

fn parse_body(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<ProcBody, Box<dyn Error>> {
    let statements = parse_statement_list(tokens, index)?;
    Ok(ProcBody { statements })
}

fn parse_statement_list(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Vec<AstNode<Statement>>, Box<dyn Error>> {
    let mut statements = vec![];
    while peek_next(tokens, *index)?.0 != Token::Keyword(Keyword::END)
        && peek_next(tokens, *index)?.0 != Token::Keyword(Keyword::FI)
        && peek_next(tokens, *index)?.0 != Token::Keyword(Keyword::ELSE)
        && peek_next(tokens, *index)?.0 != Token::Keyword(Keyword::OD)
    {
        statements.push(parse_statement(tokens, index)?);
    }
    Ok(statements)
}

fn parse_statement(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Statement>, Box<dyn Error>> {
    let next_token = peek_next(tokens, *index)?;

    let statement = match next_token.0 {
        Token::Ident(_) => {
            // <id> [ <expr>, <expr> ] := <expr>;
            let identifier_shape = parse_identifier_shape(tokens, index)?;
            let location = match_next(tokens, Token::ASSIGN, index)?;
            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::SEMI, index)?;
            AstNode {
                node: Statement::Assign(identifier_shape, expr),
                location,
            }
        }
        Token::Keyword(Keyword::READ) => {
            // read <id> [ <expr>, <expr> ];
            let location = match_next(tokens, Token::Keyword(Keyword::READ), index)?;
            let identifier_shape = parse_identifier_shape(tokens, index)?;
            match_next(tokens, Token::SEMI, index)?;
            AstNode {
                node: Statement::Read(identifier_shape),
                location,
            }
        }
        Token::Keyword(Keyword::WRITE) => {
            // write <expr>;
            let location = match_next(tokens, Token::Keyword(Keyword::WRITE), index)?;
            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::SEMI, index)?;
            AstNode {
                node: Statement::Write(expr),
                location,
            }
        }
        Token::Keyword(Keyword::CALL) => {
            // call
            let location = match_next(tokens, Token::Keyword(Keyword::CALL), index)?;
            // <id>
            let identifier = parse_identifier(tokens, index)?;
            // (
            match_next(tokens, Token::LPAREN, index)?;

            // <expr-list>
            // 0 or more parameters
            let mut params = vec![];
            while peek_next(tokens, *index)?.0 != Token::RPAREN {
                // Parameter
                params.push(parse_expression(tokens, index)?);

                // Comma
                if peek_next(tokens, *index)?.0 != Token::COMMA {
                    break;
                }
                *index += 1;
            }

            // );
            match_next(tokens, Token::RPAREN, index)?;
            match_next(tokens, Token::SEMI, index)?;

            AstNode {
                node: Statement::Call(identifier, params),
                location,
            }
        }
        Token::Keyword(Keyword::WHILE) => {
            // while <expr> do <stmt-list> od
            let location = match_next(tokens, Token::Keyword(Keyword::WHILE), index)?;

            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::Keyword(Keyword::DO), index)?;
            let statement_list = parse_statement_list(tokens, index)?;
            match_next(tokens, Token::Keyword(Keyword::OD), index)?;

            AstNode {
                node: Statement::While(expr, statement_list),
                location,
            }
        }
        Token::Keyword(Keyword::IF) => {
            // if <expr> then <stmt-list> fi
            let location = match_next(tokens, Token::Keyword(Keyword::IF), index)?;

            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::Keyword(Keyword::THEN), index)?;
            let stmt_list = parse_statement_list(tokens, index)?;

            let next_token = peek_next(tokens, *index)?;
            if next_token.0 == Token::Keyword(Keyword::ELSE) {
                // if <expr> then <stmt-list> else <stmt-list> fi
                match_next(tokens, Token::Keyword(Keyword::ELSE), index)?;
                let else_stmt_list = parse_statement_list(tokens, index)?;
                match_next(tokens, Token::Keyword(Keyword::FI), index)?;
                AstNode {
                    node: Statement::IfElse(expr, stmt_list, else_stmt_list),
                    location,
                }
            } else {
                // No else statement
                match_next(tokens, Token::Keyword(Keyword::FI), index)?;
                AstNode {
                    node: Statement::If(expr, stmt_list),
                    location,
                }
            }
        }
        _ => Err(format!(
            "Expected statement, but found {:?} at {:?}",
            next_token.0, next_token.1
        ))?,
    };

    Ok(statement)
}

fn parse_expression(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Expression>, Box<dyn Error>> {
    parse_expression_or(tokens, index)
}

fn parse_expression_or(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Expression>, Box<dyn Error>> {
    let mut current = parse_expression_and(tokens, index)?;

    while peek_next(tokens, *index)?.0 == Token::OR {
        let token_location = match_next(tokens, Token::OR, index)?;
        let right = parse_expression_and(tokens, index)?;
        current = AstNode {
            node: Expression::BinopExpr(Binop::OR, Box::new(current), Box::new(right)),
            location: token_location,
        };
    }

    Ok(current)
}

fn parse_expression_and(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Expression>, Box<dyn Error>> {
    let mut current = parse_expression_not(tokens, index)?;

    while peek_next(tokens, *index)?.0 == Token::ADD {
        let token_location = match_next(tokens, Token::AND, index)?;
        let right = parse_expression_not(tokens, index)?;
        current = AstNode {
            node: Expression::BinopExpr(Binop::AND, Box::new(current), Box::new(right)),
            location: token_location,
        };
    }

    Ok(current)
}

fn parse_expression_not(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Expression>, Box<dyn Error>> {
    if peek_next(tokens, *index)?.0 == Token::NOT {
        let token_location = match_next(tokens, Token::NOT, index)?;
        let right = parse_expression_not(tokens, index)?;
        Ok(AstNode {
            node: Expression::UnopExpr(Unop::NOT, Box::new(right)),
            location: token_location,
        })
    } else {
        Ok(parse_expression_comparison(tokens, index)?)
    }
}

fn parse_expression_comparison(
    tokens: &Vec<TokenInfo>,
    index: &mut usize,
) -> Result<AstNode<Expression>, Box<dyn Error>> {
    let mut current = parse_expression_add_sub(tokens, index)?;

    loop {
        let next_token = peek_next(tokens, *index)?;
        if next_token.0 == Token::GT {
            let token_location = match_next(tokens, Token::GT, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::GT, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        if next_token.0 == Token::GTE {
            let token_location = match_next(tokens, Token::GTE, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::GTE, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        if next_token.0 == Token::LT {
            let token_location = match_next(tokens, Token::LT, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::LT, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        if next_token.0 == Token::LTE {
            let token_location = match_next(tokens, Token::LTE, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::LTE, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        if next_token.0 == Token::EQ {
            let token_location = match_next(tokens, Token::EQ, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::EQ, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        if next_token.0 == Token::NE {
            let token_location = match_next(tokens, Token::NE, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::NEQ, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        break;
    }

    Ok(current)
}

fn parse_expression_add_sub(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Expression>, Box<dyn Error>> {
    let mut current = parse_expression_mul_div(tokens, index)?;

    loop {
        let next_token = peek_next(tokens, *index)?;
        if next_token.0 == Token::ADD {
            let token_location = match_next(tokens, Token::ADD, index)?;
            let right = parse_expression_mul_div(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::Add, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        if next_token.0 == Token::SUB {
            let token_location = match_next(tokens, Token::SUB, index)?;
            let right = parse_expression_mul_div(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::Minus, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        break;
    }

    Ok(current)
}

fn parse_expression_mul_div(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<AstNode<Expression>, Box<dyn Error>> {
    let mut current = parse_expression_unary_minus(tokens, index)?;

    loop {
        let next_token = peek_next(tokens, *index)?;
        if next_token.0 == Token::MUL {
            let token_location = match_next(tokens, Token::MUL, index)?;
            let right = parse_expression_unary_minus(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::Multiply, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        if next_token.0 == Token::DIV {
            let token_location = match_next(tokens, Token::DIV, index)?;
            let right = parse_expression_unary_minus(tokens, index)?;
            current = AstNode {
                node: Expression::BinopExpr(Binop::Divide, Box::new(current), Box::new(right)),
                location: token_location,
            };
            continue;
        }
        break;
    }

    Ok(current)
}

fn parse_expression_unary_minus(
    tokens: &Vec<TokenInfo>,
    index: &mut usize,
) -> Result<AstNode<Expression>, Box<dyn Error>> {
    if peek_next(tokens, *index)?.0 == Token::SUB {
        let token_location = match_next(tokens, Token::SUB, index)?;
        let right = parse_expression_unary_minus(tokens, index)?;
        Ok(AstNode {
            node: Expression::UnopExpr(Unop::Minus, Box::new(right)),
            location: token_location,
        })
    } else {
        Ok(parse_expression_terminal(tokens, index)?)
    }
}

fn parse_expression_terminal(
    tokens: &Vec<TokenInfo>,
    index: &mut usize,
) -> Result<AstNode<Expression>, Box<dyn Error>> {
    let next_token = peek_next(tokens, *index)?;

    // Brackets
    if next_token.0 == Token::LPAREN {
        match_next(tokens, Token::LPAREN, index)?;
        let expr = parse_expression(tokens, index)?;
        match_next(tokens, Token::RPAREN, index)?;
        return Ok(expr);
    }

    let expr = match &next_token.0 {
        Token::Ident(_) => Expression::IdentifierShape(parse_identifier_shape(tokens, index)?),
        Token::IntConst(n) => {
            *index += 1;
            Expression::IntConst(*n)
        }
        Token::Keyword(Keyword::TRUE) => {
            *index += 1;
            Expression::BoolConst(true)
        }
        Token::Keyword(Keyword::FALSE) => {
            *index += 1;
            Expression::BoolConst(false)
        }
        Token::BoolConst(b) => {
            *index += 1;
            Expression::BoolConst(*b)
        }
        Token::FloatConst(n) => {
            *index += 1;
            Expression::FloatConst(n.into())
        }
        Token::StringConst(n) => {
            *index += 1;
            Expression::StringConst(n.into())
        }
        _ => Err(format!(
            "Expected expression, but found {:?} at {:?}",
            next_token.0, next_token.1
        ))?,
    };

    Ok(AstNode {
        location: next_token.1,
        node: expr,
    })
}

fn parse_identifier_shape(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<IdentifierShape, Box<dyn Error>> {
    // Identifier
    let identifier = parse_identifier(tokens, index)?;

    // No left bracket
    if peek_next(tokens, *index)?.0 != Token::LBRACKET {
        return Ok(IdentifierShape::Identifier(identifier));
    }
    // With left bracket
    *index += 1;

    let expr_left = parse_expression(tokens, index)?;

    if peek_next(tokens, *index)?.0 != Token::COMMA {
        // Right bracket
        match_next(tokens, Token::RBRACKET, index)?;

        return Ok(IdentifierShape::IdentifierArray(identifier, Box::new(expr_left)));
    }
    // The comma
    *index += 1;

    let expr_right = parse_expression(tokens, index)?;

    // Right bracket
    match_next(tokens, Token::RBRACKET, index)?;

    Ok(IdentifierShape::IdentifierArray2D(
        identifier,
        Box::new(expr_left),
        Box::new(expr_right),
    ))
}

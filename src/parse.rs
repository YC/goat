use crate::ast::*;
use crate::types::{Keyword, Token, TokenInfo};
use std::error::Error;

pub fn parse(tokens: &Vec<TokenInfo>) -> Result<GoatProgram, Box<dyn Error>> {
    let mut index = 0;
    let mut procs = vec![];

    while index < tokens.len() {
        procs.push(parse_proc(tokens, &mut index)?);
    }

    Ok(GoatProgram { procedure: procs })
}

fn match_next(tokens: &Vec<TokenInfo>, token: Token, index: &mut usize) -> Result<(), Box<dyn Error>> {
    if *index >= tokens.len() {
        Err("No more input available")?
    }
    if tokens[*index].0 != token {
        Err(format!(
            "Expected token {:?}, but found {:?} at {:?}",
            token, tokens[*index].0, tokens[*index].1
        ))?
    }
    *index += 1;
    Ok(())
}

fn get_next(tokens: &Vec<TokenInfo>, index: usize) -> Result<&TokenInfo, Box<dyn Error>> {
    if index >= tokens.len() {
        Err("No more input available")?
    }
    Ok(&tokens[index])
}

fn parse_proc(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Procedure, Box<dyn Error>> {
    // proc
    match_next(tokens, Token::Keyword(Keyword::PROC), index)?;

    // Header
    let (identifier, parameters) = parse_header(tokens, index)?;

    // Variable declarations
    let mut variable_declarations = vec![];
    while get_next(tokens, *index)?.0 != Token::Keyword(Keyword::BEGIN) {
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

fn parse_header(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<(String, Vec<Parameter>), Box<dyn Error>> {
    // Identifier after proc
    let identifier = parse_identifier(tokens, index)?;

    // (
    match_next(tokens, Token::LPAREN, index)?;

    // 0 or more parameters
    let mut parameters = vec![];
    while get_next(tokens, *index)?.0 != Token::RPAREN {
        // Parameter
        parameters.push(parse_parameter(tokens, index)?);

        // Comma
        if get_next(tokens, *index)?.0 != Token::COMMA {
            break;
        } else {
            *index += 1;
        }
    }

    // )
    match_next(tokens, Token::RPAREN, index)?;

    Ok((identifier, parameters))
}

fn parse_parameter(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Parameter, Box<dyn Error>> {
    let indicator_token = get_next(tokens, *index)?;
    let indicator = match indicator_token.0 {
        Token::Keyword(Keyword::VAL) => ParameterPassIndicator::Val,
        Token::Keyword(Keyword::REF) => ParameterPassIndicator::Ref,
        _ => Err(format!(
            "Expected ref/val at {:?}, but found {:?}",
            indicator_token.1, indicator_token.0
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

fn parse_identifier(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Identifier, Box<dyn Error>> {
    let ident_token = get_next(tokens, *index)?;
    let ident: String = match &ident_token.0 {
        Token::Ident(t) => t.clone(),
        _ => Err(format!(
            "Expected identifier at {:?}, but found {:?}",
            ident_token.1, ident_token.0
        ))?,
    };
    *index += 1;
    Ok(ident)
}

fn parse_type(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<ParameterType, Box<dyn Error>> {
    let type_token = get_next(tokens, *index)?;
    let r#type = match type_token.0 {
        Token::Keyword(Keyword::BOOL) => ParameterType::Bool,
        Token::Keyword(Keyword::INT) => ParameterType::Int,
        Token::Keyword(Keyword::FLOAT) => ParameterType::Float,
        _ => Err(format!(
            "Expected type at {:?}, but found {:?}",
            type_token.1, type_token.0
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
    if get_next(tokens, *index)?.0 != Token::LBRACKET {
        return Ok(IdentifierShapeDeclaration::Identifier(identifier));
    }
    // With left bracket
    *index += 1;

    let next_token = get_next(tokens, *index)?;
    let left = match next_token.0 {
        Token::IntConst(left) => left,
        _ => Err(format!(
            "Expecting IntConst for shape m at {:?}, but found {:?}",
            next_token.1, next_token.0
        ))?,
    };
    // The int constant m
    *index += 1;

    if get_next(tokens, *index)?.0 != Token::COMMA {
        // Right bracket
        match_next(tokens, Token::RBRACKET, index)?;

        return Ok(IdentifierShapeDeclaration::IdentifierArray(identifier, left));
    }
    // The comma
    *index += 1;

    let next_token = get_next(tokens, *index)?;
    let right = match next_token.0 {
        Token::IntConst(left) => left,
        _ => Err(format!(
            "Expecting IntConst for shape n at {:?}, but found {:?}",
            next_token.1, next_token.0
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

fn parse_statement_list(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Vec<Statement>, Box<dyn Error>> {
    let mut statements = vec![];
    while get_next(tokens, *index)?.0 != Token::Keyword(Keyword::END)
        && get_next(tokens, *index)?.0 != Token::Keyword(Keyword::FI)
        && get_next(tokens, *index)?.0 != Token::Keyword(Keyword::ELSE)
        && get_next(tokens, *index)?.0 != Token::Keyword(Keyword::OD)
    {
        statements.push(parse_statement(tokens, index)?)
    }
    Ok(statements)
}

fn parse_statement(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Statement, Box<dyn Error>> {
    let next_token = get_next(tokens, *index)?;

    let statement = match next_token.0 {
        Token::Ident(_) => {
            // <id> [ <expr>, <expr> ] := <expr>;
            let identifier_shape = parse_identifier_shape(tokens, index)?;
            match_next(tokens, Token::ASSIGN, index)?;
            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::SEMI, index)?;
            Statement::Assign(identifier_shape, expr)
        }
        Token::Keyword(Keyword::READ) => {
            // read <id> [ <expr>, <expr> ];
            match_next(tokens, Token::Keyword(Keyword::READ), index)?;
            let identifier_shape = parse_identifier_shape(tokens, index)?;
            match_next(tokens, Token::SEMI, index)?;
            Statement::Read(identifier_shape)
        }
        Token::Keyword(Keyword::WRITE) => {
            // write <expr>;
            match_next(tokens, Token::Keyword(Keyword::WRITE), index)?;
            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::SEMI, index)?;
            Statement::Write(expr)
        }
        Token::Keyword(Keyword::CALL) => {
            // call
            match_next(tokens, Token::Keyword(Keyword::CALL), index)?;

            // <id>
            let identifier = parse_identifier(tokens, index)?;

            // (
            match_next(tokens, Token::LPAREN, index)?;

            // <expr-list>
            let mut params = vec![];
            while get_next(tokens, *index)?.0 != Token::RPAREN {
                params.push(parse_expression(tokens, index)?);
            }

            // );
            match_next(tokens, Token::RPAREN, index)?;
            match_next(tokens, Token::SEMI, index)?;

            Statement::Call(identifier, params)
        }
        Token::Keyword(Keyword::WHILE) => {
            // while <expr> do <stmt-list> od
            match_next(tokens, Token::Keyword(Keyword::WHILE), index)?;
            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::Keyword(Keyword::DO), index)?;
            let statement_list = parse_statement_list(tokens, index)?;
            match_next(tokens, Token::Keyword(Keyword::OD), index)?;
            Statement::While(expr, statement_list)
        }
        Token::Keyword(Keyword::IF) => {
            // if <expr> then <stmt-list> fi
            match_next(tokens, Token::Keyword(Keyword::IF), index)?;
            let expr = parse_expression(tokens, index)?;
            match_next(tokens, Token::Keyword(Keyword::THEN), index)?;
            let stmt_list = parse_statement_list(tokens, index)?;

            let next_token = get_next(tokens, *index)?;
            if next_token.0 != Token::Keyword(Keyword::ELSE) {
                match_next(tokens, Token::Keyword(Keyword::FI), index)?;
                Statement::If(expr, stmt_list)
            } else {
                // if <expr> then <stmt-list> else <stmt-list> fi
                match_next(tokens, Token::Keyword(Keyword::ELSE), index)?;
                let else_stmt_list = parse_statement_list(tokens, index)?;
                match_next(tokens, Token::Keyword(Keyword::FI), index)?;
                Statement::IfElse(expr, stmt_list, else_stmt_list)
            }
        }
        _ => Err(format!(
            "Expected statement at {:?}, but found {:?}",
            next_token.1, next_token.0
        ))?,
    };

    Ok(statement)
}

fn parse_expression(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    parse_expression_or(tokens, index)
}

fn parse_expression_or(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    let mut current = parse_expression_and(tokens, index)?;

    while let Token::OR = get_next(tokens, *index)?.0 {
        match_next(tokens, Token::OR, index)?;
        let right = parse_expression_and(tokens, index)?;
        current = Expression::BinopExpr(Binop::OR, Box::new(current), Box::new(right));
    }

    Ok(current)
}

fn parse_expression_and(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    let mut current = parse_expression_not(tokens, index)?;

    while let Token::AND = get_next(tokens, *index)?.0 {
        match_next(tokens, Token::AND, index)?;
        let right = parse_expression_not(tokens, index)?;
        current = Expression::BinopExpr(Binop::AND, Box::new(current), Box::new(right));
    }

    Ok(current)
}

fn parse_expression_not(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    if let Token::NOT = get_next(tokens, *index)?.0 {
        match_next(tokens, Token::NOT, index)?;
        let right = parse_expression_not(tokens, index)?;
        Ok(Expression::UnopExpr(Unop::NOT, Box::new(right)))
    } else {
        Ok(parse_expression_comparison(tokens, index)?)
    }
}

fn parse_expression_comparison(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    let mut current = parse_expression_add_sub(tokens, index)?;

    loop {
        let next_token = get_next(tokens, *index)?;
        if let Token::GT = next_token.0 {
            match_next(tokens, Token::GT, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = Expression::BinopExpr(Binop::GT, Box::new(current), Box::new(right));
            continue;
        }
        if let Token::GTE = next_token.0 {
            match_next(tokens, Token::GTE, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = Expression::BinopExpr(Binop::GTE, Box::new(current), Box::new(right));
            continue;
        }
        if let Token::LT = next_token.0 {
            match_next(tokens, Token::LT, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = Expression::BinopExpr(Binop::LT, Box::new(current), Box::new(right));
            continue;
        }
        if let Token::LTE = next_token.0 {
            match_next(tokens, Token::LTE, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = Expression::BinopExpr(Binop::LTE, Box::new(current), Box::new(right));
            continue;
        }
        if let Token::EQ = next_token.0 {
            match_next(tokens, Token::EQ, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = Expression::BinopExpr(Binop::EQ, Box::new(current), Box::new(right));
            continue;
        }
        if let Token::NE = next_token.0 {
            match_next(tokens, Token::NE, index)?;
            let right = parse_expression_add_sub(tokens, index)?;
            current = Expression::BinopExpr(Binop::NEQ, Box::new(current), Box::new(right));
            continue;
        }
        break;
    }

    Ok(current)
}

fn parse_expression_add_sub(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    let mut current = parse_expression_mul_div(tokens, index)?;

    loop {
        let next_token = get_next(tokens, *index)?;
        if let Token::ADD = next_token.0 {
            match_next(tokens, Token::ADD, index)?;
            let right = parse_expression_mul_div(tokens, index)?;
            current = Expression::BinopExpr(Binop::Add, Box::new(current), Box::new(right));
            continue;
        }
        if let Token::SUB = next_token.0 {
            match_next(tokens, Token::SUB, index)?;
            let right = parse_expression_mul_div(tokens, index)?;
            current = Expression::BinopExpr(Binop::Minus, Box::new(current), Box::new(right));
            continue;
        }
        break;
    }

    Ok(current)
}

fn parse_expression_mul_div(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    let mut current = parse_expression_unary_minus(tokens, index)?;

    loop {
        let next_token = get_next(tokens, *index)?;
        if let Token::MUL = next_token.0 {
            match_next(tokens, Token::MUL, index)?;
            let right = parse_expression_unary_minus(tokens, index)?;
            current = Expression::BinopExpr(Binop::Multiply, Box::new(current), Box::new(right));
            continue;
        }
        if let Token::DIV = next_token.0 {
            match_next(tokens, Token::DIV, index)?;
            let right = parse_expression_unary_minus(tokens, index)?;
            current = Expression::BinopExpr(Binop::Divide, Box::new(current), Box::new(right));
            continue;
        }
        break;
    }

    Ok(current)
}

fn parse_expression_unary_minus(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    if let Token::SUB = get_next(tokens, *index)?.0 {
        match_next(tokens, Token::SUB, index)?;
        let right = parse_expression_unary_minus(tokens, index)?;
        Ok(Expression::UnopExpr(Unop::Minus, Box::new(right)))
    } else {
        Ok(parse_expression_terminal(tokens, index)?)
    }
}

fn parse_expression_terminal(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Expression, Box<dyn Error>> {
    let next_token = get_next(tokens, *index)?;

    // Brackets
    if let Token::LPAREN = next_token.0 {
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
        _ => Err(format!("... {:?}", next_token))?,
    };

    Ok(expr)
}

fn parse_identifier_shape(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<IdentifierShape, Box<dyn Error>> {
    // Identifier
    let identifier = parse_identifier(tokens, index)?;

    // No left bracket
    if get_next(tokens, *index)?.0 != Token::LBRACKET {
        return Ok(IdentifierShape::Identifier(identifier));
    }
    // With left bracket
    *index += 1;

    let expr_left = parse_expression(tokens, index)?;

    if get_next(tokens, *index)?.0 != Token::COMMA {
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

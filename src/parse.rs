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
        Err(format!("Expected token {:?}, but found {:?}", token, tokens[*index]))?
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

    // Body
    let body = parse_body(tokens, index)?;

    // TODO: remove
    for i in *index..tokens.len() {
        if tokens[i].0 == Token::Keyword(Keyword::END) {
            *index = i;
            break;
        }
    }

    // end
    match_next(tokens, Token::Keyword(Keyword::END), index)?;

    Ok(Procedure {
        identifier,
        parameters,
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
        _ => Err(format!("Expected ref/val, but found {:?}", indicator_token))?,
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
        _ => Err(format!("Expected identifier, found {:?}", ident_token))?,
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
        _ => Err(format!("Expected type, but found {:?}", type_token))?,
    };
    *index += 1;
    Ok(r#type)
}

fn parse_body(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<ProcBody, Box<dyn Error>> {
    let mut variable_declarations = vec![];

    while get_next(tokens, *index)?.0 != Token::Keyword(Keyword::BEGIN) {
        variable_declarations.push(parse_variable_declaration(tokens, index)?);
    }

    Ok(ProcBody {
        variable_declarations,
        statements: vec![],
    })
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
            "Expecting IntConst for shape m at TODO, but found {:?}",
            next_token
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
            "Expecting IntConst for shape n at TODO, but found {:?}",
            next_token
        ))?,
    };
    // The int constant n
    *index += 1;

    // Right bracket
    match_next(tokens, Token::RBRACKET, index)?;

    Ok(IdentifierShapeDeclaration::IdentifierArray2D(identifier, left, right))
}

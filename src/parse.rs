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

fn match_next(tokens: &Vec<TokenInfo>, token: Token, index: usize) -> Result<(), Box<dyn Error>> {
    if index >= tokens.len() {
        Err("No more input available")?
    }
    if tokens[index].0 != token {
        Err(format!("Expected token {:?}, but found {:?}", token, tokens[index]))?
    }
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
    match_next(tokens, Token::Keyword(Keyword::PROC), *index)?;
    *index += 1;

    let (ident, parameters) = parse_header(tokens, index)?;

    // TODO: remove
    for i in *index..tokens.len() {
        if tokens[i].0 == Token::Keyword(Keyword::END) {
            *index = i;
            break;
        }
    }

    // end
    match_next(tokens, Token::Keyword(Keyword::END), *index)?;
    *index += 1;

    Ok(Procedure {
        identifier: ident,
        parameters,
        body: ProcBody {
            variable_declarations: vec![],
            statements: vec![],
        },
    })
}

fn parse_header(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<(String, Vec<Parameter>), Box<dyn Error>> {
    // Identifier after proc
    let ident = parse_identifier(tokens, index)?;

    // (
    match_next(tokens, Token::LPAREN, *index)?;
    *index += 1;

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
    match_next(tokens, Token::RPAREN, *index)?;
    *index += 1;

    Ok((ident, parameters))
}

fn parse_parameter(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Parameter, Box<dyn Error>> {
    let indicator_token = get_next(tokens, *index)?;
    let indicator = match indicator_token.0 {
        Token::Keyword(Keyword::VAL) => ParameterPassIndicator::Val,
        Token::Keyword(Keyword::REF) => ParameterPassIndicator::Ref,
        _ => Err(format!("Expected ref/val, but found {:?}", indicator_token))?,
    };
    *index += 1;

    let type_token = get_next(tokens, *index)?;
    let r#type = match type_token.0 {
        Token::Keyword(Keyword::BOOL) => ParameterType::Bool,
        Token::Keyword(Keyword::INT) => ParameterType::Int,
        Token::Keyword(Keyword::FLOAT) => ParameterType::Float,
        _ => Err(format!("Expected type, but found {:?}", type_token))?,
    };
    *index += 1;

    let ident = parse_identifier(tokens, index)?;

    Ok(Parameter {
        passing_indicator: indicator,
        r#type,
        identifier: ident,
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

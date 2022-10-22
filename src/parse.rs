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
    let ident_token = get_next(tokens, *index)?;
    let ident: String = match &ident_token.0 {
        Token::Ident(t) => t.clone(),
        _ => Err(format!("Expected identifier after proc, found {:?}", ident_token))?,
    };
    *index += 1;

    Ok((ident, vec![]))
}

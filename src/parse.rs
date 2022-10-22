use crate::ast::*;
use crate::types::{TokenInfo, Token, Keyword};
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
        Err(format!("Expected token, but found {:?}", tokens[index]))?
    }
    Ok(())
}

fn parse_proc(tokens: &Vec<TokenInfo>, index: &mut usize) -> Result<Procedure, Box<dyn Error>> {
    // proc
    match_next(tokens, Token::Keyword(Keyword::PROC), *index)?;
    *index += 1;

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

    Ok(Procedure { identifier: "test".to_string(), parameters: vec![], body: ProcBody { variable_declarations: vec![], statements: vec![] } })
}

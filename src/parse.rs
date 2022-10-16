use crate::ast::*;
use crate::types::TokenInfo;
use std::error::Error;

pub fn parse(tokens: Vec<TokenInfo>) -> Result<GoatProgram, Box<dyn Error>> {
    Ok(GoatProgram { procedure: vec![] })
}

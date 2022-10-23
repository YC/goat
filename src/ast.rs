#![allow(dead_code)]

use std::fmt::{Display, Formatter, Result};

/// Program consists of 1 or more procedure definitions
#[derive(Debug)]
pub struct GoatProgram {
    pub procedure: Vec<Procedure>,
}

impl Display for GoatProgram {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = self
            .procedure
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join("\n\n");
        write!(f, "{}", s)
    }
}

pub type Identifier = String;

/// Procedure definition consists of "proc", header, "begin", body, "end"
#[derive(Debug)]
pub struct Procedure {
    /// Procedure's name
    pub identifier: Identifier,
    /// Header has identifier, parameters
    pub parameters: Vec<Parameter>,
    /// Variable declarations
    pub variable_declarations: Vec<VariableDeclaration>,
    /// Procedure's body
    pub body: ProcBody,
}

impl Display for Procedure {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "proc {} ({})\n{}{}begin\n???\nend",
            self.identifier,
            self.parameters
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<String>>()
                .join(", "),
            self.variable_declarations
                .iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join("\n"),
            if self.variable_declarations.is_empty() {
                ""
            } else {
                "\n"
            },
        )
    }
}

/// Parameter has passing indicator, type, identifier
#[derive(Debug)]
pub struct Parameter {
    pub passing_indicator: ParameterPassIndicator,
    pub r#type: ParameterType,
    pub identifier: Identifier,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} {} {}", self.passing_indicator, self.r#type, self.identifier)
    }
}

#[derive(Debug)]
pub enum ParameterPassIndicator {
    Val,
    Ref,
}

impl Display for ParameterPassIndicator {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            ParameterPassIndicator::Ref => "ref",
            ParameterPassIndicator::Val => "val",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub enum ParameterType {
    Bool,
    Float,
    Int,
}

impl Display for ParameterType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            ParameterType::Bool => "bool",
            ParameterType::Float => "float",
            ParameterType::Int => "int",
        };
        write!(f, "{}", s)
    }
}

/// Procedure body consists of 0 or more variable declarations, then statements
#[derive(Debug)]
pub struct ProcBody {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub r#type: ParameterType,
    pub identifier_declaration: IdentifierShapeDeclaration,
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "    {} {};", self.r#type, self.identifier_declaration)
    }
}

#[derive(Debug)]
pub enum IdentifierShapeDeclaration {
    Identifier(Identifier),
    IdentifierArray(Identifier, u128),
    IdentifierArray2D(Identifier, u128, u128),
}

impl Display for IdentifierShapeDeclaration {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Self::Identifier(ident) => ident.clone(),
            Self::IdentifierArray(ident, m) => format!("{}[{}]", ident, m),
            Self::IdentifierArray2D(ident, m, n) => format!("{}[{},{}]", ident, m, n),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub enum Statement {
    Assign(IdentifierShape, Expression),
    Read(IdentifierShape),
    Write(Expression),
    Call(Identifier, Vec<Expression>),

    If(Expression, Vec<Statement>),
    IfElse(Expression, Vec<Statement>, Vec<Statement>),
    While(Expression, Vec<Statement>),
}

#[derive(Debug)]
pub enum Expression {
    /// identifier with shape
    IdentifierShape(IdentifierShape),

    /// Non-empty sequence of digits
    IntConst(u128),
    /// One or more digits, decimal point, one or more digits
    FloatConst(String),
    /// false or true
    BoolConst(bool),
    /// Sequence of characters between double quotes,
    /// cannot contain double quotes or newline/tab,
    /// can contain \n
    StringConst(String),

    BinopExpr(Binop, Box<Expression>, Box<Expression>),
    UnopExpr(Unop, Box<Expression>),
}

#[derive(Debug)]
pub enum IdentifierShape {
    Identifier(Identifier),
    IdentifierArray(Identifier, Box<Expression>),
    IdentifierArray2D(Identifier, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum Binop {
    Add,
    Minus,
    Multiply,
    Divide,

    OR,
    AND,

    EQ,
    NEQ,

    LT,
    LTE,
    GT,
    GTE,
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum Unop {
    NOT,
    Negative,
}

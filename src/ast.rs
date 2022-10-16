#![allow(dead_code)]

/// Program consists of 1 or more procedure definitions
#[derive(Debug)]
pub struct GoatProgram {
    pub procedure: Vec<Procedure>,
}

pub type Identifier = String;

/// Procedure definition consists of "proc", header, "begin", body, "end"
#[derive(Debug)]
pub struct Procedure {
    /// Procedure's name
    pub identifier: Identifier,
    /// Header has identifier, parameters
    pub parameters: Vec<Parameter>,
    /// Procedure's body
    pub body: ProcBody,
}

/// Parameter has passing indicator, type, identifier
#[derive(Debug)]
pub struct Parameter {
    pub passing_indicator: ParameterPassIndicator,
    pub r#type: ParameterType,
    pub identifier: Identifier,
}

#[derive(Debug)]
pub enum ParameterPassIndicator {
    Val,
    Ref,
}

#[derive(Debug)]
pub enum ParameterType {
    Bool,
    Float,
    Int,
}

/// Procedure body consists of 0 or more variable declarations, then statements
#[derive(Debug)]
pub struct ProcBody {
    pub variable_declarations: Vec<VariableDeclaration>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub r#type: ParameterType,
    pub identifier: Identifier,

    // [n] or [m, n]
    pub shape: (u64, u64),
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
pub enum Unop {
    NOT,
    Negative,
}

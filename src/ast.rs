#![allow(dead_code)]

use std::fmt::{Display, Formatter, Result};

/// Program consists of 1 or more procedure definitions
#[derive(Debug)]
pub struct GoatProgram {
    pub procedures: Vec<Procedure>,
}

impl Display for GoatProgram {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = self
            .procedures
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join("\n\n");
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct AstNode<N> {
    pub location: TokenLocation,
    pub node: N,
}

pub type Identifier = String;

pub type TokenLocation = (u64, u64);

/// Procedure definition consists of "proc", header, "begin", body, "end"
#[derive(Debug)]
pub struct Procedure {
    /// Procedure's name
    pub identifier: AstNode<Identifier>,
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
            "proc {} ({})\n{}{}begin\n{}\nend",
            self.identifier.node,
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
            self.body
        )
    }
}

/// Parameter has passing indicator, type, identifier
#[derive(Debug)]
pub struct Parameter {
    pub passing_indicator: ParameterPassIndicator,
    pub r#type: VariableType,
    pub identifier: AstNode<Identifier>,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} {} {}", self.passing_indicator, self.r#type, self.identifier.node)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum VariableType {
    Bool,
    Float,
    Int,
}

impl Display for VariableType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            VariableType::Bool => "bool",
            VariableType::Float => "float",
            VariableType::Int => "int",
        };
        write!(f, "{}", s)
    }
}

/// Procedure body consists of 0 or more variable declarations, then statements
#[derive(Debug)]
pub struct ProcBody {
    pub statements: Vec<AstNode<Statement>>,
}

impl Display for ProcBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = self
            .statements
            .iter()
            .map(|s| format!("{}", s.node))
            .collect::<Vec<String>>()
            .join("\n");
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub r#type: VariableType,
    pub identifier_declaration: IdentifierShapeDeclaration,
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "    {} {};", self.r#type, self.identifier_declaration)
    }
}

#[derive(Debug, Clone)]
pub enum IdentifierShapeDeclaration {
    Identifier(AstNode<Identifier>),
    IdentifierArray(AstNode<Identifier>, u128),
    IdentifierArray2D(AstNode<Identifier>, u128, u128),
}

impl Display for IdentifierShapeDeclaration {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Self::Identifier(ident) => ident.node.clone(),
            Self::IdentifierArray(ident, m) => format!("{}[{}]", ident.node, m),
            Self::IdentifierArray2D(ident, m, n) => format!("{}[{}, {}]", ident.node, m, n),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub enum Statement {
    Assign(IdentifierShape, AstNode<Expression>),
    Read(IdentifierShape),
    Write(AstNode<Expression>),
    Call(AstNode<Identifier>, Vec<AstNode<Expression>>),

    If(AstNode<Expression>, Vec<AstNode<Statement>>),
    IfElse(AstNode<Expression>, Vec<AstNode<Statement>>, Vec<AstNode<Statement>>),
    While(AstNode<Expression>, Vec<AstNode<Statement>>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", pad_space(self.pretty_print(), 1))
    }
}

fn pad_space(s: String, level: u64) -> String {
    let repeat = " ".repeat(level as usize * 4);
    s.split('\n')
        .map(|s| repeat.clone() + s)
        .collect::<Vec<String>>()
        .join("\n")
}

impl Statement {
    fn pretty_print(&self) -> String {
        match self {
            Statement::Assign(ident, expr) => {
                format!("{} := {};", ident, expr.node)
            }
            Statement::Read(ident) => {
                format!("read {};", ident)
            }
            Statement::Write(expr) => {
                format!("write {};", expr.node)
            }
            Statement::Call(ident, expr_list) => {
                format!(
                    "call {}({});",
                    ident.node,
                    expr_list
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Statement::While(expr, stmt_list) => {
                format!(
                    "while {} do\n{}{}od",
                    expr.node,
                    stmt_list
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if !stmt_list.is_empty() { "\n" } else { "" },
                )
            }
            Statement::If(expr, stmt_list) => {
                format!(
                    "if {} then\n{}{}fi",
                    expr.node,
                    stmt_list
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if !stmt_list.is_empty() { "\n" } else { "" },
                )
            }
            Statement::IfElse(expr, stmt_if, stmt_else) => {
                format!(
                    "if {} then\n{}{}else\n{}{}fi",
                    expr.node,
                    stmt_if
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if !stmt_if.is_empty() { "\n" } else { "" },
                    stmt_else
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if !stmt_else.is_empty() { "\n" } else { "" },
                )
            }
        }
    }
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

    BinopExpr(Binop, Box<AstNode<Expression>>, Box<AstNode<Expression>>),
    UnopExpr(Unop, Box<AstNode<Expression>>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Self::IdentifierShape(s) => s.to_string(),
            Self::IntConst(n) => n.to_string(),
            Self::FloatConst(n) => n.to_string(),
            Self::BoolConst(true) => "true".to_string(),
            Self::BoolConst(false) => "false".to_string(),
            Self::StringConst(s) => format!("\"{}\"", s),
            Self::BinopExpr(op, expr_left, expr_right) => {
                format!(
                    "{} {} {}",
                    wrap_bracket(expr_left.node.is_binop(), expr_left.node.to_string()),
                    op,
                    wrap_bracket(expr_right.node.is_binop(), expr_right.node.to_string())
                )
            }
            Self::UnopExpr(op, expr) => {
                format!("{}{}", op, wrap_bracket(expr.node.is_binop(), expr.node.to_string()))
            }
        };
        write!(f, "{}", s)
    }
}

fn wrap_bracket(wrap: bool, input: String) -> String {
    if wrap {
        format!("({})", input)
    } else {
        input
    }
}

impl Expression {
    fn is_binop(&self) -> bool {
        matches!(self, Expression::BinopExpr(_, _, _))
    }
}

#[derive(Debug)]
pub enum IdentifierShape {
    Identifier(AstNode<Identifier>),
    IdentifierArray(AstNode<Identifier>, Box<AstNode<Expression>>),
    IdentifierArray2D(AstNode<Identifier>, Box<AstNode<Expression>>, Box<AstNode<Expression>>),
}

impl Display for IdentifierShape {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Self::Identifier(ident) => ident.node.clone(),
            Self::IdentifierArray(ident, m) => format!("{}[{}]", ident.node, m.node),
            Self::IdentifierArray2D(ident, m, n) => format!("{}[{}, {}]", ident.node, m.node, n.node),
        };
        write!(f, "{}", s)
    }
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

impl Display for Binop {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Binop::Add => "+",
            Binop::Minus => "-",
            Binop::Multiply => "*",
            Binop::Divide => "/",

            Binop::OR => "||",
            Binop::AND => "&&",
            Binop::EQ => "=",
            Binop::NEQ => "!=",

            Binop::LT => "<",
            Binop::LTE => "<=",
            Binop::GT => ">",
            Binop::GTE => ">=",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum Unop {
    NOT,
    Minus,
}

impl Display for Unop {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Unop::NOT => "!",
            Unop::Minus => "-",
        };
        write!(f, "{}", s)
    }
}

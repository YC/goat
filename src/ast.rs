use core::fmt::{Display, Formatter, Result};

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

#[derive(Debug, Copy, Clone)]
pub struct Node<N> {
    pub location: TokenLocation,
    pub node: N,
}

pub type Identifier = String;

pub type TokenLocation = (u64, u64);

/// Procedure definition consists of "proc", header, "begin", body, "end"
#[derive(Debug)]
pub struct Procedure {
    /// Procedure's name
    pub identifier: Node<Identifier>,
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
    pub identifier: Node<Identifier>,
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
        let s = match *self {
            Self::Ref => "ref",
            Self::Val => "val",
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
        let s = match *self {
            Self::Bool => "bool",
            Self::Float => "float",
            Self::Int => "int",
        };
        write!(f, "{}", s)
    }
}

/// Procedure body consists of 0 or more variable declarations, then statements
#[derive(Debug)]
pub struct ProcBody {
    pub statements: Vec<Node<Statement>>,
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

#[derive(Debug)]
pub enum IdentifierShapeDeclaration {
    Identifier(Node<Identifier>),
    IdentifierArray(Node<Identifier>, u32),
    IdentifierArray2D(Node<Identifier>, u32, u32),
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
    Assign(IdentifierShape, Node<Expression>),
    Read(IdentifierShape),
    Write(Node<Expression>),
    Call(Node<Identifier>, Vec<Node<Expression>>),

    If(Node<Expression>, Vec<Node<Statement>>),
    IfElse(Node<Expression>, Vec<Node<Statement>>, Vec<Node<Statement>>),
    While(Node<Expression>, Vec<Node<Statement>>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", pad_space(&self.pretty_print(), 1))
    }
}

pub fn pad_space(s: &str, level: usize) -> String {
    let repeat = " ".repeat(level * 4);
    #[allow(clippy::string_add)]
    s.split('\n')
        .map(|s| repeat.clone() + s)
        .collect::<Vec<String>>()
        .join("\n")
}

impl Statement {
    fn pretty_print(&self) -> String {
        match self {
            Self::Assign(ident, expr) => {
                format!("{} := {};", ident, expr.node)
            }
            Self::Read(ident) => {
                format!("read {};", ident)
            }
            Self::Write(expr) => {
                format!("write {};", expr.node)
            }
            Self::Call(ident, expr_list) => {
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
            Self::While(expr, stmt_list) => {
                format!(
                    "while {} do\n{}{}od",
                    expr.node,
                    stmt_list
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if stmt_list.is_empty() { "" } else { "\n" },
                )
            }
            Self::If(expr, stmt_list) => {
                format!(
                    "if {} then\n{}{}fi",
                    expr.node,
                    stmt_list
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if stmt_list.is_empty() { "" } else { "\n" },
                )
            }
            Self::IfElse(expr, stmt_if, stmt_else) => {
                format!(
                    "if {} then\n{}{}else\n{}{}fi",
                    expr.node,
                    stmt_if
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if stmt_if.is_empty() { "" } else { "\n" },
                    stmt_else
                        .iter()
                        .map(|s| format!("{}", s.node))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    if stmt_else.is_empty() { "" } else { "\n" },
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
    IntConst(i32),
    /// One or more digits, decimal point, one or more digits
    FloatConst(String),
    /// false or true
    BoolConst(bool),
    /// Sequence of characters between double quotes,
    /// cannot contain double quotes or newline/tab,
    /// can contain \n
    StringConst(String),

    BinopExpr(Binop, Box<Node<Expression>>, Box<Node<Expression>>),
    UnopExpr(Unop, Box<Node<Expression>>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Self::IdentifierShape(s) => s.to_string(),
            Self::IntConst(n) => n.to_string(),
            Self::FloatConst(n) => n.to_string(),
            Self::BoolConst(true) => "true".to_owned(),
            Self::BoolConst(false) => "false".to_owned(),
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
    const fn is_binop(&self) -> bool {
        matches!(*self, Self::BinopExpr(_, _, _))
    }
}

#[derive(Debug)]
pub enum IdentifierShape {
    Identifier(Node<Identifier>),
    IdentifierArray(Node<Identifier>, Box<Node<Expression>>),
    IdentifierArray2D(Node<Identifier>, Box<Node<Expression>>, Box<Node<Expression>>),
}

impl IdentifierShapeDeclaration {
    pub fn to_shape_expression<'a>(&'a self) -> IdentifierShape {
        match self {
            IdentifierShapeDeclaration::Identifier(ident) => IdentifierShape::Identifier(ident.clone()),
            IdentifierShapeDeclaration::IdentifierArray(ident, m) => IdentifierShape::IdentifierArray(
                ident.clone(),
                Box::new(Node {
                    location: (0, 0),
                    node: Expression::IntConst(i32::try_from(*m).unwrap()),
                }),
            ),
            IdentifierShapeDeclaration::IdentifierArray2D(ident, m, n) => IdentifierShape::IdentifierArray2D(
                ident.clone(),
                Box::new(Node {
                    location: (0, 0),
                    node: Expression::IntConst(i32::try_from(*m).unwrap()),
                }),
                Box::new(Node {
                    location: (0, 0),
                    node: Expression::IntConst(i32::try_from(*n).unwrap()),
                }),
            ),
        }
    }
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
        let s = match *self {
            Self::Add => "+",
            Self::Minus => "-",
            Self::Multiply => "*",
            Self::Divide => "/",

            Self::OR => "||",
            Self::AND => "&&",
            Self::EQ => "=",
            Self::NEQ => "!=",

            Self::LT => "<",
            Self::LTE => "<=",
            Self::GT => ">",
            Self::GTE => ">=",
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
        let s = match *self {
            Self::NOT => "!",
            Self::Minus => "-",
        };
        write!(f, "{}", s)
    }
}

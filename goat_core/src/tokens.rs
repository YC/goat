use core::fmt::{Display, Formatter, Result};

/// Token with line number, column number
pub type TokenInfo = (Token, (u64, u64));

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(clippy::upper_case_acronyms, clippy::missing_docs_in_private_items)]
/// Token of the Goat language
pub enum Token {
    /// Reserved tokens
    Keyword(Keyword),

    /// Whitespace
    Whitespace(String),
    /// Newline
    NewLine,
    /// Comment
    Comment(String),

    /// Non-empty sequence of alphanumeric, underscore and apostrophe
    /// Must start with upper or lowercase character
    Ident(String),
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

    ASSIGN,
    LPAREN,
    RPAREN,
    SEMI,
    OR,
    AND,
    NOT,
    EQ,
    NE,

    LT,
    LTE,
    GT,
    GTE,

    ADD,
    SUB,
    MUL,
    DIV,

    LBRACKET,
    RBRACKET,
    COMMA,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Self::NewLine => "\n".to_owned(),
            Self::Comment(i) | Self::Whitespace(i) | Self::Ident(i) | Self::StringConst(i) | Self::FloatConst(i) => {
                i.to_string()
            }
            Self::Keyword(k) => k.to_string(),
            Self::IntConst(i) => i.to_string(),
            Self::BoolConst(i) => i.to_string(),
            Self::ASSIGN => ":=".to_owned(),
            Self::LPAREN => "(".to_owned(),
            Self::RPAREN => ")".to_owned(),
            Self::SEMI => ";".to_owned(),
            Self::EQ => "=".to_owned(),
            Self::NE => "!=".to_owned(),
            Self::NOT => "!".to_owned(),
            Self::OR => "||".to_owned(),
            Self::AND => "&&".to_owned(),
            Self::LT => "<".to_owned(),
            Self::LTE => "<=".to_owned(),
            Self::GT => ">".to_owned(),
            Self::GTE => ">=".to_owned(),
            Self::ADD => "+".to_owned(),
            Self::SUB => "-".to_owned(),
            Self::MUL => "*".to_owned(),
            Self::DIV => "/".to_owned(),
            Self::LBRACKET => "[".to_owned(),
            Self::RBRACKET => "]".to_owned(),
            Self::COMMA => ",".to_owned(),
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[allow(clippy::upper_case_acronyms, clippy::missing_docs_in_private_items)]
/// Reserved keyword of the Goat language
pub enum Keyword {
    BEGIN,
    BOOL,
    CALL,
    DO,
    ELSE,
    END,
    FALSE,
    FI,
    FLOAT,
    IF,
    INT,
    OD,
    PROC,
    REF,
    THEN,
    TRUE,
    READ,
    VAL,
    WHILE,
    WRITE,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match *self {
            Self::READ => "read",
            Self::WRITE => "write",
            Self::END => "end",
            Self::BEGIN => "begin",
            Self::PROC => "proc",
            Self::INT => "int",
            Self::BOOL => "bool",
            Self::FALSE => "false",
            Self::TRUE => "true",
            Self::DO => "do",
            Self::OD => "od",
            Self::IF => "if",
            Self::FI => "fi",
            Self::ELSE => "else",
            Self::THEN => "then",
            Self::VAL => "val",
            Self::WHILE => "while",
            Self::FLOAT => "float",
            Self::CALL => "call",
            Self::REF => "ref",
        };
        write!(f, "{s}")
    }
}

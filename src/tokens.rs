use std::fmt::{Display, Formatter, Result};

/// Token with line number, column number
pub type TokenInfo = (Token, (u64, u64));

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
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
    IntConst(u128),
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

impl Token {
    fn as_str(&self) -> String {
        match self {
            Token::NewLine => "\n".to_string(),
            Token::Comment(i) => i.to_string(),
            Token::Whitespace(i) => i.to_string(),
            Token::Ident(i) => i.to_string(),
            Token::StringConst(i) => i.to_string(),
            Token::Keyword(k) => k.to_string(),
            Token::IntConst(i) => i.to_string(),
            Token::BoolConst(i) => i.to_string(),
            Token::FloatConst(i) => i.to_string(),
            Token::ASSIGN => ":=".to_string(),
            Token::LPAREN => "(".to_string(),
            Token::RPAREN => ")".to_string(),
            Token::SEMI => ";".to_string(),
            Token::EQ => "=".to_string(),
            Token::NE => "!=".to_string(),
            Token::NOT => "!".to_string(),
            Token::OR => "||".to_string(),
            Token::AND => "&&".to_string(),
            Token::LT => "<".to_string(),
            Token::LTE => "<=".to_string(),
            Token::GT => ">".to_string(),
            Token::GTE => ">=".to_string(),
            Token::ADD => "+".to_string(),
            Token::SUB => "-".to_string(),
            Token::MUL => "*".to_string(),
            Token::DIV => "/".to_string(),
            Token::LBRACKET => "[".to_string(),
            Token::RBRACKET => "]".to_string(),
            Token::COMMA => ",".to_string(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
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
        write!(f, "{}", self.as_str())
    }
}

impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::READ => "read",
            Keyword::WRITE => "write",
            Keyword::END => "end",
            Keyword::BEGIN => "begin",
            Keyword::PROC => "proc",
            Keyword::INT => "int",
            Keyword::BOOL => "bool",
            Keyword::FALSE => "false",
            Keyword::TRUE => "true",
            Keyword::DO => "do",
            Keyword::OD => "od",
            Keyword::IF => "if",
            Keyword::FI => "fi",
            Keyword::ELSE => "else",
            Keyword::THEN => "then",
            Keyword::VAL => "val",
            Keyword::WHILE => "while",
            Keyword::FLOAT => "float",
            Keyword::CALL => "call",
            Keyword::REF => "ref",
        }
    }
}

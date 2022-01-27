use std::fmt::{Display, Formatter, Result};

enum Token {
    Ident(String),
    Keyword(Keyword),
    IntConst(u128),
    FloatConst(f64),
    BoolConst(bool),
    Literal(String),

    ASSIGN,
    LPAREN,
    RPAREN,
    SEMI,
    OR,
    AND,
    NEG,
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
    COMMA
}

// impl Token {
//     fn as_str(&self) -> &str {
//         match self {
//             Token::Ident(i) => i,
//             Token::Literal(i) => i,
//             Token::Keyword(k) => k.as_str(),
//             Token::INT_CONST(i) => i.to_string().as_str(),
//             Token::BOOL_CONST(i) => i.to_string().as_str(),
//             Token::FLOAT_CONST(i) => i.to_string().as_str(),
//             Token::ASSIGN => ":=",
//             Token::LPAREN => "(",
//             Token::RPAREN => ")",
//             Token::SEMI => ";",
//             Token::EQ => "=",
//             Token::NE => "!=",
//             Token::NEG => "!",
//             Token::OR => "||",
//             Token::AND => "&&",
//             Token::LT => "<",
//             Token::LTE => "<=",
//             Token::GT => ">",
//             Token::GTE => ">=",
//             Token::ADD => "+",
//             Token::SUB => "-",
//             Token::MUL => "*",
//             Token::DIV => "/",
//             Token::LBRACKET => "[",
//             Token::RBRACKET => "]",
//             Token::COMMA => ",",
//         }
//     }
// }

#[allow(dead_code)]
enum Keyword {
    READ,
    WRITE,
    END,
    BEGIN,
    PROC,
    INT,
    BOOL,
    FALSE,
    TRUE,
    DO,
    OD,
    ELSE,
    IF,
    FI,
    REF,
    THEN,
    VAL,
    WHILE,
    FLOAT,
    CALL
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.as_str())
    }
}

impl Keyword {
    fn as_str(&self) -> &'static str {
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

use crate::types::{Keyword, Token};

#[derive(Debug)]
#[allow(dead_code)]
pub enum RegEx {
    Empty,
    Charset(Charset),
    Concat(Vec<Box<RegEx>>),
    Or(Vec<Box<RegEx>>),
    Literal(String),
    Star(Box<RegEx>),
}

#[derive(Debug)]
pub enum Charset {
    Char(char),
    Chars(bool, Vec<char>),
    CharRange(char, char),
}

pub fn construct_regex() {
    let mut regex: Vec<(RegEx, &dyn Fn(&str) -> Token)> = vec![];

    regex.push((Keyword::BEGIN.as_regex(), &|_| Token::Keyword(Keyword::BEGIN)));
    regex.push((Keyword::BOOL.as_regex(), &|_| Token::Keyword(Keyword::BOOL)));
    regex.push((Keyword::CALL.as_regex(), &|_| Token::Keyword(Keyword::CALL)));
    regex.push((Keyword::DO.as_regex(), &|_| Token::Keyword(Keyword::DO)));
    regex.push((Keyword::ELSE.as_regex(), &|_| Token::Keyword(Keyword::ELSE)));
    regex.push((Keyword::END.as_regex(), &|_| Token::Keyword(Keyword::END)));
    regex.push((Keyword::FALSE.as_regex(), &|_| Token::Keyword(Keyword::FALSE)));
    regex.push((Keyword::FI.as_regex(), &|_| Token::Keyword(Keyword::FI)));
    regex.push((Keyword::FLOAT.as_regex(), &|_| Token::Keyword(Keyword::FLOAT)));
    regex.push((Keyword::IF.as_regex(), &|_| Token::Keyword(Keyword::IF)));
    regex.push((Keyword::INT.as_regex(), &|_| Token::Keyword(Keyword::INT)));
    regex.push((Keyword::OD.as_regex(), &|_| Token::Keyword(Keyword::OD)));
    regex.push((Keyword::PROC.as_regex(), &|_| Token::Keyword(Keyword::PROC)));
    regex.push((Keyword::REF.as_regex(), &|_| Token::Keyword(Keyword::REF)));
    regex.push((Keyword::THEN.as_regex(), &|_| Token::Keyword(Keyword::THEN)));
    regex.push((Keyword::TRUE.as_regex(), &|_| Token::Keyword(Keyword::TRUE)));
    regex.push((Keyword::READ.as_regex(), &|_| Token::Keyword(Keyword::READ)));
    regex.push((Keyword::VAL.as_regex(), &|_| Token::Keyword(Keyword::VAL)));
    regex.push((Keyword::WHILE.as_regex(), &|_| Token::Keyword(Keyword::WHILE)));
    regex.push((Keyword::WRITE.as_regex(), &|_| Token::Keyword(Keyword::WRITE)));

    regex.push((
        RegEx::Concat(vec![
            Box::new(RegEx::Or(vec![
                Box::new(RegEx::Charset(Charset::CharRange('a', 'z'))),
                Box::new(RegEx::Charset(Charset::CharRange('A', 'Z'))),
            ])),
            Box::new(RegEx::Star(Box::new(RegEx::Or(vec![
                Box::new(RegEx::Charset(Charset::CharRange('0', '9'))),
                Box::new(RegEx::Charset(Charset::CharRange('a', 'z'))),
                Box::new(RegEx::Charset(Charset::CharRange('A', 'Z'))),
                Box::new(RegEx::Charset(Charset::Char('_'))),
                Box::new(RegEx::Charset(Charset::Char('\''))),
            ])))),
        ]),
        &|s| Token::Ident(s.to_string()),
    ));

    regex.push((
        RegEx::Concat(vec![
            Box::new(RegEx::Charset(Charset::CharRange('0', '9'))),
            Box::new(RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9'))))),
        ]),
        &|s| Token::IntConst(s.parse::<u128>().unwrap()),
    ));

    regex.push((
        RegEx::Concat(vec![
            Box::new(RegEx::Charset(Charset::CharRange('0', '9'))),
            Box::new(RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9'))))),
            Box::new(RegEx::Charset(Charset::Char('.'))),
            Box::new(RegEx::Charset(Charset::CharRange('0', '9'))),
            Box::new(RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9'))))),
        ]),
        &|s| Token::FloatConst(s.parse::<f64>().unwrap()),
    ));

    regex.push((
        RegEx::Concat(vec![
            Box::new(RegEx::Charset(Charset::Char('"'))),
            Box::new(RegEx::Charset(Charset::Chars(false, vec!['"', '\n', '\t']))),
            Box::new(RegEx::Charset(Charset::Char('"'))),
        ]),
        &|s| Token::StringConst(s.to_string()),
    ));

    regex.push((
        RegEx::Or(vec![
            Box::new(RegEx::Literal("true".to_string())),
            Box::new(RegEx::Literal("false".to_string())),
        ]),
        &|s| {
            if s == "true" {
                Token::BoolConst(true)
            } else {
                Token::BoolConst(false)
            }
        },
    ));

    regex.push((RegEx::Literal("=".to_string()), &|_| Token::ASSIGN));
    regex.push((RegEx::Literal("(".to_string()), &|_| Token::LPAREN));
    regex.push((RegEx::Literal(")".to_string()), &|_| Token::RPAREN));
    regex.push((RegEx::Literal(";".to_string()), &|_| Token::SEMI));
    regex.push((RegEx::Literal("||".to_string()), &|_| Token::OR));
    regex.push((RegEx::Literal("&&".to_string()), &|_| Token::AND));
    regex.push((RegEx::Literal("!".to_string()), &|_| Token::NEG));
    regex.push((RegEx::Literal("==".to_string()), &|_| Token::EQ));
    regex.push((RegEx::Literal("!=".to_string()), &|_| Token::NE));

    regex.push((RegEx::Literal("<".to_string()), &|_| Token::LT));
    regex.push((RegEx::Literal("<=".to_string()), &|_| Token::LTE));
    regex.push((RegEx::Literal(">".to_string()), &|_| Token::GT));
    regex.push((RegEx::Literal(">=".to_string()), &|_| Token::GTE));

    regex.push((RegEx::Literal("+".to_string()), &|_| Token::ADD));
    regex.push((RegEx::Literal("-".to_string()), &|_| Token::SUB));
    regex.push((RegEx::Literal("*".to_string()), &|_| Token::MUL));
    regex.push((RegEx::Literal("/".to_string()), &|_| Token::DIV));

    regex.push((RegEx::Literal("(".to_string()), &|_| Token::LBRACKET));
    regex.push((RegEx::Literal(")".to_string()), &|_| Token::RBRACKET));
    regex.push((RegEx::Literal(",".to_string()), &|_| Token::COMMA));

    println!("{:?}", regex.iter().map(|x| &x.0).collect::<Vec<&RegEx>>());
}

impl Keyword {
    pub fn as_regex(&self) -> RegEx {
        RegEx::Literal(self.to_string())
    }
}

use crate::types::{Keyword, Token};

#[derive(Debug)]
pub enum RegEx {
    Charset(Charset),
    Concat(Vec<Box<RegEx>>),
    Or(Vec<Box<RegEx>>),
    Literal(String),
    Star(Box<RegEx>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Charset {
    Char(char),
    Chars(bool, Vec<char>),
    CharRange(char, char),
}

pub struct NFA {
    start: usize,
    accept: (usize, Option<Box<TokenFunction>>),
    transitions: Vec<(usize, usize, NFATransition)>,
}

impl std::fmt::Debug for NFA {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "NFA {{ start: {}, accept: {}, transition: {:?} }}",
            self.start, self.accept.0, self.transitions
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub enum NFATransition {
    Charset(Charset),
    Empty,
}

type TokenFunction = dyn Fn(&str) -> Token;

pub fn regexes_to_nfa(regexes: Vec<(RegEx, Box<TokenFunction>)>) -> Vec<NFA> {
    let nfas: Vec<NFA> = vec![];

    for regex in regexes {
        let _ = regex_to_nfa(&regex.0, Some(regex.1));
    }

    nfas
}

pub fn regex_to_nfa(regex: &RegEx, f: Option<Box<TokenFunction>>) -> NFA {
    match regex {
        RegEx::Charset(c) => {
            // (start) -> char (accept)
            let mut transition = Vec::new();
            transition.push((0, 1, NFATransition::Charset(c.clone())));

            NFA {
                start: 0,
                accept: (1, f),
                transitions: transition,
            }
        }
        RegEx::Concat(v) => {
            // Map regex to nfas
            let nfas: Vec<NFA> = v.iter().map(|b| regex_to_nfa(b, None)).collect();

            let mut transitions = vec![];
            let mut accept = (0, f);

            // The number of the last accept state
            let mut last_next_usable = 0;

            // (start) -> nfa 1 -> nfa 2 -> nfa 3 (accept)
            // accept state of previous nfa becomes start state of next nfa
            for nfa in nfas {
                // The maximum state number
                let max_state = std::cmp::max(
                    nfa.start,
                    nfa.transitions.iter().map(|x| x.1).max().unwrap_or(nfa.start),
                );

                // If the max state number is the accept state, it can be used by the next nfa
                // If not, need to increment by 1
                let next_usable = if max_state != nfa.accept.0 {
                    max_state + 1
                } else {
                    max_state
                };

                // Then, all states are shifted by last_next_usable to prevent numbering clashes
                for transition in nfa.transitions {
                    // Rewrite reference to (start) to previous accept state
                    let new_source = if transition.0 == nfa.start {
                        accept.0
                    } else {
                        transition.0 + last_next_usable
                    };
                    let new_dest = if transition.1 == nfa.start {
                        accept.0
                    } else {
                        transition.1 + last_next_usable
                    };
                    transitions.push((new_source, new_dest, transition.2));
                }
                accept.0 = nfa.accept.0 + last_next_usable;

                last_next_usable += next_usable;
            }

            NFA {
                start: 0,
                accept,
                transitions,
            }
        }
        RegEx::Or(_) => {
            // Map regex to nfas
            // let nfas: Vec<NFA> = v.iter().map(|b| regex_to_nfa(b, None)).collect();

            // let state_offset = 1;
            // let accept_state: usize = nfas.iter().map(|n| n.accept.0).sum::<usize>() + 2;

            // for nfa in nfas {

            // }

            // NFA {
            //     start: 0,
            //     accept: (0, f),
            //     transition: vec![],
            // }
            panic!("not impl");
        }
        RegEx::Star(_) => {
            // NFA {
            //     start: 0,
            //     accept: (0, f),
            //     transition: vec![],
            // }
            panic!("not impl");
        }
        RegEx::Literal(s) => {
            let mut transition = Vec::new();

            // (start) -> 't' -> 'o' -> 'k' (accept)
            for (i, c) in s.chars().enumerate() {
                transition.push((i, i + 1, NFATransition::Charset(Charset::Char(c))));
            }

            NFA {
                start: 0,
                accept: (s.chars().count(), f),
                transitions: transition,
            }
        }
    }
}

#[test]
fn test_regex_to_nfa_literal() {
    let regex = RegEx::Literal("hi".to_string());
    let nfa = regex_to_nfa(&regex, None);
    // (start) --h-> 1 --i-> (2, accept)
    assert_eq!(0, nfa.start);
    assert_eq!(2, nfa.accept.0);

    assert_eq!(2, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NFATransition::Charset(Charset::Char('h'))));
    assert_eq!(nfa.transitions[1], (1, 2, NFATransition::Charset(Charset::Char('i'))));
}

#[test]
fn test_regex_to_nfa_concat_literal() {
    let regex1 = RegEx::Literal("ab".to_string());
    let regex2 = RegEx::Literal("cd".to_string());
    let regex = RegEx::Concat(vec![Box::new(regex1), Box::new(regex2)]);
    let nfa = regex_to_nfa(&regex, None);

    // (start) --a-> 1 --b-> 2 --c-> 3 --d--> (4, accept)
    assert_eq!(0, nfa.start);
    assert_eq!(4, nfa.accept.0);

    assert_eq!(4, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NFATransition::Charset(Charset::Char('a'))));
    assert_eq!(nfa.transitions[1], (1, 2, NFATransition::Charset(Charset::Char('b'))));
    assert_eq!(nfa.transitions[2], (2, 3, NFATransition::Charset(Charset::Char('c'))));
    assert_eq!(nfa.transitions[3], (3, 4, NFATransition::Charset(Charset::Char('d'))));
}

#[test]
fn test_regex_to_nfa_concat_literal_more() {
    let regex1 = RegEx::Literal("ab".to_string());
    let regex2 = RegEx::Literal("cd".to_string());
    let regex3 = RegEx::Literal("e".to_string());
    let regex = RegEx::Concat(vec![Box::new(regex1), Box::new(regex2), Box::new(regex3)]);
    let nfa = regex_to_nfa(&regex, None);

    // (start) --a-> 1 --b-> 2 --c-> 3 --d--> 4 --e--> (5, accept)
    assert_eq!(0, nfa.start);
    assert_eq!(5, nfa.accept.0);

    assert_eq!(5, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NFATransition::Charset(Charset::Char('a'))));
    assert_eq!(nfa.transitions[1], (1, 2, NFATransition::Charset(Charset::Char('b'))));
    assert_eq!(nfa.transitions[2], (2, 3, NFATransition::Charset(Charset::Char('c'))));
    assert_eq!(nfa.transitions[3], (3, 4, NFATransition::Charset(Charset::Char('d'))));
    assert_eq!(nfa.transitions[4], (4, 5, NFATransition::Charset(Charset::Char('e'))));
}

pub fn construct_regex() -> Vec<(RegEx, Box<TokenFunction>)> {
    let mut regex: Vec<(RegEx, Box<TokenFunction>)> = vec![];

    regex.push((Keyword::BEGIN.as_regex(), Box::new(|_| Token::Keyword(Keyword::BEGIN))));
    regex.push((Keyword::BOOL.as_regex(), Box::new(|_| Token::Keyword(Keyword::BOOL))));
    regex.push((Keyword::CALL.as_regex(), Box::new(|_| Token::Keyword(Keyword::CALL))));
    regex.push((Keyword::DO.as_regex(), Box::new(|_| Token::Keyword(Keyword::DO))));
    regex.push((Keyword::ELSE.as_regex(), Box::new(|_| Token::Keyword(Keyword::ELSE))));
    regex.push((Keyword::END.as_regex(), Box::new(|_| Token::Keyword(Keyword::END))));
    regex.push((Keyword::FALSE.as_regex(), Box::new(|_| Token::Keyword(Keyword::FALSE))));
    regex.push((Keyword::FI.as_regex(), Box::new(|_| Token::Keyword(Keyword::FI))));
    regex.push((Keyword::FLOAT.as_regex(), Box::new(|_| Token::Keyword(Keyword::FLOAT))));
    regex.push((Keyword::IF.as_regex(), Box::new(|_| Token::Keyword(Keyword::IF))));
    regex.push((Keyword::INT.as_regex(), Box::new(|_| Token::Keyword(Keyword::INT))));
    regex.push((Keyword::OD.as_regex(), Box::new(|_| Token::Keyword(Keyword::OD))));
    regex.push((Keyword::PROC.as_regex(), Box::new(|_| Token::Keyword(Keyword::PROC))));
    regex.push((Keyword::REF.as_regex(), Box::new(|_| Token::Keyword(Keyword::REF))));
    regex.push((Keyword::THEN.as_regex(), Box::new(|_| Token::Keyword(Keyword::THEN))));
    regex.push((Keyword::TRUE.as_regex(), Box::new(|_| Token::Keyword(Keyword::TRUE))));
    regex.push((Keyword::READ.as_regex(), Box::new(|_| Token::Keyword(Keyword::READ))));
    regex.push((Keyword::VAL.as_regex(), Box::new(|_| Token::Keyword(Keyword::VAL))));
    regex.push((Keyword::WHILE.as_regex(), Box::new(|_| Token::Keyword(Keyword::WHILE))));
    regex.push((Keyword::WRITE.as_regex(), Box::new(|_| Token::Keyword(Keyword::WRITE))));

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
        Box::new(|s| Token::Ident(s.to_string())),
    ));

    regex.push((
        RegEx::Concat(vec![
            Box::new(RegEx::Charset(Charset::CharRange('0', '9'))),
            Box::new(RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9'))))),
        ]),
        Box::new(|s| Token::IntConst(s.parse::<u128>().unwrap())),
    ));

    regex.push((
        RegEx::Concat(vec![
            Box::new(RegEx::Charset(Charset::CharRange('0', '9'))),
            Box::new(RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9'))))),
            Box::new(RegEx::Charset(Charset::Char('.'))),
            Box::new(RegEx::Charset(Charset::CharRange('0', '9'))),
            Box::new(RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9'))))),
        ]),
        Box::new(|s| Token::FloatConst(s.parse::<f64>().unwrap())),
    ));

    regex.push((
        RegEx::Concat(vec![
            Box::new(RegEx::Charset(Charset::Char('"'))),
            Box::new(RegEx::Charset(Charset::Chars(false, vec!['"', '\n', '\t']))),
            Box::new(RegEx::Charset(Charset::Char('"'))),
        ]),
        Box::new(|s| Token::StringConst(s.to_string())),
    ));

    regex.push((
        RegEx::Or(vec![
            Box::new(RegEx::Literal("true".to_string())),
            Box::new(RegEx::Literal("false".to_string())),
        ]),
        Box::new(|s| {
            if s == "true" {
                Token::BoolConst(true)
            } else {
                Token::BoolConst(false)
            }
        }),
    ));

    regex.push((RegEx::Literal("=".to_string()), Box::new(|_| Token::ASSIGN)));
    regex.push((RegEx::Literal("(".to_string()), Box::new(|_| Token::LPAREN)));
    regex.push((RegEx::Literal(")".to_string()), Box::new(|_| Token::RPAREN)));
    regex.push((RegEx::Literal(";".to_string()), Box::new(|_| Token::SEMI)));
    regex.push((RegEx::Literal("||".to_string()), Box::new(|_| Token::OR)));
    regex.push((RegEx::Literal("&&".to_string()), Box::new(|_| Token::AND)));
    regex.push((RegEx::Literal("!".to_string()), Box::new(|_| Token::NEG)));
    regex.push((RegEx::Literal("==".to_string()), Box::new(|_| Token::EQ)));
    regex.push((RegEx::Literal("!=".to_string()), Box::new(|_| Token::NE)));

    regex.push((RegEx::Literal("<".to_string()), Box::new(|_| Token::LT)));
    regex.push((RegEx::Literal("<=".to_string()), Box::new(|_| Token::LTE)));
    regex.push((RegEx::Literal(">".to_string()), Box::new(|_| Token::GT)));
    regex.push((RegEx::Literal(">=".to_string()), Box::new(|_| Token::GTE)));

    regex.push((RegEx::Literal("+".to_string()), Box::new(|_| Token::ADD)));
    regex.push((RegEx::Literal("-".to_string()), Box::new(|_| Token::SUB)));
    regex.push((RegEx::Literal("*".to_string()), Box::new(|_| Token::MUL)));
    regex.push((RegEx::Literal("/".to_string()), Box::new(|_| Token::DIV)));

    regex.push((RegEx::Literal("(".to_string()), Box::new(|_| Token::LBRACKET)));
    regex.push((RegEx::Literal(")".to_string()), Box::new(|_| Token::RBRACKET)));
    regex.push((RegEx::Literal(",".to_string()), Box::new(|_| Token::COMMA)));

    println!("{:?}", regex.iter().map(|x| &x.0).collect::<Vec<&RegEx>>());
    regex
}

impl Keyword {
    pub fn as_regex(&self) -> RegEx {
        RegEx::Literal(self.to_string())
    }
}

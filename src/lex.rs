use crate::types::{Keyword, Token};

#[derive(Debug)]
pub enum RegEx {
    Charset(Charset),
    Concat(Vec<RegEx>),
    Or(Vec<RegEx>),
    Literal(String),
    Star(Box<RegEx>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Charset {
    Char(char),
    Chars(bool, Vec<char>),
    CharRange(char, char),
}

// (Priority, Function)
type NfaAcceptFunction = (u128, Box<TokenFunction>);

pub struct Nfa {
    // Start state
    start: usize,
    // Accept state, which has a priority and an accept function which consumes
    // the string up to this point and produces a token
    accept: Vec<(usize, Option<NfaAcceptFunction>)>,
    // Transition, from source to dest
    transitions: Vec<(usize, usize, NfaTransition)>,
}

impl std::fmt::Debug for Nfa {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "NFA {{ start: {}, accept: {:?}, transition: {:?} }}",
            self.start,
            self.accept.iter().map(|x| x.0).collect::<Vec<usize>>(),
            self.transitions
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub enum NfaTransition {
    Charset(Charset),
    Empty,
}

type TokenFunction = dyn Fn(&str) -> Token;

pub fn regexes_to_nfa(regexes: Vec<(RegEx, (u128, Box<TokenFunction>))>) -> Vec<Nfa> {
    let nfas: Vec<Nfa> = vec![];

    for regex in regexes {
        let _ = regex_to_nfa(&regex.0, Some(regex.1));
    }

    nfas
}

pub fn regex_to_nfa(regex: &RegEx, f: Option<NfaAcceptFunction>) -> Nfa {
    match regex {
        RegEx::Charset(c) => {
            // (start) -> char (accept)
            Nfa {
                start: 0,
                accept: vec![(1, f)],
                transitions: vec![(0, 1, NfaTransition::Charset(c.clone()))],
            }
        }
        RegEx::Concat(v) => {
            // Map regex to nfas
            let nfas: Vec<Nfa> = v.iter().map(|b| regex_to_nfa(b, None)).collect();

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
                if nfa.accept.len() != 1 {
                    panic!("cannot currently concat nfa with multiple accept");
                }
                let current_accept = nfa.accept[0].0;
                let next_usable = if max_state != current_accept {
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
                accept.0 = current_accept + last_next_usable;

                last_next_usable += next_usable;
            }

            Nfa {
                start: 0,
                accept: vec![accept],
                transitions,
            }
        }
        RegEx::Or(v) => {
            // Map regex to nfas
            let nfas: Vec<Nfa> = v.iter().map(|b| regex_to_nfa(b, None)).collect();

            let mut state_offset = 1;
            let mut transitions = vec![];
            let mut nfas_accept = vec![];

            for nfa in nfas {
                // The maximum state number
                let max_state = std::cmp::max(
                    nfa.start,
                    nfa.transitions.iter().map(|x| x.1).max().unwrap_or(nfa.start),
                );

                // Epsilon from 0 (new start) to start of nfa
                transitions.push((0, nfa.start + state_offset, NfaTransition::Empty));
                // Shift transitions
                for transition in nfa.transitions {
                    transitions.push((transition.0 + state_offset, transition.1 + state_offset, transition.2))
                }

                // For every accept state of current nfa, push accept to add transition later
                for current_accept in nfa.accept {
                    nfas_accept.push(current_accept.0 + state_offset);
                }

                state_offset += max_state + 1;
            }

            // Need to link accept states of individual nfas to new overall accept state
            let accept_state = state_offset;
            for prev_accept in nfas_accept {
                transitions.push((prev_accept, accept_state, NfaTransition::Empty))
            }

            Nfa {
                start: 0,
                accept: vec![(state_offset, f)],
                transitions,
            }
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

            // (0, start) --'t'-> (1) --'k'-> (2, accept)
            for (i, c) in s.chars().enumerate() {
                transition.push((i, i + 1, NfaTransition::Charset(Charset::Char(c))));
            }

            Nfa {
                start: 0,
                accept: vec![(s.chars().count(), f)],
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
    assert_eq!(1, nfa.accept.len());
    assert_eq!(2, nfa.accept[0].0);

    assert_eq!(2, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NfaTransition::Charset(Charset::Char('h'))));
    assert_eq!(nfa.transitions[1], (1, 2, NfaTransition::Charset(Charset::Char('i'))));
}

#[test]
fn test_regex_to_nfa_or_literal() {
    let regex1 = RegEx::Literal("ab".to_string());
    let regex2 = RegEx::Literal("c".to_string());
    let regex = RegEx::Or(vec![regex1, regex2]);
    let nfa = regex_to_nfa(&regex, None);

    //            --e-> (1) --a-> (2) --b-> (3)
    // (0, start)                               --e--> (6, accept)
    //                --e-> (4) --c-> (5)

    assert_eq!(0, nfa.start);
    assert_eq!(1, nfa.accept.len());
    assert_eq!(6, nfa.accept[0].0);

    assert_eq!(7, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NfaTransition::Empty));
    assert_eq!(nfa.transitions[1], (1, 2, NfaTransition::Charset(Charset::Char('a'))));
    assert_eq!(nfa.transitions[2], (2, 3, NfaTransition::Charset(Charset::Char('b'))));
    assert_eq!(nfa.transitions[3], (0, 4, NfaTransition::Empty));
    assert_eq!(nfa.transitions[4], (4, 5, NfaTransition::Charset(Charset::Char('c'))));
    assert_eq!(nfa.transitions[5], (3, 6, NfaTransition::Empty));
    assert_eq!(nfa.transitions[6], (5, 6, NfaTransition::Empty));
}

#[test]
fn test_regex_to_nfa_concat_literal() {
    let regex1 = RegEx::Literal("ab".to_string());
    let regex2 = RegEx::Literal("cd".to_string());
    let regex3 = RegEx::Literal("e".to_string());
    let regex = RegEx::Concat(vec![regex1, regex2, regex3]);
    let nfa = regex_to_nfa(&regex, None);

    // (start) --a-> 1 --b-> 2 --c-> 3 --d--> 4 --e--> (5, accept)
    assert_eq!(0, nfa.start);
    assert_eq!(1, nfa.accept.len());
    assert_eq!(5, nfa.accept[0].0);

    assert_eq!(5, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NfaTransition::Charset(Charset::Char('a'))));
    assert_eq!(nfa.transitions[1], (1, 2, NfaTransition::Charset(Charset::Char('b'))));
    assert_eq!(nfa.transitions[2], (2, 3, NfaTransition::Charset(Charset::Char('c'))));
    assert_eq!(nfa.transitions[3], (3, 4, NfaTransition::Charset(Charset::Char('d'))));
    assert_eq!(nfa.transitions[4], (4, 5, NfaTransition::Charset(Charset::Char('e'))));
}

pub fn construct_regex() -> Vec<(RegEx, (u128, Box<TokenFunction>))> {
    let mut regex: Vec<(RegEx, (u128, Box<TokenFunction>))> = vec![];

    regex.push((
        Keyword::BEGIN.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::BEGIN))),
    ));
    regex.push((
        Keyword::BOOL.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::BOOL))),
    ));
    regex.push((
        Keyword::CALL.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::CALL))),
    ));
    regex.push((Keyword::DO.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::DO)))));
    regex.push((
        Keyword::ELSE.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::ELSE))),
    ));
    regex.push((Keyword::END.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::END)))));
    regex.push((
        Keyword::FALSE.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::FALSE))),
    ));
    regex.push((Keyword::FI.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::FI)))));
    regex.push((
        Keyword::FLOAT.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::FLOAT))),
    ));
    regex.push((Keyword::IF.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::IF)))));
    regex.push((Keyword::INT.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::INT)))));
    regex.push((Keyword::OD.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::OD)))));
    regex.push((
        Keyword::PROC.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::PROC))),
    ));
    regex.push((Keyword::REF.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::REF)))));
    regex.push((
        Keyword::THEN.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::THEN))),
    ));
    regex.push((
        Keyword::TRUE.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::TRUE))),
    ));
    regex.push((
        Keyword::READ.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::READ))),
    ));
    regex.push((Keyword::VAL.as_regex(), (1, Box::new(|_| Token::Keyword(Keyword::VAL)))));
    regex.push((
        Keyword::WHILE.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::WHILE))),
    ));
    regex.push((
        Keyword::WRITE.as_regex(),
        (1, Box::new(|_| Token::Keyword(Keyword::WRITE))),
    ));

    // Identifier
    regex.push((
        RegEx::Concat(vec![
            RegEx::Or(vec![
                RegEx::Charset(Charset::CharRange('a', 'z')),
                RegEx::Charset(Charset::CharRange('A', 'Z')),
            ]),
            RegEx::Star(Box::new(RegEx::Or(vec![
                RegEx::Charset(Charset::CharRange('0', '9')),
                RegEx::Charset(Charset::CharRange('a', 'z')),
                RegEx::Charset(Charset::CharRange('A', 'Z')),
                RegEx::Charset(Charset::Char('_')),
                RegEx::Charset(Charset::Char('\'')),
            ]))),
        ]),
        (5, Box::new(|s| Token::Ident(s.to_string()))),
    ));

    // IntConst
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::CharRange('0', '9')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9')))),
        ]),
        (4, Box::new(|s| Token::IntConst(s.parse::<u128>().unwrap()))),
    ));

    // FloatConst
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::CharRange('0', '9')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9')))),
            RegEx::Charset(Charset::Char('.')),
            RegEx::Charset(Charset::CharRange('0', '9')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::CharRange('0', '9')))),
        ]),
        (3, Box::new(|s| Token::FloatConst(s.parse::<f64>().unwrap()))),
    ));

    // StrConst
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::Char('"')),
            RegEx::Charset(Charset::Chars(false, vec!['"', '\n', '\t'])),
            RegEx::Charset(Charset::Char('"')),
        ]),
        (2, Box::new(|s| Token::StringConst(s.to_string()))),
    ));

    // BoolConst
    regex.push((
        RegEx::Or(vec![
            RegEx::Literal("true".to_string()),
            RegEx::Literal("false".to_string()),
        ]),
        (
            2,
            Box::new(|s| {
                if s == "true" {
                    Token::BoolConst(true)
                } else {
                    Token::BoolConst(false)
                }
            }),
        ),
    ));

    regex.push((RegEx::Literal("=".to_string()), (1, Box::new(|_| Token::ASSIGN))));
    regex.push((RegEx::Literal("(".to_string()), (1, Box::new(|_| Token::LPAREN))));
    regex.push((RegEx::Literal(")".to_string()), (1, Box::new(|_| Token::RPAREN))));
    regex.push((RegEx::Literal(";".to_string()), (1, Box::new(|_| Token::SEMI))));
    regex.push((RegEx::Literal("||".to_string()), (1, Box::new(|_| Token::OR))));
    regex.push((RegEx::Literal("&&".to_string()), (1, Box::new(|_| Token::AND))));
    regex.push((RegEx::Literal("!".to_string()), (1, Box::new(|_| Token::NEG))));
    regex.push((RegEx::Literal("==".to_string()), (1, Box::new(|_| Token::EQ))));
    regex.push((RegEx::Literal("!=".to_string()), (1, Box::new(|_| Token::NE))));

    regex.push((RegEx::Literal("<".to_string()), (1, Box::new(|_| Token::LT))));
    regex.push((RegEx::Literal("<=".to_string()), (1, Box::new(|_| Token::LTE))));
    regex.push((RegEx::Literal(">".to_string()), (1, Box::new(|_| Token::GT))));
    regex.push((RegEx::Literal(">=".to_string()), (1, Box::new(|_| Token::GTE))));

    regex.push((RegEx::Literal("+".to_string()), (1, Box::new(|_| Token::ADD))));
    regex.push((RegEx::Literal("-".to_string()), (1, Box::new(|_| Token::SUB))));
    regex.push((RegEx::Literal("*".to_string()), (1, Box::new(|_| Token::MUL))));
    regex.push((RegEx::Literal("/".to_string()), (1, Box::new(|_| Token::DIV))));

    regex.push((RegEx::Literal("(".to_string()), (1, Box::new(|_| Token::LBRACKET))));
    regex.push((RegEx::Literal(")".to_string()), (1, Box::new(|_| Token::RBRACKET))));
    regex.push((RegEx::Literal(",".to_string()), (1, Box::new(|_| Token::COMMA))));

    println!("{:?}", regex.iter().map(|x| &x.0).collect::<Vec<&RegEx>>());
    regex
}

impl Keyword {
    pub fn as_regex(&self) -> RegEx {
        RegEx::Literal(self.to_string())
    }
}

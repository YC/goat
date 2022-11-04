use crate::tokens::{Keyword, Token, TokenInfo};
use std::error::Error;

#[derive(Debug)]
enum RegEx {
    Charset(Charset),
    Concat(Vec<RegEx>),
    Or(Vec<RegEx>),
    Literal(String),
    Star(Box<RegEx>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Charset {
    Char(char),
    CharExclude(Vec<char>),
    CharRange(char, char),
}

// (Priority, Function)
type NfaAcceptFunction = (u64, Box<TokenFunction>);

struct Nfa {
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
enum NfaTransition {
    Charset(Charset),
    Empty,
}

impl NfaTransition {
    fn takes_character(&self, c: char) -> bool {
        match self {
            Self::Empty => false,
            Self::Charset(charset) => match charset {
                Charset::Char(char) => c == *char,
                Charset::CharRange(left, right) => c >= *left && c <= *right,
                Charset::CharExclude(e) => !e.contains(&c),
            },
        }
    }
}

type TokenFunction = dyn Fn(&str) -> Token;

pub fn lex(input: &str) -> Result<Vec<TokenInfo>, Box<dyn Error>> {
    let regexes = construct_regex();

    let mut nfas: Vec<Nfa> = vec![];
    for regex in regexes {
        let nfa = regex_to_nfa(&regex.0, Some(regex.1));
        nfas.push(nfa);
    }

    let nfa = nfa_combine(nfas, false, None);
    let tokens = execute_nfa(input, &nfa)?;
    let filtered = tokens
        .into_iter()
        .filter(|t| !matches!(t.0, Token::Whitespace(_) | Token::NewLine | Token::Comment(_)))
        .collect::<Vec<TokenInfo>>();
    Ok(filtered)
}

/// Executes nfa based on input, and get list of tokens.
fn execute_nfa(input: &str, nfa: &Nfa) -> Result<Vec<TokenInfo>, Box<dyn Error>> {
    let mut input: Vec<char> = input.chars().into_iter().collect();
    let mut tokens = vec![];
    let mut lineno: u64 = 1;
    let mut linecol: u64 = 1;

    // While input is not all consumed
    while !input.is_empty() {
        let mut current_states: Vec<usize> = vec![0];
        let mut chars: Vec<char> = vec![];
        let mut offset: u64 = 0;
        // Number of characters, priority of accept, token
        let mut accepted: Vec<(u64, u64, Token)> = vec![];

        while !current_states.is_empty() {
            // Perform all epsilon transitions
            current_states = follow_epsilon(nfa, current_states);

            // For those which are now in the accept state
            for state in &current_states {
                let mut dest_accept = nfa.accept.iter().filter(|x| x.0 == *state);
                if let Some((_, nfa_accept)) = dest_accept.next() {
                    let chars_str: String = chars.iter().collect();
                    let token = (nfa_accept.as_ref().unwrap().1)(&chars_str);
                    accepted.push((offset, nfa_accept.as_ref().unwrap().0, token));
                }
            }

            // End of input
            if input.len() as u64 <= offset {
                break;
            }

            // Consume next character
            let c = input[offset as usize];
            chars.push(c);
            offset += 1;

            // Transition via character
            let mut new_states = vec![];
            for state in current_states {
                for transition in &nfa.transitions {
                    let (source, dest, transition_function) = transition;
                    if *source == state && transition_function.takes_character(c) {
                        new_states.push(*dest);
                    }
                }
            }
            current_states = new_states;
        }

        if accepted.is_empty() {
            return Err(format!(
                "cannot consume input '{}', at line {} col {}",
                chars.iter().collect::<String>(),
                lineno,
                linecol
            ))?;
        }

        // Sort tokens by characters consumed (higher is better), then token priority (lower is better)
        accepted.sort_by(|a, b| {
            if a.0 == b.0 {
                return b.1.partial_cmp(&a.1).unwrap();
            }
            a.1.partial_cmp(&b.1).unwrap()
        });

        // Push the token
        let token = accepted.pop().unwrap();

        let pos = (lineno, linecol);
        if token.2 == Token::NewLine {
            lineno += 1;
            linecol = 1;
        } else {
            linecol += token.0;
        }

        tokens.push((token.2, pos));

        // Increment input by n characters consumed
        for _ in 0..token.0 {
            _ = input.remove(0);
        }
    }

    if !input.is_empty() {
        return Err(format!("unconsumed input, at line {} col {}", lineno, linecol))?;
    }

    Ok(tokens)
}

fn follow_epsilon(nfa: &Nfa, current_states: Vec<usize>) -> Vec<usize> {
    let mut states = vec![];

    for state in &current_states {
        for transition in &nfa.transitions {
            if transition.0 == *state && transition.2 == NfaTransition::Empty {
                states.extend(follow_epsilon(nfa, vec![transition.1]));
            }
        }
    }

    states.extend(current_states);
    states
}

fn regex_to_nfa(regex: &RegEx, f: Option<NfaAcceptFunction>) -> Nfa {
    match regex {
        RegEx::Charset(c) => {
            // (start) -> char (accept)
            Nfa {
                start: 0,
                accept: vec![(1, f)],
                transitions: vec![(0, 1, NfaTransition::Charset(c.clone()))],
            }
        }
        RegEx::Concat(r) => {
            // Map regex to nfas
            let nfas: Vec<Nfa> = r.iter().map(|b| regex_to_nfa(b, None)).collect();
            nfa_concat(nfas, f)
        }
        RegEx::Or(r) => {
            // Map regex to nfas
            let nfas: Vec<Nfa> = r.iter().map(|b| regex_to_nfa(b, None)).collect();
            nfa_combine(nfas, true, f)
        }
        RegEx::Star(regex) => {
            // Map regex to nfas
            let nfa: Nfa = regex_to_nfa(regex, None);
            let mut transitions = vec![];

            // The maximum state number
            let max_state = std::cmp::max(
                nfa.start,
                nfa.transitions.iter().map(|x| x.1).max().unwrap_or(nfa.start),
            );

            // From new start state to nfa's start state
            transitions.push((0, nfa.start + 1, NfaTransition::Empty));

            // Transition from new start to new accept
            let accept = max_state + 2;
            transitions.push((0, accept, NfaTransition::Empty));

            // Copy over transitions with offset of 1 for new start state
            for transition in nfa.transitions {
                transitions.push((transition.0 + 1, transition.1 + 1, transition.2));
            }

            // From accept to start
            transitions.push((nfa.accept[0].0 + 1, nfa.start + 1, NfaTransition::Empty));

            // Transition from accept to new accept
            if nfa.accept.len() != 1 {
                panic!("cannot currently support star nfa with multiple accept");
            }
            transitions.push((nfa.accept[0].0 + 1, accept, NfaTransition::Empty));

            Nfa {
                start: 0,
                accept: vec![(accept, f)],
                transitions,
            }
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

fn nfa_concat(nfas: Vec<Nfa>, f: Option<NfaAcceptFunction>) -> Nfa {
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
            panic!("cannot currently support concat nfa with multiple accept");
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

fn nfa_combine(nfas: Vec<Nfa>, merge_accepts: bool, f: Option<NfaAcceptFunction>) -> Nfa {
    let mut state_offset = 1;
    let mut transitions = vec![];
    let mut nfas_accept = vec![];
    let mut accept = vec![];

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
            transitions.push((transition.0 + state_offset, transition.1 + state_offset, transition.2));
        }

        for current_accept in nfa.accept {
            if merge_accepts {
                // For every accept state of current nfa, push accept to add transition later
                nfas_accept.push(current_accept.0 + state_offset);
            } else {
                // Add accept state
                accept.push((current_accept.0 + state_offset, current_accept.1));
            }
        }

        state_offset += max_state + 1;
    }

    if merge_accepts {
        // Need to link accept states of individual nfas to new overall accept state
        let accept_state = state_offset;
        for prev_accept in nfas_accept {
            transitions.push((prev_accept, accept_state, NfaTransition::Empty));
        }

        Nfa {
            start: 0,
            accept: vec![(state_offset, f)],
            transitions,
        }
    } else {
        Nfa {
            start: 0,
            accept,
            transitions,
        }
    }
}

fn construct_regex() -> Vec<(RegEx, (u64, Box<TokenFunction>))> {
    let mut regex: Vec<(RegEx, (u64, Box<TokenFunction>))> = vec![];

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

    // Whitespace
    regex.push((
        RegEx::Concat(vec![
            RegEx::Or(vec![
                RegEx::Charset(Charset::Char(' ')),
                RegEx::Charset(Charset::Char('\t')),
                RegEx::Charset(Charset::Char('\r')),
            ]),
            RegEx::Star(Box::new(RegEx::Or(vec![
                RegEx::Charset(Charset::Char(' ')),
                RegEx::Charset(Charset::Char('\t')),
                RegEx::Charset(Charset::Char('\r')),
            ]))),
        ]),
        (10, Box::new(|s| Token::Whitespace(s.to_owned()))),
    ));

    // Newline
    regex.push((RegEx::Charset(Charset::Char('\n')), (1, Box::new(|_| Token::NewLine))));
    // Comment
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::Char('#')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::CharExclude(vec!['\n'])))),
        ]),
        (10, Box::new(|s| Token::Comment(s.to_owned()))),
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
        (5, Box::new(|s| Token::Ident(s.to_owned()))),
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
        (3, Box::new(|s| Token::FloatConst(s.to_owned()))),
    ));

    // StrConst
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::Char('"')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::CharExclude(vec!['"', '\n', '\t'])))),
            RegEx::Charset(Charset::Char('"')),
        ]),
        (
            2,
            Box::new(|s| Token::StringConst(s.strip_prefix('\"').unwrap().strip_suffix('\"').unwrap().to_owned())),
        ),
    ));

    // BoolConst
    regex.push((
        RegEx::Or(vec![
            RegEx::Literal("true".to_owned()),
            RegEx::Literal("false".to_owned()),
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

    regex.push((RegEx::Literal(":=".to_owned()), (1, Box::new(|_| Token::ASSIGN))));
    regex.push((RegEx::Literal("(".to_owned()), (1, Box::new(|_| Token::LPAREN))));
    regex.push((RegEx::Literal(")".to_owned()), (1, Box::new(|_| Token::RPAREN))));
    regex.push((RegEx::Literal(";".to_owned()), (1, Box::new(|_| Token::SEMI))));
    regex.push((RegEx::Literal("||".to_owned()), (1, Box::new(|_| Token::OR))));
    regex.push((RegEx::Literal("&&".to_owned()), (1, Box::new(|_| Token::AND))));
    regex.push((RegEx::Literal("!".to_owned()), (1, Box::new(|_| Token::NOT))));
    regex.push((RegEx::Literal("=".to_owned()), (1, Box::new(|_| Token::EQ))));
    regex.push((RegEx::Literal("!=".to_owned()), (1, Box::new(|_| Token::NE))));

    regex.push((RegEx::Literal("<".to_owned()), (1, Box::new(|_| Token::LT))));
    regex.push((RegEx::Literal("<=".to_owned()), (1, Box::new(|_| Token::LTE))));
    regex.push((RegEx::Literal(">".to_owned()), (1, Box::new(|_| Token::GT))));
    regex.push((RegEx::Literal(">=".to_owned()), (1, Box::new(|_| Token::GTE))));

    regex.push((RegEx::Literal("+".to_owned()), (1, Box::new(|_| Token::ADD))));
    regex.push((RegEx::Literal("-".to_owned()), (1, Box::new(|_| Token::SUB))));
    regex.push((RegEx::Literal("*".to_owned()), (1, Box::new(|_| Token::MUL))));
    regex.push((RegEx::Literal("/".to_owned()), (1, Box::new(|_| Token::DIV))));

    regex.push((RegEx::Literal("[".to_owned()), (1, Box::new(|_| Token::LBRACKET))));
    regex.push((RegEx::Literal("]".to_owned()), (1, Box::new(|_| Token::RBRACKET))));
    regex.push((RegEx::Literal(",".to_owned()), (1, Box::new(|_| Token::COMMA))));

    // println!("{:?}", regex.iter().map(|x| &x.0).collect::<Vec<&RegEx>>());
    regex
}

impl Keyword {
    fn as_regex(&self) -> RegEx {
        RegEx::Literal(self.to_string())
    }
}

#[test]
fn test_regex_to_nfa_literal() {
    let regex = RegEx::Literal("hi".to_owned());
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
    let regex1 = RegEx::Literal("ab".to_owned());
    let regex2 = RegEx::Literal("c".to_owned());
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
fn test_nfa_combine() {
    let regex1 = RegEx::Literal("a".to_owned());
    let regex2 = RegEx::Literal("z".to_owned());
    let nfa1 = regex_to_nfa(&regex1, None);
    let nfa2 = regex_to_nfa(&regex2, None);
    let nfas = vec![nfa1, nfa2];

    let nfa = nfa_combine(nfas, false, None);

    assert_eq!(0, nfa.start);
    assert_eq!(2, nfa.accept.len());
    assert_eq!(2, nfa.accept[0].0);
    assert_eq!(4, nfa.accept[1].0);

    assert_eq!(4, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NfaTransition::Empty));
    assert_eq!(nfa.transitions[1], (1, 2, NfaTransition::Charset(Charset::Char('a'))));
    assert_eq!(nfa.transitions[2], (0, 3, NfaTransition::Empty));
    assert_eq!(nfa.transitions[3], (3, 4, NfaTransition::Charset(Charset::Char('z'))));
}

#[test]
fn test_regex_to_nfa_concat_literal() {
    let regex1 = RegEx::Literal("ab".to_owned());
    let regex2 = RegEx::Literal("cd".to_owned());
    let regex3 = RegEx::Literal("e".to_owned());
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

#[test]
fn test_regex_to_nfa_star_literal() {
    let regex = RegEx::Star(Box::new(RegEx::Literal("tk".to_owned())));
    let nfa = regex_to_nfa(&regex, None);

    //          --------------------e------------------------>
    // (0, start) --e-> (1) --'t'-> (2) --'k'-> (3) --e --> (4, accept)
    //                   <---------e-------------
    assert_eq!(0, nfa.start);
    assert_eq!(1, nfa.accept.len());
    assert_eq!(4, nfa.accept[0].0);

    assert_eq!(6, nfa.transitions.len());
    assert_eq!(nfa.transitions[0], (0, 1, NfaTransition::Empty));
    assert_eq!(nfa.transitions[1], (0, 4, NfaTransition::Empty));
    assert_eq!(nfa.transitions[2], (1, 2, NfaTransition::Charset(Charset::Char('t'))));
    assert_eq!(nfa.transitions[3], (2, 3, NfaTransition::Charset(Charset::Char('k'))));
    assert_eq!(nfa.transitions[4], (3, 1, NfaTransition::Empty));
    assert_eq!(nfa.transitions[5], (3, 4, NfaTransition::Empty));
}

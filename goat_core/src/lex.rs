use crate::tokens::{Keyword, Token, TokenInfo};
use std::{
    collections::{HashMap, HashSet},
    error::Error,
};

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
    Chars(Vec<char>),
    CharExclude(Vec<char>),
}

impl Charset {
    fn from_range(l: char, r: char) -> Self {
        let mut chars = vec![];
        for c in l..=r {
            chars.push(c);
        }
        Charset::Chars(chars)
    }
}

// (Priority, Function)
type TokenFunction = fn(&str) -> Result<Token, Box<dyn Error>>;
type NfaAcceptFunction = (u64, Box<TokenFunction>);

struct Nfa {
    /// Start state
    start: usize,
    /// Accept state, which has a priority and an accept function which consumes
    /// the string up to this point and produces a token
    accept: Vec<(usize, Option<NfaAcceptFunction>)>,
    /// Transition, from source to dest
    transitions: Vec<(usize, usize, NfaTransition)>,
}

struct Dfa {
    /// Start state
    start: usize,
    /// Accept state, which has a priority and accept functions which consumes
    /// the string up to this point and produces a token
    accept: HashMap<usize, Vec<NfaAcceptFunction>>,
    /// Transition, from source to dest, no epsilon transition allowed
    transitions: Vec<(usize, usize, Charset)>,
}

impl core::fmt::Debug for Nfa {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
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
            Self::Charset(charset) => charset.takes_character(c),
        }
    }
}

impl Charset {
    fn takes_character(&self, c: char) -> bool {
        match self {
            Charset::Char(char) => c == *char,
            Charset::Chars(e) => e.contains(&c),
            Charset::CharExclude(e) => !e.contains(&c),
        }
    }
}

/// Lexes input into tokens
#[allow(clippy::similar_names)]
pub fn lex(input: &str) -> Result<Vec<TokenInfo>, Box<dyn Error>> {
    let regexes = construct_regex();
    let nfa = generate_nfa(regexes);
    let dfa = nfa_to_dfa(&nfa);

    let tokens = execute_dfa(input, &dfa)?;
    let filtered = tokens
        .into_iter()
        .filter(|t| !matches!(t.0, Token::Whitespace(_) | Token::NewLine | Token::Comment(_)))
        .collect::<Vec<TokenInfo>>();
    Ok(filtered)
}

fn nfa_to_dfa(nfa: &Nfa) -> Dfa {
    // From source, to multiple dests, on charset transition
    let mut transitions: Vec<(usize, usize, Charset)> = vec![];
    // Accept functions for each state
    let mut accept: HashMap<usize, Vec<NfaAcceptFunction>> = HashMap::new();

    let mut state_incr: usize = 1;
    let mut state_map: HashMap<Vec<usize>, usize> = HashMap::new();
    let mut pending_visit: Vec<Vec<usize>> = vec![];
    let mut visited: HashSet<usize> = HashSet::new();

    // Start from start state
    let initial_state = vec![nfa.start];
    state_map.insert(initial_state.clone(), 0);
    pending_visit.push(initial_state);

    while !pending_visit.is_empty() {
        let mut current_states = pending_visit.pop().expect("cannot pop pending_visit");
        let state_num = *state_map
            .get(&current_states)
            .expect("failed to map states to state_num");

        // Visited already?
        if visited.contains(&state_num) {
            continue;
        }
        visited.insert(state_num);

        // Add states which can be visited on epsilon transition from this state
        let mut states_reachable = follow_epsilon(nfa, current_states.clone());
        current_states.append(&mut states_reachable);

        // Look at every possible transition from these states
        // e.g. On 'a' or 'b', we can get to x, y, z
        //      On 'c', we can get to x, y
        let applicable_transitions: Vec<&(usize, usize, NfaTransition)> = nfa
            .transitions
            .iter()
            .filter(|x| current_states.contains(&x.0))
            .collect();

        // For each char in Char or Chars, we need to test each transition
        // But first, need to collect chars which cause transition
        let mut chars_to_test = vec![];
        let mut exclude_chars: HashSet<char> = HashSet::new();
        let mut exclude_dest = vec![];
        for transition in &applicable_transitions {
            match &transition.2 {
                // Ignore epsilon transitions, since they should have all been handled
                NfaTransition::Empty => {}
                // Charset transitions
                NfaTransition::Charset(Charset::Char(c)) => {
                    chars_to_test.push(*c);
                }
                NfaTransition::Charset(Charset::Chars(c)) => {
                    chars_to_test.append(&mut c.clone());
                }
                NfaTransition::Charset(Charset::CharExclude(ce)) => {
                    exclude_chars.extend(ce.clone());
                    exclude_dest.push(transition.1);
                }
            }
        }

        // Map of transitions on chars, key: destination states, value: chars which will take you to the set
        let mut transitions_char: HashMap<Vec<usize>, Vec<char>> = HashMap::new();

        for c in &chars_to_test {
            // Find the set of (destination) states which is reachable via c
            let mut dest = vec![];
            for transition in &applicable_transitions {
                if transition.2.takes_character(*c) {
                    dest.push(transition.1);
                }
            }
            dest.sort_unstable();
            dest.dedup();

            // Other sets of chars which lead to the same set of destination states
            match transitions_char.get_mut(&dest) {
                None => {
                    transitions_char.insert(dest, vec![*c]);
                }
                Some(chars) => {
                    chars.push(*c);
                }
            }
        }

        // With map, create transitions
        for (mut destination_states, mut chars) in transitions_char {
            destination_states.sort_unstable();
            destination_states.dedup();
            chars.sort_unstable();
            chars.dedup();

            // Destination (a set of states) on charset transition
            let dest = match state_map.get(&destination_states) {
                None => {
                    pending_visit.push(destination_states.clone());
                    state_map.insert(destination_states, state_incr);
                    state_incr += 1;
                    state_incr - 1
                }
                Some(n) => *n,
            };

            let charset = Charset::Chars(chars);
            transitions.push((state_num, dest, charset));
        }

        // If char_exclude, collect all chars, and all excluded chars
        if !exclude_dest.is_empty() {
            exclude_dest.sort_unstable();
            exclude_dest.dedup();

            // Beside all the existing excluded chars, include all char transitions
            exclude_chars.extend(chars_to_test);

            // Add set of states reachable via exclude charsets
            let dest = match state_map.get(&exclude_dest) {
                None => {
                    pending_visit.push(exclude_dest.clone());
                    state_map.insert(exclude_dest, state_incr);
                    state_incr += 1;
                    state_incr - 1
                }
                Some(n) => *n,
            };

            let charset = Charset::CharExclude(exclude_chars.into_iter().collect());
            transitions.push((state_num, dest, charset));
        }

        // Find original accept functions of current set of states, and join together
        let accept_functions: Vec<NfaAcceptFunction> = nfa
            .accept
            .iter()
            .filter(|a| current_states.contains(&a.0))
            .map(|a| {
                let accept_function = a.1.as_ref().unwrap();
                (accept_function.0, accept_function.1.clone())
            })
            .collect();
        accept.insert(state_num, accept_functions);
    }

    Dfa {
        start: 0,
        accept,
        transitions,
    }
}

/// Executes dfa based on input, and get list of tokens.
fn execute_dfa(input: &str, dfa: &Dfa) -> Result<Vec<TokenInfo>, Box<dyn Error>> {
    let mut input: Vec<char> = input.chars().into_iter().collect();
    let mut tokens = vec![];
    let mut lineno: u64 = 1;
    let mut linecol: u64 = 1;

    // While input is not all consumed
    while !input.is_empty() {
        // Current DFA state
        let mut current_state: usize = dfa.start;
        // chars consumed
        let mut chars: Vec<char> = vec![];
        // Offset from start of current input
        let mut offset: u64 = 0;
        // Number of characters, priority of accept, token
        let mut accepted: Vec<(u64, u64, Token)> = vec![];

        loop {
            // Is accept function?
            if let Some(accept_fns) = dfa.accept.get(&current_state) {
                for accept in accept_fns {
                    let chars_str: String = chars.iter().collect();
                    let token = (accept.1)(&chars_str);

                    match token {
                        Ok(token) => {
                            accepted.push((offset, accept.0, token));
                        }
                        Err(e) => {
                            return Err(format!("Cannot parse token \"{chars_str}\": {e}").into());
                        }
                    }
                }
            }

            if offset > usize::MAX as u64 {
                return Err("Cannot cast offset to usize due to overflow".into());
            }
            #[allow(clippy::cast_possible_truncation)]
            let offset_usize = offset as usize;

            // End of input
            if input.len() <= offset_usize {
                break;
            }

            // Consume next character
            #[allow(clippy::indexing_slicing)]
            let c = input[offset_usize];
            chars.push(c);
            offset += 1;

            // Transition via character
            let mut new_state = None;
            for transition in &dfa.transitions {
                let (source, dest, transition_function) = transition;
                if *source == current_state && transition_function.takes_character(c) {
                    new_state = Some(*dest);
                }
            }

            // No more transitions
            let Some(new_state) = new_state else {
                break;
            };

            current_state = new_state;
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
                return b.1.cmp(&a.1);
            }
            a.1.cmp(&b.1)
        });

        // Push the token
        let token = accepted.pop().ok_or("no token in accepted list")?;

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
        return Err(format!("unconsumed input, at line {lineno} col {linecol}"))?;
    }

    Ok(tokens)
}

fn generate_nfa(regexes: Vec<(RegEx, (u64, Box<TokenFunction>))>) -> Nfa {
    let nfas: Vec<Nfa> = regexes
        .into_iter()
        .map(|regex| regex_to_nfa(&regex.0, Some(regex.1)))
        .collect();
    nfa_combine(nfas, false, None)
}

/// Executes nfa based on input, and get list of tokens.
#[allow(dead_code)]
fn execute_nfa(input: &str, nfa: &Nfa) -> Result<Vec<TokenInfo>, Box<dyn Error>> {
    let mut input: Vec<char> = input.chars().into_iter().collect();
    let mut tokens = vec![];
    let mut lineno: u64 = 1;
    let mut linecol: u64 = 1;

    // While input is not all consumed
    while !input.is_empty() {
        // NFA states at current step
        let mut current_states: Vec<usize> = vec![nfa.start];
        // chars consumed
        let mut chars: Vec<char> = vec![];
        // Offset from start of current input
        let mut offset: u64 = 0;
        // Number of characters, priority of accept, token
        let mut accepted: Vec<(u64, u64, Token)> = vec![];

        while !current_states.is_empty() {
            // Perform all epsilon transitions
            current_states = follow_epsilon(nfa, current_states);

            // For those which are now in the accept state
            for state in &current_states {
                let mut dest_accept = nfa.accept.iter().filter(|x| x.0 == *state);
                if let Some((_, Some(nfa_accept))) = dest_accept.next() {
                    let chars_str: String = chars.iter().collect();
                    let token = (nfa_accept.1)(&chars_str);

                    match token {
                        Ok(token) => {
                            accepted.push((offset, nfa_accept.0, token));
                        }
                        Err(e) => {
                            return Err(format!("Cannot parse token \"{chars_str}\": {e}").into());
                        }
                    }
                }
            }

            if offset > usize::MAX as u64 {
                return Err("Cannot cast offset to usize due to overflow".into());
            }
            #[allow(clippy::cast_possible_truncation)]
            let offset_usize = offset as usize;

            // End of input
            if input.len() <= offset_usize {
                break;
            }

            // Consume next character
            #[allow(clippy::indexing_slicing)]
            let c = input[offset_usize];
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
                return b.1.cmp(&a.1);
            }
            a.1.cmp(&b.1)
        });

        // Push the token
        let token = accepted.pop().ok_or("no token in accepted list")?;

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
        return Err(format!("unconsumed input, at line {lineno} col {linecol}"))?;
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
            let max_state = core::cmp::max(
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

            for old_accept in nfa.accept {
                // From accept to start
                #[allow(clippy::indexing_slicing)]
                transitions.push((old_accept.0 + 1, nfa.start + 1, NfaTransition::Empty));

                // Transition from accept to new accept
                #[allow(clippy::indexing_slicing)]
                transitions.push((old_accept.0 + 1, accept, NfaTransition::Empty));
            }

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
        let max_state = core::cmp::max(
            nfa.start,
            nfa.transitions.iter().map(|x| x.1).max().unwrap_or(nfa.start),
        );

        // If the max state number is the accept state, it can be used by the next nfa
        // If not, need to increment by 1
        assert!(
            nfa.accept.len() == 1,
            "cannot currently support concat nfa with multiple accept"
        );
        #[allow(clippy::indexing_slicing)]
        let current_accept = nfa.accept[0].0;
        let next_usable = if max_state == current_accept {
            max_state
        } else {
            max_state + 1
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
        let max_state = core::cmp::max(
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
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::BEGIN)))),
    ));
    regex.push((
        Keyword::BOOL.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::BOOL)))),
    ));
    regex.push((
        Keyword::CALL.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::CALL)))),
    ));
    regex.push((
        Keyword::DO.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::DO)))),
    ));
    regex.push((
        Keyword::ELSE.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::ELSE)))),
    ));
    regex.push((
        Keyword::END.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::END)))),
    ));
    regex.push((
        Keyword::FALSE.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::FALSE)))),
    ));
    regex.push((
        Keyword::FI.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::FI)))),
    ));
    regex.push((
        Keyword::FLOAT.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::FLOAT)))),
    ));
    regex.push((
        Keyword::IF.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::IF)))),
    ));
    regex.push((
        Keyword::INT.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::INT)))),
    ));
    regex.push((
        Keyword::OD.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::OD)))),
    ));
    regex.push((
        Keyword::PROC.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::PROC)))),
    ));
    regex.push((
        Keyword::REF.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::REF)))),
    ));
    regex.push((
        Keyword::THEN.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::THEN)))),
    ));
    regex.push((
        Keyword::TRUE.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::TRUE)))),
    ));
    regex.push((
        Keyword::READ.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::READ)))),
    ));
    regex.push((
        Keyword::VAL.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::VAL)))),
    ));
    regex.push((
        Keyword::WHILE.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::WHILE)))),
    ));
    regex.push((
        Keyword::WRITE.as_regex(),
        (1, Box::new(|_| Ok(Token::Keyword(Keyword::WRITE)))),
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
        (10, Box::new(|s| Ok(Token::Whitespace(s.to_owned())))),
    ));

    // Newline
    regex.push((
        RegEx::Charset(Charset::Char('\n')),
        (1, Box::new(|_| Ok(Token::NewLine))),
    ));
    // Comment
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::Char('#')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::CharExclude(vec!['\n'])))),
        ]),
        (10, Box::new(|s| Ok(Token::Comment(s.to_owned())))),
    ));

    // Identifier
    regex.push((
        RegEx::Concat(vec![
            RegEx::Or(vec![
                RegEx::Charset(Charset::from_range('a', 'z')),
                RegEx::Charset(Charset::from_range('A', 'Z')),
            ]),
            RegEx::Star(Box::new(RegEx::Or(vec![
                RegEx::Charset(Charset::from_range('0', '9')),
                RegEx::Charset(Charset::from_range('a', 'z')),
                RegEx::Charset(Charset::from_range('A', 'Z')),
                RegEx::Charset(Charset::Char('_')),
                RegEx::Charset(Charset::Char('\'')),
            ]))),
        ]),
        (5, Box::new(|s| Ok(Token::Ident(s.to_owned())))),
    ));

    // IntConst
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::from_range('0', '9')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::from_range('0', '9')))),
        ]),
        (3, Box::new(|s| Ok(Token::IntConst(s.parse::<i32>()?)))),
    ));

    // FloatConst
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::from_range('0', '9')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::from_range('0', '9')))),
            RegEx::Charset(Charset::Char('.')),
            RegEx::Charset(Charset::from_range('0', '9')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::from_range('0', '9')))),
        ]),
        (
            4,
            Box::new(|s| {
                s.parse::<f32>()?;
                Ok(Token::FloatConst(s.to_owned()))
            }),
        ),
    ));

    // StrConst
    regex.push((
        RegEx::Concat(vec![
            RegEx::Charset(Charset::Char('"')),
            RegEx::Star(Box::new(RegEx::Charset(Charset::CharExclude(vec!['"', '\n', '\t'])))),
            RegEx::Charset(Charset::Char('"')),
        ]),
        (2, Box::new(|s| Ok(Token::StringConst(s.trim_matches('"').to_owned())))),
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
                    Ok(Token::BoolConst(true))
                } else {
                    Ok(Token::BoolConst(false))
                }
            }),
        ),
    ));

    regex.push((RegEx::Literal(":=".to_owned()), (1, Box::new(|_| Ok(Token::ASSIGN)))));
    regex.push((RegEx::Literal("(".to_owned()), (1, Box::new(|_| Ok(Token::LPAREN)))));
    regex.push((RegEx::Literal(")".to_owned()), (1, Box::new(|_| Ok(Token::RPAREN)))));
    regex.push((RegEx::Literal(";".to_owned()), (1, Box::new(|_| Ok(Token::SEMI)))));
    regex.push((RegEx::Literal("||".to_owned()), (1, Box::new(|_| Ok(Token::OR)))));
    regex.push((RegEx::Literal("&&".to_owned()), (1, Box::new(|_| Ok(Token::AND)))));
    regex.push((RegEx::Literal("!".to_owned()), (1, Box::new(|_| Ok(Token::NOT)))));
    regex.push((RegEx::Literal("=".to_owned()), (1, Box::new(|_| Ok(Token::EQ)))));
    regex.push((RegEx::Literal("!=".to_owned()), (1, Box::new(|_| Ok(Token::NE)))));

    regex.push((RegEx::Literal("<".to_owned()), (1, Box::new(|_| Ok(Token::LT)))));
    regex.push((RegEx::Literal("<=".to_owned()), (1, Box::new(|_| Ok(Token::LTE)))));
    regex.push((RegEx::Literal(">".to_owned()), (1, Box::new(|_| Ok(Token::GT)))));
    regex.push((RegEx::Literal(">=".to_owned()), (1, Box::new(|_| Ok(Token::GTE)))));

    regex.push((RegEx::Literal("+".to_owned()), (1, Box::new(|_| Ok(Token::ADD)))));
    regex.push((RegEx::Literal("-".to_owned()), (1, Box::new(|_| Ok(Token::SUB)))));
    regex.push((RegEx::Literal("*".to_owned()), (1, Box::new(|_| Ok(Token::MUL)))));
    regex.push((RegEx::Literal("/".to_owned()), (1, Box::new(|_| Ok(Token::DIV)))));

    regex.push((RegEx::Literal("[".to_owned()), (1, Box::new(|_| Ok(Token::LBRACKET)))));
    regex.push((RegEx::Literal("]".to_owned()), (1, Box::new(|_| Ok(Token::RBRACKET)))));
    regex.push((RegEx::Literal(",".to_owned()), (1, Box::new(|_| Ok(Token::COMMA)))));

    // println!("{:?}", regex.iter().map(|x| &x.0).collect::<Vec<&RegEx>>());
    regex
}

impl Keyword {
    fn as_regex(self) -> RegEx {
        RegEx::Literal(self.to_string())
    }
}

#[test]
fn test_execute_nfa_int_float() {
    let regexes = construct_regex();
    let nfa = generate_nfa(regexes);

    let tokens_int = execute_nfa("42", &nfa).unwrap();
    assert_eq!(1, tokens_int.len());
    assert_eq!(Token::IntConst(42), tokens_int[0].0);

    let tokens_float = execute_nfa("42.42", &nfa).unwrap();
    assert_eq!(1, tokens_float.len());
    assert_eq!(Token::FloatConst("42.42".to_owned()), tokens_float[0].0);
}

#[test]
fn test_execute_dfa_int_float() {
    let regexes = construct_regex();
    let nfa = generate_nfa(regexes);
    let dfa = nfa_to_dfa(&nfa);

    let tokens_int = execute_dfa("42", &dfa).unwrap();
    assert_eq!(1, tokens_int.len());
    assert_eq!(Token::IntConst(42), tokens_int[0].0);

    let tokens_float = execute_dfa("42.42", &dfa).unwrap();
    assert_eq!(1, tokens_float.len());
    assert_eq!(Token::FloatConst("42.42".to_owned()), tokens_float[0].0);
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize
}

use std::fmt;
use std::fmt::Display;

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	write!(f, "line {}, column {} to line {}, column {}", self.start_line, self.start_col, self.end_line-1, self.end_col-1)
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    Add1,
    Sub1,
    Let,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    Keyword::Add1 => write!(f, "add1"),
	    Keyword::Sub1 => write!(f, "sub1"),
	    Keyword::Let => write!(f, "let"),
	}
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<'input> {
    LParen,
    RParen,
    Sym(&'input str),
    Int(i64),
    Keyword(Keyword),
}



fn lex_digits<'input>(str: &'input str, start_ix: usize, start_line: usize, start_col: usize) -> Result<(i64, Span, &'input str), String> {
    let mut chars = str[start_ix..].chars().peekable();
    let mut end_ix = start_ix;

    while let Some(c) = chars.peek() {
	if c.is_digit(10) {
	    chars.next();
	    end_ix += 1;
	    continue;
	} else {
	    break;
	}
    }
    use std::str::FromStr;
    let n = i64::from_str(&str[0..end_ix]).map_err(|_| format!("Number literal {} doesn't fit into 64 bits", &str[0..end_ix]))?;
    return Ok((n, Span{ start_line, start_col, end_col: start_col + end_ix, end_line: start_line + 1 }, &str[end_ix..]));
    
}

// Lexes a single token in the sexpression language.
// Returns None when all input is consumed with no tokens
// Returns Some(Err(...)) if there is no valid token to start the string
// Returns Some(Ok((tok,span,str))) indicating the first token, the span that that token took up, and the remaining tail of the string being lexed
fn lex_one<'input>(str: &'input str, mut start_line: usize, mut start_col: usize) -> Option<Result<(Token<'input>, Span, &'input str), String>> {
    let mut chars = str.char_indices().peekable();

    loop {
	match chars.next() {
	    None => {
		return None
	    }
	    Some((i, c)) => {
		match c {
		    '(' => {
			return Some(Ok((Token::LParen, Span { start_line, start_col, end_line: start_line + 1, end_col: start_col + 1 }, &str[i+1..])))
		    },
		    ')' => {
			return Some(Ok((Token::RParen, Span { start_line, start_col, end_line: start_line + 1, end_col: start_col + 1 }, &str[i+1..])))
		    },
		    '\t'|'\r'|' ' => {
			start_col += 1;
			continue;
		    },
		    '\n' => {
			start_line += 1;
			start_col = 0;
			continue;
		    }
                    '-' => {
                        match chars.next() {
			    Some((i, c)) => {
				if c.is_digit(10) {
                                    return match lex_digits(&str[i-1..], 2, start_line, start_col) {
                                        Err(e) => Some(Err(e)),
                                        Ok((n,sp,leftover)) => Some(Ok((Token::Int(n),sp,leftover)))
                                    }
				    
				}
			    }
			    None => { }
			} // wasn't a digit
			return Some(Err(format!("'-' found  at line {}, column {} not part of negative number literal", start_line, start_col)));
		    }
		    _ => { //
			if c.is_digit(10) {
                            return match lex_digits(&str[i..], 1, start_line, start_col) {
                                Err(e) => Some(Err(e)),
                                Ok((n,sp,leftover)) => Some(Ok((Token::Int(n),sp,leftover)))
                            }
			} else if c.is_alphabetic() {
			    let mut end_ix  = i + 1;
			    let mut end_col = start_col + 1;
			    loop {
				match chars.peek() {
				    Some((_,c)) => {
					if c.is_alphabetic() || c.is_digit(10) || *c == '_' {
					    chars.next();
					    end_ix  += 1;
					    end_col += 1;
					    continue;
					}
				    }
				    None => { }
				}
				// If we reached here we are done, return the current lexeme/token
				let sym = &str[i..end_ix];
				let span = Span{ start_line, start_col, end_col, end_line: start_line + 1 };
				let tok = match sym {
				    "let" => Token::Keyword(Keyword::Let),
				    "add1" => Token::Keyword(Keyword::Add1),
				    "sub1" => Token::Keyword(Keyword::Sub1),
				    _ => Token::Sym(sym),
				};
				let tail = &str[end_ix..];
				return Some(Ok((tok,span,tail)))
			    }
			} else {
			    return Some(Err(format!("lexer error: expected a number, symbol, bool or paren but got {}", c)));
			}
		    }
		};
	    }
	}
    }
}

pub fn tokenize(mut inp: &str) -> Result<Vec<(Token, Span)>, String> {
    let mut toks = Vec::new();
    let mut cur_line = 0;
    let mut cur_col  = 0;
    loop {
	match lex_one(inp, cur_line, cur_col) {
	    None => {
		return Ok(toks);
	    }
	    Some(Ok((tok, span, rest))) => {
		inp = rest;
		cur_line = span.end_line - 1;
		cur_col  = span.end_col;
		toks.push((tok, span));
	    }
	    Some(Err(e)) => {
		return Err(e);
	    }
	}
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Atom {
    Sym(String),
    Int(i64),
    Keyword(Keyword),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Sexp<Ann> {
    Atom(Atom, Ann),
    Nest(Vec<Sexp<Ann>>, Ann)
}


impl Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    Atom::Sym(s) => s.fmt(f),
	    Atom::Int(n) => n.fmt(f),
	    Atom::Keyword(k) => k.fmt(f),
	}
    }
}

impl<Ann> Display for Sexp<Ann> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    Sexp::Atom(at, _) => at.fmt(f),
	    Sexp::Nest(vs, _) => {
		write!(f, "(")?;
		for sexp in vs {
		    write!(f, "{} ", sexp)?;
		}
		write!(f, ")")
	    }
	}
    }
}

// given an input sequence of tokens,

// parses as many s-expressions as possible up to either an unmatched
// right paren or end of input, returning a Vector of s-expressions

// - If it ends on empty input, also returns None
// - If it ends on a right paren, also returns the span of that paren and the remainder of the input
pub fn parse_sexps<'arr,'inp>(mut tokens: &'arr[(Token<'inp>, Span)])
                              -> Result<(Vec<Sexp<Span>>, Option<(Span, &'arr [(Token<'inp>, Span)])>), String> {
    let mut out = Vec::new();
    while let Some((tok, span)) = tokens.get(0) {
	match tok {
	    Token::RParen => {
		return Ok((out, Some((*span, &tokens[1..]))));
	    }
	    Token::LParen => {
		let (nested, where_we_stopped) = parse_sexps(&tokens[1..])?;
                match where_we_stopped {
                    None => {
                        return Err(format!("Left paren '(' at line {}, column {} never matched", span.start_line, span.start_col));
                    }
                    Some((r_span, leftovers)) => {
                        out.push(Sexp::Nest(nested, Span { start_line: span.start_line, start_col: span.start_col, end_line: r_span.end_line, end_col: r_span.end_col }));
                        tokens = leftovers;
                        continue;
                    }
                }
	    }
	    Token::Sym(s) => {
		out.push(Sexp::Atom(Atom::Sym(String::from(*s)), *span));
		tokens = &tokens[1..];
		continue;
	    }
	    Token::Int(n) => {
		out.push(Sexp::Atom(Atom::Int(*n), *span));
		tokens = &tokens[1..];
		continue;

	    }
	    Token::Keyword(k) => {
		out.push(Sexp::Atom(Atom::Keyword(*k), *span));
		tokens = &tokens[1..];
		continue;
	    }
	}
    }
    Ok((out, None))
}

pub fn parse_tokens(tokens: &[(Token, Span)]) -> Result<Vec<Sexp<Span>>, String> {
    let (sexps, where_we_stopped) = parse_sexps(tokens)?;
    match where_we_stopped {
	None => Ok(sexps),
	Some((span, _leftovers)) => {
	    Err(format!("Unmatched right paren ')' on line {}, column {}", span.start_line, span.start_col))
	}
    }
}

pub fn parse_sexp_from_tokens(tokens: &[(Token, Span)]) -> Result<Sexp<Span>, String> {
    let mut sexps = parse_tokens(tokens)?.into_iter();
    match sexps.next() {
	None => Err(format!("Expected a single expression but got empty input")),
	Some(sexp) => {
	    if sexps.len() == 0 {
		Ok(sexp)
	    } else {
		let remaining: Vec<Sexp<Span>> = sexps.collect();
		Err(format!("Expected a single s-expr but got a second one: {}", remaining[0]))
	    }
	}
    }
}

/* Expressions */
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp<Ann> {
    Num(i64, Ann),
    Var(String, Ann),
    Prim1(Prim1, Box<Exp<Ann>>, Ann),
    Let { bindings: Vec<(String, Exp<Ann>)>, // new binding declarations
          body: Box<Exp<Ann>>,  // the expression in which the new variables are bound
          ann: Ann
        }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Prim1 {
    Add1,
    Sub1,
}

impl<Ann> Exp<Ann> {
    pub fn ann(&self) -> Ann where Ann: Clone {
        match self {
            Exp::Num(_, a) => a.clone(),
            Exp::Var(_, a) => a.clone(),
            Exp::Prim1(_, _, a) => a.clone(),
            Exp::Let { ann: a, .. } => a.clone(),
        }
    }
}

pub fn parse_bindings_from_sexp(sexp: &Sexp<Span>) -> Result<Vec<(String, Exp<Span>)>, String> {
    let a = match sexp {
	Sexp::Nest(sexps, _) => sexps,
	_ => {
	    return Err(format!("Expected a sequence of bindings, got {:?}", sexp))
	}
    };
    let mut bindings = Vec::new();
    for sexp in a {
	match sexp {
	    Sexp::Nest(sexps, _) => {
		match &sexps[..] {
		    [Sexp::Atom(Atom::Sym(s), _), sexp] => {
			let exp = parse_exp_from_sexp(&sexp)?;
			bindings.push((String::from(s), exp));
		    }
		    _ => {
			return Err(format!("Expected a binding, got {:?}", sexps));
		    }
		}
	    }
	    _ => {
		return Err(format!("Expected a parenthesized binding, got {:?}", sexp));
	    }
	}
    }
    Ok(bindings)
}

pub fn parse_exp_from_sexp(s: &Sexp<Span>) -> Result<Exp<Span>, String> {
    match s {
	Sexp::Atom(Atom::Int(n), span) => {
	    Ok(Exp::Num(*n, *span))
	}
	Sexp::Atom(Atom::Sym(s), span) => {
	    Ok(Exp::Var(String::from(s), *span))
	}
	Sexp::Atom(Atom::Keyword(kw), span) => {
	    Err(format!("Invalid use of keyword {:?} at line {}, column {}", kw, span.start_line, span.start_col))
	}
	Sexp::Nest(sexps, span) => {
	    match &sexps[..] {
		[Sexp::Atom(Atom::Keyword(Keyword::Add1), _), sexp] => {
		    Ok(Exp::Prim1(Prim1::Add1, Box::new(parse_exp_from_sexp(&sexp)?), *span))
		}
		[Sexp::Atom(Atom::Keyword(Keyword::Sub1), _), sexp] => {
		    Ok(Exp::Prim1(Prim1::Sub1, Box::new(parse_exp_from_sexp(&sexp)?), *span))
		}
		[Sexp::Atom(Atom::Keyword(Keyword::Let), _), bindings_sexp, body_sexp] => {
		    let bindings = parse_bindings_from_sexp(&bindings_sexp)?;
		    let body = Box::new(parse_exp_from_sexp(&body_sexp)?);
		    Ok(Exp::Let { bindings, body, ann: *span})
		}
		_ => Err(format!("Expected an expression, got {:?}", sexps))
	    }
	}
    }
}

pub fn parse_exp_from_sexps(s: &[Sexp<Span>]) -> Result<Exp<Span>, String> {
    match s.len() {
	0 => Err(String::from("Expected an expression got empty input")),
	1 => parse_exp_from_sexp(&s[0]),
	_ => Err(format!("Expected a single expression but got many: {:?}", s)),
    }
}

use crate::syntax::{Exp, Prim1, Span};

pub fn get(env: &[(&str, i64)], x: &str) -> Option<i64> {
    for (y,n) in env.iter().rev() {
        if x == *y {
            return Some(*n);
        }
    }
    None
}

fn interpret_prim1(p: &Prim1, n: i64) -> i64 {
    match p {
	Prim1::Add1 => n + 1,
	Prim1::Sub1 => n - 1,
    }
}

/*
 *
 *  Evaluates the expression to an i64, using the given environment
 *  mapping strings to i64 to interpret variable names.
 *
 *  The lifetime 'exp here says that the string slices in the
 *  environment live as long as the input expression.
*/
fn interpret_help<'exp>(e: &'exp Exp<Span>, mut env: Vec<(&'exp str, i64)>) -> Result<i64, String> {
    match e {
        Exp::Num(i, _) => Ok(*i),
        Exp::Var(x, _) => {
            match get(&env, x) {
                None => Err(format!("unbound variable: {}", x)),
                Some(i) => Ok(i),
            }
        },
        Exp::Prim1(p, e2, _) => Ok(interpret_prim1(p, interpret_help(e2, env)?)),
        Exp::Let { bindings, body, .. } => {
	    if bindings.len() != 1 {
		return Err(format!("Reference interpreter only supports one binding at a time"));
	    } else {
		let (variable, bound) = &bindings[0];
		let x = interpret_help(bound, env.clone())?;
		env.push((&variable, x));
	    }
	    interpret_help(body, env)
        },
    }
}

// Runs the reference interpreter.

// To avoid giving away how to check for errors specific to multiple
// bindings, the reference interpreter only supports the single
// binding language we discussed in class
pub fn interpret(e: &Exp<Span>) -> Result<i64, String> {
    interpret_help(e, Vec::new())
}

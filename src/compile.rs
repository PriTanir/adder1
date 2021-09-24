use std::convert::TryInto;
use crate::syntax::{Exp, Span, Prim1};
use crate::asm::{instrs_to_string};
use crate::asm::{Instr, Reg, Arg64, Arg32, Reg32, MovArgs, BinArgs, MemRef};
use std::collections::HashSet;


// The possible error messages for the compiler
#[derive(Debug, PartialEq, Eq)]
pub enum CompileErr {
    // The location here is the Span of unbound variable
    UnboundVariable{ unbound: String, location: Span },

    // The Span here is the Span of the let-expression that has the duplicated bindings
    DuplicateBinding{ duplicated_name: String, location: Span },
}

use std::fmt;
use std::fmt::Display;

impl Display for CompileErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    CompileErr::UnboundVariable{ unbound, location } => write!(f, "Unbound variable {} at {}", unbound, location),
	    CompileErr::DuplicateBinding{ duplicated_name, location } => write!(f, "Variable {} defined twice in let-expression at {}", duplicated_name, location),
	}
    }
}

// You may or may not find the following function useful.
//
// If you use it, make sure you have good reason why the .unwap() will
// not fail.
fn usize_to_i32(x: usize) -> i32 {
    x.try_into().unwrap()
}

// Finds the binding in the environment with the highest index
fn get(env: &[(&str, i32)], x: &str) -> Option<i32> {
    for (y,n) in env.iter().rev() {
        if x == *y {
            return Some(*n); //if None, then err
        }
    }
    None
}

/*
 * The lifetime 'exp here says that the string slices in the
 * environment live as long as the input expression. This allows you
 * to put references to the strings in the input expression into the
 * environment.
 */

 
   fn compile_with_env<'exp>(e: &'exp Exp<Span>, mut env: Vec<(&'exp str, i32)>) -> Result<Vec<Instr>, CompileErr> {
    
    match e {
        Exp::Num(n,a) => {
            Ok(vec![ Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Imm(*n)))])
        }
        Exp::Var(s,span) =>{ 
            
            match get(&env,s)
            {
                None =>  Err(CompileErr::UnboundVariable{unbound:String::from(s), location:*span}),
                Some(offset) =>  Ok(vec![Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem (MemRef{reg:Reg::Rsp, offset:offset})))])

            }
        }
        
    Exp::Prim1(Prim1::Add1,e,a)=> {
          let mut is = compile_with_env(e,env)?;
          is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Imm(1))));
          Ok(is)

        }
        Exp::Prim1(Prim1::Sub1,e,a) => {
            
          let mut is = compile_with_env(e,env)?;
          is.push(Instr::Sub(BinArgs::ToReg(Reg::Rax, Arg32::Imm(1))));
          Ok(is)
        }
        Exp::Let{bindings:binding, body:e_body, ann:span} => {

            let mut hs:HashSet<&str> = HashSet::new();
            let mut is = Vec::new();
            for(s,e) in binding
            {
             if hs.contains(s.as_str())
             {
                return Err(CompileErr::DuplicateBinding{duplicated_name:String::from(s), location:*span})
             }  
             else
             {
                 hs.insert(&s);
             }
            is.extend(compile_with_env(e, env.clone())?);
            let offset =  usize_to_i32(env.len() + 1); // doubt: how to Calculate the offset from env - is this right?
            env.push((s, offset));
            is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::Rsp, offset: offset }, Reg32::Reg(Reg::Rax))));
        
            
            }
            is.extend(compile_with_env(e_body, env.clone())?);
            Ok(is)
          }
      }
}


fn compile_to_instrs(e: &Exp<Span>) -> Result<Vec<Instr>, CompileErr> {
    let mut is = compile_with_env(e, Vec::new())?;
    is.push(Instr::Ret);
    Ok(is)
}

pub fn compile_to_string(e: &Exp<Span>) -> Result<String, CompileErr> {
    Ok(format!("\
        section .text
        global start_here
start_here:
{}       
", instrs_to_string(&compile_to_instrs(e)?)))
}

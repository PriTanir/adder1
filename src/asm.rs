/*  
 * This file contains the type of our assembly instructions, their
 * arguments, register etc.
 */
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Reg {
    Rax,
    Rsp,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MemRef {
    pub reg: Reg,
    
    pub offset: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arg64 {
    Reg(Reg),
    Imm(i64),
    Mem(MemRef),
}



#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arg32 {
    Reg(Reg),
    Imm(i32),
    Mem(MemRef),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reg32 {
    Reg(Reg),
    Imm(i32),
}

// Possible forms of arguments to the mov instruction
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MovArgs {
    ToReg(Reg, Arg64),
    ToMem(MemRef, Reg32),
}
 

// Possible forms of arguments to most binary instructions: add, sub,
// mul, etc.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinArgs {
    ToReg(Reg, Arg32),
    ToMem(MemRef, Reg32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instr {
    Mov(MovArgs),
    Add(BinArgs),
    Sub(BinArgs),
    Ret,
}


//doubt: how to convert value in register to string
fn reg_to_string(r: Reg) -> String {
    //Match rax and return a string String::from, return string from rax
    match r{
        Reg::Rax => {
            //String::from(r)
            return "Rax".to_string()
        }
        Reg::Rsp => {
            return "Rsp".to_string()
        }
    }
    
}

fn imm32_to_string(i: i32) -> String {
    i.to_string()
}

fn imm64_to_string(i: i64) -> String {
    i.to_string()
}

fn mem_ref_to_string(m: MemRef) -> String {
    // print mem ref - lect 3 and 4[], [reg + 1 * 8], m.offset = 1, 2. m.reg, no need to match
    let s1 = reg_to_string(m.reg);
    let s2 = imm32_to_string(m.offset);
    // lecture 3 - use above 
    //If register is RSP and offset is 10, then we need to return "[RSP-8*10]"
    //return "[m.reg-8*m.offset]"
    format!("{}", "[s1-8*s2]")

}

fn reg32_to_string(r_or_i: Reg32) -> String {
    match r_or_i {
        Reg32::Reg(r) => reg_to_string(r),
        Reg32::Imm(i) => imm32_to_string(i),
    }
}

fn arg32_to_string(arg: Arg32) -> String {
    match arg {
        Arg32::Reg(r) => reg_to_string(r),
        Arg32::Imm(i) => imm32_to_string(i),
        Arg32::Mem(m) => mem_ref_to_string(m),
    }
}

fn arg64_to_string(arg: Arg64) -> String {
    match arg {
        Arg64::Reg(r) => reg_to_string(r),
        Arg64::Imm(i) => imm64_to_string(i),
        Arg64::Mem(m) => mem_ref_to_string(m),
    }
}

fn mov_args_to_string(args: MovArgs) -> String {
    
    match args{
        MovArgs::ToMem(m,r) => {
            
            //let s1 = mem_ref_to_string(m);
            //let s2 = reg32_to_string(r);
            //format!(“{}, {}”, s1, s2)

            format!("{}", mem_ref_to_string(m));
           format!("{}", reg32_to_string(r))

        }
        MovArgs::ToReg(r,a) => {
            
           // format!(“{}, {}”, reg32_to_string(r), arg64_to_string(m))
           format!("{}", reg_to_string(r));
           format!("{}", arg64_to_string(a))
        }
        

    }
    
}

fn bin_args_to_string(args: BinArgs) -> String {
    
    match args{
        BinArgs::ToReg(r,a) =>{
            //format!(“{},{}”, reg32_to_string(r), arg32_to_string(m))
            format!("{}", reg_to_string(r));
         
            format!("{}", arg32_to_string(a))
        }
        BinArgs::ToMem(m,r) =>
        {
            //format!(“{},{}”, mem_ref_to_string(m), reg32_to_string(r))
            format!("{}", mem_ref_to_string(m));
            format!("{}", reg32_to_string(r))
           
        }
    }
}

pub fn instr_to_string(i: Instr) -> String {
    match i {
        Instr::Mov(args) => {
            format!("        mov {}", mov_args_to_string(args))
        }
        Instr::Add(args) => {
            format!("        add {}", bin_args_to_string(args))
        }
        Instr::Sub(args) => {
            format!("        sub {}", bin_args_to_string(args))
        }
        Instr::Ret => {
            format!("	     ret")
        }
	  }	     
}


pub fn instrs_to_string(is: &[Instr]) -> String {
    let mut buf = String::new();
    for i in is {
        buf.push_str(&instr_to_string(*i));
        buf.push_str("\n");
    }
    buf
}

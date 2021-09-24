use std::process::Command;
use std::io::{Read, Write};
use std::path::{Path};
use std::fs::File;

use std::fmt::{Display, Formatter};

use crate::syntax::{tokenize, parse_sexp_from_tokens, parse_exp_from_sexp, Span, Exp};
use crate::compile::{CompileErr, compile_to_string};

#[derive(Debug, PartialEq, Eq)]
pub enum RunnerErr {
    FileOpen(String),
    Lex(String),
    Read(String),
    Parse(String),
    CodeGen(CompileErr),
    Link(String),
    Interp(String),
    // Run(String),
}

impl Display for RunnerErr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
	match self {
	    RunnerErr::FileOpen(s) => write!(f, "Error reading file: {}", s),
	    RunnerErr::Lex(s) => write!(f, "Error lexing input: {}", s),
	    RunnerErr::Read(s) => write!(f, "Error reading input into sexpr: {}", s),
	    RunnerErr::Parse(s) => write!(f, "Error parsing input from sexpr: {}", s),
	    RunnerErr::CodeGen(ce) => write!(f, "Error generating assembly: {}", ce),
	    RunnerErr::Link(s) => write!(f, "Error linking generated assembly with runtime: {}", s),
	    RunnerErr::Interp(s) => write!(f, "Error in interpreter: {}", s),
	}
    }
}

fn handle_errs(r: Result<String, RunnerErr>) {
    match r {
	Ok(s) => println!("{}", s),
	Err(e) => {
	    eprintln!("{}", e);
	    std::process::exit(1);
	}
    }
}

pub fn emit_assembly(p: &Path) {
    handle_errs(compile_file(p))
}

pub fn run(p: &Path) {
    handle_errs(compile_and_run_file(p, Path::new("runtime")))
}

pub fn interp(p: &Path) {
    handle_errs(interpret_file(p))
}

fn interpret_file(p: &Path) -> Result<String, RunnerErr> {
    let exp = parse_file(p)?;
    match crate::interp::interpret(&exp) {
	Ok(n) => Ok(format!("{}", n)),
	Err(e) => Err(RunnerErr::Interp(e))
    }
}

pub fn compile_and_run_file(p: &Path, dir: &Path) -> Result<String, RunnerErr> {
    let asm = compile_file(p)?;
    link_and_run(&asm, dir).map_err(|e| RunnerErr::Link(e.to_string()))
}

fn compile_file(p: &Path) -> Result<String, RunnerErr> {
    let exp = parse_file(p)?;
    compile_to_string(&exp).map_err(|e| RunnerErr::CodeGen(e))
}

fn read_file(p: &Path) -> Result<String, RunnerErr> {
    let mut f = File::open(p).map_err(|e| RunnerErr::FileOpen(e.to_string()))?;
    let mut buf = String::new();
    f.read_to_string(&mut buf).map_err(|e| RunnerErr::FileOpen(e.to_string()))?;
    Ok(buf)
}

fn parse_file(p: &Path) -> Result<Exp<Span>, RunnerErr> {
    let s = read_file(p)?;
    let toks = tokenize(&s).map_err(|e| RunnerErr::Lex(e))?;
    let sexp  = parse_sexp_from_tokens(&toks).map_err(|e| RunnerErr::Read(e))?;
    parse_exp_from_sexp(&sexp).map_err(|e| RunnerErr::Parse(e))    
}

fn link_and_run(assembly: &str, dir: &Path) -> Result<String, String> {
    let (nasm_format, lib_name) = if cfg!(target_os = "linux") {
	("elf64", "libcompiled_code.a")
    } else if cfg!(target_os = "macos") {
	("macho64", "libcompiled_code.a")
    } else if cfg!(target_os = "windows"){
	("win64", "compiled_code.lib")
    } else {
	panic!("Runner script only works on linux, macos and windows")
    };

    let asm_fname = dir.join("compiled_code.s");
    let obj_fname = dir.join("compiled_code.o");
    let lib_fname = dir.join(lib_name);
    let exe_fname = dir.join("stub.exe");
    
    // first put the assembly in a new file compiled_code.s
    let mut asm_file = File::create(&asm_fname).map_err(|e| e.to_string())?;
    asm_file.write(assembly.as_bytes()).map_err(|e| e.to_string())?;
    asm_file.flush().map_err(|e| e.to_string())?;
    
    // nasm -fFORMAT -o compiled_code.o compiled_code.s
    let nasm_out = Command::new("nasm").arg("-f").arg(nasm_format).arg("-o").arg(&obj_fname).arg(&asm_fname).output().map_err(|e| format!("nasm err: {}", e))?;
    if ! nasm_out.status.success() {
	return Err(format!("Failure in nasm call: {}\n{}", nasm_out.status, std::str::from_utf8(&nasm_out.stderr).expect("nasm produced invalid UTF-8")));
    }

    // ar r libcompiled_code.a compiled_code.o
    let ar_out = Command::new("ar").arg("rus").arg(lib_fname).arg(&obj_fname).output().map_err(|e| format!("ar err: {}", e))?;
    if ! ar_out.status.success() {
	return Err(format!("Failure in ar call:\n{}\n{}", ar_out.status, std::str::from_utf8(&ar_out.stderr).expect("ar produced invalid UTF-8")));
    }

    // rustc stub.rs -L tmp
    let rustc_out = Command::new("rustc").arg("runtime/stub.rs").arg("-L").arg(dir).arg("-o").arg(&exe_fname).output().map_err(|e| format!("rustc err: {}", e))?;
    if ! rustc_out.status.success() {
	return Err(format!("Failure in rustc call: {}\n{}", rustc_out.status, std::str::from_utf8(&rustc_out.stderr).expect("rustc produced invalid UTF-8")));
    }

    let compiled_out = Command::new(&exe_fname).output().map_err(|e| e.to_string())?;
    if ! compiled_out.status.success() {
	return Err(format!("Error when running compiled code: {}\n{}", compiled_out.status, std::str::from_utf8(&compiled_out.stderr).expect("compiled code produced invalid UTF-8 error message")));
    }

    Ok(std::str::from_utf8(&compiled_out.stdout).map_err(|e| format!("Rust compiler produced invalid UTF-8 ouput: {}", e))?.to_owned())
}

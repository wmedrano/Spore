use ast::Ast;
use bumpalo::Bump;
use ir::{Constant, Ir, IrError};

use crate::{
    instruction::Instruction,
    val::{functions::ByteCodeFunction, Val},
    vm::Vm,
    SporeRc,
};

pub mod ast;
mod ir;
pub mod span;
pub mod tokenizer;

#[derive(Debug, PartialEq)]
pub enum CompileError {
    Ir(IrError),
}

pub fn compile<'a>(
    vm: &mut Vm,
    source: &'a str,
    arena: &'a Bump,
) -> Result<SporeRc<[Instruction]>, CompileError> {
    let mut instructions = Vec::new();
    let mut compiler = Compiler { vm, args: &[] };
    for ast in Ast::with_source(source) {
        let ir = match ir::Ir::with_ast(source, &ast, arena) {
            Ok(ir) => ir,
            Err(err) => return Err(CompileError::Ir(err)),
        };
        compiler.compile(&mut instructions, &ir);
    }
    let instructions: SporeRc<[Instruction]> = instructions.into();
    Ok(instructions)
}

struct Compiler<'a> {
    pub vm: &'a mut Vm,
    pub args: &'a [&'a str],
}

impl<'a> Compiler<'a> {
    fn deref_idx(&self, symbol: &str) -> Option<usize> {
        for (i, arg) in self.args.iter().enumerate() {
            if *arg == symbol {
                return Some(i);
            }
        }
        None
    }

    fn compile(&mut self, dst: &mut Vec<Instruction>, ir: &Ir) {
        match ir {
            Ir::Constant(constant) => {
                let c = match constant {
                    Constant::Int(x) => Val::Int(*x),
                    Constant::Float(x) => Val::Float(*x),
                    Constant::Symbol(x) => Val::Symbol(self.vm.objects.symbols.symbol_id(x)),
                };
                dst.push(Instruction::Push(c));
            }
            Ir::Deref(ident) => {
                let instruction = match self.deref_idx(ident) {
                    Some(idx) => Instruction::Get(idx),
                    None => Instruction::Deref(self.vm.objects.symbols.symbol_id(ident)),
                };
                dst.push(instruction);
            }
            Ir::FunctionCall { function, args } => {
                self.compile(dst, function);
                for arg in args.iter() {
                    self.compile(dst, arg);
                }
                dst.push(Instruction::Eval(1 + args.len()));
            }
            Ir::Define { symbol, expr } => {
                dst.push(Instruction::Deref(
                    self.vm.objects.symbols.symbol_id("%define"),
                ));
                dst.push(Instruction::Push(Val::Symbol(
                    self.vm.objects.symbols.symbol_id(symbol),
                )));
                self.compile(dst, expr);
                dst.push(Instruction::Eval(3));
            }
            Ir::Lambda { args, exprs } => {
                let mut compiler = Compiler { vm: self.vm, args };
                let mut lambda_instructions = Vec::new();
                for expr in exprs.iter() {
                    compiler.compile(&mut lambda_instructions, expr);
                }
                let lambda = ByteCodeFunction {
                    instructions: lambda_instructions.into(),
                    args: args.len() as u32,
                };
                let lambda_id = self.vm.objects.bytecode_functions.register(lambda);
                dst.push(Instruction::Push(Val::BytecodeFunction(lambda_id)));
            }
        }
    }
}

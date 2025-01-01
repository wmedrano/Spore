use ast::Ast;
use bumpalo::Bump;
use ir::IrError;

use crate::{instruction::Instruction, val::symbol::SymbolTable, SporeRc};

pub mod ast;
mod ir;
pub mod span;
pub mod tokenizer;

#[derive(Debug)]
pub enum CompileError {
    Ir(IrError),
}

pub fn compile<'a>(
    symbols: &mut SymbolTable,
    source: &'a str,
    arena: &'a Bump,
) -> Result<SporeRc<[Instruction]>, CompileError> {
    let asts = Ast::with_source(source);
    let mut instructions = Vec::new();
    for ast in asts {
        let ir = match ir::Ir::with_ast(source, &ast, arena) {
            Ok(ir) => ir,
            Err(err) => return Err(CompileError::Ir(err)),
        };
        ir.compile(symbols, &mut instructions);
    }
    let instructions: SporeRc<[Instruction]> = instructions.into();
    Ok(instructions)
}

use ast::Ast;
use bumpalo::Bump;

use crate::{instruction::Instruction, val::symbol::SymbolTable, SporeRc};

pub mod ast;
mod ir;
pub mod span;
pub mod tokenizer;

pub fn compile<'a>(
    symbols: &mut SymbolTable,
    arena: &'a Bump,
    source: &'a str,
) -> SporeRc<[Instruction]> {
    let asts = Ast::with_source(source);
    let mut instructions = Vec::new();
    for ast in asts {
        let ir = ir::Ir::with_ast(arena, source, &ast);
        ir.compile(symbols, &mut instructions);
    }
    instructions.into()
}

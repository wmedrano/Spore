use bumpalo::Bump;

use crate::{
    instruction::Instruction,
    val::{symbol::SymbolTable, Val},
};

use super::{ast::Ast, span::Span, tokenizer::Token};

type BumpVec<'a, T> = bumpalo::collections::Vec<'a, T>;

#[derive(Debug)]
pub enum Ir<'a> {
    Constant(Constant<'a>),
    Deref(&'a str),
    FunctionCall {
        function: &'a Ir<'a>,
        args: BumpVec<'a, Ir<'a>>,
    },
}

#[derive(Debug)]
pub enum Constant<'a> {
    Int(i64),
    Float(f64),
    Symbol(&'a str),
}

#[derive(Debug)]
pub enum IrError {
    EmptyFunctionCall(Span),
}

pub struct IrBuilder<'a> {
    source: &'a str,
    arena: &'a Bump,
}

impl<'a> IrBuilder<'a> {
    fn build(&self, ast: &Ast) -> Result<Ir<'a>, IrError> {
        match ast {
            Ast::Tree(span, tree) => self.build_function_call(*span, tree.as_slice()),
            Ast::Leaf(token) => Ok(self.build_token(token)),
        }
    }

    fn build_function_call(&self, span: Span, asts: &[Ast]) -> Result<Ir<'a>, IrError> {
        match asts {
            [] => Err(IrError::EmptyFunctionCall(span)),
            [f, arg_asts @ ..] => {
                let function = self.arena.alloc(self.build(f)?);
                let mut args = BumpVec::with_capacity_in(arg_asts.len(), &self.arena);
                for a in arg_asts {
                    args.push(self.build(a)?);
                }
                Ok(Ir::FunctionCall { function, args })
            }
        }
    }

    fn build_token(&self, token: &Token) -> Ir<'a> {
        let text = token.text(self.source);
        let leading_char = text.chars().next().unwrap_or(' ');
        if leading_char == '-' || leading_char.is_ascii_digit() {
            if let Ok(x) = text.parse() {
                return Ir::Constant(Constant::Int(x));
            }
            if let Ok(x) = text.parse() {
                return Ir::Constant(Constant::Float(x));
            }
        }
        if text.starts_with('\'') {
            return Ir::Constant(Constant::Symbol(&text[1..]));
        }
        Ir::Deref(text)
    }
}

impl<'a> Ir<'a> {
    pub fn with_ast(source: &'a str, ast: &Ast, arena: &'a Bump) -> Result<Ir<'a>, IrError> {
        let builder = IrBuilder { source, arena };
        builder.build(ast)
    }
}

impl<'a> Ir<'a> {
    pub fn compile(&self, symbols: &mut SymbolTable, instructions: &mut Vec<Instruction>) {
        match self {
            Ir::Constant(constant) => {
                let c = match constant {
                    Constant::Int(x) => Val::Int(*x),
                    Constant::Float(x) => Val::Float(*x),
                    Constant::Symbol(x) => Val::Symbol(symbols.symbol_id(x)),
                };
                instructions.push(Instruction::Push(c));
            }
            Ir::Deref(ident) => {
                let symbol = symbols.symbol_id(ident);
                instructions.push(Instruction::Deref(symbol));
            }
            Ir::FunctionCall { function, args } => {
                function.compile(symbols, instructions);
                for arg in args {
                    arg.compile(symbols, instructions);
                }
                instructions.push(Instruction::Eval(1 + args.len()));
            }
        }
    }
}

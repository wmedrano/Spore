use bumpalo::Bump;
use compact_str::CompactString;

use crate::{instruction::Instruction, val::Val};

use super::{ast::Ast, tokenizer::Token};

type BumpVec<'a, T> = bumpalo::collections::Vec<'a, T>;

pub enum Ir<'a> {
    Constant(Constant),
    Deref(&'a str),
    FunctionCall {
        function: &'a Ir<'a>,
        args: BumpVec<'a, Ir<'a>>,
    },
}

pub enum Constant {
    Int(i64),
    Float(f64),
}

impl<'a> Ir<'a> {
    pub fn with_ast(arena: &'a Bump, source: &'a str, ast: &Ast) -> Ir<'a> {
        match ast {
            Ast::Tree(_, tree) => Ir::with_function_call(arena, source, tree.as_slice()),
            Ast::Leaf(token) => Ir::with_token(source, token),
        }
    }

    fn with_function_call(arena: &'a Bump, source: &'a str, asts: &[Ast]) -> Ir<'a> {
        match asts {
            [] => todo!(),
            [f, arg_asts @ ..] => {
                let function = arena.alloc(Ir::with_ast(arena, source, f));
                let mut args = BumpVec::with_capacity_in(arg_asts.len(), arena);
                for a in arg_asts {
                    args.push(Ir::with_ast(arena, source, a));
                }
                Ir::FunctionCall { function, args }
            }
        }
    }

    fn with_token(source: &'a str, token: &Token) -> Ir<'a> {
        let text = token.span.context(source);
        let leading_char = text.chars().next().unwrap_or(' ');
        if leading_char == '-' || leading_char.is_ascii_digit() {
            if let Ok(x) = text.parse() {
                return Ir::Constant(Constant::Int(x));
            }
            if let Ok(x) = text.parse() {
                return Ir::Constant(Constant::Float(x));
            }
        }
        Ir::Deref(text)
    }
}

impl<'a> Ir<'a> {
    pub fn compile(&self, instructions: &mut Vec<Instruction>) {
        match self {
            Ir::Constant(constant) => {
                let c = match constant {
                    Constant::Int(x) => Val::Int(*x),
                    Constant::Float(x) => Val::Float(*x),
                };
                instructions.push(Instruction::Push(c));
            }
            Ir::Deref(ident) => instructions.push(Instruction::Deref(CompactString::new(ident))),
            Ir::FunctionCall { function, args } => {
                function.compile(instructions);
                for arg in args {
                    arg.compile(instructions);
                }
                instructions.push(Instruction::Eval(1 + args.len()));
            }
        }
    }
}

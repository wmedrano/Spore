use ast::Ast;
use bumpalo::Bump;
use compact_str::CompactString;
use tokenizer::Token;

use crate::{instruction::Instruction, val::Val};

pub mod ast;
pub mod span;
pub mod tokenizer;

type BumpVec<'a, T> = bumpalo::collections::Vec<'a, T>;

pub fn compile<'a>(arena: &'a Bump, source: &'a str) -> Vec<Instruction> {
    let asts = Ast::with_source(source);
    let mut instructions = Vec::new();
    for ast in asts {
        let ir = Ir::with_ast(arena, source, &ast);
        ir.compile(&mut instructions);
    }
    instructions
}

enum Ir<'a> {
    Constant(Constant),
    Deref(&'a str),
    FunctionCall {
        function: &'a Ir<'a>,
        args: BumpVec<'a, Ir<'a>>,
    },
}

enum Constant {
    Int(i64),
    Float(f64),
}

impl<'a> Ir<'a> {
    fn with_ast(arena: &'a Bump, source: &'a str, ast: &Ast) -> Ir<'a> {
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
    fn compile(&self, instructions: &mut Vec<Instruction>) {
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

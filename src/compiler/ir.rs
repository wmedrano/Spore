use bumpalo::Bump;

use crate::{
    instruction::Instruction,
    val::{symbol::SymbolTable, Val},
};

use super::{ast::Ast, span::Span};

type BumpVec<'a, T> = bumpalo::collections::Vec<'a, T>;

#[derive(Debug)]
pub enum Ir<'a> {
    Constant(Constant<'a>),
    Deref(&'a str),
    FunctionCall {
        function: &'a Ir<'a>,
        args: BumpVec<'a, Ir<'a>>,
    },
    Define {
        symbol: &'a str,
        expr: &'a Ir<'a>,
    },
}

#[derive(Debug)]
pub enum Constant<'a> {
    Int(i64),
    Float(f64),
    Symbol(&'a str),
}

pub enum ParsedText<'a> {
    Constant(Constant<'a>),
    Identifier(&'a str),
}

impl<'a> ParsedText<'a> {
    fn new(text: &'a str) -> Self {
        let leading_char = text.chars().next().unwrap_or(' ');
        if leading_char == '-' || leading_char.is_ascii_digit() {
            if let Ok(x) = text.parse() {
                return ParsedText::Constant(Constant::Int(x));
            }
            if let Ok(x) = text.parse() {
                return ParsedText::Constant(Constant::Float(x));
            }
        }
        if text.starts_with('\'') {
            return ParsedText::Constant(Constant::Symbol(&text[1..]));
        }
        ParsedText::Identifier(text)
    }
}

#[derive(Debug)]
pub enum IrError {
    EmptyFunctionCall(Span),
    ConstantNotCallable(Span),
    BadDefine(Span),
    DefineExpectedIdentifierButFoundConstant(Span),
    DefineExpectedSymbol(Span),
}

pub struct IrBuilder<'a> {
    source: &'a str,
    arena: &'a Bump,
}

impl<'a> IrBuilder<'a> {
    fn build(&self, ast: &Ast) -> Result<Ir<'a>, IrError> {
        match ast {
            Ast::Tree { span, children } => {
                let leading_token = children.first().and_then(|ast| match ast {
                    Ast::Tree { .. } => None,
                    Ast::Leaf { span } => {
                        let parsed = ParsedText::new(span.text(self.source));
                        Some((*span, parsed))
                    }
                });
                match leading_token {
                    Some((span, ParsedText::Constant(_))) => {
                        Err(IrError::ConstantNotCallable(span))
                    }
                    Some((_, ParsedText::Identifier("define"))) => match children.as_slice() {
                        [_define, symbol, expr] => self.build_define(symbol, expr),
                        _ => Err(IrError::BadDefine(*span)),
                    },
                    _ => self.build_function_call(*span, children.as_slice()),
                }
            }
            Ast::Leaf { span } => match ParsedText::new(span.text(self.source)) {
                ParsedText::Constant(c) => Ok(Ir::Constant(c)),
                ParsedText::Identifier(ident) => Ok(Ir::Deref(ident)),
            },
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

    fn build_define(&self, symbol: &Ast, expr: &Ast) -> Result<Ir<'a>, IrError> {
        let symbol_text = symbol
            .leaf_text(self.source)
            .ok_or_else(|| IrError::DefineExpectedSymbol(symbol.span()))?;
        let symbol_text = match ParsedText::new(symbol_text) {
            ParsedText::Constant(_) => {
                return Err(IrError::DefineExpectedIdentifierButFoundConstant(
                    symbol.span(),
                ))
            }
            ParsedText::Identifier(symbol) => symbol,
        };
        let expr = self.arena.alloc(self.build(expr)?);
        Ok(Ir::Define {
            symbol: symbol_text,
            expr,
        })
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
            Ir::Define { symbol, expr } => {
                instructions.push(Instruction::Deref(symbols.symbol_id("%define")));
                instructions.push(Instruction::Push(Val::Symbol(symbols.symbol_id(symbol))));
                expr.compile(symbols, instructions);
                instructions.push(Instruction::Eval(3));
            }
        }
    }
}

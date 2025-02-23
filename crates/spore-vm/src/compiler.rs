use ast::{Ast, AstError};
use bumpalo::Bump;
use compact_str::CompactString;
use ir::{Constant, Ir, IrError};
use span::Span;

use crate::builtins;
use crate::{
    instruction::Instruction,
    val::{bytecode_function::ByteCodeFunction, Val},
    vm::Vm,
    SporeRc,
};

pub mod ast;
mod ir;
pub mod span;
pub mod tokenizer;

#[derive(Copy, Clone, Debug, PartialEq)]
/// Represents an error that can occur during compilation.
pub enum CompileError {
    Ast(AstError),
    Ir(IrError),
}

impl From<IrError> for CompileError {
    fn from(value: IrError) -> Self {
        CompileError::Ir(value)
    }
}

impl From<AstError> for CompileError {
    fn from(value: AstError) -> Self {
        CompileError::Ast(value)
    }
}

impl std::error::Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Ast(e) => write!(f, "{e}"),
            CompileError::Ir(e) => write!(f, "{e}"),
        }
    }
}

impl CompileError {
    pub fn with_context(self, source: &str) -> CompileErrorWithContext<'_> {
        CompileErrorWithContext { err: self, source }
    }
}

#[derive(Debug, PartialEq)]
pub struct CompileErrorWithContext<'a> {
    err: CompileError,
    source: &'a str,
}

impl std::error::Error for CompileErrorWithContext<'_> {}

impl std::fmt::Display for CompileErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.err {
            CompileError::Ast(e) => write!(f, "{}", e.with_context(self.source)),
            CompileError::Ir(e) => write!(f, "{}", e.with_context(self.source)),
        }
    }
}

/// Compiles a string of source code into bytecode instructions.
pub fn compile_module<'a>(
    vm: &mut Vm,
    source: &'a str,
    asts: impl Iterator<Item = &'a Ast>,
    arena: &'a Bump,
) -> Result<SporeRc<[Instruction]>, CompileError> {
    let mut instructions = Vec::new();
    let mut compiler = Compiler { vm, args: &[] };
    let ir = ir::Ir::with_ast(source, asts, arena)?;
    compiler.compile(&mut instructions, CompilerContext::Module, &ir)?;
    let instructions: SporeRc<[Instruction]> = instructions.into();
    Ok(instructions)
}

struct Compiler<'a> {
    pub vm: &'a mut Vm,
    pub args: &'a [&'a str],
}

#[derive(Copy, Clone, PartialEq)]
enum CompilerContext {
    Module,
    Lambda,
    Expression,
}

impl Compiler<'_> {
    /// Compiles an IR into bytecode instructions.
    fn compile(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerContext,
        ir: &Ir,
    ) -> Result<(), IrError> {
        match ir {
            Ir::Constant(constant) => {
                dst.push(self.compile_constant(constant));
            }
            Ir::Deref(ident) => dst.push(self.compile_deref(ident)),
            Ir::FunctionCall { function, args } => {
                self.compile_function_call(dst, function, args)?;
            }
            Ir::Define { span, symbol, expr } => match context {
                CompilerContext::Module => self.compile_module_define(dst, symbol, expr)?,
                CompilerContext::Lambda => return Err(IrError::BadDefine(*span)),
                CompilerContext::Expression => return Err(IrError::BadDefine(*span)),
            },
            Ir::Lambda { name, args, exprs } => {
                dst.push(self.compile_lambda(*name, args, exprs)?);
            }
            Ir::If {
                pred,
                true_branch,
                false_branch,
            } => {
                self.compile_if(dst, pred, true_branch, false_branch)?;
            }
            Ir::MultiExpr { exprs } => self.compile_multi_expr(dst, context, exprs)?,
            Ir::Return { span, exprs } => {
                self.compile_return(dst, context, *span, exprs)?;
            }
        };
        Ok(())
    }

    fn compile_constant(&mut self, constant: &Constant) -> Instruction {
        let val = match constant {
            Constant::Void => Val::Void,
            Constant::Bool(x) => Val::Bool(*x),
            Constant::Int(x) => Val::Int(*x),
            Constant::Float(x) => Val::Float(*x),
            Constant::Symbol(x) => self.vm.make_symbol(x),
            Constant::String(x) => self.vm.make_string(*x),
        };
        Instruction::Push(val)
    }

    fn compile_deref(&mut self, identifier: &str) -> Instruction {
        for (idx, arg) in self.args.iter().enumerate() {
            if *arg == identifier {
                return Instruction::Get(idx);
            }
        }
        let symbol_id = self.vm.make_symbol_id(identifier);
        Instruction::Deref(symbol_id)
    }

    fn compile_function_call(
        &mut self,
        dst: &mut Vec<Instruction>,
        function: &Ir,
        args: &[Ir],
    ) -> Result<(), IrError> {
        self.compile(dst, CompilerContext::Expression, function)?;
        for arg in args.iter() {
            self.compile(dst, CompilerContext::Expression, arg)?;
        }
        dst.push(Instruction::Eval(1 + args.len()));
        Ok(())
    }

    fn compile_module_define(
        &mut self,
        dst: &mut Vec<Instruction>,
        symbol: &str,
        expr: &Ir,
    ) -> Result<(), IrError> {
        dst.push(Instruction::Deref(
            self.vm
                .objects
                .symbols
                .make_symbol_id(builtins::INTERNAL_DEFINE_FUNCTION),
        ));
        dst.push(Instruction::Push(Val::Symbol(
            self.vm.make_symbol_id(symbol),
        )));
        self.compile(dst, CompilerContext::Expression, expr)?;
        dst.push(Instruction::Eval(3));
        Ok(())
    }

    fn compile_lambda(
        &mut self,
        name: Option<&str>,
        args: &[&str],
        exprs: &[Ir],
    ) -> Result<Instruction, IrError> {
        let mut compiler = Compiler { vm: self.vm, args };
        let mut lambda_instructions = Vec::new();
        for expr in exprs.iter() {
            compiler.compile(&mut lambda_instructions, CompilerContext::Lambda, expr)?;
        }
        let lambda = ByteCodeFunction {
            name: name.map(CompactString::new),
            instructions: lambda_instructions.into(),
            args: args.len() as u32,
        };
        let lambda_id = self.vm.objects.register_bytecode(lambda);
        Ok(Instruction::Push(Val::BytecodeFunction { id: lambda_id }))
    }

    fn compile_if(
        &mut self,
        dst: &mut Vec<Instruction>,
        pred: &Ir,
        true_branch: &Ir,
        false_branch: &Ir,
    ) -> Result<(), IrError> {
        self.compile(dst, CompilerContext::Expression, pred)?;
        let condition_jump = dst.len();
        dst.push(Instruction::JumpIf(0));

        let false_start = dst.len();
        self.compile(dst, CompilerContext::Expression, false_branch)?;
        let jump = dst.len();
        dst.push(Instruction::Jump(0));
        let false_end = dst.len();

        let true_start = dst.len();
        self.compile(dst, CompilerContext::Expression, true_branch)?;
        let true_end = dst.len();

        dst[condition_jump] = Instruction::JumpIf(false_end - false_start);
        dst[jump] = Instruction::Jump(true_end - true_start);
        Ok(())
    }

    fn compile_multi_expr(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerContext,
        exprs: &[Ir],
    ) -> Result<(), IrError> {
        for expr in exprs.iter() {
            self.compile(dst, context, expr)?;
        }
        if exprs.is_empty() {
            dst.push(Instruction::Push(Val::Void));
        }
        if exprs.len() > 1 {
            dst.push(Instruction::Compact(exprs.len()));
        }
        Ok(())
    }

    fn compile_return(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerContext,
        span: Span,
        exprs: &[Ir],
    ) -> Result<(), IrError> {
        if context == CompilerContext::Module {
            return Err(IrError::BadReturn(span));
        }
        for expr in exprs.iter() {
            self.compile(dst, CompilerContext::Expression, expr)?;
        }
        if exprs.is_empty() {
            dst.push(Instruction::Push(Val::Void));
        }
        dst.push(Instruction::Return);
        Ok(())
    }
}

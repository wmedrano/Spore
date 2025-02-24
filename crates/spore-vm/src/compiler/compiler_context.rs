use compact_str::CompactString;

use crate::{
    builtins,
    instruction::Instruction,
    val::{bytecode_function::ByteCodeFunction, symbol::SymbolId, Val},
    vm::Vm,
};

use super::{
    ir::{Ir, IrError},
    span::Span,
};

pub struct CompilerContext<'a> {
    pub vm: &'a mut Vm,
    pub args: &'a [&'a str],
    pub locals: Vec<CompactString>,
}

#[derive(Copy, Clone, PartialEq)]
pub enum CompilerScope {
    Module,
    Lambda,
    Expression,
}

enum ResolvedVariable {
    Argument(usize),
    Local(usize),
    Global(SymbolId),
}

impl CompilerContext<'_> {
    /// Compiles an IR into bytecode instructions.
    pub fn compile(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerScope,
        ir: &Ir,
    ) -> Result<(), IrError> {
        match ir {
            Ir::Constant(constant) => {
                dst.push(Instruction::Push(constant.to_val(self.vm)));
            }
            Ir::Deref(ident) => dst.push(self.compile_deref(ident)),
            Ir::FunctionCall { function, args } => {
                self.compile_function_call(dst, function, args)?;
            }
            Ir::Define { span, symbol, expr } => match context {
                CompilerScope::Module => self.compile_module_define(dst, symbol, expr)?,
                CompilerScope::Lambda => self.compile_lambda_define(dst, symbol, expr)?,
                CompilerScope::Expression => return Err(IrError::BadDefine(*span)),
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

    fn resolve_variable(&mut self, identifier: &str) -> ResolvedVariable {
        for (idx, arg) in self.locals.iter().enumerate() {
            if *arg == identifier {
                return ResolvedVariable::Local(idx + self.args.len());
            }
        }
        for (idx, arg) in self.args.iter().enumerate() {
            if *arg == identifier {
                return ResolvedVariable::Argument(idx);
            }
        }
        let symbol_id = self.vm.make_symbol_id(identifier);
        ResolvedVariable::Global(symbol_id)
    }

    fn compile_deref(&mut self, identifier: &str) -> Instruction {
        match self.resolve_variable(identifier) {
            ResolvedVariable::Argument(idx) | ResolvedVariable::Local(idx) => Instruction::Get(idx),
            ResolvedVariable::Global(symbol_id) => Instruction::Deref(symbol_id),
        }
    }

    fn compile_function_call(
        &mut self,
        dst: &mut Vec<Instruction>,
        function: &Ir,
        args: &[Ir],
    ) -> Result<(), IrError> {
        self.compile(dst, CompilerScope::Expression, function)?;
        for arg in args.iter() {
            self.compile(dst, CompilerScope::Expression, arg)?;
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
        self.compile(dst, CompilerScope::Expression, expr)?;
        dst.push(Instruction::Eval(3));
        Ok(())
    }

    fn compile_lambda_define(
        &mut self,
        dst: &mut Vec<Instruction>,
        symbol: &str,
        expr: &Ir,
    ) -> Result<(), IrError> {
        let idx = self.locals.len() + self.args.len();
        self.compile(dst, CompilerScope::Expression, expr)?;
        dst.push(Instruction::Set(idx));
        self.locals.push(symbol.into());
        Ok(())
    }

    fn compile_lambda(
        &mut self,
        name: Option<&str>,
        args: &[&str],
        exprs: &[Ir],
    ) -> Result<Instruction, IrError> {
        let mut compiler = CompilerContext {
            vm: self.vm,
            args,
            locals: Vec::new(),
        };
        let mut lambda_instructions = Vec::new();
        for expr in exprs.iter() {
            compiler.compile(&mut lambda_instructions, CompilerScope::Lambda, expr)?;
        }
        let locals = compiler.locals.len() as u32;
        let lambda = ByteCodeFunction {
            name: name.map(CompactString::new),
            instructions: lambda_instructions.into(),
            args: args.len() as u32,
            locals,
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
        self.compile(dst, CompilerScope::Expression, pred)?;
        let condition_jump = dst.len();
        dst.push(Instruction::JumpIf(0));

        let false_start = dst.len();
        self.compile(dst, CompilerScope::Expression, false_branch)?;
        let jump = dst.len();
        dst.push(Instruction::Jump(0));
        let false_end = dst.len();

        let true_start = dst.len();
        self.compile(dst, CompilerScope::Expression, true_branch)?;
        let true_end = dst.len();

        dst[condition_jump] = Instruction::JumpIf(false_end - false_start);
        dst[jump] = Instruction::Jump(true_end - true_start);
        Ok(())
    }

    fn compile_multi_expr(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerScope,
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
        context: CompilerScope,
        span: Span,
        exprs: &[Ir],
    ) -> Result<(), IrError> {
        if context == CompilerScope::Module {
            return Err(IrError::BadReturn(span));
        }
        for expr in exprs.iter() {
            self.compile(dst, CompilerScope::Expression, expr)?;
        }
        if exprs.is_empty() {
            dst.push(Instruction::Push(Val::Void));
        }
        dst.push(Instruction::Return);
        Ok(())
    }
}

use std::collections::{HashMap, HashSet};

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
    pub locals: Vec<&'a str>,
    pub capturable: HashSet<&'a str>,
    pub captures: Vec<Capture>,
}

pub struct Capture {
    /// The identifier for the variable to capture.
    identifier: CompactString,
    /// The index of the instruction that wants to dereference a captured variable.
    instruction_index: usize,
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
    Captured(SymbolId),
    Global(SymbolId),
}

fn swap_kv<K: Clone, V: Clone + std::hash::Hash + Eq>(m: &HashMap<K, V>) -> HashMap<V, K> {
    HashMap::from_iter(m.iter().map(|(k, v)| (v.clone(), k.clone())))
}

impl<'a> CompilerContext<'a> {
    /// Create a new compiler without any arguments or capturable variables.
    pub fn new(vm: &'a mut Vm) -> CompilerContext<'a> {
        CompilerContext {
            vm,
            args: &[],
            locals: Vec::new(),
            capturable: HashSet::new(),
            captures: Vec::new(),
        }
    }

    /// Create a new compiler with arguments and a set of capturable variables.
    pub fn with_args(
        vm: &'a mut Vm,
        args: &'a [&str],
        capturable: HashSet<&'a str>,
    ) -> CompilerContext<'a> {
        CompilerContext {
            vm,
            args,
            locals: Vec::new(),
            capturable,
            captures: Vec::new(),
        }
    }

    pub fn compile_to_instructions(
        &mut self,
        context: CompilerScope,
        irs: impl Iterator<Item = &'a Ir<'a>>,
    ) -> Result<Vec<Instruction>, IrError> {
        let mut dst = Vec::new();
        for expr in irs {
            self.compile(&mut dst, context, expr)?;
        }
        Ok(dst)
    }

    /// Compiles an IR into bytecode instructions.
    pub fn compile(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerScope,
        ir: &'a Ir,
    ) -> Result<(), IrError> {
        match ir {
            Ir::Constant(constant) => {
                dst.push(Instruction::Push(constant.to_val(self.vm)));
            }
            Ir::Deref(ident) => self.compile_deref(dst, ident),
            Ir::FunctionCall { function, args } => {
                self.compile_function_call(dst, function, args)?;
            }
            Ir::Define { span, symbol, expr } => match context {
                CompilerScope::Module => self.compile_global_define(dst, symbol, expr)?,
                CompilerScope::Lambda => self.compile_local_define(dst, symbol, expr)?,
                CompilerScope::Expression => return Err(IrError::BadDefine(*span)),
            },
            Ir::Lambda { name, args, exprs } => {
                self.compile_lambda(dst, *name, args, exprs)?;
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
        if self.capturable.contains(identifier) {
            ResolvedVariable::Captured(symbol_id)
        } else {
            ResolvedVariable::Global(symbol_id)
        }
    }

    fn compile_deref(&mut self, dst: &mut Vec<Instruction>, identifier: &str) {
        let instruction = match self.resolve_variable(identifier) {
            ResolvedVariable::Argument(idx) | ResolvedVariable::Local(idx) => Instruction::Get(idx),
            ResolvedVariable::Global(symbol_id) => Instruction::Deref(symbol_id),
            ResolvedVariable::Captured(symbol_id) => {
                self.captures.push(Capture {
                    identifier: CompactString::new(identifier),
                    instruction_index: dst.len(),
                });
                Instruction::Deref(symbol_id)
            }
        };
        dst.push(instruction);
    }

    fn compile_function_call(
        &mut self,
        dst: &mut Vec<Instruction>,
        function: &'a Ir,
        args: &'a [Ir],
    ) -> Result<(), IrError> {
        self.compile(dst, CompilerScope::Expression, function)?;
        for arg in args.iter() {
            self.compile(dst, CompilerScope::Expression, arg)?;
        }
        dst.push(Instruction::Eval(1 + args.len()));
        Ok(())
    }

    fn compile_global_define(
        &mut self,
        dst: &mut Vec<Instruction>,
        symbol: &str,
        expr: &'a Ir,
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

    fn compile_local_define(
        &mut self,
        dst: &mut Vec<Instruction>,
        symbol: &'a str,
        expr: &'a Ir,
    ) -> Result<(), IrError> {
        let idx = self.locals.len() + self.args.len();
        self.compile(dst, CompilerScope::Expression, expr)?;
        dst.push(Instruction::Set(idx));
        self.locals.push(symbol);
        Ok(())
    }

    fn compile_if(
        &mut self,
        dst: &mut Vec<Instruction>,
        pred: &'a Ir,
        true_branch: &'a Ir,
        false_branch: &'a Ir,
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
        exprs: &'a [Ir],
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
        exprs: &'a [Ir],
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

    fn compile_lambda(
        &mut self,
        dst: &mut Vec<Instruction>,
        name: Option<&str>,
        args: &[&str],
        exprs: &[Ir],
    ) -> Result<(), IrError> {
        let mut compiler = CompilerContext::with_args(
            self.vm,
            args,
            HashSet::from_iter(
                self.capturable
                    .iter()
                    .copied()
                    .chain(self.locals.iter().copied())
                    .chain(self.args.iter().copied()),
            ),
        );
        let mut lambda_instructions =
            compiler.compile_to_instructions(CompilerScope::Lambda, exprs.iter())?;
        let capture_to_idx = compiler.capture_to_idx();
        compiler.fix_captures(&mut lambda_instructions, &capture_to_idx);
        let locals = compiler.locals.len() as u32;
        let lambda_id = self.vm.objects.register_bytecode(ByteCodeFunction {
            name: name.map(CompactString::new),
            instructions: lambda_instructions.into(),
            args: args.len() as u32,
            locals,
            captures: capture_to_idx.len() as u32,
        });
        if capture_to_idx.is_empty() {
            dst.push(Instruction::Push(Val::BytecodeFunction {
                id: lambda_id,
                captures: None,
            }));
        } else {
            self.compile_capture_derefs(dst, &capture_to_idx);
            dst.push(Instruction::Capture {
                capture_count: capture_to_idx.len() as u32,
                id: lambda_id,
            });
        }
        Ok(())
    }

    fn compile_capture_derefs(
        &mut self,
        dst: &mut Vec<Instruction>,
        capture_to_idx: &HashMap<CompactString, u32>,
    ) {
        let idx_to_capture = swap_kv(capture_to_idx);
        for idx in 0..capture_to_idx.len() as u32 {
            let identifier = idx_to_capture.get(&idx).unwrap();
            self.compile_deref(dst, identifier);
        }
    }

    fn capture_to_idx(&self) -> HashMap<CompactString, u32> {
        let mut ret = HashMap::new();
        for capture in self.captures.iter() {
            if !ret.contains_key(&capture.identifier) {
                let idx = ret.len() as u32;
                ret.insert(capture.identifier.clone(), idx);
            }
        }
        ret
    }

    fn fix_captures(
        &self,
        instructions: &mut [Instruction],
        capture_to_idx: &HashMap<CompactString, u32>,
    ) {
        for capture in self.captures.iter() {
            let capture_idx = *capture_to_idx.get(&capture.identifier).unwrap() as usize;
            let instruction = Instruction::Get(self.args.len() + self.locals.len() + capture_idx);
            instructions[capture.instruction_index] = instruction;
        }
    }
}

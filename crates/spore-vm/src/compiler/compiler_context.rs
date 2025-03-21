use std::collections::{HashMap, HashSet};

use compact_str::CompactString;

use crate::{
    builtins,
    instruction::Instruction,
    val::{Val, bytecode_function::ByteCodeFunction, identifier::IdentifierId},
    vm::Vm,
};

use super::span::Span;

#[derive(Copy, Clone, Debug, PartialEq)]
/// Represents an error that can occur during IR building.
pub enum CompileError {
    EmptyFunctionCall(Span),
    ConstantNotCallable(Span),
    BadDefine(Span),
    BadWhen(Span),
    BadLambda(Span),
    BadIf(Span),
    BadReturn(Span),
}

impl std::error::Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::EmptyFunctionCall(span) => write!(f, "empty function call at {span}"),
            CompileError::ConstantNotCallable(span) => write!(f, "constant not callable at {span}"),
            CompileError::BadDefine(span) => write!(f, "bad define at {span}"),
            CompileError::BadWhen(span) => write!(f, "bad when at {span}"),
            CompileError::BadLambda(span) => write!(f, "bad lambda at {span}"),
            CompileError::BadIf(span) => write!(f, "bad if at {span}"),
            CompileError::BadReturn(span) => write!(f, "bad return at {span}"),
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
            CompileError::EmptyFunctionCall(span) => write!(
                f,
                "empty function call at {span}: {text}",
                text = span.text(self.source)
            ),
            CompileError::ConstantNotCallable(span) => {
                write!(
                    f,
                    "constant not callable at {span}: {text}, text=span.text(self.source)",
                    text = span.text(self.source),
                )
            }
            CompileError::BadDefine(span) => write!(
                f,
                "bad define at {span}: {text}",
                text = span.text(self.source)
            ),
            CompileError::BadLambda(span) => write!(
                f,
                "bad lambda at {span}: {text}",
                text = span.text(self.source)
            ),
            CompileError::BadIf(span) => {
                write!(f, "bad if at {span}: {text}", text = span.text(self.source))
            }
            CompileError::BadWhen(span) => {
                write!(
                    f,
                    "bad when at {span}: {text}",
                    text = span.text(self.source)
                )
            }
            CompileError::BadReturn(span) => {
                write!(
                    f,
                    "bad return at {span}: {text}",
                    text = span.text(self.source)
                )
            }
        }
    }
}

pub struct CompilerContext<'a> {
    pub vm: &'a mut Vm,
    pub args: Vec<CompactString>,
    pub locals: Vec<CompactString>,
    pub capturable: HashSet<CompactString>,
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
    Captured(IdentifierId),
    Global(IdentifierId),
}

fn swap_kv<K: Clone, V: Clone + std::hash::Hash + Eq>(m: &HashMap<K, V>) -> HashMap<V, K> {
    HashMap::from_iter(m.iter().map(|(k, v)| (v.clone(), k.clone())))
}

impl<'a> CompilerContext<'a> {
    /// Create a new compiler without any arguments or capturable variables.
    pub fn new(vm: &'a mut Vm) -> CompilerContext<'a> {
        CompilerContext {
            vm,
            args: Vec::new(),
            locals: Vec::new(),
            capturable: HashSet::new(),
            captures: Vec::new(),
        }
    }

    /// Create a new compiler with arguments and a set of capturable variables.
    pub fn with_args(
        vm: &'a mut Vm,
        args: Vec<CompactString>,
        capturable: HashSet<CompactString>,
    ) -> CompilerContext<'a> {
        CompilerContext {
            vm,
            args,
            locals: Vec::new(),
            capturable,
            captures: Vec::new(),
        }
    }

    pub fn compile_sexp_to_instructions(
        &mut self,
        context: CompilerScope,
        sexps: impl Iterator<Item = Val>,
    ) -> Result<Vec<Instruction>, CompileError> {
        let mut dst = Vec::new();
        for sexp in sexps {
            self.compile_sexp(&mut dst, context, sexp)?;
        }
        Ok(dst)
    }

    pub fn compile_sexp(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerScope,
        sexp: Val,
    ) -> Result<(), CompileError> {
        match sexp {
            Val::Void
            | Val::Bool(_)
            | Val::Int(_)
            | Val::Float(_)
            | Val::Key(_)
            | Val::String(_)
            | Val::ShortString(_)
            | Val::Struct(_)
            | Val::NativeFunction(_)
            | Val::BytecodeFunction { .. }
            | Val::Custom(_)
            | Val::Box(_)
            | Val::DataType(_) => dst.push(Instruction::Push(sexp)),
            Val::Symbol(identifier_id) => {
                match self.vm.identifier_name(identifier_id) {
                    None => dst.push(Instruction::Push(sexp)),
                    // TODO: Consider situation where ident is a
                    // series of apostraphes.
                    Some(ident) if ident.starts_with('\'') => {
                        let unquoted_ident = CompactString::new(&ident[1..]);
                        let symbol = self.vm.make_symbol(&unquoted_ident);
                        dst.push(Instruction::Push(symbol));
                    }
                    Some(ident) => {
                        let ident = CompactString::new(ident);
                        self.compile_deref(dst, &ident);
                    }
                }
            }
            Val::List(list_id) => {
                let items = self.vm.objects.get_list(list_id).unwrap().clone();
                self.compile_sexp_list(dst, context, items)?;
            }
        };
        Ok(())
    }

    // TODO: Get rid of all "maybe_fix" functions for readability.
    fn maybe_fix_define(&mut self, expr: Vec<Val>) -> Vec<Val> {
        match expr.as_slice() {
            [
                Val::Symbol(define_identifier),
                Val::List(args_id),
                body @ ..,
            ] => {
                let define_identifier = self.vm.identifier_name(*define_identifier);
                let args = self.vm.objects.get_list(*args_id);
                match (define_identifier, args) {
                    (Some("define"), Some(args)) => {
                        if body.is_empty() {
                            return expr;
                        }
                        let args = args.clone();
                        let mut lambda_expr = Vec::new();
                        lambda_expr.push(self.vm.make_symbol("lambda"));
                        lambda_expr.push(self.vm.make_list(args[1..].to_vec()));
                        lambda_expr.extend_from_slice(body);
                        Vec::from_iter([expr[0], args[0], self.vm.make_list(lambda_expr)])
                    }
                    _ => expr,
                }
            }
            _ => expr,
        }
    }

    // TODO: Get rid of all "maybe_fix" functions for readability.
    fn maybe_fix_when(&mut self, expr: Vec<Val>) -> Vec<Val> {
        match expr.as_slice() {
            [Val::Symbol(define_identifier), pred, body @ ..] => {
                let when_identifier = self.vm.identifier_name(*define_identifier);
                match when_identifier {
                    Some("when") => {
                        let if_symbol = self.vm.make_symbol("if");
                        let do_symbol = self.vm.make_symbol("do");
                        let body =
                            Vec::from_iter(std::iter::once(do_symbol).chain(body.iter().copied()));
                        vec![if_symbol, *pred, self.vm.make_list(body)]
                    }
                    _ => expr,
                }
            }
            _ => expr,
        }
    }

    pub fn compile_sexp_list(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerScope,
        sexp: Vec<Val>,
    ) -> Result<(), CompileError> {
        let leading_val = match sexp.first() {
            Some(v) => *v,
            None => return Err(CompileError::EmptyFunctionCall(Span::default())),
        };
        let leading_ident = match leading_val {
            Val::Symbol(identifier_id) => self.vm.identifier_name(identifier_id).unwrap(),
            _ => return Err(CompileError::ConstantNotCallable(Span::default())),
        };
        match leading_ident {
            "define" => {
                let sexp = self.maybe_fix_define(sexp);
                let (symbol, expr) = match sexp.as_slice() {
                    [_, symbol, expr] => (*symbol, *expr),
                    _ => return Err(CompileError::BadDefine(Span::default())),
                };
                match context {
                    CompilerScope::Module => self.compile_global_define_sexp(dst, symbol, expr)?,
                    CompilerScope::Lambda => self.compile_local_define_sexp(dst, symbol, expr)?,
                    CompilerScope::Expression => {
                        return Err(CompileError::BadDefine(Span::default()));
                    }
                }
            }
            "lambda" => {
                let (args, exprs) = match sexp.as_slice() {
                    [_, args, exprs @ ..] => {
                        if exprs.is_empty() {
                            return Err(CompileError::BadLambda(Span::default()));
                        }
                        (*args, exprs)
                    }
                    _ => return Err(CompileError::BadLambda(Span::default())),
                };
                self.compile_lambda_sexp(dst, None, args, exprs)?;
            }
            "if" | "when" => {
                let sexp = self.maybe_fix_when(sexp);
                let (pred, true_branch, false_branch) = match sexp.as_slice() {
                    [_, pred, true_branch] => (*pred, *true_branch, Val::Void),
                    [_, pred, true_branch, false_branch] => (*pred, *true_branch, *false_branch),
                    _ => {
                        return Err(CompileError::BadIf(Span::default()));
                    }
                };
                self.compile_if_sexp(dst, pred, true_branch, false_branch)?;
            }
            "return" => self.compile_return_sexp(dst, context, &sexp[1..])?,
            _ => {
                self.compile_function_call_sexp(dst, leading_val, &sexp[1..])?;
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
        let symbol_id = self.vm.make_identifier_id(identifier);
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

    fn compile_function_call_sexp(
        &mut self,
        dst: &mut Vec<Instruction>,
        function: Val,
        args: &[Val],
    ) -> Result<(), CompileError> {
        self.compile_sexp(dst, CompilerScope::Expression, function)?;
        for arg in args.iter() {
            self.compile_sexp(dst, CompilerScope::Expression, *arg)?;
        }
        dst.push(Instruction::Eval(1 + args.len()));
        Ok(())
    }

    fn compile_global_define_sexp(
        &mut self,
        dst: &mut Vec<Instruction>,
        symbol: Val,
        expr: Val,
    ) -> Result<(), CompileError> {
        dst.push(Instruction::Deref(
            self.vm
                .objects
                .symbols
                .make_identifier_id(builtins::INTERNAL_DEFINE_FUNCTION),
        ));
        match symbol {
            Val::Symbol(_) => {}
            _ => return Err(CompileError::BadDefine(Span::default())),
        };
        dst.push(Instruction::Push(symbol));
        self.compile_sexp(dst, CompilerScope::Expression, expr)?;
        dst.push(Instruction::Eval(3));
        Ok(())
    }

    fn compile_local_define_sexp(
        &mut self,
        dst: &mut Vec<Instruction>,
        symbol: Val,
        expr: Val,
    ) -> Result<(), CompileError> {
        let idx = self.locals.len() + self.args.len();
        self.compile_sexp(dst, CompilerScope::Expression, expr)?;
        dst.push(Instruction::Set(idx));
        match symbol {
            Val::Symbol(identifier_id) => {
                let identifier = self.vm.identifier_name(identifier_id).unwrap();
                self.locals.push(CompactString::new(identifier));
            }
            _ => return Err(CompileError::BadDefine(Span::default())),
        };
        Ok(())
    }

    fn compile_if_sexp(
        &mut self,
        dst: &mut Vec<Instruction>,
        pred: Val,
        true_branch: Val,
        false_branch: Val,
    ) -> Result<(), CompileError> {
        self.compile_sexp(dst, CompilerScope::Expression, pred)?;
        let condition_jump = dst.len();
        dst.push(Instruction::JumpIf(0));

        let false_start = dst.len();
        self.compile_sexp(dst, CompilerScope::Expression, false_branch)?;
        let jump = dst.len();
        dst.push(Instruction::Jump(0));
        let false_end = dst.len();

        let true_start = dst.len();
        self.compile_sexp(dst, CompilerScope::Expression, true_branch)?;
        let true_end = dst.len();

        dst[condition_jump] = Instruction::JumpIf(false_end - false_start);
        dst[jump] = Instruction::Jump(true_end - true_start);
        Ok(())
    }

    fn compile_return_sexp(
        &mut self,
        dst: &mut Vec<Instruction>,
        context: CompilerScope,
        exprs: &[Val],
    ) -> Result<(), CompileError> {
        if context == CompilerScope::Module {
            return Err(CompileError::BadReturn(Span::default()));
        }
        for expr in exprs.iter() {
            self.compile_sexp(dst, CompilerScope::Expression, *expr)?;
        }
        if exprs.is_empty() {
            dst.push(Instruction::Push(Val::Void));
        }
        dst.push(Instruction::Return);
        Ok(())
    }

    fn compile_lambda_sexp(
        &mut self,
        dst: &mut Vec<Instruction>,
        name: Option<&str>,
        args: Val,
        exprs: &[Val],
    ) -> Result<(), CompileError> {
        let args = match args {
            Val::List(list_id) => {
                let list = self.vm.objects.get_list(list_id).unwrap();
                let mut args = Vec::with_capacity(list.len());
                for v in list.iter().copied() {
                    match v {
                        Val::Symbol(identifier_id) => {
                            let identifier = self.vm.identifier_name(identifier_id).unwrap();
                            args.push(CompactString::new(identifier));
                        }
                        _ => return Err(CompileError::BadLambda(Span::default())),
                    }
                }
                args
            }
            _ => return Err(CompileError::BadLambda(Span::default())),
        };
        let arg_count = args.len();
        let capturable = HashSet::from_iter(
            self.capturable
                .iter()
                .cloned()
                .chain(self.locals.iter().cloned())
                .chain(self.args.iter().cloned()),
        );
        let mut compiler = CompilerContext::with_args(self.vm, args, capturable);
        let mut lambda_instructions =
            compiler.compile_sexp_to_instructions(CompilerScope::Lambda, exprs.iter().copied())?;
        let capture_to_idx = compiler.capture_to_idx();
        compiler.fix_captures(&mut lambda_instructions, &capture_to_idx);
        let locals = compiler.locals.len() as u32;
        let lambda_id = self.vm.objects.register_bytecode(ByteCodeFunction {
            name: name.map(CompactString::new),
            instructions: lambda_instructions.into(),
            args: arg_count as u32,
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

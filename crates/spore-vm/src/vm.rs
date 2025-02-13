use std::collections::HashMap;

use bumpalo::Bump;
use compact_str::CompactString;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct CommonSymbols {
    /// '%global
    pub(crate) global: SymbolId,
}

use crate::{
    builtins::register_builtins,
    compiler::CompileError,
    instruction::Instruction,
    module::Module,
    object_store::Objects,
    val::{
        functions::{ByteCodeFunction, NativeFunction},
        symbol::SymbolId,
        Val,
    },
};

#[derive(Debug)]
/// The virtual machine.
///
/// The Vm struct represents the virtual machine that executes bytecode.
pub struct Vm {
    pub(crate) common_symbols: CommonSymbols,
    pub(crate) modules: HashMap<SymbolId, Module>,
    stack: Vec<Val>,
    stack_frame: StackFrame,
    previous_stack_frames: Vec<StackFrame>,
    /// The object store.
    pub(crate) objects: Objects,
}

#[derive(Debug, Default)]
pub struct StackFrame {
    stack_start: usize,
    bytecode_idx: usize,
    function: ByteCodeFunction,
}

impl StackFrame {
    pub fn iter_instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.function.instructions.iter()
    }
}

impl Default for Vm {
    fn default() -> Vm {
        let mut objects = Objects::default();
        let common_symbols = CommonSymbols {
            global: objects.symbols.symbol_id("%global"),
        };
        let mut vm = Vm {
            common_symbols,
            modules: HashMap::new(),
            stack: Vec::with_capacity(4096),
            stack_frame: StackFrame::default(),
            previous_stack_frames: Vec::with_capacity(64),
            objects,
        };
        vm.modules.insert(vm.common_symbols.global, Module::new());
        register_builtins(&mut vm);
        vm
    }
}

impl Vm {
    /// Registers a native function in the VM.
    ///
    /// This function registers a native (Rust) function with the VM, making it callable
    /// from spore code. It checks for name conflicts and adds the function to the
    /// global module.
    pub fn register_native_function(&mut self, f: NativeFunction) -> &mut Self {
        let symbol = self.objects.symbols.symbol_id(f.name());
        let globals = self.modules.get_mut(&self.common_symbols.global).unwrap();
        assert!(
            !globals.values.contains_key(&symbol),
            "register_function called with existing function named {name}.",
            name = f.name()
        );
        let id = self
            .objects
            .native_functions
            .register(f, self.objects.reachable_color.swap());
        globals.values.insert(symbol, Val::NativeFunction(id));
        self
    }

    /// Returns the arguments passed to the current function.
    ///
    /// This function returns a slice of `Val` representing the arguments passed to the
    /// currently executing function. The slice is valid for the duration of the function
    /// call.
    pub fn args(&self) -> &[Val] {
        let start = self.stack_frame.stack_start;
        &self.stack[start..]
    }

    /// Looks up the name of a symbol.
    ///
    /// Given a `SymbolId`, this function attempts to retrieve the corresponding name
    /// from the symbol table. It returns `Some(&str)` if the symbol is found, and `None`
    /// otherwise.
    pub fn symbol_name(&self, symbol_id: SymbolId) -> Option<&str> {
        self.objects.symbols.symbol_name(symbol_id)
    }

    pub fn get_or_create_symbol_id(&mut self, name: &str) -> SymbolId {
        self.objects.symbols.symbol_id(name)
    }
}

#[derive(Debug, PartialEq)]
/// Represents errors that can occur during VM execution.
pub enum VmError {
    Compile(CompileError),
    SymbolNotFound(SymbolId),
    NotCallable(Val),
    WrongType,
    WrongArity {
        expected: u32,
        actual: u32,
    },
    Custom(CompactString),
    Format(std::fmt::Error),
    /// Something unexpected happened with the interpreter. This is an issue with Spore itself and not the executed code.
    InterpreterBug(CompactString),
}

/// The result type for VM operations.
pub type VmResult<T> = Result<T, VmError>;

impl From<CompileError> for VmError {
    fn from(value: CompileError) -> Self {
        VmError::Compile(value)
    }
}

impl From<std::fmt::Error> for VmError {
    fn from(value: std::fmt::Error) -> VmError {
        VmError::Format(value)
    }
}

impl Vm {
    /// Evaluates a string of code.
    ///
    /// This function takes a string of spore code, compiles it into bytecode, and then
    /// executes the bytecode in the VM. It returns the resulting `Val` or an error
    /// if compilation or execution fails.
    pub fn eval_str(&mut self, s: &str) -> VmResult<Val> {
        self.objects.run_gc(
            &self.stack,
            self.previous_stack_frames
                .iter()
                .chain(std::iter::once(&self.stack_frame)),
            self.modules.values(),
        );
        self.stack.clear();
        let bytecode = ByteCodeFunction::with_str(self, s, &Bump::new())?;
        self.eval(bytecode)
    }

    /// Evaluates a pre-compiled bytecode function.
    ///
    /// This function executes a pre-compiled `ByteCodeFunction` within the VM. It sets up
    /// a new stack frame, executes the bytecode instructions, and returns the resulting
    /// `Val` or an error if execution fails.
    pub fn eval(&mut self, bytecode: ByteCodeFunction) -> VmResult<Val> {
        assert_eq!(bytecode.args, 0);
        let initial_stack_frames = self.previous_stack_frames.len();
        let previous_stack_frame = std::mem::replace(
            &mut self.stack_frame,
            StackFrame {
                stack_start: self.stack.len(),
                bytecode_idx: 0,
                function: bytecode,
            },
        );
        self.previous_stack_frames.push(previous_stack_frame);
        while self.previous_stack_frames.len() != initial_stack_frames {
            self.run_next()?;
        }
        let res = self.stack.last().cloned().expect("Value should exist");
        Ok(res)
    }

    /// Executes the next instruction in the current stack frame.
    ///
    /// This function retrieves the next instruction from the current bytecode function
    /// and executes it. It handles various instructions such as `Push`, `Eval`, `Get`, `Deref`, and `Return`.
    fn run_next(&mut self) -> VmResult<()> {
        let bytecode_idx = self.stack_frame.bytecode_idx;
        self.stack_frame.bytecode_idx = bytecode_idx + 1;
        let instruction = self
            .stack_frame
            .function
            .instructions
            .get(bytecode_idx)
            .unwrap_or(&Instruction::Return);
        match instruction {
            Instruction::Push(v) => self.stack.push(*v),
            Instruction::Eval(n) => self.execute_eval(*n)?,
            Instruction::Get(idx) => {
                let idx = self.stack_frame.stack_start + idx;
                let v = self.stack[idx];
                self.stack.push(v);
            }
            Instruction::Deref(symbol) => {
                let v = match self
                    .modules
                    .get_mut(&self.common_symbols.global)
                    .unwrap()
                    .values
                    .get(symbol)
                {
                    Some(v) => *v,
                    None => return Err(VmError::SymbolNotFound(*symbol)),
                };
                self.stack.push(v);
            }
            Instruction::Jump(n) => self.execute_jump(*n),
            Instruction::JumpIf(n) => self.execute_jump_if(*n)?,
            Instruction::Return => self.execute_return(),
        }
        Ok(())
    }

    /// Executes a return instruction.
    ///
    /// This function handles the `Return` instruction, which involves restoring the previous
    /// stack frame, retrieving the return value, and updating the stack.
    fn execute_return(&mut self) {
        let stack_start = self.stack_frame.stack_start;
        match self.previous_stack_frames.pop() {
            Some(sf) => self.stack_frame = sf,
            None => self.stack_frame.stack_start = 0,
        }
        self.stack_frame = self.previous_stack_frames.pop().unwrap_or_default();
        let return_value = if self.stack.len() >= stack_start {
            *self.stack.last().expect("Value should exist")
        } else {
            todo!()
        };
        self.stack.truncate(stack_start);
        match self.stack.last_mut() {
            Some(v) => *v = return_value,
            None => self.stack.push(return_value),
        }
    }

    /// Executes the `Eval` instruction.
    ///
    /// This function handles the `Eval` instruction, which is responsible for function calls.
    /// It retrieves the function to be called, sets up a new stack frame, and executes the function.
    fn execute_eval(&mut self, n: usize) -> VmResult<()> {
        let function_idx = self.stack.len() - n;
        let stack_start = function_idx + 1;
        let function = self.stack[function_idx];
        match function {
            Val::NativeFunction(native_function) => {
                let previous_stack_frame = std::mem::replace(
                    &mut self.stack_frame,
                    StackFrame {
                        stack_start,
                        bytecode_idx: 0,
                        function: self.objects.null_bytecode.clone(),
                    },
                );
                self.previous_stack_frames.push(previous_stack_frame);
                let function = self
                    .objects
                    .native_functions
                    .get(native_function)
                    .unwrap()
                    .clone();
                let ret = function.call(self)?;
                self.stack.truncate(stack_start);
                *self.stack.last_mut().expect("Value should exist") = ret;
                self.stack_frame = self.previous_stack_frames.pop().unwrap();
            }
            Val::BytecodeFunction(bytecode_function) => {
                let function = self
                    .objects
                    .bytecode_functions
                    .get(bytecode_function)
                    .unwrap()
                    .clone();
                let arg_count = n as u32 - 1;
                if function.args != arg_count {
                    return Err(VmError::WrongArity {
                        expected: function.args,
                        actual: arg_count,
                    });
                }
                let previous_stack_frame = std::mem::replace(
                    &mut self.stack_frame,
                    StackFrame {
                        stack_start,
                        bytecode_idx: 0,
                        function,
                    },
                );
                self.previous_stack_frames.push(previous_stack_frame);
            }
            v => return Err(VmError::NotCallable(v)),
        }
        Ok(())
    }

    fn execute_jump(&mut self, n: usize) {
        self.stack_frame.bytecode_idx += n;
    }

    fn execute_jump_if(&mut self, n: usize) -> VmResult<()> {
        let v = self.stack.pop().ok_or_else(|| {
            VmError::InterpreterBug("jump_if called with no value on stack".into())
        })?;
        if v.is_truthy() {
            self.execute_jump(n);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call_calls_function_with_args() {
        assert_eq!(Vm::default().eval_str("(+ 1 2 3 4)").unwrap(), Val::Int(10));
    }

    #[test]
    fn define_sets_value_of_symbol() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(define x 12)").unwrap(), Val::Void);
        assert_eq!(vm.eval_str("x").unwrap(), Val::Int(12));
    }

    #[test]
    fn define_lambda_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(define x (lambda () 12))").unwrap(), Val::Void);
        assert_eq!(vm.eval_str("(x)").unwrap(), Val::Int(12));
    }

    #[test]
    fn define_lambda_with_args_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.eval_str("(define x (lambda (a b c) (+ a b c)))")
                .unwrap(),
            Val::Void
        );
        assert_eq!(vm.eval_str("(x 1 2 3)").unwrap(), Val::Int(6));
    }

    #[test]
    fn define_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.eval_str("(define (foo) (+ 1 2 3 4))").unwrap(),
            Val::Void,
        );
        assert_eq!(vm.eval_str("(foo)").unwrap(), Val::Int(10));
    }

    #[test]
    fn define_with_args_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.eval_str("(define (foo a b c) (+ a b c))").unwrap(),
            Val::Void,
        );
        assert_eq!(vm.eval_str("(foo 1 2 3)").unwrap(), Val::Int(6));
    }

    #[test]
    fn function_with_wrong_number_of_args_fails() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.eval_str("(define (foo a b c) (+ a b c))").unwrap(),
            Val::Void
        );
        assert_eq!(
            vm.eval_str("(foo 1)").unwrap_err(),
            VmError::WrongArity {
                expected: 3,
                actual: 1
            }
        );
    }
}

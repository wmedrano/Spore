use bumpalo::Bump;

use crate::{
    compiler::CompileError,
    instruction::Instruction,
    module::Module,
    object_store::TypedObjectStore,
    val::{
        functions::{ByteCodeFunction, NativeFunction},
        symbol::{SymbolId, SymbolTable},
        Val,
    },
};

#[derive(Debug)]
pub struct Vm {
    pub(crate) globals: Module,
    stack: Vec<Val>,
    stack_frame: StackFrame,
    previous_stack_frames: Vec<StackFrame>,
    compile_arena: Bump,
    objects: Objects,
}

#[derive(Debug, Default)]
struct Objects {
    native_functions: TypedObjectStore<NativeFunction>,
    bytecode_functions: TypedObjectStore<ByteCodeFunction>,
    symbols: SymbolTable,
    null_bytecode: ByteCodeFunction,
}

#[derive(Debug)]
struct StackFrame {
    stack_start: usize,
    bytecode_idx: usize,
    function: ByteCodeFunction,
}

impl Default for StackFrame {
    fn default() -> StackFrame {
        StackFrame {
            stack_start: 0,
            bytecode_idx: 0,
            function: ByteCodeFunction::default(),
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        let mut vm = Vm {
            globals: Module::new(),
            stack: Vec::with_capacity(4096),
            stack_frame: StackFrame {
                stack_start: 0,
                bytecode_idx: 0,
                function: ByteCodeFunction::default(),
            },
            previous_stack_frames: Vec::with_capacity(128),
            compile_arena: Bump::new(),
            objects: Objects::default(),
        };
        crate::builtins::register_builtins(&mut vm);
        vm
    }
}

impl Vm {
    pub fn register_native_function(&mut self, f: NativeFunction) -> &mut Self {
        let symbol = self.objects.symbols.symbol_id(f.name());
        assert!(
            !self.globals.values.contains_key(&symbol),
            "register_function called with existing function named {name}.",
            name = f.name()
        );
        let id = self.objects.native_functions.register(f);
        self.globals.values.insert(symbol, Val::NativeFunction(id));
        self
    }

    pub fn args(&self) -> &[Val] {
        let start = self.stack_frame.stack_start;
        &self.stack[start..]
    }

    pub fn symbol_name(&self, symbol_id: SymbolId) -> Option<&str> {
        self.objects.symbols.symbol_name(symbol_id)
    }
}

#[derive(Debug)]
pub enum VmError {
    Compile(CompileError),
    SymbolNotFound(SymbolId),
    NotCallable(Val),
}

pub type VmResult<T> = Result<T, VmError>;

impl From<CompileError> for VmError {
    fn from(value: CompileError) -> Self {
        VmError::Compile(value)
    }
}

impl Vm {
    pub fn eval_str(&mut self, s: &str) -> VmResult<Val> {
        let bytecode =
            ByteCodeFunction::with_str(&mut self.objects.symbols, s, &self.compile_arena)?;
        self.compile_arena.reset();
        self.eval(bytecode)
    }

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
        let res = self.stack.last().cloned().unwrap_or(Val::Void);
        Ok(res)
    }

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
            Instruction::Push(v) => self.stack.push(v.clone()),
            Instruction::Eval(n) => self.execute_eval(*n)?,
            Instruction::Deref(symbol) => {
                let v = match self.globals.values.get(symbol) {
                    Some(v) => v.clone(),
                    None => return Err(VmError::SymbolNotFound(*symbol)),
                };
                self.stack.push(v);
            }
            Instruction::Return => self.execute_return(),
        }
        Ok(())
    }

    fn execute_return(&mut self) {
        let stack_start = self.stack_frame.stack_start;
        match self.previous_stack_frames.pop() {
            Some(sf) => self.stack_frame = sf,
            None => self.stack_frame.stack_start = 0,
        }
        self.stack_frame = self.previous_stack_frames.pop().unwrap_or_default();
        let return_value = if self.stack.len() >= stack_start {
            self.stack.last().unwrap().clone()
        } else {
            todo!()
        };
        self.stack.truncate(stack_start);
        match self.stack.last_mut() {
            Some(v) => *v = return_value,
            None => self.stack.push(return_value),
        }
    }

    fn execute_eval(&mut self, n: usize) -> VmResult<()> {
        let function_idx = self.stack.len() - n;
        let stack_start = function_idx + 1;
        let function = self.stack[function_idx].clone();
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
                *self.stack.last_mut().unwrap() = ret;
                self.stack_frame = self.previous_stack_frames.pop().unwrap();
            }
            Val::BytecodeFunction(bytecode_function) => {
                let previous_stack_frame = std::mem::replace(
                    &mut self.stack_frame,
                    StackFrame {
                        stack_start,
                        bytecode_idx: 0,
                        function: self
                            .objects
                            .bytecode_functions
                            .get(bytecode_function)
                            .unwrap()
                            .clone(),
                    },
                );
                self.previous_stack_frames.push(previous_stack_frame);
            }
            v => return Err(VmError::NotCallable(v)),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call() {
        assert_eq!(Vm::default().eval_str("(+ 1 2 3 4)").unwrap(), Val::Int(10));
    }

    #[test]
    fn define() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(define x 12)").unwrap(), Val::Void);
        assert_eq!(vm.eval_str("x").unwrap(), Val::Int(12));
    }
}

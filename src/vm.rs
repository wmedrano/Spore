use bumpalo::Bump;

use crate::{
    instruction::Instruction,
    module::Module,
    object_store::TypedObjectStore,
    val::{
        functions::{ByteCodeFunction, NativeFunction},
        symbol::SymbolTable,
        Val,
    },
};

pub struct Vm {
    globals: Module,
    stack: Vec<Val>,
    stack_frames: Vec<StackFrame>,
    compile_arena: Bump,
    objects: Objects,
}

#[derive(Default)]
struct Objects {
    native_functions: TypedObjectStore<NativeFunction>,
    bytecode_functions: TypedObjectStore<ByteCodeFunction>,
    symbols: SymbolTable,
}

struct StackFrame {
    stack_start: usize,
    bytecode_idx: usize,
    function: ByteCodeFunction,
}

impl Default for Vm {
    fn default() -> Self {
        Vm::new()
    }
}

impl Vm {
    pub fn new() -> Vm {
        let mut vm = Vm {
            globals: Module::new(),
            stack: Vec::with_capacity(4096),
            stack_frames: Vec::with_capacity(128),
            compile_arena: Bump::new(),
            objects: Objects::default(),
        };
        vm.register_function("+", plus);
        vm
    }

    pub fn register_function(
        &mut self,
        name: &str,
        f: impl 'static + Fn(&[Val]) -> Val,
    ) -> &mut Self {
        let symbol = self.objects.symbols.symbol_id(name);
        assert!(
            !self.globals.values.contains_key(&symbol),
            "register_function called with existing function named {name}."
        );
        let id = self
            .objects
            .native_functions
            .register(NativeFunction::new(f));
        self.globals.values.insert(symbol, Val::NativeFunction(id));
        self
    }

    pub fn args(&self) -> &[Val] {
        let start = self
            .stack_frames
            .last()
            .map(|sf| sf.stack_start)
            .unwrap_or(self.stack.len());
        &self.stack[start..]
    }
}

impl Vm {
    pub fn eval_str(&mut self, s: &str) -> Val {
        let bytecode = ByteCodeFunction::with_str(&self.compile_arena, s);
        self.compile_arena.reset();
        self.eval(bytecode)
    }

    pub fn eval(&mut self, bytecode: ByteCodeFunction) -> Val {
        assert_eq!(bytecode.args, 0);
        let initial_stack_frames = self.stack_frames.len();
        self.stack_frames.push(StackFrame {
            stack_start: self.stack.len(),
            bytecode_idx: 0,
            function: bytecode,
        });
        while self.stack_frames.len() != initial_stack_frames {
            self.run_next();
        }
        self.stack.last().cloned().unwrap_or(Val::Void)
    }

    fn run_next(&mut self) {
        let bytecode_idx = self.stack_frames.last().unwrap().bytecode_idx;
        self.stack_frames.last_mut().unwrap().bytecode_idx = bytecode_idx + 1;
        let instruction = self
            .stack_frames
            .last()
            .unwrap()
            .function
            .instructions
            .get(bytecode_idx)
            .unwrap_or(&Instruction::Return);
        match instruction {
            Instruction::Push(v) => self.stack.push(v.clone()),
            Instruction::Eval(n) => self.execute_eval(*n),
            Instruction::Deref(ident) => {
                let ident_symbol = self.objects.symbols.symbol_id(ident);
                let v = self.globals.values.get(&ident_symbol).unwrap();
                self.stack.push(v.clone());
            }
            Instruction::Return => self.execute_return(),
        }
    }

    fn execute_return(&mut self) {
        let stack_start = self.stack_frames.pop().unwrap().stack_start;
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

    fn execute_eval(&mut self, n: usize) {
        let function_idx = self.stack.len() - n;
        let stack_start = function_idx + 1;
        let function = self.stack[function_idx].clone();
        match function {
            Val::NativeFunction(native_function) => {
                let ret = self
                    .objects
                    .native_functions
                    .get(native_function)
                    .unwrap()
                    .call(self);
                self.stack.truncate(stack_start);
                *self.stack.last_mut().unwrap() = ret;
            }
            Val::BytecodeFunction(bytecode_function) => {
                let stack_frame = StackFrame {
                    stack_start,
                    bytecode_idx: 0,
                    function: self
                        .objects
                        .bytecode_functions
                        .get(bytecode_function)
                        .unwrap()
                        .clone(),
                };
                self.stack_frames.push(stack_frame);
            }
            _ => todo!(),
        }
    }
}

fn plus(args: &[Val]) -> Val {
    let mut int_sum = 0;
    let mut float_sum = 0.0;
    for arg in args {
        match arg {
            Val::Int(x) => int_sum += *x,
            Val::Float(x) => float_sum += *x,
            _ => todo!(),
        }
    }
    if float_sum == 0.0 {
        Val::Int(int_sum)
    } else {
        Val::Float(float_sum + int_sum as f64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call() {
        assert_eq!(Vm::new().eval_str("(+ 1 2 3 4)"), Val::Int(10));
    }
}

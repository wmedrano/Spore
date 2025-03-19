use bumpalo::Bump;
use compact_str::CompactString;

use crate::{
    SporeCustomType, SporeList, SporeRc, SporeStruct,
    builtins::register_builtins,
    compiler::ast::Ast,
    error::{VmError, VmErrorWithContext, VmResult},
    gc::{ObjectId, Objects},
    instruction::Instruction,
    module::Module,
    val::{
        ShortString, Val, bytecode_function::ByteCodeFunction, identifier::IdentifierId,
        native_function::NativeFunction,
    },
};

#[derive(Debug)]
/// The virtual machine.
///
/// The Vm struct represents the virtual machine that executes bytecode.
pub struct Vm {
    pub(crate) globals: Module,
    pub(crate) stack: Vec<Val>,
    stack_frame: StackFrame,
    previous_stack_frames: Vec<StackFrame>,
    /// The object store.
    pub(crate) objects: Objects,
}

#[derive(Debug, Default)]
pub struct StackFrame {
    stack_start: usize,
    bytecode_idx: usize,
    function: Option<ObjectId<ByteCodeFunction>>,
    instructions: SporeRc<[Instruction]>,
}

impl StackFrame {
    /// Returns the backing function id or `None` if the current function is a native function.
    pub fn function_id(&self) -> Option<ObjectId<ByteCodeFunction>> {
        self.function
    }
}

impl Default for Vm {
    fn default() -> Vm {
        let mut vm = Vm {
            globals: Module::new(),
            stack: Vec::with_capacity(4096),
            stack_frame: StackFrame::default(),
            previous_stack_frames: Vec::with_capacity(64),
            objects: Objects::default(),
        };
        register_builtins(&mut vm);
        vm
    }
}

impl Vm {
    /// Apply `f` to `self` and return the value.
    pub fn with(self, f: impl Fn(&mut Vm)) -> Vm {
        let mut vm = self;
        vm.apply_mut(f);
        vm
    }

    /// Apply `f` to `Vm` and return a mutable reference to self.
    pub fn apply_mut(&mut self, f: impl Fn(&mut Vm)) -> &mut Vm {
        f(self);
        self
    }

    /// Registers a native function in the VM.
    ///
    /// This function registers a native (Rust) function with the VM, making it callable
    /// from spore code. It checks for name conflicts and adds the function to the
    /// global module.
    pub fn register_native_function(&mut self, f: NativeFunction) -> &mut Self {
        let symbol = self.objects.symbols.make_identifier_id(f.name());
        assert!(
            !self.globals.values.contains_key(&symbol),
            "register_function called with existing function named {name}.",
            name = f.name()
        );
        let id = self
            .objects
            .native_functions
            .register(f, self.objects.reachable_color.swap());
        self.globals.values.insert(symbol, Val::NativeFunction(id));
        self
    }

    /// Set a global value.
    pub fn set_global(&mut self, symbol: IdentifierId, value: Val) {
        self.globals.values.insert(symbol, value);
    }

    /// Set a global value.
    pub fn set_global_by_name(&mut self, name: &str, value: Val) {
        let symbol = self.make_identifier_id(name);
        self.globals.values.insert(symbol, value);
    }

    /// Get a global value or `None` if it does not exist.
    pub fn get_global(&self, symbol: IdentifierId) -> Option<Val> {
        self.globals.values.get(&symbol).copied()
    }

    /// Get a global value by name or `None` if it does not exist.
    pub fn get_global_by_name(&self, name: &str) -> Option<Val> {
        let symbol = self.objects.symbols.identifier_id(name)?;
        self.get_global(symbol)
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
    pub fn symbol_name(&self, symbol_id: IdentifierId) -> Option<&str> {
        self.objects.symbols.identifier(symbol_id)
    }

    /// Make a new identifier and return its id.
    pub fn make_identifier_id(&mut self, name: &str) -> IdentifierId {
        self.objects.symbols.make_identifier_id(name)
    }

    /// Get the id of an identifier or return `None` if it does not exist.
    pub fn identifier_id(&self, name: &str) -> Option<IdentifierId> {
        self.objects.symbols.identifier_id(name)
    }

    /// Make a new identifier and return it as a `Val`.
    pub fn make_symbol(&mut self, name: &str) -> Val {
        Val::Symbol(self.make_identifier_id(name))
    }

    pub fn make_key(&mut self, k: &str) -> Val {
        Val::Key(self.make_identifier_id(k))
    }

    /// Make a new string.
    pub fn make_string(&mut self, s: impl Into<CompactString>) -> Val {
        let s = s.into();
        match ShortString::new(&s) {
            Some(s) => Val::ShortString(s),
            None => {
                let id = self.objects.register_string(s);
                Val::String(id)
            }
        }
    }

    /// Make a new list.
    pub fn make_list(&mut self, lst: impl Into<SporeList>) -> Val {
        let lst = self.objects.register_list(lst.into());
        Val::List(lst)
    }

    /// Make a new box containg value.
    pub fn make_box(&mut self, value: Val) -> Val {
        let b = self.objects.register_box(value);
        Val::Box(b)
    }

    /// Make a new struct.
    pub fn make_struct(&mut self, strct: impl Into<SporeStruct>) -> Val {
        let strct = self.objects.register_struct(strct.into());
        Val::Struct(strct)
    }

    /// Make a new custom value.
    pub fn make_custom(&mut self, custom: impl SporeCustomType) -> Val {
        let id = self.objects.register_custom(custom);
        Val::Custom(id)
    }
}

impl Vm {
    /// Evaluates the function `f` with `args`.
    pub fn clean_eval_function(&mut self, f: Val, args: &[Val]) -> VmResult<Val> {
        self.stack.clear();
        self.eval_function(f, args)
    }

    /// Evaluates a string of Spore code.
    ///
    /// Note: This should not be used in a Spore Native Function as it resets the evaluation.
    pub fn clean_eval_str<'a>(&'a mut self, s: &'a str) -> Result<Val, VmErrorWithContext<'a>> {
        let mut inner = || -> VmResult<Val> {
            self.stack.clear();
            let asts = Ast::with_source(s)?;
            let bytecode =
                ByteCodeFunction::with_module_source(self, s, asts.iter(), &Bump::new())?;
            let bytecode_id = self.objects.register_bytecode(bytecode);
            self.eval_function(
                Val::BytecodeFunction {
                    id: bytecode_id,
                    captures: None,
                },
                &[],
            )
        };
        match inner() {
            Ok(val) => Ok(val),
            Err(err) => Err(err.with_context(self, s)),
        }
    }

    /// Evaluates the AST of Spore code.
    ///
    /// This is similar to `clean_eval_str`, but is more efficient if the AST is already built up.
    ///
    /// Note: This should not be used in a Spore Native Function as it resets the evaluation.
    pub fn clean_eval_ast<'a>(
        &'a mut self,
        s: &'a str,
        ast: &Ast,
    ) -> Result<Val, VmErrorWithContext<'a>> {
        let mut inner = || -> VmResult<Val> {
            self.stack.clear();
            let bytecode =
                ByteCodeFunction::with_module_source(self, s, std::iter::once(ast), &Bump::new())?;
            let bytecode_id = self.objects.register_bytecode(bytecode);
            self.eval_function(
                Val::BytecodeFunction {
                    id: bytecode_id,
                    captures: None,
                },
                &[],
            )
        };
        match inner() {
            Ok(val) => Ok(val),
            Err(err) => Err(err.with_context(self, s)),
        }
    }

    /// Evaluate a function and return its result.
    ///
    /// Note: This does not reset the environment so pending computations and errors will not be
    /// cleared.
    pub fn eval_function(&mut self, f: Val, args: &[Val]) -> VmResult<Val> {
        let initial_stack_frames = self.previous_stack_frames.len();
        self.stack.push(f);
        self.stack.extend_from_slice(args);
        self.execute_eval(1 + args.len())?;
        self.run(initial_stack_frames)
    }

    fn run(&mut self, stop_stack_frame: usize) -> VmResult<Val> {
        while self.previous_stack_frames.len() != stop_stack_frame {
            self.run_next()?;
        }
        let res = self.stack.pop().expect("Value should exist");
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
            Instruction::Set(idx) => {
                let idx = self.stack_frame.stack_start + idx;
                let v = self.stack.pop().unwrap();
                self.stack[idx] = v;
            }
            Instruction::Deref(symbol) => {
                let v = match self.globals.values.get(symbol) {
                    Some(v) => *v,
                    None => return Err(VmError::IdentifierNotFound(*symbol))?,
                };
                self.stack.push(v);
            }
            Instruction::Jump(n) => self.execute_jump(*n),
            Instruction::JumpIf(n) => self.execute_jump_if(*n)?,
            Instruction::Compact(n) => self.execute_compact(*n),
            Instruction::Return => self.execute_return(),
            Instruction::Capture { id, capture_count } => self.execute_capture(*id, *capture_count),
        }
        Ok(())
    }

    /// Executes a return instruction.
    ///
    /// This function handles the `Return` instruction, which involves restoring the previous
    /// stack frame, retrieving the return value, and updating the stack.
    fn execute_return(&mut self) {
        let stack_start = self.stack_frame.stack_start;
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
                        function: None,
                        instructions: self.objects.null_bytecode.instructions.clone(),
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
            Val::BytecodeFunction {
                id: bytecode_function,
                captures,
            } => {
                let function = self
                    .objects
                    .bytecode_functions
                    .get(bytecode_function)
                    .unwrap();
                let arg_count = n as u32 - 1;
                if function.args != arg_count {
                    return Err(VmError::WrongArity {
                        function_name: function
                            .name
                            .clone()
                            .unwrap_or(CompactString::const_new("")),
                        expected: function.args,
                        actual: arg_count,
                    })?;
                }
                self.stack
                    .extend(std::iter::repeat_n(Val::Void, function.locals as usize));
                let actual_captures = match captures {
                    Some(captures) => {
                        let captures = self.objects.get_list(captures).ok_or_else(|| {
                            VmError::InterpreterBug(CompactString::new(
                                "captures not registered with VM",
                            ))
                        })?;
                        self.stack.extend_from_slice(captures);
                        captures.len() as u32
                    }
                    None => 0,
                };
                if actual_captures != function.captures {
                    return Err(VmError::InterpreterBug(CompactString::new(
                        "wrong number of captures for lambda",
                    )))?;
                }
                let previous_frame = std::mem::replace(
                    &mut self.stack_frame,
                    StackFrame {
                        stack_start,
                        bytecode_idx: 0,
                        function: Some(bytecode_function),
                        instructions: function.instructions.clone(),
                    },
                );
                self.previous_stack_frames.push(previous_frame);
            }
            v => return Err(VmError::NotCallable(v))?,
        }
        Ok(())
    }

    /// Execute an instruction jump.
    fn execute_jump(&mut self, n: usize) {
        self.stack_frame.bytecode_idx += n;
    }

    /// Execute an instruction jump if the top value in the stack is `true`. This will consume the
    /// top value.
    fn execute_jump_if(&mut self, n: usize) -> VmResult<()> {
        let v = self.stack.pop().ok_or_else(|| {
            VmError::InterpreterBug("jump_if called with no value on stack".into())
        })?;
        if v.is_truthy() {
            self.execute_jump(n);
        }
        Ok(())
    }

    fn execute_compact(&mut self, n: usize) {
        assert!(n > 0);
        self.stack.drain(self.stack.len() - n..self.stack.len() - 1);
    }

    fn execute_capture(&mut self, id: ObjectId<ByteCodeFunction>, capture_count: u32) {
        let stack_start = self.stack.len() - capture_count as usize;
        let captures: Vec<_> = self.stack.drain(stack_start..).collect();
        let captures_id = self.objects.register_list(captures);
        self.stack.push(Val::BytecodeFunction {
            id,
            captures: Some(captures_id),
        });
    }
}

impl Vm {
    /// Run the garbage collector.
    pub fn run_gc(&mut self) -> usize {
        self.objects.run_gc(
            &self.stack,
            self.previous_stack_frames
                .iter()
                .chain(std::iter::once(&self.stack_frame)),
            std::iter::once(&self.globals),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call_calls_function_with_args() {
        assert_eq!(
            Vm::default().clean_eval_str("(+ 1 2 3 4)").unwrap(),
            Val::Int(10)
        );
    }

    #[test]
    fn define_sets_value_of_symbol() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(define x 12)").unwrap(), Val::Void);
        assert_eq!(vm.clean_eval_str("x").unwrap(), Val::Int(12));
    }

    #[test]
    fn define_lambda_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(define x (lambda () 12))").unwrap(),
            Val::Void
        );
        assert_eq!(vm.clean_eval_str("(x)").unwrap(), Val::Int(12));
    }

    #[test]
    fn define_lambda_with_args_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(define x (lambda (a b c) (+ a b c)))")
                .unwrap(),
            Val::Void
        );
        assert_eq!(vm.clean_eval_str("(x 1 2 3)").unwrap(), Val::Int(6));
    }

    #[test]
    fn define_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(define (foo) (+ 1 2 3 4))").unwrap(),
            Val::Void,
        );
        assert_eq!(vm.clean_eval_str("(foo)").unwrap(), Val::Int(10));
    }

    #[test]
    fn function_with_multiple_values_returns_last_value() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(define (foo) 1 2 3 4)").unwrap(),
            Val::Void,
        );
        assert_eq!(vm.clean_eval_str("(foo)").unwrap(), Val::Int(4));
    }

    #[test]
    fn define_with_args_creates_callable_function() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(define (foo a b c) (+ a b c))").unwrap(),
            Val::Void,
        );
        assert_eq!(vm.clean_eval_str("(foo 1 2 3)").unwrap(), Val::Int(6));
    }

    #[test]
    fn function_with_wrong_number_of_args_fails() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(define (foo a b c) (+ a b c))").unwrap(),
            Val::Void
        );
        assert_eq!(
            vm.clean_eval_str("(foo 1)")
                .map_err(VmError::from)
                .unwrap_err(),
            VmError::WrongArity {
                function_name: "foo".into(),
                expected: 3,
                actual: 1
            }
        );
    }

    #[test]
    fn if_with_true_pred_returns_true_branch() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(if (< 1 2) 3 4)").unwrap(), Val::Int(3));
    }

    #[test]
    fn if_with_false_pred_returns_false_branch() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(if (< 2 1) 3 4)").unwrap(), Val::Int(4));
    }

    #[test]
    fn if_with_true_pred_and_no_false_branch_returns_true_branch() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(if (< 1 2) 3)").unwrap(), Val::Int(3));
    }

    #[test]
    fn if_with_false_pred_and_no_false_branch_returns_true_void() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(if (< 2 1) 3)").unwrap(), Val::Void);
    }

    #[test]
    fn recursive_function_call() {
        let mut vm = Vm::default();
        let source = r#"
(define (fib n)
  (if (< n 2) (return n))
  (+ (fib (- n 2)) (fib (- n 1))))"#;
        assert_eq!(vm.clean_eval_str(source).unwrap(), Val::Void);
        assert_eq!(vm.clean_eval_str("(fib 10)").unwrap(), Val::Int(55));
    }

    #[test]
    fn define_in_function_sets_local_variable() {
        let mut vm = Vm::default();
        vm.clean_eval_str(
            r#"
(define x 10)
(define (double-x)
  (define x (+ x x))
  x)
(define (add-x arg)
  (define x (+ arg x))
  x)
"#,
        )
        .unwrap();
        assert_eq!(
            vm.clean_eval_str("(list (double-x) (add-x 20) x)")
                .unwrap()
                .as_list(&vm)
                .unwrap(),
            &[Val::Int(20), Val::Int(30), Val::Int(10)]
        );
    }

    #[test]
    fn variables_are_captured() {
        let mut vm = Vm::default();
        let source = r#"
    (define (add-x-fn x)
      (lambda (y) (+ x y)))

    (define add-2 (add-x-fn 2))"#;
        assert_eq!(
            vm.clean_eval_str(source).map_err(VmError::from),
            Ok(Val::Void)
        );
        let source = "(add-2 10)";
        assert_eq!(vm.clean_eval_str(source).unwrap(), Val::Int(12));
    }
}

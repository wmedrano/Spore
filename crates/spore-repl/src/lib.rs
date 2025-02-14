use rustyline::error::ReadlineError;
use spore_vm::{
    compiler::ast::{Ast, AstError},
    vm::{Vm, VmError, VmResult},
};

/// REPL for Spore VM.
pub struct Repl {
    /// The backing Vm.
    vm: Vm,
    /// The number of expressions that have been evaluated.
    expressions_count: usize,
}

impl Default for Repl {
    fn default() -> Repl {
        Repl::new(Vm::default())
    }
}

impl Repl {
    /// Creates a new REPL.
    pub fn new(vm: Vm) -> Repl {
        Repl {
            vm,
            expressions_count: 0,
        }
    }

    /// Get the underlying Vm.
    pub fn vm(&self) -> &Vm {
        &self.vm
    }

    pub fn run(&mut self) -> VmResult<()> {
        let mut rl = rustyline::DefaultEditor::new().unwrap();
        let mut input_buffer = String::new();
        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(input) => {
                    input_buffer.push_str(&input);
                    match Ast::with_source(&input_buffer) {
                        Err(AstError::UnclosedParen(_)) => {}
                        _ => match self.execute_to_string(&input_buffer) {
                            Ok(out) => println!("{out}"),
                            Err(err) => match err {
                                VmError::InterpreterBug(_) => return Err(err),
                                err => println!("Error: {err}"),
                            },
                        },
                    };
                }
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }
        Ok(())
    }

    /// Executes a line of code in the REPL.
    pub fn execute(&mut self, mut writer: impl std::fmt::Write, input: &str) -> VmResult<()> {
        let val = self.vm.eval_str(input)?;
        write!(
            writer,
            "${n} => {val}",
            n = self.expressions_count,
            val = val.formatted(&self.vm)
        )?;
        self.expressions_count += 1;
        Ok(())
    }

    pub fn execute_to_string(&mut self, input: &str) -> VmResult<String> {
        let mut res = String::new();
        self.execute(&mut res, input)?;
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use spore_vm::compiler::ast::AstError;
    use spore_vm::compiler::span::Span;
    use spore_vm::compiler::CompileError;
    use spore_vm::vm::VmError;

    #[test]
    fn execute_prints_final_value() {
        let mut repl = Repl::new(Vm::default());
        assert_eq!(repl.execute_to_string("(+ 2 2)").unwrap(), "$0 => 4");
    }

    #[test]
    fn execute_numbers_increment() {
        let mut repl = Repl::new(Vm::default());
        assert_eq!(repl.execute_to_string("1").unwrap(), "$0 => 1");
        assert_eq!(repl.execute_to_string("2").unwrap(), "$1 => 2");
        assert_eq!(repl.execute_to_string("3").unwrap(), "$2 => 3");
    }

    #[test]
    fn bad_statement_returns_error() {
        let mut repl = Repl::new(Vm::default());
        assert_eq!(
            repl.execute_to_string(")"),
            Err(VmError::Compile(CompileError::Ast(
                AstError::UnexpectedCloseParen(Span { start: 0, end: 1 })
            )))
        );
    }

    #[test]
    fn runtime_error_returns_error() {
        let mut repl = Repl::new(Vm::default());
        let got = repl.execute_to_string("(undefined)");
        assert_eq!(
            got,
            Err(VmError::SymbolNotFound(repl.vm.make_symbol_id("undefined")))
        );
    }
}

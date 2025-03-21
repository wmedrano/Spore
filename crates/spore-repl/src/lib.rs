use rustyline::error::ReadlineError;
use spore_vm::{
    compiler::sexp::{ParseError, SexpBuilder},
    error::{VmError, VmResult},
    vm::Vm,
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
                    input_buffer.push('\n');
                    match SexpBuilder::new(&input_buffer).last(&mut self.vm) {
                        Some(Err(ParseError::UnclosedParen(_))) => {}
                        None => {}
                        _ => {
                            let code = std::mem::take(&mut input_buffer);
                            let res = self.execute_code(&code);
                            let input_text = &input_buffer;
                            match res {
                                Ok(out) => {
                                    if !out.is_empty() {
                                        println!("{out}")
                                    }
                                }
                                Err(err) => match err {
                                    VmError::InterpreterBug(_) => return Err(err),
                                    err => {
                                        println!(
                                            "Error: {}",
                                            err.with_context(&self.vm, &input_text)
                                        )
                                    }
                                },
                            }
                        }
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

    fn execute_code(&mut self, input: &str) -> VmResult<String> {
        let val = self.vm.clean_eval_str(input)?;
        if val.is_void() {
            return Ok(String::new());
        }
        let res = format!(
            "${n} => {val}",
            n = self.expressions_count,
            val = val.formatted(&self.vm)
        );
        self.expressions_count += 1;
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use spore_vm::compiler::error::ParseOrCompileError;
    use spore_vm::compiler::span::Span;

    #[test]
    fn execute_prints_final_value() {
        let mut repl = Repl::new(Vm::default());
        assert_eq!(repl.execute_code("(+ 2 2)").unwrap(), "$0 => 4");
    }

    #[test]
    fn execute_numbers_increment() {
        let mut repl = Repl::new(Vm::default());
        assert_eq!(repl.execute_code("1").unwrap(), "$0 => 1");
        assert_eq!(repl.execute_code("2").unwrap(), "$1 => 2");
        assert_eq!(repl.execute_code("3").unwrap(), "$2 => 3");
    }

    #[test]
    fn bad_statement_returns_error() {
        let mut repl = Repl::new(Vm::default());
        assert_eq!(
            repl.execute_code(")"),
            Err(VmError::Compile(ParseOrCompileError::Parse(
                ParseError::UnexpectedCloseParen(Span { start: 0, end: 1 })
            )))
        );
    }

    #[test]
    fn runtime_error_returns_error() {
        let mut repl = Repl::new(Vm::default());
        let got = repl.execute_code("(undefined)");
        assert_eq!(
            got,
            Err(VmError::IdentifierNotFound(
                repl.vm.make_identifier_id("undefined")
            ))
        );
    }
}

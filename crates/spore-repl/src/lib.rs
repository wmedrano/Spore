use rustyline::error::ReadlineError;
use spore_vm::{
    compiler::ast::{Ast, AstError},
    vm::{Vm, VmError, VmErrorInner, VmResult},
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
                    match Ast::with_source(&input_buffer) {
                        Err(AstError::UnclosedParen(_)) => {}
                        _ => {
                            let res = self.execute_to_string(&input_buffer);
                            let input_text = std::mem::take(&mut input_buffer);
                            match res {
                                Ok(out) => {
                                    if !out.is_empty() {
                                        println!("{out}")
                                    }
                                }
                                Err(err) => match *err.0 {
                                    VmErrorInner::InterpreterBug(_) => return Err(err),
                                    err => {
                                        println!(
                                            "Error: {}",
                                            VmError(Box::new(err))
                                                .with_context(&self.vm, &input_text)
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

    pub fn execute_to_string(&mut self, input: &str) -> VmResult<String> {
        let asts = Ast::with_source(input)?;
        match asts
            .first()
            .and_then(|ast| ast.leaf_text(input))
            .unwrap_or("")
        {
            ",ast" => self.show_ast(input, &asts[1..]),
            _ => self.execute_code(input, &asts),
        }
    }

    fn show_ast(&self, input: &str, asts: &[Ast]) -> VmResult<String> {
        let mut res = Vec::with_capacity(asts.len());
        for ast in asts {
            res.push(format!("{:#?}", ast.with_text(input)));
        }
        Ok(res.join("\n"))
    }

    fn execute_code(&mut self, input: &str, asts: &[Ast]) -> VmResult<String> {
        let mut res = Vec::with_capacity(asts.len());
        for ast in asts {
            let val = self.vm.clean_eval_ast(input, ast)?;
            if val.is_void() {
                continue;
            }
            res.push(format!(
                "${n} => {val}",
                n = self.expressions_count,
                val = val.formatted(&self.vm)
            ));
            self.expressions_count += 1;
        }
        Ok(res.join("\n"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use spore_vm::compiler::ast::AstError;
    use spore_vm::compiler::error::CompileError;
    use spore_vm::compiler::span::Span;
    use spore_vm::vm::VmErrorInner;

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
            Err(
                VmErrorInner::Compile(CompileError::Ast(AstError::UnexpectedCloseParen(Span {
                    start: 0,
                    end: 1
                })))
                .into()
            )
        );
    }

    #[test]
    fn runtime_error_returns_error() {
        let mut repl = Repl::new(Vm::default());
        let got = repl.execute_to_string("(undefined)");
        assert_eq!(
            got,
            Err(VmErrorInner::SymbolNotFound(repl.vm.make_symbol_id("undefined")).into())
        );
    }
}

use spore_vm::vm::Vm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut vm = Vm::default();
    vm.eval_str(
        r#"
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 2)) (fib (- n 1)))))
"#,
    )
    .unwrap();
    vm.eval_str("(fib 34)").unwrap();
    Ok(())
}

use spore_vm::vm::Vm;

fn main() {
    let mut vm = Vm::default();
    let message = vm.eval_str(r#""Hello World!""#).unwrap();
    println!("{}", message.formatted(&vm));
}

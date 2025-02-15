use ratatui::{DefaultTerminal, Frame};
use spore_vm::{val::Val, vm::Vm};

mod buffer;
mod events;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut vm = Vm::default().apply(buffer::register_buffer);
    let terminal = ratatui::init();
    let result = run(&mut vm, terminal);
    ratatui::restore();
    spore_repl::Repl::new(vm).run()?;
    result
}

fn run(vm: &mut Vm, mut terminal: DefaultTerminal) -> Result<(), Box<dyn std::error::Error>> {
    vm.eval_str(
        r#"
(define exit? false)
(define text (new-buffer ""))

(define (handle-event! event)
  (if (< (string-len event) 2)
    (buffer-append! text event)
    (define exit? true)))
"#,
    )
    .unwrap();
    let exit_sym = vm.make_symbol_id("exit?");
    while !vm.get_global(exit_sym).unwrap_or_default().is_truthy() {
        let text = vm
            .eval_str("(buffer->string text)")
            .unwrap()
            .as_str(&vm)
            .unwrap();
        terminal.draw(|frame: &mut Frame| frame.render_widget(text, frame.area()))?;
        handle_events(vm);
    }
    Ok(())
}

fn handle_events(vm: &mut Vm) {
    let sym = vm.make_symbol_id("tmp-event");
    vm.set_global(sym, Val::Void);
    for event in events::events() {
        let s = vm.make_string(event.clone());
        vm.set_global(sym, s);
        vm.eval_str("(handle-event! tmp-event)").unwrap();
    }
}

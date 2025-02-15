use compact_str::CompactString;
use ratatui::{DefaultTerminal, Frame};
use spore_vm::{val::symbol::SymbolId, vm::Vm};

mod events;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let terminal = ratatui::init();
    let result = run(terminal);
    ratatui::restore();
    result
}

fn run(mut terminal: DefaultTerminal) -> Result<(), Box<dyn std::error::Error>> {
    let mut vm = Vm::default();
    let text_sym = vm.make_symbol_id("text");
    set_text(&mut vm, text_sym, CompactString::const_new(""));
    loop {
        let text = vm.get_global(text_sym).unwrap().as_str(&vm).unwrap();
        terminal.draw(|frame: &mut Frame| frame.render_widget(text, frame.area()))?;
        if handle_events(&mut vm, text_sym) == Action::Exit {
            break Ok(());
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Action {
    Continue,
    Exit,
}

fn handle_events(vm: &mut Vm, text_sym: SymbolId) -> Action {
    for event in events::events() {
        if event == "<esc>" {
            return Action::Exit;
        }
        set_text(vm, text_sym, event);
    }
    Action::Continue
}

fn set_text(vm: &mut Vm, text_sym: SymbolId, text: CompactString) {
    let val = vm.make_string(text);
    vm.set_global(text_sym, val);
}

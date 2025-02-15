use std::time::{Duration, Instant};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use ratatui::{DefaultTerminal, Frame};
use spore_vm::vm::Vm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let terminal = ratatui::init();
    let result = run(terminal);
    ratatui::restore();
    result
}

fn run(mut terminal: DefaultTerminal) -> Result<(), Box<dyn std::error::Error>> {
    let mut vm = Vm::default();
    vm.eval_str("(define text \"\")").unwrap();
    loop {
        let text = vm.eval_str("text").unwrap().as_str(&vm).unwrap();
        terminal.draw(|frame: &mut Frame| frame.render_widget(text, frame.area()))?;
        if handle_events(&mut vm) == Action::Exit {
            break Ok(());
        }
    }
}

#[derive(PartialEq)]
enum Action {
    Continue,
    Exit,
}

fn handle_events(vm: &mut Vm) -> Action {
    let deadline = Instant::now() + Duration::from_millis(100);
    while let Ok(true) = event::poll(Instant::now().duration_since(deadline)) {
        match event::read().unwrap() {
            Event::Key(KeyEvent {
                code,
                kind: KeyEventKind::Press,
                ..
            }) => {
                if code == KeyCode::Esc {
                    return Action::Exit;
                }
                let sym = vm.make_symbol_id("text");
                let val = vm.make_string(format!("{code:?}"));
                vm.set_global(sym, val);
            }
            _ => (),
        }
    }
    Action::Continue
}

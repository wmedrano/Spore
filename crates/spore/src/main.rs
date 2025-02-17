use std::time::Duration;

use buffer::TextBuffer;
use clap::{Parser, ValueEnum};
use ratatui::{DefaultTerminal, Frame};
use spore_vm::{val::Val, vm::Vm};
use widgets::BufferWidget;

mod buffer;
mod events;
mod shell;
mod widgets;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(value_enum, short, long, default_value = "editor")]
    pub mode: Mode,
}

#[derive(Copy, Clone, Parser, PartialEq, Default, Debug, ValueEnum)]
pub enum Mode {
    #[default]
    Editor,
    Repl,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let mut vm = Vm::default()
        .with(buffer::register_buffer)
        .with(shell::register_shell);
    match args.mode {
        Mode::Editor => {
            let terminal = ratatui::init();
            let result = run(&mut vm, terminal);
            ratatui::restore();
            result
        }
        Mode::Repl => {
            spore_repl::Repl::new(vm).run()?;
            Ok(())
        }
    }
}

fn run(vm: &mut Vm, mut terminal: DefaultTerminal) -> Result<(), Box<dyn std::error::Error>> {
    vm.eval_str(include_str!("main.lisp")).unwrap();
    while !vm
        .get_global_by_name("exit?")
        .unwrap_or_default()
        .is_truthy()
    {
        let cursor = vm
            .get_global_by_name("cursor")
            .unwrap_or(Val::Int(0))
            .as_int()
            .unwrap() as usize;
        let text_buffer_val = vm.eval_str("text").unwrap();
        let text_buffer: &TextBuffer = text_buffer_val.as_custom(&vm).unwrap();
        terminal.draw(|frame: &mut Frame| {
            frame.render_widget(ratatui::widgets::Clear, frame.area());
            frame.render_widget(BufferWidget::new(text_buffer, cursor), frame.area());
        })?;
        handle_events(vm);
    }
    Ok(())
}

fn handle_events(vm: &mut Vm) {
    vm.set_global_by_name("tmp-event", Val::Void);
    for event in events::events(Duration::from_secs(1)) {
        let s = vm.make_string(event.clone());
        vm.set_global_by_name("tmp-event", s);
        vm.eval_str("(handle-event! tmp-event)").unwrap();
    }
}

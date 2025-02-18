use std::time::Duration;

use clap::{Parser, ValueEnum};
use ratatui::{DefaultTerminal, Frame};
use spore_vm::{
    val::{symbol::SymbolId, Val},
    vm::Vm,
};
use widgets::BufferWidget;

mod buffer;
mod events;
mod files;
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
        .with(shell::register_shell)
        .with(files::register_files);
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

#[derive(Copy, Clone)]
pub struct Symbols {
    buffer: SymbolId,
    cursor: SymbolId,
    exit_p: SymbolId,
    screen: SymbolId,
}

impl Symbols {
    fn new(vm: &mut Vm) -> Symbols {
        Symbols {
            buffer: vm.make_symbol_id("buffer"),
            cursor: vm.make_symbol_id("cursor"),
            exit_p: vm.make_symbol_id("exit?"),
            screen: vm.make_symbol_id("screen"),
        }
    }
}

fn run(vm: &mut Vm, mut terminal: DefaultTerminal) -> Result<(), Box<dyn std::error::Error>> {
    let symbols = Symbols::new(vm);
    vm.clean_eval_str(include_str!("main.lisp")).unwrap();
    while !vm
        .get_global(symbols.exit_p)
        .unwrap_or_default()
        .is_truthy()
    {
        vm.run_gc();
        terminal.draw(|frame: &mut Frame| {
            draw(frame, vm, &symbols);
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
        vm.clean_eval_str("(handle-event! tmp-event)").unwrap();
    }
}

fn draw(frame: &mut Frame, vm: &Vm, symbols: &Symbols) {
    let cursor = vm
        .get_global(symbols.cursor)
        .unwrap_or(Val::Int(0))
        .as_int()
        .unwrap() as usize;

    frame.render_widget(ratatui::widgets::Clear, frame.area());
    let screen = vm.get_global(symbols.screen).unwrap().as_list(vm).unwrap();
    for widget in screen {
        let widget_struct = widget.as_struct(vm).unwrap();
        let text_buffer = widget_struct
            .get(&symbols.buffer)
            .unwrap()
            .as_custom(vm)
            .unwrap();
        let cursor = widget_struct
            .get(&symbols.cursor)
            .unwrap_or(&Val::Int(cursor as i64))
            .as_int()
            .unwrap();
        let cursor = if cursor < 0 { 0 } else { cursor as usize };
        frame.render_widget(BufferWidget::new(text_buffer, cursor), frame.area());
    }
}

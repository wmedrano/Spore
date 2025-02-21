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

#[derive(Default, Debug)]
pub struct Stats {
    frames_rendered: usize,
    gc_invocations: usize,
    objects_sweeped: usize,
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
            println!("{stats:#?}", stats = result?);
        }
        Mode::Repl => {
            spore_repl::Repl::new(vm).run()?;
        }
    }
    Ok(())
}

#[derive(Copy, Clone)]
pub struct Symbols {
    buffer: SymbolId,
    cursor: SymbolId,
    exit_p: SymbolId,
    windows: SymbolId,
}

impl Symbols {
    fn new(vm: &mut Vm) -> Symbols {
        Symbols {
            buffer: vm.make_symbol_id("buffer"),
            cursor: vm.make_symbol_id("cursor"),
            exit_p: vm.make_symbol_id("exit?"),
            windows: vm.make_symbol_id("windows"),
        }
    }
}

fn run(vm: &mut Vm, mut terminal: DefaultTerminal) -> Result<Stats, Box<dyn std::error::Error>> {
    let symbols = Symbols::new(vm);
    let mut stats = Stats::default();
    vm.clean_eval_str(include_str!("main.lisp")).unwrap();
    while !vm
        .get_global(symbols.exit_p)
        .unwrap_or_default()
        .is_truthy()
    {
        if stats.frames_rendered % 10 == 0 {
            stats.objects_sweeped += vm.run_gc();
            stats.gc_invocations += 1;
        }
        terminal.draw(|frame: &mut Frame| {
            draw(frame, vm, &symbols);
            stats.frames_rendered += 1;
        })?;
        handle_events(vm);
    }
    Ok(stats)
}

fn handle_events(vm: &mut Vm) {
    for event in events::events(Duration::from_secs(1)) {
        let f = vm.get_global_by_name("handle-event!").unwrap();
        let s = vm.make_string(event.clone());
        vm.clean_eval_function(f, &[s]).unwrap();
    }
}

fn draw(frame: &mut Frame, vm: &Vm, symbols: &Symbols) {
    frame.render_widget(ratatui::widgets::Clear, frame.area());
    let windows = vm.get_global(symbols.windows).unwrap().as_list(vm).unwrap();
    for window in windows {
        let window_struct = window.as_struct(vm).unwrap();
        let text_buffer = window_struct
            .get(&symbols.buffer)
            .unwrap()
            .as_custom(vm)
            .unwrap();
        let cursor = window_struct
            .get(&symbols.cursor)
            .unwrap_or(&Val::Int(0 as i64))
            .as_int()
            .unwrap();
        let cursor = if cursor < 0 { 0 } else { cursor as usize };
        frame.render_widget(BufferWidget::new(text_buffer, cursor), frame.area());
    }
}

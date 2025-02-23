use std::time::Duration;

use clap::{Parser, ValueEnum};
use ratatui::{DefaultTerminal, Frame};
use spore_vm::{
    val::symbol::SymbolId,
    vm::{Vm, VmError},
};
use widgets::BufferWidget;

mod events;
mod files;
mod rope;
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
        .with(rope::register_rope)
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
    cursor: SymbolId,
    exit_p: SymbolId,
    text: SymbolId,
    windows: SymbolId,
}

impl Symbols {
    fn new(vm: &mut Vm) -> Symbols {
        Symbols {
            cursor: vm.make_symbol_id("cursor"),
            exit_p: vm.make_symbol_id("exit?"),
            text: vm.make_symbol_id("text"),
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
        .maybe_unbox(vm)
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
        handle_events(vm).map_err(|err| err.with_context(vm, "").to_string())?;
    }
    Ok(stats)
}

fn handle_events(vm: &mut Vm) -> Result<(), VmError> {
    for event in events::events(Duration::from_secs(1)) {
        let f = vm.get_global_by_name("handle-event!").unwrap();
        let s = vm.make_string(event.clone());
        vm.clean_eval_function(f, &[s])?;
    }
    Ok(())
}

fn draw(frame: &mut Frame, vm: &Vm, symbols: &Symbols) {
    frame.render_widget(ratatui::widgets::Clear, frame.area());
    let windows = vm.get_global(symbols.windows).unwrap().as_list(vm).unwrap();
    for window in windows {
        let window_struct = window.as_struct(vm).unwrap();
        let text = window_struct
            .get(&symbols.text)
            .unwrap()
            .as_custom(vm)
            .unwrap();
        let cursor = window_struct
            .get(&symbols.cursor)
            .map(|x| x.as_int().unwrap())
            .and_then(|x| if x < 0 { None } else { Some(x as usize) });
        frame.render_widget(BufferWidget::new(text, cursor), frame.area());
    }
}

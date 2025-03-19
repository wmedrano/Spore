use std::time::Duration;

use clap::{Parser, ValueEnum};
use ratatui::{
    DefaultTerminal, Frame,
    style::{Color, Style},
    widgets::{Block, BorderType, Borders},
};
use spore_vm::{SporeStruct, error::VmResult, val::identifier::IdentifierId, vm::Vm};
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
    border: IdentifierId,
    title: IdentifierId,
    buffer: IdentifierId,
    cursor: IdentifierId,
    percent_exit_p: IdentifierId,
    percent_windows: IdentifierId,
    text: IdentifierId,
}

impl Symbols {
    fn new(vm: &mut Vm) -> Symbols {
        Symbols {
            border: vm.make_symbol_id("border"),
            title: vm.make_symbol_id("title"),
            buffer: vm.make_symbol_id("buffer"),
            cursor: vm.make_symbol_id("cursor"),
            percent_exit_p: vm.make_symbol_id("%exit?"),
            percent_windows: vm.make_symbol_id("%windows"),
            text: vm.make_symbol_id("text"),
        }
    }
}

fn run(vm: &mut Vm, mut terminal: DefaultTerminal) -> Result<Stats, Box<dyn std::error::Error>> {
    let symbols = Symbols::new(vm);
    let mut stats = Stats::default();
    vm.clean_eval_str(include_str!("../lisp/buffer.lisp"))
        .unwrap();
    vm.clean_eval_str(include_str!("../lisp/window.lisp"))
        .unwrap();
    vm.clean_eval_str(include_str!("../lisp/main.lisp"))
        .unwrap();
    while !vm
        .get_global(symbols.percent_exit_p)
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

fn handle_events(vm: &mut Vm) -> VmResult<()> {
    for event in events::events(Duration::from_secs(1)) {
        let f = vm.get_global_by_name("handle-event!").unwrap();
        let s = vm.make_string(event.clone());
        vm.clean_eval_function(f, &[s])?;
    }
    Ok(())
}

fn draw(frame: &mut Frame, vm: &Vm, symbols: &Symbols) {
    frame.render_widget(ratatui::widgets::Clear, frame.area());
    let windows = vm
        .get_global(symbols.percent_windows)
        .unwrap()
        .maybe_unbox(vm)
        .as_list(vm)
        .unwrap();
    for window in windows {
        let window_struct = window.as_struct(vm).unwrap();
        draw_window(frame, vm, symbols, window_struct);
    }
}

fn draw_window(frame: &mut Frame, vm: &Vm, symbols: &Symbols, window_struct: &SporeStruct) {
    let buffer_struct = window_struct
        .get(&symbols.buffer)
        .expect(":buffer not found in window")
        .as_struct(vm)
        .unwrap();
    let text = buffer_struct
        .get(&symbols.text)
        .expect(":text not found in buffer struct")
        .as_custom(vm)
        .expect(":text for buffer was not a rope");
    let cursor = buffer_struct
        .get(&symbols.cursor)
        .map(|x| x.as_int().unwrap())
        .and_then(|x| if x < 0 { None } else { Some(x as usize) });
    let mut area = frame.area();
    if window_struct
        .get(&symbols.border)
        .map(|v| v.is_truthy())
        .unwrap_or(false)
    {
        let mut block = Block::new()
            .border_type(BorderType::Rounded)
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::White))
            .style(Style::default().bg(Color::Black));
        if let Some(title) = window_struct.get(&symbols.title).and_then(|v| v.as_str(vm)) {
            block = block.title(title);
        };
        frame.render_widget(&block, area);
        area = block.inner(area);
    }
    frame.render_widget(BufferWidget::new(text, cursor), area);
}

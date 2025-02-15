use std::time::{Duration, Instant};

use compact_str::{format_compact, CompactString};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};

pub fn events() -> impl Iterator<Item = CompactString> {
    let deadline = Instant::now() + Duration::from_millis(100);
    std::iter::from_fn(move || {
        let has_event = event::poll(Instant::now().duration_since(deadline)).unwrap_or(false);
        if !has_event {
            return None;
        }
        match event::read().ok()? {
            Event::Key(KeyEvent {
                code,
                modifiers,
                kind: KeyEventKind::Press,
                ..
            }) => {
                let has_shift = modifiers.contains(KeyModifiers::SHIFT);
                let prefix = if has_shift { "s-" } else { "" };
                match code {
                    KeyCode::Backspace => Some(format_compact!("<{prefix}backspace>")),
                    KeyCode::Enter => Some(format_compact!("<{prefix}enter>")),
                    KeyCode::Left => Some(format_compact!("<{prefix}left>")),
                    KeyCode::Right => Some(format_compact!("<{prefix}right>")),
                    KeyCode::Up => Some(format_compact!("<{prefix}up>")),
                    KeyCode::Down => Some(format_compact!("<{prefix}down>")),
                    KeyCode::Home => Some(format_compact!("<{prefix}home>")),
                    KeyCode::End => Some(format_compact!("<{prefix}end>")),
                    KeyCode::PageUp => Some(format_compact!("<{prefix}page-up>")),
                    KeyCode::PageDown => Some(format_compact!("<{prefix}page-down>")),
                    KeyCode::Tab => Some(format_compact!("<{prefix}tab>")),
                    KeyCode::BackTab => Some(format_compact!("<{prefix}back-tab>")),
                    KeyCode::Delete => Some(format_compact!("<{prefix}delete>")),
                    KeyCode::Insert => Some(format_compact!("<{prefix}insert>")),
                    KeyCode::F(n) => Some(format_compact!("<{prefix}f{n}>")),
                    KeyCode::Char(ch) => Some(format_compact!("{ch}")),
                    KeyCode::Null => Some(format_compact!("<{prefix}null>")),
                    KeyCode::Esc => Some(format_compact!("<{prefix}esc>")),
                    KeyCode::CapsLock => Some(format_compact!("<{prefix}caps-lock>")),
                    KeyCode::ScrollLock => Some(format_compact!("<{prefix}scroll-lock>")),
                    KeyCode::NumLock => Some(format_compact!("<{prefix}num-lock>")),
                    KeyCode::PrintScreen => Some(format_compact!("<{prefix}print-screen>")),
                    KeyCode::Pause => Some(format_compact!("<{prefix}pause>")),
                    KeyCode::Menu => Some(format_compact!("<{prefix}menu>")),
                    KeyCode::KeypadBegin => Some(format_compact!("<{prefix}keypad-begin>")),
                    KeyCode::Media(_media_key_code) => Some(format_compact!("<{prefix}media>")),
                    KeyCode::Modifier(_modifier_key_code) => {
                        Some(format_compact!("<{prefix}modifier>"))
                    }
                }
            }
            _ => Some(CompactString::const_new("<unsupported>")),
        }
    })
}

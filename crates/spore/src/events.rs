use std::time::Duration;

use compact_str::{format_compact, CompactString};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};

pub fn events(timeout: Duration) -> impl Iterator<Item = CompactString> {
    let mut has_event = event::poll(timeout).unwrap_or(false);
    std::iter::from_fn(move || {
        if !has_event {
            return None;
        }
        let ret = match event::read().ok()? {
            Event::Key(KeyEvent {
                code,
                modifiers,
                kind: KeyEventKind::Press,
                ..
            }) => {
                let is_char = matches!(code, KeyCode::Char(_));
                let mut prefix = CompactString::new("");
                if modifiers.contains(KeyModifiers::CONTROL) {
                    prefix.push_str("c-");
                }
                if modifiers.contains(KeyModifiers::ALT) {
                    prefix.push_str("a-");
                }
                if modifiers.contains(KeyModifiers::SHIFT) && !is_char {
                    prefix.push_str("s-");
                }
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
                    KeyCode::Char(ch) => {
                        if prefix.is_empty() {
                            Some(format_compact!("{ch}"))
                        } else {
                            Some(format_compact!("<{prefix}{ch}>"))
                        }
                    }
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
        };
        has_event = event::poll(Duration::from_secs(0)).unwrap_or(false);
        ret
    })
}

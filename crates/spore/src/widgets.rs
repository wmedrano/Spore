use std::borrow::Cow;

use crop::Rope;
use ratatui::{
    prelude::{Buffer, Rect},
    style::{Color, Style},
    widgets::Widget,
};

#[derive(Copy, Clone)]
pub struct BufferWidgetTheme {
    foreground: Color,
    background: Color,
    cursor: Color,
}

impl Default for BufferWidgetTheme {
    fn default() -> BufferWidgetTheme {
        BufferWidgetTheme {
            foreground: Color::White,
            background: Color::Black,
            cursor: Color::LightCyan,
        }
    }
}

pub struct BufferWidget<'a> {
    text: &'a Rope,
    cursor_x: u16,
    cursor_y: u16,
}

impl<'a> BufferWidget<'a> {
    pub fn new(text: &'a crate::buffer::TextBuffer, cursor: usize) -> BufferWidget<'a> {
        let cursor_y = text.text.line_of_byte(cursor) as u16;
        let cursor_x = (cursor - text.text.byte_of_line(cursor_y as usize)) as u16;
        BufferWidget {
            text: &text.text,
            cursor_x,
            cursor_y,
        }
    }

    fn render_text(&self, area: Rect, buf: &mut Buffer) {
        let theme = BufferWidgetTheme::default();
        let mut text_lines = self.text.lines();
        for y in area.y..area.bottom() {
            let mut text_line = text_lines.next().map(|l| l.graphemes());
            for x in area.x..area.right() {
                let string = text_line
                    .as_mut()
                    .and_then(|iter| iter.next())
                    .unwrap_or(Cow::Borrowed(" "));
                let style = if (self.cursor_x, self.cursor_y) == (x, y) {
                    Style::reset().bg(theme.cursor).fg(theme.foreground)
                } else {
                    Style::reset().bg(theme.background).fg(theme.foreground)
                };
                buf[(x, y)].set_symbol(&string).set_style(style);
            }
        }
    }
}

impl<'a> Widget for BufferWidget<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        self.render_text(area, buf);
    }
}

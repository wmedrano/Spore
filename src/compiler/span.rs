#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn text(self, source: &str) -> &str {
        &source[self.start as usize..self.end as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_returns_correct_substring() {
        let source = "hello world";
        let span = Span { start: 0, end: 5 };
        assert_eq!(span.text(source), "hello");

        let span = Span { start: 6, end: 11 };
        assert_eq!(span.text(source), "world");
    }
}

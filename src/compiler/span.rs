#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// Represents a span of text in the source code.
pub struct Span {
    /// The start index of the span.
    pub start: u32,
    /// The end index of the span.
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

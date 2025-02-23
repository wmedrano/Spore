#[derive(Copy, Clone, Default, PartialEq, Eq, Debug)]
/// Represents a span of text in the source code.
pub struct Span {
    /// The start index of the span.
    pub start: u32,
    /// The end index of the span.
    pub end: u32,
}

impl Span {
    /// Returns the text of the span.
    pub fn text(self, source: &str) -> &str {
        &source[self.start as usize..self.end as usize]
    }

    pub fn expand(self, other: Span) -> Span {
        if other.is_empty() {
            return self;
        }
        if self.is_empty() {
            return other;
        }
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn len(self) -> usize {
        (self.end - self.start) as usize
    }

    pub fn is_empty(self) -> bool {
        self.start == self.end
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{start}..{end}", start = self.start, end = self.end)
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

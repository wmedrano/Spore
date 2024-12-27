#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn context(self, source: &str) -> &str {
        &source[self.start as usize..self.end as usize]
    }
}

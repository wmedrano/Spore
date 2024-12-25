use std::{iter::Peekable, str::CharIndices};

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

pub fn tokenize<'a>(source: &'a str) -> impl 'a + Iterator<Item = Span> {
    let mut source_iter = source.char_indices().peekable();
    std::iter::from_fn(move || loop {
        let (idx, ch) = source_iter.peek().cloned()?;
        match ch {
            ch if ch.is_whitespace() => {
                source_iter.next();
            }
            '(' | ')' => {
                source_iter.next();
                let start = idx as u32;
                return Some(Span {
                    start,
                    end: start + 1,
                });
            }
            _ => return Some(parse_token(idx as u32, &mut source_iter)),
        }
    })
}

fn parse_token(start: u32, source_iter: &mut Peekable<CharIndices>) -> Span {
    let mut end = start;
    loop {
        let (idx, ch) = match source_iter.peek().cloned() {
            None => {
                return Span {
                    start,
                    end: end + 1,
                }
            }
            Some(x) => x,
        };
        end = idx as u32;
        match ch {
            ch if ch.is_whitespace() => return Span { start, end },
            '(' | ')' => return Span { start, end },
            _ => {
                source_iter.next();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize_to_vec(source: &str) -> Vec<&str> {
        tokenize(source).map(|span| span.context(source)).collect()
    }

    #[test]
    fn empty_str_is_empty() {
        assert_eq!(tokenize_to_vec(""), Vec::<&str>::new());
    }

    #[test]
    fn whitespace_is_empty() {
        assert_eq!(tokenize_to_vec(" \n \t "), Vec::<&str>::new());
    }

    #[test]
    fn words_are_parsed() {
        assert_eq!(
            tokenize_to_vec("hello world 1 2.0 three"),
            vec!["hello", "world", "1", "2.0", "three"]
        );
    }

    #[test]
    fn expressions_are_parsed() {
        assert_eq!(
            tokenize_to_vec("(+ (- 1 2) (/ 4 5))"),
            vec!["(", "+", "(", "-", "1", "2", ")", "(", "/", "4", "5", ")", ")"],
        );
    }
}

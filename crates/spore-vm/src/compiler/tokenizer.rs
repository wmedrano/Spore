use std::{iter::Peekable, str::CharIndices};

use super::span::Span;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// Represents a token in the source code.
pub struct Token {
    /// The span of the token in the source code.
    pub span: Span,
    /// The type of the token.
    pub token_type: TokenType,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// Represents the type of a token.
pub enum TokenType {
    /// An open parenthesis.
    OpenParen,
    /// A close parenthesis.
    CloseParen,
    /// An identifier.
    Identifier,
}

impl Token {
    /// Returns the text of the token.
    pub fn text(self, source: &str) -> &str {
        self.span.text(source)
    }
}

/// Tokenizes a string of source code.
pub fn tokenize(source: &str) -> impl '_ + Iterator<Item = Token> {
    let mut source_iter = source.char_indices().peekable();
    std::iter::from_fn(move || loop {
        let (idx, ch) = source_iter.peek().cloned()?;
        match ch {
            ch if ch.is_whitespace() => {
                source_iter.next();
            }
            '(' | ')' => {
                source_iter.next();
                let span = Span {
                    start: idx as u32,
                    end: idx as u32 + 1,
                };
                return Some(Token {
                    span,
                    token_type: if ch == '(' {
                        TokenType::OpenParen
                    } else {
                        TokenType::CloseParen
                    },
                });
            }
            '"' => return Some(parse_string(idx as u32, &mut source_iter)),
            _ => return Some(parse_token(idx as u32, &mut source_iter)),
        }
    })
}

/// Parses a string token from the source code.
fn parse_string(start: u32, source_iter: &mut Peekable<CharIndices>) -> Token {
    let mut end = start;
    loop {
        let (idx, ch) = match source_iter.next() {
            None => {
                let span = Span {
                    start,
                    end: end + 1,
                };
                return Token {
                    span,
                    token_type: TokenType::Identifier,
                };
            }
            Some(x) => x,
        };
        if idx == 0 {
            assert_eq!(ch, '"');
        }
        end = idx as u32;
        if ch == '"' && idx != 0 {
            return Token {
                span: Span {
                    start,
                    end: end + 1,
                },
                token_type: TokenType::Identifier,
            };
        }
    }
}

/// Parses a token from the source code.
fn parse_token(start: u32, source_iter: &mut Peekable<CharIndices>) -> Token {
    let mut end = start;
    loop {
        let (idx, ch) = match source_iter.peek().cloned() {
            None => {
                let span = Span {
                    start,
                    end: end + 1,
                };
                return Token {
                    span,
                    token_type: TokenType::Identifier,
                };
            }
            Some(x) => x,
        };
        end = idx as u32;
        let span = Span { start, end };
        match ch {
            ch if ch.is_whitespace() => {
                return Token {
                    span,
                    token_type: TokenType::Identifier,
                }
            }
            '(' | ')' => {
                return Token {
                    span,
                    token_type: TokenType::Identifier,
                }
            }
            _ => {
                source_iter.next();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize_to_vec(source: &str) -> Vec<(&str, Token)> {
        tokenize(source).map(|t| (t.text(source), t)).collect()
    }

    #[test]
    fn empty_str_is_empty() {
        assert_eq!(tokenize_to_vec(""), Vec::<_>::new());
    }

    #[test]
    fn whitespace_is_empty() {
        assert_eq!(
            tokenize_to_vec(
                " 
 	 "
            ),
            Vec::<_>::new()
        );
    }

    #[test]
    fn quotes_surrounded_text_is_parsed_as_single_token() {
        assert_eq!(
            tokenize_to_vec("\"hello world\""),
            vec![(
                "\"hello world\"",
                Token {
                    span: Span { start: 0, end: 13 },
                    token_type: TokenType::Identifier
                }
            )]
        );
    }

    #[test]
    fn words_are_parsed() {
        assert_eq!(
            tokenize_to_vec("hello world 1 2.0 three"),
            vec![
                (
                    "hello",
                    Token {
                        span: Span { start: 0, end: 5 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "world",
                    Token {
                        span: Span { start: 6, end: 11 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "1",
                    Token {
                        span: Span { start: 12, end: 13 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "2.0",
                    Token {
                        span: Span { start: 14, end: 17 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "three",
                    Token {
                        span: Span { start: 18, end: 23 },
                        token_type: TokenType::Identifier
                    }
                )
            ],
        );
    }

    #[test]
    fn expressions_are_parsed() {
        assert_eq!(
            tokenize_to_vec("(+ (- 1 2) (/ 4 5))"),
            vec![
                (
                    "(",
                    Token {
                        span: Span { start: 0, end: 1 },
                        token_type: TokenType::OpenParen
                    }
                ),
                (
                    "+",
                    Token {
                        span: Span { start: 1, end: 2 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "(",
                    Token {
                        span: Span { start: 3, end: 4 },
                        token_type: TokenType::OpenParen
                    }
                ),
                (
                    "-",
                    Token {
                        span: Span { start: 4, end: 5 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "1",
                    Token {
                        span: Span { start: 6, end: 7 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "2",
                    Token {
                        span: Span { start: 8, end: 9 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    ")",
                    Token {
                        span: Span { start: 9, end: 10 },
                        token_type: TokenType::CloseParen
                    }
                ),
                (
                    "(",
                    Token {
                        span: Span { start: 11, end: 12 },
                        token_type: TokenType::OpenParen
                    }
                ),
                (
                    "/",
                    Token {
                        span: Span { start: 12, end: 13 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "4",
                    Token {
                        span: Span { start: 14, end: 15 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "5",
                    Token {
                        span: Span { start: 16, end: 17 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    ")",
                    Token {
                        span: Span { start: 17, end: 18 },
                        token_type: TokenType::CloseParen
                    }
                ),
                (
                    ")",
                    Token {
                        span: Span { start: 18, end: 19 },
                        token_type: TokenType::CloseParen
                    }
                )
            ],
        );
    }
}

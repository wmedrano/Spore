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
    /// A string.
    String,
    /// A comment.
    Comment,
}

impl Token {
    /// Returns the text of the token.
    pub fn text(self, source: &str) -> &str {
        self.span.text(source)
    }
}

pub struct Tokenizer<'a> {
    source_iter: Peekable<CharIndices<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Tokenizer<'a> {
        let source_iter = source.char_indices().peekable();
        Tokenizer { source_iter }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (idx, ch) = self.source_iter.peek().cloned()?;
            match ch {
                ch if ch.is_whitespace() => {
                    self.source_iter.next();
                }
                '(' | ')' => {
                    self.source_iter.next();
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
                '"' => {
                    return Some(parse_string(idx as u32, &mut self.source_iter));
                }
                ';' => {
                    return Some(parse_comment(idx as u32, &mut self.source_iter));
                }
                _ => return Some(parse_token(idx as u32, &mut self.source_iter)),
            }
        }
    }
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
                    token_type: TokenType::String,
                };
            }
            Some(x) => x,
        };
        end = idx as u32;
        if idx as u32 == start {
            assert_eq!(ch, '"');
        } else if ch == '"' {
            return Token {
                span: Span {
                    start,
                    end: end + 1,
                },
                token_type: TokenType::String,
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
                };
            }
            '(' | ')' => {
                return Token {
                    span,
                    token_type: TokenType::Identifier,
                };
            }
            _ => {
                source_iter.next();
            }
        }
    }
}

/// Parses a comment token from the source code.
fn parse_comment(start: u32, source_iter: &mut Peekable<CharIndices>) -> Token {
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
                    token_type: TokenType::Comment,
                };
            }
            Some(x) => x,
        };
        end = idx as u32;
        if idx as u32 == start {
            assert_eq!(ch, ';');
        } else if ch == '\n' {
            return Token {
                span: Span {
                    start,
                    end: end + 1,
                },
                token_type: TokenType::Comment,
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize_to_vec(source: &str) -> Vec<(&str, Token)> {
        Tokenizer::new(source)
            .map(|t| (t.text(source), t))
            .collect()
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
            tokenize_to_vec("\"hello\" \"world\""),
            vec![
                (
                    "\"hello\"",
                    Token {
                        span: Span { start: 0, end: 7 },
                        token_type: TokenType::String
                    }
                ),
                (
                    "\"world\"",
                    Token {
                        span: Span { start: 8, end: 15 },
                        token_type: TokenType::String
                    }
                )
            ]
        );
    }

    #[test]
    fn newline_in_strings_is_preservide() {
        assert_eq!(
            tokenize_to_vec(
                r#"
"multi line
string"
"#
            ),
            vec![(
                "\"multi line\nstring\"",
                Token {
                    span: Span { start: 1, end: 20 },
                    token_type: TokenType::String
                }
            ),]
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

    #[test]
    fn comments_are_parsed_until_newline() {
        assert_eq!(
            tokenize_to_vec(
                r#"
(+ 2 2) ;; This should be equal to 4.
;; This is a new "comment", unrelated to (+ 2 2)
"#
            ),
            vec![
                (
                    "(",
                    Token {
                        span: Span { start: 1, end: 2 },
                        token_type: TokenType::OpenParen
                    }
                ),
                (
                    "+",
                    Token {
                        span: Span { start: 2, end: 3 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "2",
                    Token {
                        span: Span { start: 4, end: 5 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    "2",
                    Token {
                        span: Span { start: 6, end: 7 },
                        token_type: TokenType::Identifier
                    }
                ),
                (
                    ")",
                    Token {
                        span: Span { start: 7, end: 8 },
                        token_type: TokenType::CloseParen
                    }
                ),
                (
                    ";; This should be equal to 4.\n",
                    Token {
                        span: Span { start: 9, end: 39 },
                        token_type: TokenType::Comment
                    }
                ),
                (
                    ";; This is a new \"comment\", unrelated to (+ 2 2)\n",
                    Token {
                        span: Span { start: 39, end: 88 },
                        token_type: TokenType::Comment
                    }
                )
            ]
        );
    }
}

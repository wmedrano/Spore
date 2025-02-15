use super::{
    span::Span,
    tokenizer::{tokenize, Token, TokenType},
};

#[derive(Clone, Debug, PartialEq)]
/// Represents an Abstract Syntax Tree.
pub enum Ast {
    /// Represents a tree node with a span and children.
    Tree { span: Span, children: Vec<Ast> },
    /// Represents a leaf node with a span.
    Leaf { span: Span },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// Represents an error that can occur during AST building.
pub enum AstError {
    /// Represents an unclosed parenthesis.
    UnclosedParen(Span),
    /// Represents an unexpected close parenthesis.
    UnexpectedCloseParen(Span),
}

impl std::error::Error for AstError {}

impl std::fmt::Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstError::UnclosedParen(span) => {
                write!(f, "unclosed parenthesis encountered at {span}")
            }
            AstError::UnexpectedCloseParen(span) => {
                write!(f, "found unexpected close parenthesis at {span}")
            }
        }
    }
}

impl Ast {
    /// Creates a vector of ASTs from a source string.
    pub fn with_source(source: &str) -> Result<Vec<Self>, AstError> {
        let mut asts = Vec::new();
        let mut tokens = tokenize(source);
        while let Some(ast_res) = Ast::next_ast(&mut tokens) {
            let ast = ast_res?;
            asts.push(ast);
        }
        Ok(asts)
    }

    /// Parses the next AST from the token stream.
    fn next_ast(tokens: &mut impl Iterator<Item = Token>) -> Option<Result<Self, AstError>> {
        let token = tokens.next()?;
        match token.token_type {
            TokenType::Identifier => Some(Ok(Ast::Leaf { span: token.span })),
            TokenType::OpenParen => {
                let (sub_span, sub_ast) = match Ast::parse_until_close(token.span.start, tokens) {
                    Ok(x) => x,
                    Err(err) => return Some(Err(err)),
                };
                Some(Ok(Ast::Tree {
                    span: sub_span,
                    children: sub_ast,
                }))
            }
            TokenType::CloseParen => Some(Err(AstError::UnexpectedCloseParen(token.span))),
        }
    }

    /// Parses ASTs until a closing parenthesis is encountered.
    fn parse_until_close(
        span_start: u32,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Result<(Span, Vec<Self>), AstError> {
        let mut asts = Vec::new();
        while let Some(token) = tokens.next() {
            match token.token_type {
                TokenType::Identifier => asts.push(Ast::Leaf { span: token.span }),
                TokenType::OpenParen => {
                    let (sub_span, sub_ast) = Ast::parse_until_close(token.span.start, tokens)?;
                    asts.push(Ast::Tree {
                        span: sub_span,
                        children: sub_ast,
                    });
                }
                TokenType::CloseParen => {
                    let span = Span {
                        start: span_start,
                        end: token.span.end,
                    };
                    return Ok((span, asts));
                }
            }
        }
        Err(AstError::UnclosedParen(Span {
            start: span_start,
            end: span_start + 1,
        }))
    }
}

impl Ast {
    /// Returns the span of the AST.
    pub fn span(&self) -> Span {
        match self {
            Ast::Leaf { span } => *span,
            Ast::Tree { span, .. } => *span,
        }
    }

    /// Returns the text of the leaf node or `None` if `self` is not a leaf node.
    pub fn leaf_text<'a>(&self, source: &'a str) -> Option<&'a str> {
        match self {
            Ast::Leaf { span } => Some(span.text(source)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_source_has_empty_ast() {
        assert_eq!(Ast::with_source("").unwrap().len(), 0);
    }

    #[test]
    fn single_identifier_has_single_leaf() {
        assert_eq!(
            Ast::with_source("ident").unwrap()[0],
            Ast::Leaf {
                span: Span { start: 0, end: 5 }
            }
        );
    }

    #[test]
    fn several_items_make_list_of_leafs() {
        assert_eq!(
            Ast::with_source("a b c").unwrap().as_slice(),
            &[
                Ast::Leaf {
                    span: Span { start: 0, end: 1 }
                },
                Ast::Leaf {
                    span: Span { start: 2, end: 3 }
                },
                Ast::Leaf {
                    span: Span { start: 4, end: 5 }
                },
            ]
        );
    }

    #[test]
    fn parenthesis_make_tree() {
        assert_eq!(
            Ast::with_source("(1 2 3)").unwrap(),
            &[Ast::Tree {
                span: Span { start: 0, end: 7 },
                children: vec![
                    Ast::Leaf {
                        span: Span { start: 1, end: 2 }
                    },
                    Ast::Leaf {
                        span: Span { start: 3, end: 4 }
                    },
                    Ast::Leaf {
                        span: Span { start: 5, end: 6 }
                    }
                ]
            }]
        );
    }

    #[test]
    fn string_is_parsed_as_single_leaf() {
        assert_eq!(
            Ast::with_source("\"hello world\"").unwrap(),
            &[Ast::Leaf {
                span: Span { start: 0, end: 13 }
            }]
        );
    }

    #[test]
    fn nested_parenthesis_form_nested_trees() {
        assert_eq!(
            Ast::with_source("(foo (bar 3))").unwrap(),
            &[Ast::Tree {
                span: Span { start: 0, end: 13 },
                children: vec![
                    Ast::Leaf {
                        span: Span { start: 1, end: 4 }
                    },
                    Ast::Tree {
                        span: Span { start: 5, end: 12 },
                        children: vec![
                            Ast::Leaf {
                                span: Span { start: 6, end: 9 }
                            },
                            Ast::Leaf {
                                span: Span { start: 10, end: 11 }
                            }
                        ]
                    }
                ]
            }]
        );
    }
}

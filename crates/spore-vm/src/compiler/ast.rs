use super::{
    span::Span,
    tokenizer::{tokenize, Token, TokenType},
};

/// Contains an Abstract Syntax Tree.
#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    /// The comment that precedes the AST.
    pub comment: Option<Span>,
    /// The span of text the AST covers.
    pub span: Span,
    /// The contents of the AST node.
    pub node: AstNode,
}

/// An AST node, which is either a leaf node or a subtree.
#[derive(Clone, Debug, PartialEq)]
pub enum AstNode {
    /// A terminal leaf node.
    Leaf,
    /// A subtree.
    Tree(Vec<Ast>),
}

impl Ast {
    /// Returns the text of the leaf node or `None` if `self` is not a leaf node.
    pub fn leaf_text<'a>(&self, source: &'a str) -> Option<&'a str> {
        match &self.node {
            AstNode::Leaf => Some(self.span.text(source)),
            _ => None,
        }
    }

    /// Returns `true` if `self` is a leaf node.
    pub fn is_leaf(&self) -> bool {
        matches!(self.node, AstNode::Leaf { .. })
    }

    /// Returns `true` if `self` contains a subtree.
    pub fn is_tree(&self) -> bool {
        matches!(self.node, AstNode::Tree { .. })
    }

    pub fn with_text<'a>(&self, source: &'a str) -> AstWithText<'a> {
        AstWithText::new(self, source)
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
            TokenType::Identifier => Some(Ok(Ast {
                comment: None,
                span: token.span,
                node: AstNode::Leaf,
            })),
            TokenType::OpenParen => {
                let (sub_span, sub_ast) = match Ast::parse_until_close(token.span.start, tokens) {
                    Ok(x) => x,
                    Err(err) => return Some(Err(err)),
                };
                Some(Ok(Ast {
                    comment: None,
                    span: sub_span,
                    node: AstNode::Tree(sub_ast),
                }))
            }
            TokenType::CloseParen => Some(Err(AstError::UnexpectedCloseParen(token.span))),
            TokenType::Comment => Ast::next_ast(tokens).map(|ast_res| {
                ast_res.map(|ast| Ast {
                    comment: Some(
                        ast.comment
                            .map(|comment| comment.expand(token.span))
                            .unwrap_or(token.span),
                    ),
                    span: ast.span,
                    node: ast.node,
                })
            }),
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
                TokenType::Identifier => asts.push(Ast {
                    comment: None,
                    span: token.span,
                    node: AstNode::Leaf,
                }),
                TokenType::OpenParen => {
                    let (sub_span, sub_ast) = Ast::parse_until_close(token.span.start, tokens)?;
                    asts.push(Ast {
                        comment: None,
                        span: sub_span,
                        node: AstNode::Tree(sub_ast),
                    });
                }
                TokenType::CloseParen => {
                    let span = Span {
                        start: span_start,
                        end: token.span.end,
                    };
                    return Ok((span, asts));
                }
                TokenType::Comment => (),
            }
        }
        Err(AstError::UnclosedParen(Span {
            start: span_start,
            end: span_start + 1,
        }))
    }
}

#[derive(Debug)]
#[allow(dead_code)] // Used for Debug printing.
pub struct AstWithText<'a> {
    comment: &'a str,
    node: AstWithTextNode<'a>,
}

#[derive(Debug)]
pub enum AstWithTextNode<'a> {
    Leaf(&'a str),
    Tree(Vec<AstWithText<'a>>),
}

impl<'a> AstWithText<'a> {
    pub fn new(ast: &Ast, source: &'a str) -> Self {
        match &ast.node {
            AstNode::Leaf => AstWithText {
                comment: ast.comment.unwrap_or_default().text(source),
                node: AstWithTextNode::Leaf(ast.span.text(source)),
            },
            AstNode::Tree(children) => AstWithText {
                comment: ast.comment.unwrap_or_default().text(source),
                node: AstWithTextNode::Tree(
                    children
                        .iter()
                        .map(|ast| AstWithText::new(ast, source))
                        .collect(),
                ),
            },
        }
    }
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

impl AstError {
    pub fn with_context(self, source: &str) -> AstErrorWithContext<'_> {
        AstErrorWithContext { err: self, source }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AstErrorWithContext<'a> {
    err: AstError,
    source: &'a str,
}

impl std::error::Error for AstErrorWithContext<'_> {}

impl std::fmt::Display for AstErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.err {
            AstError::UnclosedParen(span) => {
                write!(
                    f,
                    "unclosed parenthesis encountered at {span}: {text}",
                    text = span.text(self.source)
                )
            }
            AstError::UnexpectedCloseParen(span) => {
                write!(
                    f,
                    "found unexpected close parenthesis at {span}: {text}",
                    text = span.text(self.source)
                )
            }
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
            Ast {
                comment: None,
                span: Span { start: 0, end: 5 },
                node: AstNode::Leaf
            }
        );
    }

    #[test]
    fn several_items_make_list_of_leafs() {
        assert_eq!(
            Ast::with_source("a b c").unwrap().as_slice(),
            &[
                Ast {
                    comment: None,
                    span: Span { start: 0, end: 1 },
                    node: AstNode::Leaf
                },
                Ast {
                    comment: None,
                    span: Span { start: 2, end: 3 },
                    node: AstNode::Leaf
                },
                Ast {
                    comment: None,
                    span: Span { start: 4, end: 5 },
                    node: AstNode::Leaf
                },
            ]
        );
    }

    #[test]
    fn parenthesis_make_tree() {
        assert_eq!(
            Ast::with_source("(1 2 3)").unwrap(),
            &[Ast {
                comment: None,
                span: Span { start: 0, end: 7 },
                node: AstNode::Tree(vec![
                    Ast {
                        comment: None,
                        span: Span { start: 1, end: 2 },
                        node: AstNode::Leaf
                    },
                    Ast {
                        comment: None,
                        span: Span { start: 3, end: 4 },
                        node: AstNode::Leaf
                    },
                    Ast {
                        comment: None,
                        span: Span { start: 5, end: 6 },
                        node: AstNode::Leaf
                    }
                ])
            }]
        );
    }

    #[test]
    fn string_is_parsed_as_single_leaf() {
        assert_eq!(
            Ast::with_source("\"hello world\"").unwrap(),
            &[Ast {
                comment: None,
                span: Span { start: 0, end: 13 },
                node: AstNode::Leaf
            }]
        );
    }

    #[test]
    fn nested_parenthesis_form_nested_trees() {
        assert_eq!(
            Ast::with_source("(foo (bar 3))").unwrap(),
            &[Ast {
                comment: None,
                span: Span { start: 0, end: 13 },
                node: AstNode::Tree(vec![
                    Ast {
                        comment: None,
                        span: Span { start: 1, end: 4 },
                        node: AstNode::Leaf
                    },
                    Ast {
                        comment: None,
                        span: Span { start: 5, end: 12 },
                        node: AstNode::Tree(vec![
                            Ast {
                                comment: None,
                                span: Span { start: 6, end: 9 },
                                node: AstNode::Leaf
                            },
                            Ast {
                                comment: None,
                                span: Span { start: 10, end: 11 },
                                node: AstNode::Leaf
                            }
                        ])
                    }
                ])
            }]
        );
    }

    #[test]
    fn comments_are_preserved() {
        assert_eq!(
            Ast::with_source(";; todo\n(1 2 3)").unwrap(),
            &[Ast {
                comment: Some(Span { start: 0, end: 8 }),
                span: Span { start: 8, end: 15 },
                node: AstNode::Tree(vec![
                    Ast {
                        comment: None,
                        span: Span { start: 9, end: 10 },
                        node: AstNode::Leaf
                    },
                    Ast {
                        comment: None,
                        span: Span { start: 11, end: 12 },
                        node: AstNode::Leaf
                    },
                    Ast {
                        comment: None,
                        span: Span { start: 13, end: 14 },
                        node: AstNode::Leaf
                    }
                ])
            }]
        );
    }
}

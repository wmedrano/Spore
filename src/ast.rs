use crate::tokenizer::{tokenize, Span, Token, TokenType};

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Tree(Span, Vec<Ast>),
    Leaf(Token),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AstError {
    UnclosedParen(Span),
    UnexpectedCloseParen(Span),
}

impl Ast {
    pub fn with_source(source: &str) -> Vec<Ast> {
        let mut asts = Vec::new();
        let mut tokens = tokenize(source);
        while let Some(ast) = Ast::next_ast(&mut tokens) {
            asts.push(ast.unwrap());
        }
        asts
    }

    fn next_ast(tokens: &mut impl Iterator<Item = Token>) -> Option<Result<Ast, AstError>> {
        let token = tokens.next()?;
        match token.token_type {
            TokenType::Identifier => Some(Ok(Ast::Leaf(token))),
            TokenType::OpenParen => {
                let (sub_span, sub_ast) = match Ast::parse_until_close(token.span.start, tokens) {
                    Ok(x) => x,
                    Err(err) => return Some(Err(err)),
                };
                Some(Ok(Ast::Tree(sub_span, sub_ast)))
            }
            TokenType::CloseParen => Some(Err(AstError::UnexpectedCloseParen(token.span))),
        }
    }

    fn parse_until_close(
        span_start: u32,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Result<(Span, Vec<Ast>), AstError> {
        let mut asts = Vec::new();
        while let Some(token) = tokens.next() {
            match token.token_type {
                TokenType::Identifier => asts.push(Ast::Leaf(token)),
                TokenType::OpenParen => {
                    let (sub_span, sub_ast) = Ast::parse_until_close(token.span.start, tokens)?;
                    asts.push(Ast::Tree(sub_span, sub_ast));
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_source_has_empty_ast() {
        assert_eq!(Ast::with_source(""), Vec::<Ast>::new(),);
    }
}

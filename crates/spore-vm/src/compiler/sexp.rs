use compact_str::CompactString;

use crate::{val::Val, vm::Vm};

use super::{
    span::Span,
    tokenizer::{Token, TokenType, Tokenizer},
};

pub struct SexpBuilder<'a> {
    source: &'a str,
    tokenizer: Tokenizer<'a>,
}

impl<'a> SexpBuilder<'a> {
    pub fn new(source: &'a str) -> SexpBuilder<'a> {
        SexpBuilder {
            source,
            tokenizer: Tokenizer::new(source),
        }
    }

    pub fn next(&mut self, vm: &mut Vm) -> Option<Result<Val, ParseError>> {
        loop {
            let token = self.tokenizer.next()?;
            match token.token_type {
                TokenType::OpenParen => return Some(self.build_sexp(vm, token)),
                TokenType::CloseParen => {
                    return Some(Err(ParseError::UnexpectedCloseParen(token.span)));
                }
                TokenType::Identifier => return Some(Ok(self.build_identifier(vm, token))),
                TokenType::String => return Some(self.build_string(vm, token)),
                TokenType::Comment => {}
            }
        }
    }

    pub fn last(mut self, vm: &mut Vm) -> Option<Result<Val, ParseError>> {
        let mut last = None;
        while let Some(res) = self.next(vm) {
            let is_err = res.is_err();
            last = Some(res);
            if is_err {
                break;
            }
        }
        last
    }

    fn build_sexp(&mut self, vm: &mut Vm, start: Token) -> Result<Val, ParseError> {
        let mut sub_exprs = Vec::new();
        while let Some(next_sub_expr) = self.next(vm) {
            match next_sub_expr {
                Ok(v) => sub_exprs.push(v),
                Err(ParseError::UnexpectedCloseParen(_)) => {
                    return Ok(vm.make_list(sub_exprs));
                }
                Err(err) => return Err(err),
            }
        }
        return Err(ParseError::UnclosedParen(start.span));
    }

    fn build_string(&self, vm: &mut Vm, token: Token) -> Result<Val, ParseError> {
        let source_text = token.text(self.source);
        if source_text.len() < 2 {
            return Err(ParseError::BadString(token.span));
        }
        if !source_text.starts_with('"') || !source_text.starts_with('"') {
            return Err(ParseError::BadString(token.span));
        }
        let mut string = CompactString::default();
        let mut ch_iter = source_text[1..source_text.len() - 1].chars();
        while let Some(next) = ch_iter.next() {
            match next {
                '\\' => match ch_iter.next() {
                    Some('n') => string.push('\n'),
                    Some('r') => string.push('\r'),
                    Some('t') => string.push('\t'),
                    Some('\\') => string.push('\\'),
                    Some('"') => string.push('"'),
                    _ => return Err(ParseError::BadString(token.span)),
                },
                ch => string.push(ch),
            }
        }
        return Ok(vm.make_string(string));
    }

    fn build_identifier(&self, vm: &mut Vm, token: Token) -> Val {
        let text = token.text(self.source);
        match text {
            "true" => return Val::Bool(true),
            "false" => return Val::Bool(false),
            _ => {}
        }
        let leading_ch = text.chars().next().unwrap_or('\0');
        if leading_ch.is_digit(10) || leading_ch == '-' {
            if let Ok(x) = text.parse() {
                return Val::Int(x);
            }
            if let Ok(x) = text.parse() {
                return Val::Float(x);
            }
        }
        if leading_ch == ':' {
            let key = vm.make_identifier_id(&text[1..]);
            return Val::Key(key);
        }
        let identifier = vm.make_identifier_id(text);
        Val::Symbol(identifier)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// Represents an error that can occur during AST building.
pub enum ParseError {
    /// Represents an unclosed parenthesis.
    UnclosedParen(Span),
    /// Represents an unexpected close parenthesis.
    UnexpectedCloseParen(Span),
    /// Signals that a string is not well formatted.
    BadString(Span),
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnclosedParen(span) => {
                write!(f, "unclosed parenthesis encountered at {span}")
            }
            ParseError::UnexpectedCloseParen(span) => {
                write!(f, "found unexpected close parenthesis at {span}")
            }
            ParseError::BadString(span) => {
                write!(f, "found bad string at {span}")
            }
        }
    }
}

impl ParseError {
    pub fn with_context(self, source: &str) -> ParseErrorWithContext<'_> {
        ParseErrorWithContext { err: self, source }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ParseErrorWithContext<'a> {
    err: ParseError,
    source: &'a str,
}

impl std::error::Error for ParseErrorWithContext<'_> {}

impl std::fmt::Display for ParseErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.err {
            ParseError::UnclosedParen(span) => {
                write!(
                    f,
                    "unclosed parenthesis encountered at {span}: {text}",
                    text = span.text(self.source)
                )
            }
            ParseError::UnexpectedCloseParen(span) => {
                write!(
                    f,
                    "found unexpected close parenthesis at {span}: {text}",
                    text = span.text(self.source)
                )
            }
            ParseError::BadString(span) => {
                write!(
                    f,
                    "found bad string at {span}: {text}",
                    text = span.text(self.source)
                )
            }
        }
    }
}

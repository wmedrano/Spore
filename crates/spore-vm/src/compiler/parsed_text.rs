use crate::{val::Val, vm::Vm};

/// Represents a constant value.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Constant<'a> {
    /// A void constant.
    Void,
    /// A boolean constant.
    Bool(bool),
    /// An integer constant.
    Int(i64),
    /// A floating-point constant.
    Float(f64),
    /// A symbol constant.
    Symbol(&'a str),
    /// A string constant.
    String(&'a str),
}

impl Constant<'_> {
    pub fn to_val(self, vm: &mut Vm) -> Val {
        match self {
            Constant::Void => Val::Void,
            Constant::Bool(x) => Val::Bool(x),
            Constant::Int(x) => Val::Int(x),
            Constant::Float(x) => Val::Float(x),
            Constant::Symbol(x) => vm.make_symbol(x),
            Constant::String(x) => vm.make_string(x),
        }
    }
}

#[derive(Debug)]
pub enum ParsedText<'a> {
    Constant(Constant<'a>),
    Identifier(&'a str),
}

impl<'a> ParsedText<'a> {
    /// Creates a new `ParsedText` out of a string.
    pub fn new(text: &'a str) -> Self {
        None.or_else(|| Self::new_bool(text))
            .or_else(|| Self::new_number(text))
            .or_else(|| Self::new_string(text))
            .or_else(|| Self::new_symbol(text))
            .unwrap_or(ParsedText::Identifier(text))
    }

    fn new_bool(text: &'a str) -> Option<Self> {
        match text {
            "true" => Some(ParsedText::Constant(Constant::Bool(true))),
            "false" => Some(ParsedText::Constant(Constant::Bool(false))),
            _ => None,
        }
    }

    fn new_number(text: &'a str) -> Option<Self> {
        let leading_char = text.chars().next().unwrap_or(' ');
        if leading_char != '-' && !leading_char.is_ascii_digit() {
            return None;
        }
        if let Ok(x) = text.parse() {
            return Some(ParsedText::Constant(Constant::Int(x)));
        }
        if let Ok(x) = text.parse() {
            return Some(ParsedText::Constant(Constant::Float(x)));
        }
        None
    }

    fn new_string(text: &'a str) -> Option<Self> {
        if text.starts_with('"') && text.ends_with('"') && text.len() > 1 {
            Some(ParsedText::Constant(Constant::String(
                &text[1..text.len() - 1],
            )))
        } else {
            None
        }
    }

    fn new_symbol(text: &'a str) -> Option<Self> {
        if let Some(stripped) = text.strip_prefix('\'') {
            return Some(ParsedText::Constant(Constant::Symbol(stripped)));
        }
        if let Some(stripped) = text.strip_prefix(':') {
            return Some(ParsedText::Constant(Constant::Symbol(stripped)));
        }
        None
    }
}

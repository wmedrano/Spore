use compact_str::CompactString;

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
            Constant::String(x) => {
                let mut ch_iter = x.chars();
                let mut s = CompactString::default();
                while let Some(next) = ch_iter.next() {
                    match next {
                        '\\' => match ch_iter.next() {
                            Some('n') => s.push('\n'),
                            Some('r') => s.push('\r'),
                            Some('t') => s.push('\t'),
                            Some(ch) => s.push(ch),
                            None => {}
                        },
                        ch => s.push(ch),
                    }
                }
                vm.make_string(s)
            }
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

    #[cfg(test)]
    fn as_constant(&self) -> Option<&Constant<'a>> {
        match self {
            ParsedText::Constant(c) => Some(c),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn void_const_turns_to_void() {
        assert_eq!(Constant::Void.to_val(&mut Vm::default()), Val::Void);
    }

    #[test]
    fn bool_is_parsed() {
        let mut vm = Vm::default();
        assert_eq!(
            ParsedText::new("true")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            Val::Bool(true)
        );
        assert_eq!(
            ParsedText::new("false")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            Val::Bool(false)
        );
    }

    #[test]
    fn int_is_parsed() {
        let mut vm = Vm::default();
        assert_eq!(
            ParsedText::new("-1000000")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            Val::Int(-1000000)
        );
        assert_eq!(
            ParsedText::new("1000000")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            Val::Int(1000000)
        );
    }

    #[test]
    fn float_is_parsed() {
        let mut vm = Vm::default();
        assert_eq!(
            ParsedText::new("-0.1")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            Val::Float(-0.1)
        );
        assert_eq!(
            ParsedText::new("0.1")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            Val::Float(0.1)
        );
    }

    #[test]
    fn symbol_is_parsed() {
        let mut vm = Vm::default();
        let symbol_val = vm.make_symbol("symbol");
        assert_eq!(
            ParsedText::new("'symbol")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            symbol_val
        );
        assert_eq!(
            ParsedText::new(":symbol")
                .as_constant()
                .unwrap()
                .to_val(&mut vm),
            symbol_val
        );
    }

    #[test]
    fn string_is_parsed() {
        let mut vm = Vm::default();
        let val = ParsedText::new("\"hello world\"")
            .as_constant()
            .unwrap()
            .to_val(&mut vm);
        assert_eq!(val.as_str(&vm).unwrap(), "hello world");
    }

    #[test]
    fn string_escape_sequences_are_parsed() {
        let mut vm = Vm::default();
        let val = ParsedText::new(r#"" newline\n tab\t space\  quote\' quote\" backslash\\ ""#)
            .as_constant()
            .unwrap()
            .to_val(&mut vm);
        assert_eq!(
            val.as_str(&vm).unwrap(),
            " newline\n tab\t space  quote' quote\" backslash\\ "
        );
    }
}

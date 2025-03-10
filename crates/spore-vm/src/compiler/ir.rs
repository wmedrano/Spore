use bumpalo::Bump;

use super::{
    ast::{Ast, AstNode},
    parsed_text::{Constant, ParsedText},
    span::Span,
};

type BumpVec<'a, T> = bumpalo::collections::Vec<'a, T>;

/// Represents the intermediate representation of the code.
#[derive(Clone, Debug, PartialEq)]
pub enum Ir<'a> {
    /// A constant value.
    Constant(Constant<'a>),
    /// A variable dereference.
    Deref(&'a str),
    /// A function call.
    FunctionCall {
        function: &'a Ir<'a>,
        args: &'a [Self],
    },
    /// A define expression.
    Define {
        span: Span,
        symbol: &'a str,
        expr: &'a Ir<'a>,
    },
    /// A lambda expression.
    Lambda {
        name: Option<&'a str>,
        args: &'a [&'a str],
        exprs: &'a [Self],
    },
    /// An if expression.
    If {
        pred: &'a Ir<'a>,
        true_branch: &'a Ir<'a>,
        false_branch: &'a Ir<'a>,
    },
    /// Evaluate multiple expressions but return only the last one.
    MultiExpr { exprs: &'a [Ir<'a>] },
    /// An early return expression.
    Return { span: Span, exprs: &'a [Ir<'a>] },
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// Represents an error that can occur during IR building.
pub enum IrError {
    EmptyFunctionCall(Span),
    ConstantNotCallable(Span),
    BadDefine(Span),
    BadWhen(Span),
    BadLambda(Span),
    BadIf(Span),
    BadReturn(Span),
    DefineExpectedIdentifierButFoundConstant(Span),
    DefineExpectedSymbol(Span),
}

impl std::error::Error for IrError {}

impl std::fmt::Display for IrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrError::EmptyFunctionCall(span) => write!(f, "empty function call at {span}"),
            IrError::ConstantNotCallable(span) => write!(f, "constant not callable at {span}"),
            IrError::BadDefine(span) => write!(f, "bad define at {span}"),
            IrError::BadWhen(span) => write!(f, "bad when at {span}"),
            IrError::BadLambda(span) => write!(f, "bad lambda at {span}"),
            IrError::BadIf(span) => write!(f, "bad if at {span}"),
            IrError::BadReturn(span) => write!(f, "bad return at {span}"),
            IrError::DefineExpectedIdentifierButFoundConstant(span) => {
                write!(f, "define expected identifier but found constant at {span}")
            }
            IrError::DefineExpectedSymbol(span) => write!(f, "define expected symbol at {span}"),
        }
    }
}

impl IrError {
    pub fn with_context(self, source: &str) -> IrErrorWithContext<'_> {
        IrErrorWithContext { err: self, source }
    }
}

#[derive(Debug, PartialEq)]
pub struct IrErrorWithContext<'a> {
    err: IrError,
    source: &'a str,
}

impl std::error::Error for IrErrorWithContext<'_> {}

impl std::fmt::Display for IrErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.err {
            IrError::EmptyFunctionCall(span) => write!(
                f,
                "empty function call at {span}: {text}",
                text = span.text(self.source)
            ),
            IrError::ConstantNotCallable(span) => {
                write!(
                    f,
                    "constant not callable at {span}: {text}, text=span.text(self.source)",
                    text = span.text(self.source),
                )
            }
            IrError::BadDefine(span) => write!(
                f,
                "bad define at {span}: {text}",
                text = span.text(self.source)
            ),
            IrError::BadLambda(span) => write!(
                f,
                "bad lambda at {span}: {text}",
                text = span.text(self.source)
            ),
            IrError::BadIf(span) => {
                write!(f, "bad if at {span}: {text}", text = span.text(self.source))
            }
            IrError::BadWhen(span) => {
                write!(
                    f,
                    "bad when at {span}: {text}",
                    text = span.text(self.source)
                )
            }
            IrError::BadReturn(span) => {
                write!(
                    f,
                    "bad return at {span}: {text}",
                    text = span.text(self.source)
                )
            }
            IrError::DefineExpectedIdentifierButFoundConstant(span) => {
                write!(
                    f,
                    "define expected identifier but found constant at {span}: {text}",
                    text = span.text(self.source)
                )
            }
            IrError::DefineExpectedSymbol(span) => write!(
                f,
                "define expected symbol at {span}: {text}",
                text = span.text(self.source)
            ),
        }
    }
}

impl<'a> Ir<'a> {
    /// Creates an IR from an AST.
    pub fn with_ast<'b>(
        source: &'a str,
        asts: impl Iterator<Item = &'b Ast>,
        arena: &'a Bump,
    ) -> Result<Ir<'a>, IrError> {
        let mut irs = Vec::with_capacity(asts.size_hint().1.unwrap_or(0));
        for ast in asts {
            let builder = IrBuilder { source, arena };
            let ir = builder.build(ast)?;
            irs.push(ir);
        }
        match irs.len() {
            0 => Ok(Ir::Constant(Constant::Void)),
            1 => Ok(irs.pop().unwrap()),
            _ => Ok(Ir::MultiExpr {
                exprs: arena.alloc_slice_fill_iter(irs),
            }),
        }
    }
}

pub struct IrBuilder<'a> {
    source: &'a str,
    arena: &'a Bump,
}

impl<'a> IrBuilder<'a> {
    /// Builds an IR from an AST.
    fn build(&self, ast: &Ast) -> Result<Ir<'a>, IrError> {
        match &ast.node {
            AstNode::Leaf => self.build_leaf(ast.span),
            AstNode::Tree(children) => self.build_tree(ast.span, children),
        }
    }

    /// Builds an IR from an AST.
    fn build_tree(&self, span: Span, children: &[Ast]) -> Result<Ir<'a>, IrError> {
        let leading_token = children.first().and_then(|ast| match &ast.node {
            AstNode::Leaf => {
                let parsed = ParsedText::new(ast.span.text(self.source));
                Some((ast.span, parsed))
            }
            AstNode::Tree(_) => None,
        });
        match leading_token {
            Some((span, ParsedText::Constant(_))) => Err(IrError::ConstantNotCallable(span)),
            Some((_, ParsedText::Identifier("define"))) => match children {
                [_define, symbol @ Ast {
                    node: AstNode::Leaf,
                    ..
                }, expr] => self.build_define(span, symbol, expr),
                [_define, args_ast @ Ast {
                    node: AstNode::Tree(children),
                    ..
                }, exprs @ ..] => match children.split_first() {
                    Some((name, args)) => self.build_define_function(span, name, args, exprs),
                    None => Err(IrError::BadDefine(args_ast.span)),
                },
                _ => Err(IrError::BadDefine(span)),
            },
            Some((_, ParsedText::Identifier("lambda"))) => match children {
                [_lambda, args, exprs @ ..] => match &args.node {
                    AstNode::Leaf => Err(IrError::BadLambda(args.span)),
                    AstNode::Tree(children) => self.build_lambda(None, children, exprs),
                },
                _ => Err(IrError::BadLambda(span)),
            },
            Some((_, ParsedText::Identifier("if"))) => match children {
                [_if, pred, true_branch, false_branch] => {
                    self.build_if(pred, true_branch, Some(false_branch))
                }
                [_if, pred, true_branch] => self.build_if(pred, true_branch, None),
                _ => Err(IrError::BadLambda(span)),
            },
            Some((_, ParsedText::Identifier("when"))) => match children {
                [_when, pred, exprs @ ..] => self.build_when(pred, exprs),
                _ => Err(IrError::BadWhen(span)),
            },
            Some((_, ParsedText::Identifier("return"))) => match children {
                [_return] => self.build_return(span, None),
                [_return, expr] => self.build_return(span, Some(expr)),
                _ => Err(IrError::BadLambda(span)),
            },
            _ => {
                let text = span.text(self.source);
                match text {
                    "define" | "lambda" => Err(IrError::BadDefine(span)),
                    "if" => Err(IrError::BadIf(span)),
                    _ => self.build_function_call(span, children),
                }
            }
        }
    }

    /// Builds an IR from an AST.
    fn build_leaf(&self, leaf_span: Span) -> Result<Ir<'a>, IrError> {
        match ParsedText::new(leaf_span.text(self.source)) {
            ParsedText::Constant(c) => Ok(Ir::Constant(c)),
            ParsedText::Identifier(ident) => Ok(Ir::Deref(ident)),
        }
    }

    /// Builds a function call IR.
    fn build_function_call(&self, span: Span, asts: &[Ast]) -> Result<Ir<'a>, IrError> {
        match asts {
            [] => Err(IrError::EmptyFunctionCall(span)),
            [f, arg_asts @ ..] => {
                let function = self.arena.alloc(self.build(f)?);
                let mut args = BumpVec::with_capacity_in(arg_asts.len(), self.arena);
                for a in arg_asts {
                    args.push(self.build(a)?);
                }
                Ok(Ir::FunctionCall {
                    function,
                    args: args.into_bump_slice(),
                })
            }
        }
    }

    /// Builds a lambda IR.
    fn build_lambda(
        &self,
        name: Option<&'a str>,
        args: &[Ast],
        exprs: &[Ast],
    ) -> Result<Ir<'a>, IrError> {
        let mut parsed_args = BumpVec::with_capacity_in(args.len(), self.arena);
        for arg in args {
            match &arg.node {
                AstNode::Leaf => match ParsedText::new(arg.span.text(self.source)) {
                    ParsedText::Identifier(ident) => parsed_args.push(ident),
                    ParsedText::Constant(_) => return Err(IrError::BadLambda(arg.span)), // Constants are not allowed as arguments
                },
                AstNode::Tree(_) => return Err(IrError::BadLambda(arg.span)), // Trees are not allowed as arguments
            }
        }
        let mut ir_exprs = BumpVec::with_capacity_in(exprs.len(), self.arena);
        for expr in exprs {
            ir_exprs.push(self.build(expr)?);
        }
        Ok(Ir::Lambda {
            name,
            args: parsed_args.into_bump_slice(),
            exprs: ir_exprs.into_bump_slice(),
        })
    }

    /// Builds a define IR.
    fn build_define(&self, span: Span, symbol: &Ast, expr: &Ast) -> Result<Ir<'a>, IrError> {
        let symbol_text = symbol
            .leaf_text(self.source)
            .ok_or(IrError::DefineExpectedSymbol(symbol.span))?;
        let symbol_text = match ParsedText::new(symbol_text) {
            ParsedText::Constant(_) => {
                return Err(IrError::DefineExpectedIdentifierButFoundConstant(
                    symbol.span,
                ))
            }
            ParsedText::Identifier(symbol) => symbol,
        };
        let expr = self.arena.alloc(self.build(expr)?);
        Ok(Ir::Define {
            span,
            symbol: symbol_text,
            expr,
        })
    }

    /// Builds a define function IR.
    fn build_define_function(
        &self,
        span: Span,
        name: &Ast,
        args: &[Ast],
        exprs: &[Ast],
    ) -> Result<Ir<'a>, IrError> {
        let symbol_text = name
            .leaf_text(self.source)
            .ok_or(IrError::DefineExpectedSymbol(name.span))?;
        let symbol_text = match ParsedText::new(symbol_text) {
            ParsedText::Constant(_) => {
                return Err(IrError::DefineExpectedIdentifierButFoundConstant(name.span))
            }
            ParsedText::Identifier(symbol) => symbol,
        };
        let lambda = self.build_lambda(Some(symbol_text), args, exprs)?;
        Ok(Ir::Define {
            span,
            symbol: symbol_text,
            expr: self.arena.alloc(lambda),
        })
    }

    /// Builds an if Ir.
    fn build_if(
        &self,
        pred: &Ast,
        true_branch: &Ast,
        false_branch: Option<&Ast>,
    ) -> Result<Ir<'a>, IrError> {
        let pred = self.arena.alloc(self.build(pred)?);
        let true_branch = self.arena.alloc(self.build(true_branch)?);
        let false_branch = match false_branch {
            Some(false_branch) => self.arena.alloc(self.build(false_branch)?),
            None => &Ir::Constant(Constant::Void),
        };
        Ok(Ir::If {
            pred,
            true_branch,
            false_branch,
        })
    }

    /// Builds a when Ir.
    fn build_when(&self, pred: &Ast, exprs_ast: &[Ast]) -> Result<Ir<'a>, IrError> {
        let pred = self.arena.alloc(self.build(pred)?);
        let exprs = self
            .arena
            .alloc_slice_fill_clone(exprs_ast.len(), &Ir::Constant(Constant::Void));
        for (ast, out) in exprs_ast.iter().zip(exprs.iter_mut()) {
            *out = self.build(ast)?;
        }
        Ok(Ir::If {
            pred,
            true_branch: self.arena.alloc(Ir::MultiExpr { exprs }),
            false_branch: &Ir::Constant(Constant::Void),
        })
    }

    /// Builds a return Ir.
    fn build_return(&self, span: Span, expr: Option<&Ast>) -> Result<Ir<'a>, IrError> {
        match expr {
            Some(expr) => {
                let expr = self.arena.alloc(self.build(expr)?);
                Ok(Ir::Return {
                    span,
                    exprs: std::slice::from_ref(expr),
                })
            }
            None => Ok(Ir::Return { span, exprs: &[] }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::ast::Ast;

    use super::*;
    use bumpalo::Bump;

    #[test]
    fn build_function_call_produces_function_call() {
        let arena = Bump::new();
        let source = "(add 1 2)";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(
            ir,
            Ir::FunctionCall {
                function: &Ir::Deref("add"),
                args: &[
                    Ir::Constant(Constant::Int(1)),
                    Ir::Constant(Constant::Int(2)),
                ]
            }
        );
    }

    #[test]
    fn apostrophe_produces_constant_symbol() {
        let arena = Bump::new();
        let source = "'symbol";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(ir, Ir::Constant(Constant::Symbol("symbol")));
    }

    #[test]
    fn colon_produces_constant_symbol() {
        let arena = Bump::new();
        let source = ":symbol";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(ir, Ir::Constant(Constant::Symbol("symbol")));
    }

    #[test]
    fn number_produces_constant_int() {
        let arena = Bump::new();
        let source = "123";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(ir, Ir::Constant(Constant::Int(123)));
    }

    #[test]
    fn number_with_decimals_produces_constant_float() {
        let arena = Bump::new();
        let source = "123.45";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(ir, Ir::Constant(Constant::Float(123.45)));
    }

    #[test]
    fn symbol_produces_constant_symbol() {
        let arena = Bump::new();
        let source = "\'hello";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(ir, Ir::Constant(Constant::Symbol("hello")));
    }

    #[test]
    fn double_quotes_produces_constant_string() {
        let arena = Bump::new();
        let source = "\"hello world\"";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(ir, Ir::Constant(Constant::String("hello world")));
    }

    #[test]
    fn identifier_produces_deref() {
        let arena = Bump::new();
        let source = "hello";
        let ast = Ast::with_source(source).unwrap();

        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(ir, Ir::Deref("hello"));
    }

    #[test]
    fn define_produces_define_node() {
        let arena = Bump::new();
        let source = "(define x 1)";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(
            ir,
            Ir::Define {
                span: Span { start: 0, end: 12 },
                symbol: "x",
                expr: &Ir::Constant(Constant::Int(1))
            }
        );
    }

    #[test]
    fn define_function_produces_define_with_lambda() {
        let arena = Bump::new();
        let source = "(define (foo) 1)";
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(
            ir,
            Ir::Define {
                span: Span { start: 0, end: 16 },
                symbol: "foo",
                expr: &Ir::Lambda {
                    name: Some("foo"),
                    args: &[],
                    exprs: &[Ir::Constant(Constant::Int(1))]
                }
            }
        );
    }

    #[test]
    fn define_with_function_call_produces_lambda() {
        let arena = Bump::new();
        let source = r#"
(define (fib n)
  (if (< n 2) (return n))
  (+ (fib (- n 2)) (fib (- n 1))))"#;
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(
            ir,
            Ir::Define {
                span: Span { start: 1, end: 77 },
                symbol: "fib",
                expr: &Ir::Lambda {
                    name: Some("fib"),
                    args: &["n"],
                    exprs: &[
                        Ir::If {
                            pred: &Ir::FunctionCall {
                                function: &Ir::Deref("<"),
                                args: &[Ir::Deref("n"), Ir::Constant(Constant::Int(2))]
                            },
                            true_branch: &Ir::Return {
                                span: Span { start: 31, end: 41 },
                                exprs: &[Ir::Deref("n")]
                            },
                            false_branch: &Ir::Constant(Constant::Void)
                        },
                        Ir::FunctionCall {
                            function: &Ir::Deref("+"),
                            args: &[
                                Ir::FunctionCall {
                                    function: &Ir::Deref("fib"),
                                    args: &[Ir::FunctionCall {
                                        function: &Ir::Deref("-"),
                                        args: &[Ir::Deref("n"), Ir::Constant(Constant::Int(2))]
                                    }]
                                },
                                Ir::FunctionCall {
                                    function: &Ir::Deref("fib"),
                                    args: &[Ir::FunctionCall {
                                        function: &Ir::Deref("-"),
                                        args: &[Ir::Deref("n"), Ir::Constant(Constant::Int(1))]
                                    }]
                                }
                            ]
                        }
                    ]
                }
            }
        );
    }

    #[test]
    fn comments_are_ignored() {
        let arena = Bump::new();
        let source = r#"
(do
  ;; This is a function call.
  (+ 1 2 3 4) ;; It adds all the numbers.
)
"#;
        let ast = Ast::with_source(source).unwrap();
        let ir = Ir::with_ast(source, ast.iter(), &arena).unwrap();
        assert_eq!(
            ir,
            Ir::FunctionCall {
                function: &Ir::Deref("do"),
                args: &[Ir::FunctionCall {
                    function: &Ir::Deref("+"),
                    args: &[
                        Ir::Constant(Constant::Int(1)),
                        Ir::Constant(Constant::Int(2)),
                        Ir::Constant(Constant::Int(3)),
                        Ir::Constant(Constant::Int(4))
                    ]
                }]
            }
        );
    }
}

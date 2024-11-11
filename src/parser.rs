use crate::ast::*;
use chumsky::pratt::*;
use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token<'src> {
    Ident(&'src str),
    Parens(Vec<Self>),
    If,
    Then,
    Else,
    True,
    False,
    Lambda,
    Colon,
    Dot,
    Bool,
    Arrow,
}

fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Token<'src>>, extra::Err<Rich<'src, char>>> {
    recursive(|token| {
        choice((
            text::ident().map(|s| match s {
                "if" => Token::If,
                "then" => Token::Then,
                "else" => Token::Else,
                "true" => Token::True,
                "false" => Token::False,
                "bool" => Token::Bool,
                s => Token::Ident(s),
            }),
            just("\\").to(Token::Lambda),
            just(":").to(Token::Colon),
            just(".").to(Token::Dot),
            just("->").to(Token::Arrow),
            token
                .repeated()
                .collect()
                .delimited_by(just("("), just(")"))
                .map(Token::Parens),
        ))
        .padded()
    })
    .repeated()
    .collect()
}

#[test]
fn test_lexer() {
    let l = lexer();
    let i = Token::Ident;
    assert_eq!(
        l.parse(" a   b if then else true false bool \\ : . -> ")
            .into_result(),
        Ok(vec![
            i("a"),
            i("b"),
            Token::If,
            Token::Then,
            Token::Else,
            Token::True,
            Token::False,
            Token::Bool,
            Token::Lambda,
            Token::Colon,
            Token::Dot,
            Token::Arrow,
        ])
    );
    assert!(l.parse("- >").has_errors());
    assert!(l.parse("(").has_errors());
    assert!(l.parse(")").has_errors());
    assert_eq!(
        l.parse(" a (b c) ( d e ( f  g))").into_result(),
        Ok(vec![
            i("a"),
            Token::Parens(vec![i("b"), i("c")]),
            Token::Parens(vec![i("d"), i("e"), Token::Parens(vec![i("f"), i("g")])]),
        ])
    )
}

fn type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, &'tok [Token<'src>], Type, extra::Err<Rich<'tok, Token<'src>>>> + Clone {
    recursive(|type_| {
        let atom = choice((
            just(Token::Bool).to(Type::Bool),
            type_.nested_in(select_ref! { Token::Parens(ts) => ts.as_slice() }),
        ));

        atom.pratt((infix(right(0), just(Token::Arrow), |lhs, _, rhs| {
            Type::Func {
                param: Box::new(lhs),
                ret: Box::new(rhs),
            }
        }),))
    })
}

#[cfg(test)]
pub fn must_parse_type<'src>(input: &'src str) -> Type {
    let tokens = lexer().parse(input).unwrap();
    let type_ = type_parser().parse(&tokens).unwrap();
    type_
}

#[cfg(test)]
fn type_parse_error<'src>(input: &'src str) {
    let tokens = lexer().parse(input).unwrap();
    assert!(type_parser().parse(&tokens).has_errors())
}

#[test]
fn test_type_parser() {
    assert_eq!(must_parse_type("bool"), Type::Bool);
    type_parse_error("bogus");
    type_parse_error("()");
    type_parse_error("bool bool");
    assert_eq!(must_parse_type("((bool))"), Type::Bool);
    assert_eq!(
        must_parse_type("bool -> bool"),
        Type::Func {
            param: Box::new(Type::Bool),
            ret: Box::new(Type::Bool),
        }
    );
    assert_eq!(
        must_parse_type("bool -> bool -> bool"),
        Type::Func {
            param: Box::new(Type::Bool),
            ret: Box::new(Type::Func {
                param: Box::new(Type::Bool),
                ret: Box::new(Type::Bool),
            }),
        }
    );
    assert_eq!(
        must_parse_type("(bool -> bool) -> bool"),
        Type::Func {
            param: Box::new(Type::Func {
                param: Box::new(Type::Bool),
                ret: Box::new(Type::Bool),
            }),
            ret: Box::new(Type::Bool),
        }
    );
}

fn parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, &'tok [Token<'src>], Expr<'src>, extra::Err<Rich<'tok, Token<'src>>>> {
    recursive(|expr| {
        let atom = choice((
            just(Token::True).to(Expr::Bool(true)),
            just(Token::False).to(Expr::Bool(false)),
            select_ref! { Token::Ident(x) => *x }.map(Expr::Var),
            expr.clone()
                .nested_in(select_ref! { Token::Parens(ts) => ts.as_slice() }),
            just(Token::If)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Then))
                .then(expr.clone())
                .then_ignore(just(Token::Else))
                .then(expr.clone())
                .map(|((cond, then), else_)| Expr::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: Box::new(else_),
                }),
            just(Token::Lambda)
                .ignore_then(select_ref! { Token::Ident(x) => *x })
                .then_ignore(just(Token::Colon))
                .then(type_parser())
                .then_ignore(just(Token::Dot))
                .then(expr.clone())
                .map(|((param, type_), body)| Expr::Lambda {
                    param,
                    type_: Box::new(type_),
                    body: Box::new(body),
                }),
        ));

        atom.pratt((infix(left(1), empty(), |lhs, _, rhs| Expr::Apply {
            func: Box::new(lhs),
            arg: Box::new(rhs),
        }),))
    })
}

pub fn must_parse(input: &str) -> Expr<'_> {
    let tokens = lexer().parse(input).unwrap();
    let expr = parser().parse(&tokens).unwrap();
    expr
}

#[test]
fn test_parser() {
    assert_eq!(must_parse("true"), Expr::Bool(true));
    assert_eq!(must_parse("false"), Expr::Bool(false));
    assert_eq!(must_parse("x"), Expr::Var("x"));
    assert_eq!(must_parse("xyz"), Expr::Var("xyz"));
    assert_eq!(must_parse("((xyz))"), Expr::Var("xyz"));
    assert_eq!(
        must_parse("if a then b else c"),
        Expr::If {
            cond: Box::new(Expr::Var("a")),
            then: Box::new(Expr::Var("b")),
            else_: Box::new(Expr::Var("c")),
        }
    );
    assert_eq!(
        must_parse("if if a then b else c then d else e"),
        Expr::If {
            cond: Box::new(Expr::If {
                cond: Box::new(Expr::Var("a")),
                then: Box::new(Expr::Var("b")),
                else_: Box::new(Expr::Var("c")),
            }),
            then: Box::new(Expr::Var("d")),
            else_: Box::new(Expr::Var("e")),
        }
    );
    assert_eq!(
        must_parse("if a then if b then c else d else e"),
        Expr::If {
            cond: Box::new(Expr::Var("a")),
            then: Box::new(Expr::If {
                cond: Box::new(Expr::Var("b")),
                then: Box::new(Expr::Var("c")),
                else_: Box::new(Expr::Var("d")),
            }),
            else_: Box::new(Expr::Var("e")),
        }
    );
    assert_eq!(
        must_parse("if a then b else if c then d else e"),
        Expr::If {
            cond: Box::new(Expr::Var("a")),
            then: Box::new(Expr::Var("b")),
            else_: Box::new(Expr::If {
                cond: Box::new(Expr::Var("c")),
                then: Box::new(Expr::Var("d")),
                else_: Box::new(Expr::Var("e")),
            }),
        }
    );
    assert_eq!(
        must_parse("a b"),
        Expr::Apply {
            func: Box::new(Expr::Var("a")),
            arg: Box::new(Expr::Var("b")),
        }
    );
    assert_eq!(
        must_parse("a b c"),
        Expr::Apply {
            func: Box::new(Expr::Apply {
                func: Box::new(Expr::Var("a")),
                arg: Box::new(Expr::Var("b")),
            }),
            arg: Box::new(Expr::Var("c")),
        }
    );
    assert_eq!(
        must_parse("if a then b else c d"),
        Expr::If {
            cond: Box::new(Expr::Var("a")),
            then: Box::new(Expr::Var("b")),
            else_: Box::new(Expr::Apply {
                func: Box::new(Expr::Var("c")),
                arg: Box::new(Expr::Var("d")),
            }),
        },
    );
    assert_eq!(
        must_parse("\\x: bool. e"),
        Expr::Lambda {
            param: "x",
            type_: Box::new(Type::Bool),
            body: Box::new(Expr::Var("e")),
        },
    );
    assert_eq!(
        must_parse("\\x: bool -> bool. e f"),
        Expr::Lambda {
            param: "x",
            type_: Box::new(Type::Func {
                param: Box::new(Type::Bool),
                ret: Box::new(Type::Bool),
            }),
            body: Box::new(Expr::Apply {
                func: Box::new(Expr::Var("e")),
                arg: Box::new(Expr::Var("f")),
            }),
        },
    );
    assert_eq!(
        must_parse("\\x: bool. \\y: bool. x"),
        Expr::Lambda {
            param: "x",
            type_: Box::new(Type::Bool),
            body: Box::new(Expr::Lambda {
                param: "y",
                type_: Box::new(Type::Bool),
                body: Box::new(Expr::Var("x")),
            }),
        },
    );
}

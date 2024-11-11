#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Func { param: Box<Type>, ret: Box<Type> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<'src> {
    Bool(bool),
    Var(&'src str),
    If {
        cond: Box<Expr<'src>>,
        then: Box<Expr<'src>>,
        else_: Box<Expr<'src>>,
    },
    Lambda {
        param: &'src str,
        type_: Box<Type>,
        body: Box<Expr<'src>>,
    },
    Apply {
        func: Box<Expr<'src>>,
        arg: Box<Expr<'src>>,
    },
}

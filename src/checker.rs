use crate::ast::*;
#[cfg(test)]
use crate::parser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError(String);

enum Context<'src: 'ctx, 'ctx> {
    Empty,
    Pair {
        key: &'src str,
        value: Type,
        next: &'ctx Context<'src, 'ctx>,
    },
}

impl<'src: 'ctx, 'ctx> Context<'src, 'ctx> {
    fn with(self: &'ctx Context<'src, 'ctx>, key: &'src str, value: Type) -> Self {
        Self::Pair {
            key,
            value,
            next: self,
        }
    }

    fn get(self: &Context<'src, 'ctx>, key: &'src str) -> Option<&Type> {
        if let Self::Pair {
            key: self_key,
            value,
            next,
        } = &self
        {
            if &key == self_key {
                Some(value)
            } else {
                next.get(key)
            }
        } else {
            None
        }
    }
}

pub fn check(expr: &Expr<'_>) -> Result<Type, TypeError> {
    check_in(&Context::Empty, expr)
}

fn check_in<'src: 'ctx, 'ctx>(
    ctx: &'ctx Context<'src, 'ctx>,
    expr: &Expr<'src>,
) -> Result<Type, TypeError> {
    match expr {
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::Var(v) => {
            if let Some(type_) = ctx.get(v) {
                Ok(type_.clone())
            } else {
                Err(TypeError(format!("Undefined variable {v:?}")))
            }
        }
        Expr::If { cond, then, else_ } => {
            let cond_type = check_in(ctx, cond)?;
            if let Type::Bool = cond_type {
                let then_type = check_in(ctx, then)?;
                let else_type = check_in(ctx, else_)?;
                if then_type == else_type {
                    Ok(then_type)
                } else {
                    Err(TypeError(format!(
                        "Expected then and else to have same types, got {then_type:?} and {else_type:?}"
                    )))
                }
            } else {
                Err(TypeError(format!(
                    "Expected condition to be boolean, got {cond_type:?}"
                )))
            }
        }
        Expr::Lambda { param, type_, body } => {
            let call_ctx = ctx.with(param, *type_.clone());
            let body_type = check_in(&call_ctx, body)?;
            Ok(Type::Func {
                param: Box::new(*type_.clone()),
                ret: Box::new(body_type),
            })
        }
        Expr::Apply { func, arg } => {
            let func_type = check_in(ctx, func)?;
            if let Type::Func { param, ret } = func_type.clone() {
                let arg_type = check_in(ctx, arg)?;
                if arg_type == *param {
                    Ok(*ret.clone())
                } else {
                    Err(TypeError(format!(
                        "Tried to call {func_type:?} with {arg_type:?}"
                    )))
                }
            } else {
                Err(TypeError(format!(
                    "Can only call a function, got {func_type:?}"
                )))
            }
        }
    }
}

#[cfg(test)]
fn assert_typechecks_to(expr_str: &str, type_str: &str) {
    let expr = parser::must_parse(expr_str);
    let type_ = parser::must_parse_type(type_str);
    assert_eq!(check(&expr), Ok(type_))
}

#[cfg(test)]
fn assert_typecheck_error(expr_str: &str) {
    let expr = parser::must_parse(expr_str);
    assert!(check(&expr).is_err())
}

#[test]
fn test_checker() {
    assert_typechecks_to("true", "bool");
    assert_typechecks_to("false", "bool");
    assert_typechecks_to("if true then true else false", "bool");
    assert_typechecks_to(
        "if true then \\x: bool. x else \\x: bool. x",
        "bool -> bool",
    );
    assert_typecheck_error("if \\x: bool. x then true else false");
    assert_typecheck_error("if true then \\x: bool. x else false");
    assert_typecheck_error("\\x: bool. y");
    assert_typechecks_to("\\x: bool. false", "bool -> bool");
    assert_typechecks_to("\\x: bool -> bool. false", "(bool -> bool) -> bool");
    assert_typechecks_to("\\x: bool. x", "bool -> bool");
    assert_typechecks_to("\\x: bool -> bool. x", "(bool -> bool) -> bool -> bool");
    assert_typechecks_to("\\x: bool. \\y: bool. x", "bool -> bool -> bool");
    assert_typechecks_to(
        "\\x: bool. \\y: bool -> bool. x",
        "bool -> (bool -> bool) -> bool",
    );
    assert_typechecks_to(
        "\\x: bool. \\x: bool -> bool. x",
        "bool -> (bool -> bool) -> bool -> bool",
    );
    assert_typechecks_to("(\\x: bool. true) true", "bool");
    assert_typechecks_to("(\\x: bool. x) true", "bool");
    assert_typechecks_to("(\\x: bool -> bool. true) (\\x: bool. x)", "bool");
    assert_typechecks_to("(\\x: bool -> bool. x) (\\x: bool. x) true", "bool");
    assert_typecheck_error("true true");
    assert_typecheck_error("(\\x: bool. x) (\\x: bool. x)");
    assert_typecheck_error("(\\x: bool -> bool. x) true");
    assert_typechecks_to("(\\x: bool. (\\y: bool. x)) true", "bool -> bool");
    assert_typechecks_to("(\\x: bool. (\\y: bool. x)) true false", "bool");
    assert_typechecks_to(
        "(\\k: bool -> bool -> bool. k true false) (\\x: bool. (\\y: bool. x))",
        "bool",
    );
    assert_typechecks_to(
        "(\\f: bool -> bool. f true) ((\\x: bool. \\y: bool. x) true)",
        "bool",
    );
    assert_typechecks_to(
        "(\\f: bool -> bool. \\x: bool. f x) ((\\x: bool. \\y: bool. x) true)",
        "bool -> bool",
    );
}

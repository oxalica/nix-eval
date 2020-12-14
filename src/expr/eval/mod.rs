use crate::expr::{
    builtins::Builtin, expr_ref::ExprRefKind, BinOpKind, Expr, ExprRef, LambdaArg, Literal,
    PathAnchor, SmolStr, StrPart, UnaryOpKind,
};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::PathBuf;
use std::sync::Arc;

mod builtins;
mod value;

pub use self::value::{Thunk, Value};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error, Clone)]
pub enum Error {
    #[error("Aborted: {}", .reason)]
    Abort { reason: SmolStr },
    #[error("Assertion failed")]
    AssertionFailed,
    #[error("Type error: cannot {} {} and {}", .operation, .lhs, .rhs)]
    BinOpTypeError {
        operation: &'static str,
        lhs: &'static str,
        rhs: &'static str,
    },
    #[error("Builtin not implemented: {:?}", .builtin.name())]
    BuiltinNotImplemented { builtin: Builtin },
    #[error("Builtin call error: {:?}", .reason)]
    BuiltinError { reason: SmolStr },
    #[error("Type error: cannot coerce {} to string", .actual)]
    CannotCoerceToString { actual: &'static str },
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Division overflow")]
    DivisionOverflow,
    #[error("Infinite recursion")]
    InfiniteRecursion,
    #[error("Missing required argument {:?}", .name)]
    MissingArgument { name: SmolStr },
    #[error("Missing attribute {:?}", .name)]
    MissingAttribute { name: SmolStr },
    #[error("Thrown error: {}", .reason)]
    Throw { reason: SmolStr },
    #[error("Type error: expecting {}, found {}", .expect, .actual)]
    TypeError {
        expect: &'static str,
        actual: &'static str,
    },
    #[error("Unexpected argument {:?}", .name)]
    UnexpectedArgument { name: SmolStr },
}

impl Error {
    /// Check if it is a soft error, which can be caught by `tryEval`.
    pub fn is_soft_error(&self) -> bool {
        match self {
            Self::AssertionFailed | Self::Throw { .. } => true,
            _ => false,
        }
    }
}

// FIXME: Cyclic reference.
type Stack = im::Vector<Arc<Thunk>>;

pub struct Evaluator {
    builtins: Arc<Thunk>,
    nix_paths: HashMap<SmolStr, PathBuf>,
}

impl Evaluator {
    pub fn new(nix_paths: impl IntoIterator<Item = (SmolStr, PathBuf)>) -> Self {
        let builtins = Thunk::new_value_cyclic(|this| {
            let mut set = BTreeMap::new();
            for &b in Builtin::ALL {
                let value = match b {
                    // Special cased 0-argument builtins.
                    Builtin::True => Thunk::new_value(Value::Bool(true)),
                    Builtin::False => Thunk::new_value(Value::Bool(false)),
                    Builtin::Builtins => this.clone(),
                    b => Thunk::new_value(Value::PartialBuiltin(b, Vec::new())),
                };
                set.insert(b.name().into(), value);
            }
            Value::AttrSet(set)
        });
        Evaluator {
            builtins,
            nix_paths: nix_paths.into_iter().collect(),
        }
    }

    pub fn eval_expr(&self, expr: &ExprRef, deep: bool) -> Result<Value> {
        if deep {
            self.eval_deep(expr, &Stack::new())
        } else {
            self.eval(expr, &Stack::new())
        }
    }

    fn eval_deep(&self, expr: &ExprRef, stack: &Stack) -> Result<Value> {
        let v = self.eval(expr, stack)?;
        match &v {
            Value::AttrSet(set) => {
                for thunk in set.values() {
                    thunk.eval_deep(self)?;
                }
            }
            Value::List(xs) => {
                for thunk in xs {
                    thunk.eval_deep(self)?;
                }
            }
            Value::Bool(_)
            | Value::Float(_)
            | Value::Int(_)
            | Value::String(_)
            | Value::Path(_)
            | Value::Lambda(..)
            | Value::PartialBuiltin(..) => {}
        }
        Ok(v)
    }

    fn eval(&self, expr: &ExprRef, stack: &Stack) -> Result<Value> {
        let e = match expr.kind() {
            ExprRefKind::Debruijn(idx) => {
                // FIXME: No clone.
                return Ok(stack[stack.len() - 1 - idx].eval(self)?.clone());
            }
            ExprRefKind::Expr(e) => e,
        };

        match e {
            Expr::Apply { lambda, value } => {
                let lam = self.eval(lambda, stack)?;
                let argument = Thunk::new_lazy(value.clone(), stack.clone());
                if let Value::PartialBuiltin(b, mut args) = lam {
                    assert!(args.len() < b.params());
                    args.push(argument);
                    if b.params() == args.len() {
                        builtins::invoke(self, b, &args)
                    } else {
                        Ok(Value::PartialBuiltin(b, args))
                    }
                } else {
                    let (lam_expr, lam_stack) = lam.as_lambda()?;
                    let (arg, body) = match lam_expr.kind() {
                        ExprRefKind::Expr(Expr::Lambda { arg, body }) => (arg, body),
                        _ => unreachable!(),
                    };

                    match arg {
                        LambdaArg::Bind => {}
                        LambdaArg::OpenPattern { required_names } => {
                            let set = argument.eval(self)?.as_attr_set()?;
                            for name in required_names.iter() {
                                if !set.contains_key(&*name) {
                                    return Err(Error::MissingArgument { name: name.clone() });
                                }
                            }
                        }
                        LambdaArg::ClosePattern {
                            required_names,
                            optional_names,
                        } => {
                            let set = argument.eval(self)?.as_attr_set()?;
                            for name in required_names.iter() {
                                if !set.contains_key(&*name) {
                                    return Err(Error::MissingArgument { name: name.clone() });
                                }
                            }
                            // FIXME: No re-collect.
                            let possible_names: BTreeSet<_> =
                                optional_names.iter().chain(required_names.iter()).collect();
                            for name in set.keys() {
                                if !possible_names.contains(&*name) {
                                    return Err(Error::UnexpectedArgument { name: name.clone() });
                                }
                            }
                        }
                    }

                    let mut lam_stack = lam_stack.clone();
                    lam_stack.push_back(argument);
                    self.eval(body, &lam_stack)
                }
            }
            Expr::Assert { condition, body } => {
                let cond = self.eval(condition, stack)?.as_bool()?;
                if !cond {
                    return Err(Error::AssertionFailed);
                }
                self.eval(body, stack)
            }
            Expr::AttrSet { entries, dynamics } => {
                let mut set = BTreeMap::new();
                for (key, value) in entries {
                    set.insert(key.clone(), Thunk::new_lazy(value.clone(), stack.clone()));
                }
                for (key, value) in dynamics.iter() {
                    let key = self.eval_coerce_to_string(&self.eval(key, stack)?)?;
                    set.insert(key, Thunk::new_lazy(value.clone(), stack.clone()));
                }
                Ok(Value::AttrSet(set))
            }
            Expr::BinOp { operator, lhs, rhs } => self.eval_binop(*operator, lhs, rhs, stack),
            &Expr::Builtin(b) => {
                if b.params() == 0 {
                    builtins::invoke(self, b, &[])
                } else {
                    Ok(Value::PartialBuiltin(b, Vec::new()))
                }
            }
            Expr::IfElse {
                condition,
                then_body,
                else_body,
            } => {
                let cond = self.eval(condition, stack)?.as_bool()?;
                self.eval(if cond { then_body } else { else_body }, stack)
            }
            Expr::Lambda { .. } => Ok(Value::Lambda(expr.clone(), stack.clone())),
            Expr::LetIn { exprs, body } => {
                let mut stack = stack.clone();
                for e in exprs.iter() {
                    // Placeholder stack.
                    stack.push_back(Thunk::new_lazy(e.clone(), Stack::new()));
                }
                for thunk in stack.iter().rev().take(exprs.len()) {
                    unsafe { thunk.set_stack(stack.clone()) };
                }
                self.eval(body, &stack)
            }
            Expr::List { items } => {
                let list = items
                    .iter()
                    .map(|e| Thunk::new_lazy(e.clone(), stack.clone()))
                    .collect();
                Ok(Value::List(list))
            }
            Expr::Literal(lit) => Ok(match lit {
                &Literal::Bool(x) => Value::Bool(x),
                &Literal::Float(x) => Value::Float(x),
                &Literal::Int(x) => Value::Int(x),
                Literal::String(x) => Value::String(x.clone()),
                Literal::Path(anchor, p) => Value::Path(match anchor {
                    PathAnchor::Absolute => p.clone(),
                    PathAnchor::Relative => {
                        todo!()
                    }
                    PathAnchor::Home => {
                        todo!()
                    }
                    PathAnchor::Store => {
                        if let Some(path) = self.nix_paths.get(&*p) {
                            path.to_str().unwrap().into()
                        } else {
                            return Err(Error::Throw {
                                reason: format!(
                                    "file {:?} was not found in the Nix search path",
                                    p
                                )
                                .into(),
                            });
                        }
                    }
                }),
            }),
            Expr::Select {
                set,
                index,
                or_default,
            } => {
                let set = self.eval(set, stack)?;
                let set = set.as_attr_set()?;
                let index = self.eval_coerce_to_string(&self.eval(index, stack)?)?;
                match (set.get(&*index), or_default) {
                    // FIXME: No clone.
                    (Some(v), _) => Ok(v.eval(self)?.clone()),
                    (None, Some(default)) => self.eval(default, stack),
                    (None, None) => Err(Error::MissingAttribute { name: index }),
                }
            }
            Expr::Str { parts } => {
                let mut buf = String::new();
                for part in parts.iter() {
                    match part {
                        StrPart::Literal(s) => buf.push_str(s),
                        StrPart::Expr(e) => {
                            let s = self.eval_coerce_to_string(&self.eval(e, stack)?)?;
                            buf.push_str(&*s);
                        }
                    }
                }
                Ok(Value::String(buf.into()))
            }
            Expr::UnaryOp { operator, value } => {
                let mut v = self.eval(value, stack)?;
                match operator {
                    UnaryOpKind::Invert => Ok(Value::Bool(!v.as_bool()?)),
                    UnaryOpKind::Negate => {
                        match &mut v {
                            Value::Int(x) => *x = x.wrapping_neg(),
                            Value::Float(x) => *x = -*x,
                            _ => return Err(v.expecting("int or float")),
                        }
                        Ok(v)
                    }
                }
            }
        }
    }

    fn eval_binop(
        &self,
        op: BinOpKind,
        lhs: &ExprRef,
        rhs: &ExprRef,
        stack: &Stack,
    ) -> Result<Value> {
        let lhs = self.eval(lhs, stack)?;
        let rhs = || self.eval(rhs, stack);
        Ok(match op {
            BinOpKind::Concat => {
                let rhs = rhs()?;
                Value::List(
                    lhs.as_list()?
                        .iter()
                        .chain(rhs.as_list()?.iter())
                        .cloned()
                        .collect(),
                )
            }
            BinOpKind::IsSet => {
                let lhs = lhs.as_attr_set()?;
                let rhs = self.eval_coerce_to_string(&rhs()?)?;
                Value::Bool(lhs.contains_key(&*rhs))
            }
            BinOpKind::Update => {
                let mut set = lhs.as_attr_set()?.clone();
                let rhs = rhs()?;
                for (k, v) in rhs.as_attr_set()? {
                    set.insert(k.clone(), v.clone());
                }
                Value::AttrSet(set)
            }
            BinOpKind::Equal => {
                let v = self.eval_equal(&lhs, &rhs()?)?;
                Value::Bool(v)
            }
            BinOpKind::NotEqual => {
                let v = self.eval_equal(&lhs, &rhs()?)?;
                Value::Bool(!v)
            }
            BinOpKind::Add => builtins::_arith_op(Builtin::Add, &lhs, &rhs()?)?,
            BinOpKind::Sub => builtins::_arith_op(Builtin::Sub, &lhs, &rhs()?)?,
            BinOpKind::Mul => builtins::_arith_op(Builtin::Mul, &lhs, &rhs()?)?,
            BinOpKind::Div => builtins::_arith_op(Builtin::Div, &lhs, &rhs()?)?,
            BinOpKind::And => {
                let v = lhs.as_bool()? && rhs()?.as_bool()?;
                Value::Bool(v)
            }
            BinOpKind::Or => {
                let v = lhs.as_bool()? || rhs()?.as_bool()?;
                Value::Bool(v)
            }
            BinOpKind::Implication => {
                let v = !lhs.as_bool()? || rhs()?.as_bool()?;
                Value::Bool(v)
            }
            BinOpKind::Less => builtins::_arith_op(Builtin::LessThan, &lhs, &rhs()?)?,
            BinOpKind::More => builtins::_arith_op(Builtin::LessThan, &rhs()?, &lhs)?,
            BinOpKind::LessOrEq => {
                let b = builtins::_arith_op(Builtin::LessThan, &rhs()?, &lhs)?.as_bool()?;
                Value::Bool(!b)
            }
            BinOpKind::MoreOrEq => {
                let b = builtins::_arith_op(Builtin::LessThan, &lhs, &rhs()?)?.as_bool()?;
                Value::Bool(!b)
            }
        })
    }

    fn eval_equal(&self, lhs: &Value, rhs: &Value) -> Result<bool> {
        Ok(match (lhs, rhs) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::String(a), Value::String(b)) | (Value::Path(a), Value::Path(b)) => a == b,
            (Value::Lambda(..), Value::Lambda(..)) => false,
            (Value::AttrSet(a), Value::AttrSet(b)) if a.keys().eq(b.keys()) => {
                if a.keys().eq(b.keys()) {
                    for (a, b) in a.values().zip(b.values()) {
                        let a = a.eval(self)?;
                        let b = b.eval(self)?;
                        if !self.eval_equal(a, b)? {
                            return Ok(false);
                        }
                    }
                    true
                } else {
                    false
                }
            }
            (Value::List(a), Value::List(b)) if a.len() == b.len() => {
                for (a, b) in a.iter().zip(b) {
                    if !self.eval_equal(a.eval(self)?, b.eval(self)?)? {
                        return Ok(false);
                    }
                }
                true
            }
            _ => false,
        })
    }

    fn eval_coerce_to_string(&self, value: &Value) -> Result<SmolStr> {
        match value {
            Value::String(s) => Ok(s.clone()),
            // FIXME: Path to store path.
            // FIXME: Derivation to store path.
            _ => Err(Error::CannotCoerceToString {
                actual: value.type_name(),
            }),
        }
    }
}

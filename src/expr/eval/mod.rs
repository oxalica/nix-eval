use crate::expr::{expr_ref::ExprRefKind, Expr, ExprRef, LambdaArg, Literal, PathAnchor, SmolStr};
use either::Either;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::PathBuf;

macro_rules! def_cont {
    ($(
        $vis:vis fn $name:ident($e:ident $(, $arg:ident : $ty:tt)*)
        $body:block
    )*) => { $(
        $vis fn $name($e: &mut crate::expr::eval::EvalState<'_>) -> crate::expr::eval::Result<()> {
            def_cont!(__impl $e $body [] 0usize $($arg $ty)*)
        }
    )* };
    (__impl $e:ident $body:block [$($getter:stmt;)*] $idx:tt) => {{
        $($getter)* $body
    }};
    (__impl $e:ident $body:block [$($getter:stmt;)*] $idx:tt $arg:ident thunk $($rest:tt)*) => {
        def_cont!(__impl $e $body [$($getter;)* let $arg = $e.pop();] ($idx + 1usize) $($rest)*)
    };
    (__impl $e:ident $body:block [$($getter:stmt;)*] $idx:tt $arg:ident $ty:tt $($rest:tt)*) => {{
        $e.swap_arg($idx);
        $e.cont(|$e| {
            $e.swap_arg($idx);
            let _ = def_cont!(__checker $ty $e.get($idx));
            def_cont!(
                __impl $e $body
                [
                    $($getter;)*
                    let $arg = $e.pop();
                    let $arg = def_cont!(__getter $ty $arg);
                ]
                ($idx + 1usize)
                $($rest)*
            )
        });
        $e.cont(def_cont!(__eval $ty));
        Ok(())
    }};
    (__checker ($ty:tt) $x:expr) => { def_cont!(__getter $ty $x) };
    (__checker $ty:tt $x:expr) => { def_cont!(__getter $ty $x) };
    // Check only.
    (__getter ($ty:tt) $x:expr) => { $x };
    (__getter any $x:expr) => { $x.unwrap_value_ref()? };
    (__getter to_string $x:expr) => { $x.unwrap_value_ref()?.as_string().unwrap() };
    (__getter string $x:expr) => { $x.unwrap_value_ref()?.as_string()? };
    (__getter bool $x:expr) => { $x.unwrap_value_ref()?.as_bool()? };
    (__getter int $x:expr) => { $x.unwrap_value_ref()?.as_int()? };
    (__getter set $x:expr) => { $x.unwrap_value_ref()?.as_set()? };
    (__getter list $x:expr) => { $x.unwrap_value_ref()?.as_list()? };
    (__eval to_string) => { crate::expr::eval::eval_coerce_to_string };
    (__eval $tt:tt) => { crate::expr::eval::eval };
}

mod builtins;
mod error;
mod value;

pub use self::builtins::Builtin;
pub use self::error::{Error, Result};
pub use self::value::{Thunk, Value};

pub struct Context {
    builtins: Thunk,
    nix_paths: HashMap<SmolStr, PathBuf>,
}

impl Context {
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
        Self {
            builtins,
            nix_paths: nix_paths.into_iter().collect(),
        }
    }

    pub fn eval(&self, expr: ExprRef, deep: bool) -> Result<Value> {
        let mut e = EvalState::new(self);
        if deep {
            e.cont(eval_deep);
        } else {
            e.cont(eval);
        }
        e.translate_expr(expr, Default::default())?;
        let mut t = e.run()?;
        match t.unwrap_value()? {
            Either::Left(v) => Ok(v),
            Either::Right(_) => unreachable!(),
        }
    }
}

type Continuation = fn(&mut EvalState<'_>) -> Result<()>;

#[allow(non_upper_case_globals)]
const eval: Continuation = Thunk::eval;

pub struct EvalState<'a> {
    ctx: &'a Context,
    conts: Vec<Continuation>,
    data: Vec<Thunk>,
}

// FIXME: Cyclic reference.
type Closure = im::Vector<Thunk>;

impl<'a> EvalState<'a> {
    fn new(ctx: &'a Context) -> Self {
        Self {
            ctx,
            conts: Vec::new(),
            data: Vec::new(),
        }
    }

    fn run(&mut self) -> Result<Thunk> {
        while let Some(cont) = self.conts.pop() {
            cont(self)?;
        }
        assert_eq!(self.data.len(), 1);
        Ok(self.data.pop().unwrap())
    }

    fn cont(&mut self, f: Continuation) {
        self.conts.push(f);
    }

    fn push(&mut self, data: Thunk) {
        self.data.push(data)
    }

    fn push_value(&mut self, v: Value) {
        self.push(Thunk::new_value(v));
    }

    fn pop(&mut self) -> Thunk {
        self.data.pop().unwrap()
    }

    fn pop_many(&mut self, cnt: usize) {
        self.data.truncate(self.data.len() - cnt);
    }

    fn get(&self, reverse_idx: usize) -> &Thunk {
        &self.data[self.data.len() - 1 - reverse_idx]
    }

    #[inline(always)]
    fn swap_arg(&mut self, reverse_idx: usize) {
        if reverse_idx > 0 {
            let len = self.data.len();
            self.data.swap(len - 1 - reverse_idx, len - 1);
        }
    }

    fn translate_expr(&mut self, expr_ref: ExprRef, closure: Closure) -> Result<()> {
        let expr = match expr_ref.kind() {
            ExprRefKind::Debruijn(idx) => {
                self.push(closure[closure.len() - 1 - idx].clone());
                self.cont(eval);
                return Ok(());
            }
            ExprRefKind::Expr(e) => e,
        };

        match expr {
            Expr::Apply { lambda, value } => {
                self.push(Thunk::new_lazy(value.clone(), closure.clone()));
                self.push(Thunk::new_lazy(lambda.clone(), closure));
                self.cont(eval_apply);
                self.cont(eval);
            }
            Expr::AttrSet { entries, dynamics } => {
                let mut set = BTreeMap::new();
                for (key, value) in entries {
                    set.insert(key.clone(), Thunk::new_lazy(value.clone(), closure.clone()));
                }
                if !dynamics.is_empty() {
                    unimplemented!();
                }
                self.push_value(Value::AttrSet(set));
            }
            &Expr::Builtin(b) => {
                if b.params() == 0 {
                    self.cont(b.continuation());
                } else {
                    self.push_value(Value::PartialBuiltin(b, Vec::with_capacity(3)));
                }
            }
            Expr::Lambda { .. } => {
                let v = Value::Lambda(expr_ref.clone(), closure);
                self.push_value(v);
            }
            Expr::LetIn { exprs, body } => {
                let mut closure = closure;
                for e in exprs.iter() {
                    // Placeholder stack.
                    closure.push_back(Thunk::new_lazy(e.clone(), Closure::new()));
                }
                for thunk in closure.iter().rev().take(exprs.len()) {
                    unsafe { thunk.set_closure(closure.clone()) };
                }
                self.push(Thunk::new_lazy(body.clone(), closure));
                self.cont(eval);
            }
            Expr::List { items } => {
                let list = items
                    .iter()
                    .map(|e| Thunk::new_lazy(e.clone(), closure.clone()))
                    .collect();
                let v = Value::List(list);
                self.push_value(v);
            }
            Expr::Literal(lit) => {
                let v = match lit {
                    &Literal::Bool(x) => Value::Bool(x),
                    &Literal::Float(x) => Value::Float(x),
                    &Literal::Int(x) => Value::Int(x),
                    Literal::String(s) => Value::String(s.to_string()),
                    Literal::Path(anchor, p) => Value::Path(match anchor {
                        PathAnchor::Absolute => p.to_string(),
                        PathAnchor::Relative => {
                            todo!()
                        }
                        PathAnchor::Home => {
                            todo!()
                        }
                        PathAnchor::Store => {
                            if let Some(path) = self.ctx.nix_paths.get(&*p) {
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
                };
                self.push_value(v);
            }
        }
        Ok(())
    }
}

fn eval_apply(e: &mut EvalState<'_>) -> Result<()> {
    let lam = e.pop();
    let arg = e.pop();

    let lam_value = lam.unwrap_value_ref()?;
    match lam_value {
        Value::PartialBuiltin(b, args) => {
            // FIXME: No clone.
            let mut args = args.clone();
            args.push(arg);
            assert!(args.len() <= b.params());
            if args.len() == b.params() {
                args.reverse();
                for arg in args {
                    e.push(arg);
                }
                e.cont(b.continuation());
            } else {
                e.push_value(Value::PartialBuiltin(*b, args));
            }
            return Ok(());
        }
        _ => {}
    }

    let (lam_expr, lam_closure) = lam_value.as_lambda()?;

    if let ExprRefKind::Expr(Expr::Lambda {
        arg: LambdaArg::Bind,
        body,
    }) = lam_expr.kind()
    {
        let mut lam_closure = lam_closure.clone();
        lam_closure.push_back(arg);
        e.push(Thunk::new_lazy(body.clone(), lam_closure));
        e.cont(eval);
        return Ok(());
    }

    e.cont(|e| {
        let arg = e.pop();
        let set = arg.unwrap_value_ref()?.as_set()?;
        let lam = e.pop();
        let (lam_expr, lam_closure) = lam.unwrap_value_ref()?.as_lambda()?;

        let body = match lam_expr.kind() {
            ExprRefKind::Expr(Expr::Lambda {
                arg: LambdaArg::OpenPattern { required_names },
                body,
            }) => {
                for name in required_names.iter() {
                    if !set.contains_key(&*name) {
                        return Err(Error::MissingArgument { name: name.clone() });
                    }
                }
                body
            }
            ExprRefKind::Expr(Expr::Lambda {
                arg:
                    LambdaArg::ClosePattern {
                        required_names,
                        optional_names,
                    },
                body,
            }) => {
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
                body
            }
            _ => unreachable!(),
        };

        let mut lam_closure = lam_closure.clone();
        lam_closure.push_back(arg);
        e.push(Thunk::new_lazy(body.clone(), lam_closure));
        e.cont(eval);
        Ok(())
    });
    e.push(lam);
    e.push(arg);
    e.cont(eval);
    Ok(())
}

// TODO: Handle path and derivations.
fn eval_coerce_to_string(e: &mut EvalState<'_>) -> Result<()> {
    e.cont(|e| {
        let _ = e.get(0).unwrap_value_ref()?.as_string()?;
        Ok(())
    });
    e.cont(eval);
    Ok(())
}

fn eval_deep(e: &mut EvalState<'_>) -> Result<()> {
    e.cont(|e| {
        // Avoid lifetime issurs on `e`.
        let t = e.get(0).clone();
        match t.unwrap_value_ref()? {
            Value::Bool(_)
            | Value::Float(_)
            | Value::Int(_)
            | Value::String(_)
            | Value::Path(_)
            | Value::Lambda(..)
            | Value::PartialBuiltin(..) => {}
            Value::AttrSet(set) => {
                for v in set.values() {
                    e.cont(|e| {
                        e.pop();
                        Ok(())
                    });
                    e.push(v.clone());
                    e.cont(eval_deep);
                }
            }
            Value::List(xs) => {
                for v in xs {
                    e.cont(|e| {
                        e.pop();
                        Ok(())
                    });
                    e.push(v.clone());
                    e.cont(eval_deep);
                }
            }
        }
        Ok(())
    });
    e.cont(eval);
    Ok(())
}

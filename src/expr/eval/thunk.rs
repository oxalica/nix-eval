use crate::expr::{
    eval::{Error, Evaluator, Result, Stack},
    Builtin, ExprRef, SmolStr, Value,
};
use std::cell::UnsafeCell;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

#[derive(Clone)]
pub enum CValue {
    Simple(Value),
    AttrSet(BTreeMap<SmolStr, Arc<Thunk>>),
    List(Vec<Arc<Thunk>>),
    Lambda(ExprRef, Stack),
    PartialBuiltin(Builtin, Vec<Arc<Thunk>>),
}

impl fmt::Debug for CValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Simple(v) => fmt::Debug::fmt(v, f),
            Self::AttrSet(set) => f.debug_tuple("AttrSet").field(set).finish(),
            Self::List(xs) => f.debug_tuple("List").field(xs).finish(),
            Self::Lambda(_, _) => f.write_str("Lambda(..)"),
            Self::PartialBuiltin(b, args) => f
                .debug_tuple("PartialBuiltin")
                .field(b)
                .field(&args.len())
                .finish(),
        }
    }
}

impl CValue {
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Simple(Value::Bool(_)) => "bool",
            Self::Simple(Value::Float(_)) => "float",
            Self::Simple(Value::Integer(_)) => "int",
            Self::Simple(Value::String(_)) => "string",
            Self::Simple(Value::Path(..)) => "path",
            Self::AttrSet(..) => "set",
            Self::List(..) => "list",
            Self::Lambda(..) | Self::PartialBuiltin(..) => "lambda",
        }
    }

    pub fn expecting(&self, expect: &'static str) -> Error {
        Error::TypeError {
            expect,
            actual: self.type_name(),
        }
    }

    pub fn as_lambda(&self) -> Result<(&ExprRef, &Stack)> {
        match self {
            Self::Lambda(expr, stack) => Ok((&expr, &stack)),
            _ => Err(self.expecting("lambda")),
        }
    }

    pub fn as_bool(&self) -> Result<bool> {
        match self {
            &Self::Simple(Value::Bool(b)) => Ok(b),
            _ => Err(self.expecting("bool")),
        }
    }

    pub fn as_attr_set(&self) -> Result<&BTreeMap<SmolStr, Arc<Thunk>>> {
        match self {
            Self::AttrSet(set) => Ok(set),
            _ => Err(self.expecting("set")),
        }
    }

    pub fn as_list(&self) -> Result<&[Arc<Thunk>]> {
        match self {
            Self::List(v) => Ok(v),
            _ => Err(self.expecting("list")),
        }
    }
}

pub struct Thunk {
    inner: UnsafeCell<ThunkState>,
}

enum ThunkState {
    Lazy(ExprRef, Stack),
    Evaluating,
    Done(Result<CValue>),
}

impl Thunk {
    pub fn new_lazy(expr: ExprRef, stack: Stack) -> Arc<Thunk> {
        Arc::new(Self {
            inner: UnsafeCell::new(ThunkState::Lazy(expr, stack)),
        })
    }

    pub fn new_value(v: CValue) -> Arc<Thunk> {
        Arc::new(Self {
            inner: UnsafeCell::new(ThunkState::Done(Ok(v))),
        })
    }

    pub fn new_value_cyclic(f: impl FnOnce(Arc<Thunk>) -> CValue) -> Arc<Thunk> {
        let this = Arc::new(Self {
            inner: UnsafeCell::new(ThunkState::Evaluating),
        });
        let v = f(this.clone());
        unsafe { *this.inner.get() = ThunkState::Done(Ok(v)) };
        this
    }

    pub unsafe fn set_stack(&self, new_stack: Stack) {
        match &mut *self.inner.get() {
            ThunkState::Lazy(_, stack) => *stack = new_stack,
            _ => unreachable!(),
        }
    }

    fn eval_with(&self, f: impl FnOnce(&ExprRef, &Stack) -> Result<CValue>) -> Result<&CValue> {
        match unsafe { &*self.inner.get() } {
            ThunkState::Lazy { .. } => {}
            ThunkState::Evaluating => return Err(Error::InfiniteRecursion),
            ThunkState::Done(Ok(v)) => return Ok(v),
            ThunkState::Done(Err(err)) => return Err(err.clone()),
        }
        let (expr, stack) =
            match unsafe { std::ptr::replace(self.inner.get(), ThunkState::Evaluating) } {
                ThunkState::Lazy(expr, stack) => (expr, stack),
                _ => unreachable!(),
            };
        let ret = f(&expr, &stack);
        unsafe { *self.inner.get() = ThunkState::Done(ret) };
        match unsafe { &*self.inner.get() } {
            ThunkState::Done(Ok(v)) => Ok(v),
            ThunkState::Done(Err(err)) => Err(err.clone()),
            _ => unreachable!(),
        }
    }

    pub fn eval(&self, eval: &Evaluator) -> Result<&CValue> {
        self.eval_with(|expr, stack| eval.eval(expr, stack))
    }

    pub fn eval_deep(&self, eval: &Evaluator) -> Result<&CValue> {
        self.eval_with(|expr, stack| eval.eval_deep(expr, stack))
    }
}

impl fmt::Debug for Thunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match unsafe { &*self.inner.get() } {
            ThunkState::Lazy { .. } | ThunkState::Evaluating => f.write_str("Thunk(..)"),
            ThunkState::Done(Ok(v)) => fmt::Debug::fmt(v, f),
            ThunkState::Done(Err(err)) => fmt::Debug::fmt(err, f),
        }
    }
}

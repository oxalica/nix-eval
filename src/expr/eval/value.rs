use crate::expr::{
    eval::{Error, Evaluator, Result, Stack},
    Builtin, ExprRef, SmolStr,
};
use std::cell::UnsafeCell;
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::sync::Arc;

const VALUE_DUMP_INDENT: usize = 2;

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(SmolStr),
    Path(SmolStr),

    AttrSet(BTreeMap<SmolStr, Arc<Thunk>>),
    List(Vec<Arc<Thunk>>),
    Lambda(ExprRef, Stack),
    PartialBuiltin(Builtin, Vec<Arc<Thunk>>),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "bool",
            Self::Float(_) => "float",
            Self::Int(_) => "int",
            Self::String(_) => "string",
            Self::Path(..) => "path",
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
            &Self::Bool(b) => Ok(b),
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

    pub fn dump(&self) -> impl fmt::Display + '_ {
        struct Wrapper<'a>(&'a Value);
        impl<'a> fmt::Display for Wrapper<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.dump_inner(f, 0, &mut HashSet::new())
            }
        }
        Wrapper(self)
    }

    fn dump_inner(
        &self,
        f: &mut fmt::Formatter,
        mut indent: usize,
        visited: &mut HashSet<*const Value>,
    ) -> fmt::Result {
        fn is_nix_ident_key(s: &str) -> bool {
            !s.is_empty()
                && s.as_bytes()[0].is_ascii_alphabetic()
                && s.bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b"_-'".contains(&b))
        }

        if !visited.insert(self) {
            return f.write_str("<CYCLE>");
        }

        match self {
            Self::Bool(v) => fmt::Debug::fmt(v, f)?,
            Self::Float(v) => fmt::Debug::fmt(v, f)?,
            Self::Int(v) => fmt::Debug::fmt(v, f)?,
            Self::String(v) => fmt::Debug::fmt(v, f)?,
            Self::Path(v) => fmt::Debug::fmt(v, f)?,

            Self::Lambda(_, _) => f.write_str("<LAMBDA>")?,
            Self::PartialBuiltin(b, args) => write!(f, "<LABMDA.{:?}.{}>", b, args.len())?,
            Self::AttrSet(set) => {
                if set.is_empty() {
                    f.write_str("{ }")?
                } else {
                    f.write_str("{\n")?;
                    indent += VALUE_DUMP_INDENT;
                    for (k, v) in set {
                        write!(f, "{:indent$}", "", indent = indent)?;
                        if is_nix_ident_key(k) {
                            write!(f, "{} = ", k)?;
                        } else {
                            write!(f, "{:?} = ", k)?;
                        }
                        v.dump_inner(f, indent, visited)?;
                        write!(f, ";\n")?;
                    }
                    indent -= VALUE_DUMP_INDENT;
                    write!(f, "{:indent$}}}", "", indent = indent)?;
                }
            }
            Self::List(xs) => {
                if xs.is_empty() {
                    f.write_str("[ ]")?
                } else {
                    f.write_str("[\n")?;
                    indent += VALUE_DUMP_INDENT;
                    for v in xs {
                        write!(f, "{:indent$}", "", indent = indent)?;
                        v.dump_inner(f, indent, visited)?;
                        write!(f, "\n")?;
                    }
                    indent -= VALUE_DUMP_INDENT;
                    write!(f, "{:indent$}]", "", indent = indent)?;
                }
            }
        }

        visited.remove(&(self as *const _));
        Ok(())
    }
}

pub struct Thunk {
    inner: UnsafeCell<ThunkState>,
}

enum ThunkState {
    Lazy(ExprRef, Stack),
    Evaluating,
    Done(Result<Value>),
}

impl Thunk {
    pub fn new_lazy(expr: ExprRef, stack: Stack) -> Arc<Thunk> {
        Arc::new(Self {
            inner: UnsafeCell::new(ThunkState::Lazy(expr, stack)),
        })
    }

    pub fn new_value(v: Value) -> Arc<Thunk> {
        Arc::new(Self {
            inner: UnsafeCell::new(ThunkState::Done(Ok(v))),
        })
    }

    pub fn new_value_cyclic(f: impl FnOnce(Arc<Thunk>) -> Value) -> Arc<Thunk> {
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

    fn eval_with(&self, f: impl FnOnce(&ExprRef, &Stack) -> Result<Value>) -> Result<&Value> {
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

    pub fn eval(&self, eval: &Evaluator) -> Result<&Value> {
        self.eval_with(|expr, stack| eval.eval(expr, stack))
    }

    pub fn eval_deep(&self, eval: &Evaluator) -> Result<&Value> {
        self.eval_with(|expr, stack| eval.eval_deep(expr, stack))
    }

    pub fn dump(&self) -> impl fmt::Display + '_ {
        struct Wrapper<'a>(&'a Thunk);
        impl<'a> fmt::Display for Wrapper<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.dump_inner(f, 0, &mut HashSet::new())
            }
        }
        Wrapper(self)
    }

    fn dump_inner(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
        visited: &mut HashSet<*const Value>,
    ) -> fmt::Result {
        match unsafe { &*self.inner.get() } {
            ThunkState::Lazy(..) | ThunkState::Evaluating => f.write_str("<CODE>"),
            ThunkState::Done(Err(_)) => f.write_str("<ERROR>"),
            ThunkState::Done(Ok(v)) => v.dump_inner(f, indent, visited),
        }
    }
}

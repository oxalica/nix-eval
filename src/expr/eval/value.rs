use crate::expr::{
    eval::{Closure, Error, EvalState, Result},
    Builtin, ExprRef, SmolStr,
};
use either::Either;
use std::cell::UnsafeCell;
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::rc::Rc;

const VALUE_DUMP_INDENT: usize = 2;

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(SmolStr),
    Path(SmolStr),

    AttrSet(BTreeMap<SmolStr, ThunkRef>),
    List(Vec<ThunkRef>),
    Lambda(ExprRef, Closure),
    PartialBuiltin(Builtin, Vec<ThunkRef>),
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

    pub fn as_lambda(&self) -> Result<(&ExprRef, &Closure)> {
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

    pub fn as_int(&self) -> Result<i64> {
        match self {
            &Self::Int(x) => Ok(x),
            _ => Err(self.expecting("int")),
        }
    }

    pub fn as_set(&self) -> Result<&BTreeMap<SmolStr, ThunkRef>> {
        match self {
            Self::AttrSet(set) => Ok(set),
            _ => Err(self.expecting("set")),
        }
    }

    pub fn as_list(&self) -> Result<&[ThunkRef]> {
        match self {
            Self::List(v) => Ok(v),
            _ => Err(self.expecting("list")),
        }
    }

    pub fn as_string(&self) -> Result<&str> {
        match self {
            Self::String(s) => Ok(&**s),
            _ => Err(self.expecting("string")),
        }
    }

    pub fn as_int_or_float(&self) -> Result<Either<i64, f64>> {
        match self {
            &Self::Int(x) => Ok(Either::Left(x)),
            &Self::Float(x) => Ok(Either::Right(x)),
            _ => Err(self.expecting("int or float")),
        }
    }

    pub fn as_int_or_float_or_string(&self) -> Result<Either<Either<i64, f64>, &str>> {
        match self {
            &Self::Int(x) => Ok(Either::Left(Either::Left(x))),
            &Self::Float(x) => Ok(Either::Left(Either::Right(x))),
            Self::String(s) => Ok(Either::Right(&*s)),
            _ => Err(self.expecting("int or float or string")),
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
                        v.0.dump_inner(f, indent, visited)?;
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
                        v.0.dump_inner(f, indent, visited)?;
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

#[derive(Clone)]
pub struct ThunkRef(Rc<Thunk>);

impl ThunkRef {
    // pub fn get_mut(&mut self) -> Option<&mut Value> {
    //     let t = Rc::get_mut(&mut self.0)?;
    //     unsafe { &mut *t.inner.get() }
    // }
}

impl std::ops::Deref for ThunkRef {
    type Target = Thunk;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

pub struct Thunk {
    inner: UnsafeCell<ThunkState>,
}

enum ThunkState {
    Lazy(ExprRef, Closure),
    Evaluating,
    Done(Result<Value>),
}

impl Thunk {
    pub fn new_lazy(expr: ExprRef, closure: Closure) -> ThunkRef {
        ThunkRef(Rc::new(Self {
            inner: UnsafeCell::new(ThunkState::Lazy(expr, closure)),
        }))
    }

    pub fn new_value(v: Value) -> ThunkRef {
        ThunkRef(Rc::new(Self {
            inner: UnsafeCell::new(ThunkState::Done(Ok(v))),
        }))
    }

    pub fn new_value_cyclic(f: impl FnOnce(ThunkRef) -> Value) -> ThunkRef {
        let this = Rc::new(Self {
            inner: UnsafeCell::new(ThunkState::Evaluating),
        });
        let v = f(ThunkRef(this.clone()));
        unsafe { *this.inner.get() = ThunkState::Done(Ok(v)) };
        ThunkRef(this)
    }

    pub unsafe fn set_closure(&self, new_closure: Closure) {
        match &mut *self.inner.get() {
            ThunkState::Lazy(_, closure) => *closure = new_closure,
            _ => unreachable!(),
        }
    }

    pub fn unwrap(&self) -> Result<&Value> {
        match unsafe { &*self.inner.get() } {
            ThunkState::Done(Ok(v)) => Ok(v),
            ThunkState::Done(Err(e)) => Err(e.clone()),
            _ => panic!("Thunk is not ready"),
        }
    }

    pub(crate) fn eval(e: &mut EvalState<'_>) -> Result<()> {
        let inner = &e.get(0).inner;
        match unsafe { &*inner.get() } {
            ThunkState::Lazy { .. } => {}
            ThunkState::Evaluating => return Err(Error::InfiniteRecursion),
            ThunkState::Done(Ok(_)) => return Ok(()),
            ThunkState::Done(Err(err)) => return Err(err.clone()),
        }
        let (expr, closure) =
            match unsafe { std::ptr::replace(inner.get(), ThunkState::Evaluating) } {
                ThunkState::Lazy(expr, closure) => (expr, closure),
                _ => unreachable!(),
            };
        e.cont(|e| {
            let ret = e.pop();
            let ret = ret.unwrap();
            let inner = &e.get(0).inner;
            assert!(matches!(unsafe { &*inner.get() }, ThunkState::Evaluating));
            // FIXME: No clone.
            unsafe { *inner.get() = ThunkState::Done(ret.map(|v| v.clone())) };
            Ok(())
        });
        e.translate_expr(expr, closure)?;
        Ok(())
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

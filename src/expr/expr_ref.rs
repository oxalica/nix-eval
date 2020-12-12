use crate::expr::{Expr, SmolStr, Value};
use std::fmt;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::num::NonZeroUsize;
use std::sync::Arc;

/// Reference to an `Expr`, with space optimization.
pub struct ExprRef {
    // Last 2 bit:
    // 00: Arc<Expr>.
    // 01: De-bruijn index shifted 1.
    repr: NonZeroUsize,
    _marker: PhantomData<*mut Expr>,
}
static_assertions::const_assert!(std::mem::align_of::<Expr>() >= 4);

unsafe impl Send for ExprRef {}
unsafe impl Sync for ExprRef {}

#[derive(Debug)]
pub enum ExprRefKind<'a> {
    Expr(&'a Expr),
    Debruijn(usize),
}

impl ExprRef {
    pub fn new(expr: Expr) -> Self {
        let ptr = Arc::into_raw(Arc::new(expr)) as usize;
        debug_assert_eq!(ptr & 0b11, 0);
        Self {
            repr: NonZeroUsize::new(ptr).unwrap(),
            _marker: PhantomData,
        }
    }

    fn kind(&self) -> ExprRefKind<'_> {
        match self.repr.get() & 0b11 {
            0b00 => ExprRefKind::Expr(unsafe { &*(self.repr.get() as *const Expr) }),
            0b01 => ExprRefKind::Debruijn(self.repr.get() >> 2),
            _ => unreachable!(),
        }
    }

    pub fn debruijn(i: usize) -> Self {
        Self {
            repr: NonZeroUsize::new(i.checked_shl(2).unwrap() | 0b01).unwrap(),
            _marker: PhantomData,
        }
    }
}

impl Drop for ExprRef {
    fn drop(&mut self) {
        // Arc<Expr>.
        if self.repr.get() & 0b11 == 0 {
            unsafe { drop(Arc::from_raw(self.repr.get() as *mut Expr)) };
        }
    }
}

impl Clone for ExprRef {
    fn clone(&self) -> Self {
        // Arc<Expr>.
        let repr = if self.repr.get() & 0b11 == 0 {
            unsafe {
                // The original Arc is borrowed and should not be dropped.
                let expr = ManuallyDrop::new(Arc::from_raw(self.repr.get() as *const Expr));
                NonZeroUsize::new(Arc::into_raw(Arc::clone(&*expr)) as usize).unwrap()
            }
        } else {
            self.repr
        };
        Self {
            repr,
            _marker: PhantomData,
        }
    }
}

impl fmt::Debug for ExprRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.kind(), f)
    }
}

impl From<Expr> for ExprRef {
    fn from(e: Expr) -> Self {
        Self::new(e)
    }
}

impl From<Value> for ExprRef {
    fn from(v: Value) -> Self {
        Self::new(Expr::Literal(v))
    }
}

impl From<SmolStr> for ExprRef {
    fn from(s: SmolStr) -> Self {
        Self::new(Expr::Literal(Value::String(s)))
    }
}

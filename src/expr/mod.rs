use std::collections::{BTreeMap, BTreeSet};

pub use rnix::value::Anchor as PathAnchor;
pub use rnix::SmolStr;

pub use self::expr_ref::{ExprRef, ExprRefKind};
pub use crate::expr::eval::Builtin;

pub mod eval;
pub mod expr_ref;
pub mod lower;

// FIXME: Optimize memory cost.
static_assertions::assert_eq_size!(Literal, [u8; 32]);
static_assertions::assert_eq_size!(Expr, [u8; 64]);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(SmolStr),
    Path(PathAnchor, SmolStr),
}

#[derive(Debug)]
pub enum Expr {
    Apply {
        lambda: ExprRef,
        value: ExprRef,
    },
    AttrSet {
        entries: BTreeMap<SmolStr, ExprRef>,
        dynamics: Box<[(ExprRef, ExprRef)]>,
    },
    Builtin(Builtin),
    Lambda {
        arg: LambdaArg,
        body: ExprRef,
    },
    LetIn {
        exprs: Box<[ExprRef]>,
        body: ExprRef,
    },
    List {
        items: Box<[ExprRef]>,
    },
    Literal(Literal),
}

#[derive(Debug)]
pub enum LambdaArg {
    /// `x: expr`
    Bind,
    /// `{ a, b ? expr }: expr`
    ClosePattern {
        required_names: Box<[SmolStr]>,
        optional_names: BTreeSet<SmolStr>,
    },
    /// `{ a, b ? expr, ... }: expr`
    OpenPattern { required_names: Box<[SmolStr]> },
}

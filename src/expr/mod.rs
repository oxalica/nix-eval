use std::collections::{BTreeMap, BTreeSet};

pub use rnix::types::{BinOpKind, UnaryOpKind};
pub use rnix::value::Anchor as PathAnchor;
pub use rnix::SmolStr;

pub use self::builtins::Builtin;
pub use self::expr_ref::{ExprRef, ExprRefKind};

pub mod builtins;
pub mod expr_ref;
pub mod lower;

// FIXME: Optimize memory cost.
static_assertions::assert_eq_size!(Value, [u8; 32]);
static_assertions::assert_eq_size!(Expr, [u8; 64]);

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Float(f64),
    Integer(i64),
    String(SmolStr),
    Path(PathAnchor, SmolStr),
}

#[derive(Debug)]
pub enum Expr {
    Apply {
        lambda: ExprRef,
        value: ExprRef,
    },
    Assert {
        condition: ExprRef,
        body: ExprRef,
    },
    AttrSet {
        entries: BTreeMap<SmolStr, ExprRef>,
        dynamics: Box<[(ExprRef, ExprRef)]>,
    },
    BinOp {
        operator: BinOpKind,
        lhs: ExprRef,
        rhs: ExprRef,
    },
    Builtin(builtins::Builtin),
    IfElse {
        condition: ExprRef,
        then_body: ExprRef,
        else_body: ExprRef,
    },
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
    Literal(Value),
    Select {
        set: ExprRef,
        index: ExprRef,
        or_default: Option<ExprRef>,
    },
    Str {
        parts: Box<[StrPart]>,
    },
    UnaryOp {
        operator: UnaryOpKind,
        value: ExprRef,
    },
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

#[derive(Debug)]
pub enum StrPart {
    Literal(SmolStr),
    Expr(ExprRef),
}

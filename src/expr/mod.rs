use std::collections::BTreeMap;

pub use rnix::types::{BinOpKind, UnaryOpKind};
pub use rnix::value::Anchor as PathAnchor;
pub use rnix::SmolStr;

pub use self::expr_ref::{ExprRef, ExprRefKind};

pub mod expr_ref;
pub mod lower;

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
    Bind,
    Pattern {
        required_names: Box<[SmolStr]>,
        ellipsis: bool,
    },
}

#[derive(Debug)]
pub enum StrPart {
    Literal(SmolStr),
    Expr(ExprRef),
}

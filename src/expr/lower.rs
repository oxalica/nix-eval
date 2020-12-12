use crate::expr::*;
use rnix::types::{
    Dynamic, EntryHolder, Ident, Lambda, ParsedType, Pattern, Root, TokenWrapper as _,
    TypedNode as _, With, Wrapper as _,
};
use rnix::{SyntaxKind, TextRange};
use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Parse error:\n{}", fmt_errors(.0))]
    ParseError(Vec<rnix::parser::ParseError>),
    #[error("Undefined variable {:?} at {}", name, pos)]
    UndefinedVariable { name: SmolStr, pos: TextRange },
    #[error("Duplicated name {:?} at {}", name, pos)]
    DuplicatedName { name: SmolStr, pos: TextRange },
    #[error("Unsupported dynamic attribute at {}", pos)]
    UnexpectedDynamicAttr { pos: TextRange },
}

fn fmt_errors(errors: &[impl fmt::Display]) -> impl fmt::Display + '_ {
    struct Wrapper<'a, T>(&'a [T]);

    impl<'a, T: fmt::Display> fmt::Display for Wrapper<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            for v in self.0 {
                write!(f, "    {}\n", v)?;
            }
            Ok(())
        }
    }

    Wrapper(errors)
}

type Result<T> = std::result::Result<T, Error>;
type Node = rnix::SyntaxNode;

pub fn lower(input: &str) -> Result<ExprRef> {
    let ast = rnix::parse(input);
    let errors = ast.errors();
    if !errors.is_empty() {
        return Err(Error::ParseError(errors));
    }

    let mut lower = Lowerer::new();
    let ret = lower.root(ast.root())?;
    Ok(ret)
}

#[derive(Debug)]
struct Lowerer {
    let_scopes: Vec<BTreeMap<SmolStr, StackIdx>>,
    with_scopes: Vec<StackIdx>,
    stack_depth: usize,
}

/// The index of a stack variable. Starting from 0.
/// Use `stack_depth - 1 - StackIdx` to get De-Bruijn index.
#[derive(Debug, Clone, Copy)]
struct StackIdx(usize);

macro_rules! insert_or_dup {
    ($map:expr, $k:expr, $v:expr, $node:expr) => {{
        let name: SmolStr = $k;
        if $map.insert(name.clone(), $v).is_some() {
            return Err(Error::DuplicatedName {
                name,
                pos: $node.node().text_range(),
            });
        }
    }};
}

impl Lowerer {
    fn new() -> Self {
        Self {
            let_scopes: Vec::new(),
            with_scopes: Vec::new(),
            stack_depth: 0,
        }
    }

    fn to_debruijn(&self, idx: StackIdx) -> ExprRef {
        ExprRef::debruijn(self.stack_depth - 1 - idx.0)
    }

    fn stack_idx(&self, offset: usize) -> StackIdx {
        StackIdx(self.stack_depth + offset)
    }

    fn lookup_scope(&mut self, n: &Ident) -> Result<ExprRef> {
        let name = n.as_str();
        for scope in self.let_scopes.iter().rev() {
            if let Some(&idx) = scope.get(name) {
                return Ok(self.to_debruijn(idx));
            }
        }
        if let Some(&idx) = self.with_scopes.last() {
            return Ok(Expr::Select {
                set: self.to_debruijn(idx),
                index: Value::String(name.into()).into(),
                or_default: None,
            }
            .into());
        }
        if let Some(e) = crate::expr::builtins::GLOBAL_BUILTIN_EXPRS.get(name) {
            return Ok(e.clone());
        }
        Err(Error::UndefinedVariable {
            name: name.into(),
            pos: n.node().text_range(),
        })
    }

    fn push_scope(&mut self, iter: impl IntoIterator<Item = (SmolStr, StackIdx)>) {
        self.let_scopes.push(iter.into_iter().collect());
    }

    fn pop_scope(&mut self) {
        self.let_scopes.pop();
    }

    fn root(&mut self, n: Root) -> Result<ExprRef> {
        self.expr(n.inner().unwrap())
    }

    fn expr(&mut self, n: Node) -> Result<ExprRef> {
        let e = match ParsedType::try_from(n.clone()).map_err(|e| e.0).unwrap() {
            ParsedType::Apply(n) => Expr::Apply {
                lambda: self.expr(n.lambda().unwrap())?,
                value: self.expr(n.value().unwrap())?,
            },
            ParsedType::Assert(n) => Expr::Assert {
                condition: self.expr(n.condition().unwrap())?,
                body: self.expr(n.body().unwrap())?,
            },
            ParsedType::Ident(n) => return self.lookup_scope(&n),
            ParsedType::IfElse(n) => Expr::IfElse {
                condition: self.expr(n.condition().unwrap())?,
                then_body: self.expr(n.body().unwrap())?,
                else_body: self.expr(n.else_body().unwrap())?,
            },
            ParsedType::Select(n) => Expr::Select {
                set: self.expr(n.set().unwrap())?,
                index: self.index(n.index().unwrap())?,
                or_default: None,
            },
            ParsedType::Lambda(n) => return self.lambda(n),
            // `let { ...; body = ...; }` =>
            // `(rec { ...; body = ...; }).body`
            ParsedType::LegacyLet(n) => Expr::Select {
                set: self.rec_attr_set_like(&n, None)?,
                index: Value::String("body".into()).into(),
                or_default: None,
            },
            ParsedType::LetIn(n) => return self.rec_attr_set_like(&n, Some(n.body().unwrap())),
            ParsedType::List(n) => Expr::List {
                items: n.items().map(|n| self.expr(n)).collect::<Result<_>>()?,
            },
            ParsedType::BinOp(n) => Expr::BinOp {
                operator: n.operator(),
                lhs: self.expr(n.lhs().unwrap())?,
                rhs: self.expr(n.rhs().unwrap())?,
            },
            ParsedType::OrDefault(n) => {
                let s = n.index().unwrap();
                Expr::Select {
                    set: self.expr(s.set().unwrap())?,
                    index: self.index(s.index().unwrap())?,
                    or_default: Some(self.expr(n.default().unwrap())?),
                }
            }
            ParsedType::Paren(n) => return self.expr(n.inner().unwrap()),
            ParsedType::AttrSet(n) if n.recursive() => return self.rec_attr_set_like(&n, None),
            ParsedType::AttrSet(n) => return self.attr_set(&n),
            ParsedType::Str(n) => Expr::Str {
                parts: n
                    .parts()
                    .into_iter()
                    .map(|part| match part {
                        rnix::value::StrPart::Literal(s) => Ok(StrPart::Literal(s.into())),
                        rnix::value::StrPart::Ast(n) => {
                            // FIXME: NODE_STRING_INTERPOL is not a Wrapper?
                            assert_eq!(n.kind(), SyntaxKind::NODE_STRING_INTERPOL);
                            let inner = n.first_child().unwrap();
                            Ok(StrPart::Expr(self.expr(inner)?))
                        }
                    })
                    .collect::<Result<_>>()?,
            },
            ParsedType::UnaryOp(n) => Expr::UnaryOp {
                operator: n.operator(),
                value: self.expr(n.value().unwrap())?,
            },
            ParsedType::Value(n) => Expr::Literal(match n.to_value().unwrap() {
                rnix::NixValue::Float(v) => Value::Float(v),
                rnix::NixValue::Integer(v) => Value::Integer(v),
                rnix::NixValue::String(v) => Value::String(v.into()),
                rnix::NixValue::Path(anchor, path) => Value::Path(anchor, path.into()),
            }),
            ParsedType::With(n) => return self.with(n),
            ParsedType::Key(_)
            | ParsedType::Dynamic(_)
            | ParsedType::Error(_)
            | ParsedType::Inherit(_)
            | ParsedType::InheritFrom(_)
            | ParsedType::PatBind(_)
            | ParsedType::PatEntry(_)
            | ParsedType::Pattern(_)
            | ParsedType::Root(_)
            | ParsedType::KeyValue(_) => {
                unreachable!("Unexpected node kind {:?} at {}", n.kind(), n.text_range())
            }
        };
        Ok(e.into())
    }

    fn index(&mut self, n: Node) -> Result<ExprRef> {
        match n.kind() {
            SyntaxKind::NODE_IDENT => {
                let n = Ident::cast(n).unwrap();
                Ok(Value::String(n.as_str().into()).into())
            }
            SyntaxKind::NODE_DYNAMIC => {
                let n = Dynamic::cast(n).unwrap();
                self.expr(n.inner().unwrap())
            }
            k => unreachable!("Unexpected node kind {:?} at {}", k, n.text_range()),
        }
    }

    fn lambda(&mut self, n: Lambda) -> Result<ExprRef> {
        let arg = n.arg().unwrap();
        let (arg, body) = match arg.kind() {
            SyntaxKind::NODE_IDENT => {
                let arg = Ident::cast(arg).unwrap();
                self.push_scope(std::iter::once((arg.as_str().into(), self.stack_idx(0))));
                self.stack_depth += 1;
                let body = self.expr(n.body().unwrap())?;
                self.stack_depth -= 1;
                self.pop_scope();
                (LambdaArg::Bind, body)
            }
            SyntaxKind::NODE_PATTERN => {
                let arg = Pattern::cast(arg).unwrap();
                let let_cnt = arg.entries().count();

                // Initialize pattern bindings first.
                let scope = {
                    let mut scope = BTreeMap::new();
                    if let Some(at) = arg.at() {
                        scope.insert(at.as_str().into(), self.stack_idx(0));
                    }
                    for (i, ent) in arg.entries().enumerate() {
                        let ident = ent.name().unwrap();
                        let name: SmolStr = ident.as_str().into();
                        let var = self.stack_idx(1 + i);
                        insert_or_dup!(scope, name, var, ident);
                    }
                    scope
                };
                self.push_scope(scope);
                self.stack_depth += 1 + let_cnt; // The first one is argument.

                // Then handle `? expr` parts, since they can recursively refer to other arguments.
                let mut required_names = Vec::new();
                let mut optional_names = BTreeSet::new();
                let mut let_exprs = Vec::new();
                for ent in arg.entries() {
                    let name: SmolStr = ent.name().unwrap().as_str().into();
                    let index = Value::String(name.clone()).into();
                    let or_default = match ent.default() {
                        Some(default) => {
                            optional_names.insert(name.clone());
                            Some(self.expr(default)?)
                        }
                        None => {
                            required_names.push(name.clone());
                            None
                        }
                    };
                    let select = Expr::Select {
                        // Lambda argument.
                        set: ExprRef::debruijn(let_cnt),
                        index,
                        or_default,
                    }
                    .into();
                    let_exprs.push(select);
                }

                let mut body = self.expr(n.body().unwrap())?;

                self.stack_depth -= 1 + let_cnt; // The first one is argument.
                self.pop_scope();

                assert_eq!(let_exprs.len(), let_cnt);
                if let_cnt != 0 {
                    body = Expr::LetIn {
                        exprs: let_exprs.into(),
                        body,
                    }
                    .into();
                }

                let arg = if arg.ellipsis() {
                    LambdaArg::OpenPattern {
                        required_names: required_names.into(),
                    }
                } else {
                    LambdaArg::ClosePattern {
                        required_names: required_names.into(),
                        optional_names,
                    }
                };
                (arg, body)
            }
            k => unreachable!("Unexpected node kind {:?} at {}", k, arg.text_range()),
        };

        Ok(Expr::Lambda { arg, body }.into())
    }

    fn with(&mut self, n: With) -> Result<ExprRef> {
        // Add before evaluating `namespace`. `LetIn` will push expressions early.
        let ns_idx = self.stack_idx(0);
        self.stack_depth += 1;
        let ns = self.expr(n.namespace().unwrap())?;
        self.with_scopes.push(ns_idx);
        let body = self.expr(n.body().unwrap())?;
        self.with_scopes.pop();
        self.stack_depth -= 1;
        Ok(Expr::LetIn {
            exprs: Box::new([ns]),
            body,
        }
        .into())
    }

    // `{ inherit (from) a; inherit b; c = 42; }`
    // Stack: [...old... | from]
    fn attr_set(&mut self, n: &impl EntryHolder) -> Result<ExprRef> {
        let let_cnt = n.inherits().filter(|n| n.from().is_some()).count();

        self.stack_depth += let_cnt;

        // `InheritFrom` expressions.
        let let_exprs: Vec<ExprRef> = n
            .inherits()
            .filter_map(|inherit| inherit.from())
            .map(|from| self.expr(from.inner().unwrap()))
            .collect::<Result<_>>()?;
        assert_eq!(let_exprs.len(), let_cnt);

        // Normal entries.
        let mut desugar_set = AttrSetLit::default();
        for ent in n.entries() {
            let path: Vec<Node> = ent.key().unwrap().path().collect();
            let value = self.expr(ent.value().unwrap())?;
            desugar_set.insert_path(&path, value, self, true)?;
        }
        let mut set_entries = desugar_set.lower_entries(self)?;
        let set_dynamics = desugar_set.lower_dynamics(self)?;

        // `InheritFrom` idents.
        for (i, inherit) in n.inherits().filter(|n| n.from().is_some()).enumerate() {
            for ident in inherit.idents() {
                let name: SmolStr = ident.as_str().into();
                let e = Expr::Select {
                    set: ExprRef::debruijn(let_cnt - 1 - i),
                    index: Value::String(name.clone()).into(),
                    or_default: None,
                }
                .into();
                insert_or_dup!(set_entries, name, e, ident);
            }
        }

        // `Inherit` idents.
        for ident in n
            .inherits()
            .filter(|n| n.from().is_none())
            .flat_map(|n| n.idents())
        {
            let name: SmolStr = ident.as_str().into();
            let e = self.lookup_scope(&ident)?;
            insert_or_dup!(set_entries, name, e, ident);
        }

        self.stack_depth -= let_cnt;

        let ret = Expr::AttrSet {
            entries: set_entries,
            dynamics: set_dynamics,
        }
        .into();
        if let_cnt == 0 {
            Ok(ret)
        } else {
            Ok(Expr::LetIn {
                exprs: let_exprs.into(),
                body: ret,
            }
            .into())
        }
    }

    // `rec { inherit (from) a; inherit b; c = 42; }`
    // Stack order: Inherit from expresions, other names, InheritFrom names, Inherit names.
    // Stack: [...old... | from | c | a, b]
    // Note that `from` expressions can access `a`, `b` or `c` in recursive mode.
    // But `inherit` without `from` (`b`) can only access names in outer scope.
    fn rec_attr_set_like(&mut self, n: &impl EntryHolder, body: Option<Node>) -> Result<ExprRef> {
        let inherit_from_iter = || n.inherits().filter(|n| n.from().is_some());
        let inherit_iter = || n.inherits().filter(|n| n.from().is_none());

        let inherit_from_cnt = inherit_from_iter().count();
        // The result AttrSet. Also used for checking duplicated names.
        let mut set_entries = BTreeMap::new();

        let mut scope = BTreeMap::new();
        let entry_first_keys: BTreeMap<SmolStr, Ident> = n
            .entries()
            .filter_map(|ent| Ident::cast(ent.key().unwrap().path().next().unwrap()))
            .map(|ident| (ident.as_str().into(), ident))
            .collect();
        let mut name_cnt = 0;
        for (i, ident) in std::iter::empty()
            .chain(entry_first_keys.values().cloned())
            .chain(inherit_from_iter().flat_map(|n| n.idents()))
            .chain(inherit_iter().flat_map(|n| n.idents()))
            .enumerate()
        {
            let name: SmolStr = ident.as_str().into();
            let var = self.stack_idx(inherit_from_cnt + i);
            insert_or_dup!(scope, name, var, ident);
            name_cnt += 1;
        }
        let let_cnt = inherit_from_cnt + name_cnt;

        self.stack_depth += let_cnt;
        self.push_scope(scope);

        let mut let_exprs = Vec::with_capacity(let_cnt);

        // `InheritFrom` expressions.
        for from in n.inherits().flat_map(|n| n.from()) {
            let_exprs.push(self.expr(from.inner().unwrap())?);
        }

        // Normal entries.
        let mut desugar_set = AttrSetLit::default();
        for ent in n.entries() {
            let path: Vec<Node> = ent.key().unwrap().path().collect();
            let value = self.expr(ent.value().unwrap())?;
            desugar_set.insert_path(&path, value, self, body.is_none())?;
        }
        for (name, deep) in desugar_set.entries.iter() {
            let stack_idx = ExprRef::debruijn(let_cnt - 1 - let_exprs.len());
            let_exprs.push(deep.lower(self)?);
            // In LetIn, just get them from stack to avoid re-evaluation.
            // `set_entries` are currently empty, and duplicated names are reported in desugaring process.
            assert!(set_entries.insert(name.clone(), stack_idx).is_none());
        }

        // `InheritFrom` idents.
        for (i, inherit) in inherit_from_iter().enumerate() {
            for ident in inherit.idents() {
                let name: SmolStr = ident.as_str().into();
                let stack_idx = ExprRef::debruijn(let_cnt - 1 - let_exprs.len());
                let_exprs.push(
                    Expr::Select {
                        // `from` expression.
                        set: ExprRef::debruijn(let_cnt - 1 - i),
                        index: Value::String(name.clone()).into(),
                        or_default: None,
                    }
                    .into(),
                );
                // In LetIn, just get them from stack to avoid re-evaluation.
                insert_or_dup!(set_entries, name, stack_idx, ident);
            }
        }

        // Body and dynamics should be evaluated in the scope.
        let (body, set_dynamics) = match body {
            Some(body) => {
                assert!(desugar_set.dynamics.is_empty());
                (Some(self.expr(body)?), None)
            }
            None => (None, Some(desugar_set.lower_dynamics(self)?)),
        };

        // Pop scope first before evaluating simple Inherit.
        self.pop_scope();

        // `Inherit` idents.
        for ident in inherit_iter().flat_map(|n| n.idents()) {
            let stack_idx = ExprRef::debruijn(let_cnt - 1 - let_exprs.len());
            let_exprs.push(self.lookup_scope(&ident)?);
            insert_or_dup!(set_entries, ident.as_str().into(), stack_idx, ident);
        }

        self.stack_depth -= let_cnt;
        assert_eq!(let_exprs.len(), let_cnt);

        let ret = match body {
            Some(body) => body,
            None => Expr::AttrSet {
                dynamics: set_dynamics.unwrap(),
                entries: set_entries,
            }
            .into(),
        };
        if let_cnt == 0 {
            Ok(ret)
        } else {
            Ok(Expr::LetIn {
                exprs: let_exprs.into(),
                body: ret,
            }
            .into())
        }
    }
}

#[derive(Default)]
struct AttrSetLit {
    entries: BTreeMap<SmolStr, AttrSetLitValue>,
    dynamics: Vec<(ExprRef, AttrSetLitValue)>,
}

enum AttrSetLitValue {
    Expr(ExprRef),
    Deep(AttrSetLit),
}

impl AttrSetLitValue {
    fn lower(&self, lower: &mut Lowerer) -> Result<ExprRef> {
        match self {
            Self::Expr(e) => Ok(e.clone()),
            Self::Deep(deep) => deep.lower(lower),
        }
    }
}

impl AttrSetLit {
    fn lower(&self, lower: &mut Lowerer) -> Result<ExprRef> {
        let entries = self.lower_entries(lower)?;
        let dynamics = self.lower_dynamics(lower)?;
        Ok(Expr::AttrSet { entries, dynamics }.into())
    }

    fn lower_entries(&self, lower: &mut Lowerer) -> Result<BTreeMap<SmolStr, ExprRef>> {
        self.entries
            .iter()
            .map(|(name, deep)| Ok((name.clone(), deep.lower(lower)?)))
            .collect()
    }

    fn lower_dynamics(&self, lower: &mut Lowerer) -> Result<Box<[(ExprRef, ExprRef)]>> {
        self.dynamics
            .iter()
            .map(|(expr, deep)| Ok((expr.clone(), deep.lower(lower)?)))
            .collect()
    }

    fn insert_path(
        &mut self,
        path: &[Node],
        value: ExprRef,
        lower: &mut Lowerer,
        allow_dynamic: bool,
    ) -> Result<()> {
        let (n, rest_path) = path.split_first().unwrap();
        match n.kind() {
            SyntaxKind::NODE_IDENT => {
                let n = Ident::cast(n.clone()).unwrap();
                let name: SmolStr = n.as_str().into();
                if rest_path.is_empty() {
                    insert_or_dup!(self.entries, name, AttrSetLitValue::Expr(value), n);
                    Ok(())
                } else {
                    match self
                        .entries
                        .entry(name.clone())
                        .or_insert_with(|| AttrSetLitValue::Deep(Default::default()))
                    {
                        AttrSetLitValue::Deep(deep) => {
                            deep.insert_path(rest_path, value, lower, true)
                        }
                        _ => unreachable!(),
                    }
                }
            }
            SyntaxKind::NODE_DYNAMIC => {
                if !allow_dynamic {
                    return Err(Error::UnexpectedDynamicAttr {
                        pos: n.text_range(),
                    });
                }

                let expr = lower.expr(Dynamic::cast(n.clone()).unwrap().inner().unwrap())?;
                let deep = if rest_path.is_empty() {
                    AttrSetLitValue::Expr(value)
                } else {
                    let mut set = AttrSetLit::default();
                    set.insert_path(rest_path, value, lower, true)?;
                    AttrSetLitValue::Deep(set)
                };
                self.dynamics.push((expr, deep));
                Ok(())
            }
            _ => unreachable!(),
        }
    }
}

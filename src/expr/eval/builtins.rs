//! Builtins.
//! See: https://nixos.org/manual/nix/stable/#ssec-builtins
use super::{eval, eval_coerce_to_string, Continuation, Error, EvalState, Result, Thunk, Value};
use either::Either;
use strum::VariantNames;

macro_rules! define_builtin {
    ($($name:ident($params:tt) $(= $cont:path)?,)*) => {
        #[derive(Debug, Clone, Copy, strum::EnumVariantNames, strum::EnumIter)]
        #[strum(serialize_all = "camelCase")]
        pub enum Builtin {
            $($name,)*
        }

        impl Builtin {
            pub const ALL: &'static [Self] = &[
                $(Builtin::$name,)*
            ];

            const CONTINUATIONS: &'static [Continuation] = &[
                $(define_builtin!(__to_cont $name $($cont)?),)*
            ];

            const PARAMS: &'static [usize] = &[
                $($params,)*
            ];
        }
    };
    (__to_cont $name:ident) => {{
        fn not_impl(_: &mut EvalState<'_>) -> Result<()> {
            unimplemented!(stringify!($name))
        }
        not_impl
    }};
    (__to_cont $name:ident $p:path) => { $p };
}

define_builtin! {
    // Internal builtins.
    _Assert(2) = assert,
    _ConcatStr(2) = concat_str,
    _IfThenElse(3) = if_then_else,
    _SelectOrDefault(3) = select_or_default,

    // Operators.
    _And(2) = and,
    _Concat(2) = concat,
    _Equal(2) = equal,
    _Negate(1),
    _Not(1),
    _Or(2) = or,
    _Update(2),

    // Exported builtins.
    Abort(1),
    Add(2),
    All(2),
    Any(2),
    AttrNames(1),
    AttrValues(1),
    BaseNameOf(1),
    BitAnd(2),
    BitOr(2),
    BitXor(2),
    Builtins(0) = builtins,
    CompareVersions(2),
    ConcatLists(1),
    ConcatStringsSep(2),
    CurrentSystem(0),
    DeepSeq(2),
    Derivation(1),
    DirOf(1),
    Div(2),
    Elem(2),
    ElemAt(2),
    False(0) = false_,
    FetchGit(1),
    FetchTarball(1),
    Fetchurl(1),
    Filter(2),
    FilterSource(2),
    Foldl_(3),
    FromJSON(1),
    FunctionArgs(1),
    GenList(2),
    GetAttr(2) = get_attr,
    GetEnv(1),
    HasAttr(2),
    HashFile(2),
    HashString(2),
    Head(1),
    Import(1),
    IntersectAttrs(2),
    IsAttrs(1),
    IsBool(1),
    IsFloat(1),
    IsFunction(1),
    IsInt(1),
    IsList(1),
    IsNull(1),
    IsPath(1),
    IsString(1),
    Length(1),
    LessThan(2),
    ListToAttrs(1),
    Map(2),
    Match(2),
    Mul(2),
    ParseDrvName(1),
    Path(1),
    PathExists(1),
    Placeholder(1),
    ReadDir(1),
    ReadFile(1),
    RemoveAttrs(2),
    ReplaceStrings(3),
    Seq(2),
    Sort(2),
    Split(2),
    SplitVersion(1),
    StringLength(1),
    Sub(2),
    SubString(3),
    Tail(1),
    Throw(1),
    ToFile(2),
    ToJSON(1),
    ToPath(1),
    ToString(1),
    ToXML(1),
    Trace(2),
    True(0) = true_,
    TryEval(1),
    TypeOf(1) = type_of,
}

impl Builtin {
    pub const GLOBALS: &'static [Self] = &[
        Builtin::Abort,
        Builtin::BaseNameOf,
        Builtin::Builtins,
        Builtin::Derivation,
        Builtin::DirOf,
        Builtin::False,
        Builtin::FetchTarball,
        Builtin::Import,
        Builtin::IsNull,
        Builtin::Map,
        Builtin::RemoveAttrs,
        Builtin::Throw,
        Builtin::ToString,
        Builtin::True,
    ];

    pub fn params(&self) -> usize {
        Self::PARAMS[*self as usize]
    }

    pub fn name(&self) -> &'static str {
        if let Builtin::Foldl_ = self {
            "foldl'"
        } else {
            Self::VARIANTS[*self as usize]
        }
    }

    pub(crate) fn continuation(&self) -> Continuation {
        Self::CONTINUATIONS[*self as usize]
    }
}

def_cont! {
    fn true_(e) {
        e.push(Thunk::new_value(Value::Bool(true)));
        Ok(())
    }

    fn false_(e) {
        e.push(Thunk::new_value(Value::Bool(false)));
        Ok(())
    }

    fn builtins(e) {
        e.push(e.ctx.builtins.clone());
        Ok(())
    }

    fn assert(e, cond: bool, body: thunk) {
        if !cond {
            return Err(Error::AssertionFailed);
        }
        e.push(body);
        e.cont(eval);
        Ok(())
    }

    fn concat_str(e, a: (to_string), b: to_string) {
        let mut a = a;
        let s = match a.unwrap_value()? {
            Either::Left(a) => a.into_string()? + b,
            Either::Right(a) => {
                let a = a.as_string()?;
                let mut s = String::with_capacity(a.len() + b.len());
                s.push_str(a);
                s.push_str(b);
                s
            }
        };
        e.push(Thunk::new_value(Value::String(s.into())));
        Ok(())
    }

    fn if_then_else(e, cond: bool, then_body: thunk, else_body: thunk) {
        e.push(if cond { then_body } else { else_body });
        e.cont(eval);
        Ok(())
    }

    fn select_or_default(e, set: set, name: string, or_default: thunk) {
        match set.get(&*name) {
            Some(v) => e.push(v.clone()),
            None => e.push(or_default),
        }
        e.cont(eval);
        Ok(())
    }

    fn get_attr(e, name: string, set: set) {
        match set.get(&*name) {
            Some(v) => {
                e.push(v.clone());
                e.cont(eval);
                Ok(())
            }
            None => Err(Error::BuiltinError { reason: "".into() }),
        }
    }

    fn and(e, a: bool, b: thunk) {
        if a {
            e.push(b);
            e.cont(|e| {
                let _ = e.get(0).unwrap_value_ref()?.as_bool()?;
                Ok(())
            });
            e.cont(eval);
        } else {
            e.push(Thunk::new_value(Value::Bool(false)));
        }
        Ok(())
    }

    fn or(e, a: bool, b: thunk) {
        if a {
            e.push(Thunk::new_value(Value::Bool(true)));
        } else {
            e.push(b);
            e.cont(|e| {
                let _ = e.get(0).unwrap_value_ref()?.as_bool()?;
                Ok(())
            });
            e.cont(eval);
        }
        Ok(())
    }

    fn concat(e, a: list, b: list) {
        let mut v = a.to_vec();
        v.extend_from_slice(b);
        e.push(Thunk::new_value(Value::List(v)));
        Ok(())
    }

    fn type_of(e, v: any) {
        e.push(Thunk::new_value(Value::String(v.type_name().into())));
        Ok(())
    }

    fn equal(e, a: any, b: any) {
        let b = match (a, b) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::String(a), Value::String(b)) |
            (Value::Path(a), Value::Path(b)) => a == b,
            (Value::AttrSet(a), Value::AttrSet(b)) if a.keys().eq(b.keys()) => {
                todo!();
            }
            (Value::List(a), Value::List(b)) if a.len() == b.len() => {
                todo!();
            }
            _ => false,
        };
        e.push(Thunk::new_value(Value::Bool(b)));
        Ok(())
    }
}

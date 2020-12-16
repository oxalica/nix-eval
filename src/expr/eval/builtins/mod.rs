//! Builtins.
//! See: https://nixos.org/manual/nix/stable/#ssec-builtins
use super::{Continuation, EvalState, Result};
use strum::VariantNames;

mod control;
mod ops;
mod set;
mod string;
mod util;

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
    _Assert(2) = control::assert,
    _ConcatStr(2) = string::concat_str,
    _IfThenElse(3) = control::if_then_else,
    _SelectOrDefault(3) = control::select_or_default,

    // Operators.
    _And(2) = ops::and,
    _Concat(2) = ops::concat,
    _Equal(2) = ops::equal,
    _Negate(1),
    _Not(1) = ops::not,
    _Or(2) = ops::or,
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
    Builtins(0) = util::builtins,
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
    False(0) = util::false_,
    FetchGit(1),
    FetchTarball(1),
    Fetchurl(1),
    Filter(2),
    FilterSource(2),
    Foldl_(3),
    FromJSON(1),
    FunctionArgs(1),
    GenList(2),
    GetAttr(2) = set::get_attr,
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
    True(0) = util::true_,
    TryEval(1),
    TypeOf(1) = util::type_of,
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

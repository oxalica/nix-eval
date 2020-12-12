use crate::expr::{Expr, ExprRef, SmolStr, Value};
use once_cell::sync::Lazy;
use std::collections::HashMap;
use strum::VariantNames;

/// Builtins.
///
/// `true` and `false` are not included here.
/// https://nixos.org/manual/nix/stable/#ssec-builtins
#[derive(Debug, Clone, Copy, strum::EnumVariantNames, strum::EnumIter)]
#[strum(serialize_all = "camelCase")]
pub enum Builtin {
    Abort,
    Add,
    All,
    Any,
    AttrNammes,
    AttrValues,
    BaseNameOf,
    BitAnd,
    BitOr,
    BitXor,
    Builtins,
    CompareVersions,
    ConcatLists,
    ConcatStringsSep,
    CurrentSystem,
    DeepSeq,
    Derivation,
    DirOf,
    Div,
    Elem,
    ElemAt,
    FetchGit,
    FetchTarball,
    Fetchurl,
    Filter,
    FilterSource,
    Foldl_,
    FromJSON,
    FunctionArgs,
    GenList,
    GetAttr,
    GetEnv,
    HasAttr,
    HashFile,
    HashString,
    Head,
    Import,
    IntersectAttrs,
    IsAttrs,
    IsBool,
    IsFloat,
    IsFunction,
    IsInt,
    IsList,
    IsNull,
    IsPath,
    IsString,
    Length,
    LessThan,
    ListToAttrs,
    Map,
    Match,
    Mul,
    ParseDrvName,
    Path,
    PathExists,
    Placeholder,
    ReadDir,
    ReadFile,
    RemoveAttrs,
    ReplaceStrings,
    Seq,
    Sort,
    Split,
    SplitVersion,
    StringLength,
    Sub,
    SubString,
    Tail,
    Throw,
    ToFile,
    ToJSON,
    ToPath,
    ToString,
    ToXML,
    Trace,
    TryEval,
    TypeOf,
}

const GLOBAL_BUILTINS: &[Builtin] = &[
    Builtin::Abort,
    Builtin::BaseNameOf,
    Builtin::Builtins,
    Builtin::Derivation,
    Builtin::DirOf,
    Builtin::FetchTarball,
    Builtin::Import,
    Builtin::IsNull,
    Builtin::Map,
    Builtin::RemoveAttrs,
    Builtin::Throw,
    Builtin::ToString,
];

impl Builtin {
    fn name(&self) -> &'static str {
        if let Builtin::Foldl_ = self {
            "foldl'"
        } else {
            Self::VARIANTS[*self as usize]
        }
    }
}

pub static GLOBAL_BUILTIN_EXPRS: Lazy<HashMap<SmolStr, ExprRef>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("true".into(), Value::Bool(true).into());
    map.insert("false".into(), Value::Bool(false).into());
    for &b in GLOBAL_BUILTINS {
        let name = b.name().into();
        let expr: ExprRef = Expr::Builtin(b).into();
        assert!(map.insert(name, expr).is_none());
    }
    map
});

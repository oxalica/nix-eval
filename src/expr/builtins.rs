use strum::VariantNames;

macro_rules! define_builtin {
    ($($name:ident($params:tt),)*) => {
        /// Builtins.
        ///
        /// https://nixos.org/manual/nix/stable/#ssec-builtins
        #[derive(Debug, Clone, Copy, strum::EnumVariantNames, strum::EnumIter)]
        #[strum(serialize_all = "camelCase")]
        pub enum Builtin {
            $($name,)*
        }

        impl Builtin {
            pub const ALL: &'static [Self] = &[
                $(Builtin::$name,)*
            ];

            const PARAMS: &'static [usize] = &[
                $($params,)*
            ];
        }
    };
}

define_builtin! {
    // Internal builtins.
    _Assert(1),
    _ConcatStr(2),
    _IfThenElse(3),
    _SelectOrDefault(3),

    // Operators.
    _And(2),
    _Concat(2),
    _Equal(2),
    _Negate(1),
    _Not(1),
    _Or(2),
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
    Builtins(0),
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
    False(0),
    FetchGit(1),
    FetchTarball(1),
    Fetchurl(1),
    Filter(2),
    FilterSource(2),
    Foldl_(3),
    FromJSON(1),
    FunctionArgs(1),
    GenList(2),
    GetAttr(2),
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
    True(0),
    TryEval(1),
    TypeOf(1),
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
}

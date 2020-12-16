{
  typeOf = with builtins; [
    (typeOf true)
    (typeOf 1)
    (typeOf 2.0)
    (typeOf { a = throw "set"; })
    (typeOf [ (throw "list") ])
    (typeOf [ (throw "list") ])
    (typeOf [ (x: throw "lam") ])
    (typeOf typeOf)
    (typeOf builtins.builtins)
  ];
}

{
  arith = [
    (42 + 13) (42 - 13) (42 * 13) (42 / 13)
    (42 > 13) ("42" < "13")
  ];
  eq = [
    (1 == 1) (1.0 != 1.0) ("123" == "123")
    ([] == []) ([] == [(throw "empty")]) ([1 2] == [1 2])
    ({ a = 1; b = [42]; } == { a = 1; b = [42]; })
    ({ a = 1; b = [42]; c = throw "skip"; } == { a = throw "no cmp"; b = [42]; })
  ];

  builtins = [true false builtins.builtins.true builtins.false];
  tryEval = [(builtins.tryEval 42) (builtins.tryEval (throw "no"))];
  typeOf = with builtins; [
    (typeOf true)
    (typeOf 1)
    (typeOf 2.0)
    (typeOf { a = throw "set"; })
    (typeOf [ (throw "list") ])
    (typeOf [ (throw "list") ])
    (typeOf [ (x: throw "lam") ])
  ];
}

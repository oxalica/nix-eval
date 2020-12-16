{
  if_ = [
    (if true then 1 else throw "2")
    (if false then throw "1" else 2)
  ];
  tryEval = [
    (builtins.tryEval (assert false; abort "no"))
    (builtins.tryEval (assert true; "ok"))
  ];
  select = [
    ({ a = 42; }.a or throw "no")
    ({ b = 42; }.a or "okay")
  ];
}

let f = a: b: a + b; in f 1 2 + f 3 4

/*
{ f, foo, bar, baz }:
{
  inherit (f foo) ofo oof;
  inherit bar baz;
  wtf = 42;
}

lambda:
(lambda:
{
  ofo = <0>.ofo;
  oof = <0>.oof;
  bar = <1>.bar;
  baz = <1>.baz;
  wtf = 42;
}
) (f foo)

let f = a: b: a + b; in f 1 2 + f 3 4

(lambda: <0> 1 2 + <0> 3 4) (lambda: lambda: <1> + <0>)
*/

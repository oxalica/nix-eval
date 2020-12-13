{
  a = (x: x) 1;
  b = (a: b: a) 1 (throw "wtf");
  c = (a: b: b) (throw "wtf") 2;
  d = (a: b: a + b) 1 2;
  e = (f: (x: f (x x)) (x: f (x x))) (f: x: if x == 0 then 1 else x * f (x - 1)) 10;
}

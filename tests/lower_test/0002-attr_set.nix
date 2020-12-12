[
  {
    a.b.d = 3;
    a.a = 1;
    a.b.c = 2;
  }
  rec {
    a.b.d = c;
    a.a = a;
    a.b.c = 2;
    c = 42;
  }
  rec {
    b = "2";
    a = 1;
    ${"a" + b} = a;
  }
  (x: y: a: {
    inherit x;
    inherit (x) a b;
    inherit (y) c d;
    e = a;
  })
  (x: y: a: rec {
    inherit x;
    inherit (x) a b;
    inherit (y) c d;
    e = a;
  })
]

[
  (x: x)
  (a: b: a + b)
  ({ a, b }: { c, d, ... }: a c + b d)
  ({ hello ? pkgs.hello, pkgs, world ? args, ... }@args:
    [ hello pkgs world ])
  (args@{ a ? 1, b }: a + args.b)
  (this:is:string)
]

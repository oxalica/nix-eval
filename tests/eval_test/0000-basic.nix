{
  hello = "world";
  paths = [ <nixpkgs> /* ./nix not implemented */ /nix http://nix ];
  cond = if 0 == 0 then 42 else 4.2;
  foo = {}.a or { b = 42; }.b;
  buildPhase = ''
    echo "${"hello" + "world"}"
  '';
}

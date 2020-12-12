{
  hello = "world";
  paths = [ <nix> ./nix /nix http://nix ];
  cond = if 0 == 0 then 42 else 4.2;
  foo = {}.a or {}.b;
  buildPhase = ''
    echo "${"hello" + "world"}"
  '';
}

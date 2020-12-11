/*
{ pkgs ? import <nixpkgs> {}, ... }:
pkgs.mkShell {
  buildInputs = with pkgs; [ openssl ];
  buildPhase = ''
    cargo build
    echo "${pkgs.hello}"
  '';
  a.b.${c} = a.b.${c} or 1 + 1;
  a."b c" = 42;
}
*/
# let f = a: b: a + b; in f 1 2 + f 3 4
{ b, c }:
{
  a = 1;
  inherit b;
  inherit (c) d;
}

{ b, c }:
{
  a = 1;
  inherit b;
  inherit (c) d;
}

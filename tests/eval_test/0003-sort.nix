let
  # From lib/lists.nix
  mySort = with builtins;
    strictLess: list:
    let
      len = length list;
      first = head list;
      pivot' = n: acc@{ left, right }: let el = elemAt list n; next = pivot' (n + 1); in
        if n == len
          then acc
        else if strictLess first el
          then next { inherit left; right = [ el ] ++ right; }
        else
          next { left = [ el ] ++ left; inherit right; };
      pivot = pivot' 1 { left = []; right = []; };
    in
      if len < 2 then list
      else (mySort strictLess pivot.left) ++  [ first ] ++  (mySort strictLess pivot.right);

  sort1 = mySort builtins.lessThan;
  # sort2 = builtins.sort builtins.lessThan;

  # 10
  xs1 = [1 7 6 5 7 1 8 5 9 3];
  # 20
  xs2 = [10 12 4 16 14 16 10 9 18 13 18 5 2 5 16 20 19 18 11 16];
  # 100
  xs3 = [50 7 75 66 21 4 63 81 99 32 26 81 47 14 46 40 4 11 4 66 25 72 72 77 44 35 50 60 65 49 84 46 52 1 86 35 66 93 99 20 5 95 18 22 85 77 48 75 27 92 91 72 10 29 14 9 68 97 5 99 20 75 20 69 71 46 90 25 68 33 51 84 46 12 24 30 72 66 23 45 63 9 15 28 75 59 93 95 55 55 82 80 45 78 80 14 28 48 17 2];
in [
  (sort1 xs1)
  # (sort2 xs1)
  (sort1 xs2)
  # (sort2 xs2)
  # FIXME: Stack overflow.
  # (sort1 xs3)
  # (sort2 xs3)
]

```
Lisp interpreter using Haskell.

Reference: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

How to run:

$ cabal run scheme
Lisp>>> (load "lib/std.scm")
Lisp>>> (map (curry + 2) '(1 2 3 4))
(3 4 5 6)
Lisp>>>
```

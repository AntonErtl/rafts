: foo0 ( -- )
  recurse ;

: foo1 ( -- )
  1 2 + recurse ;

: foo2 ( -- )
  dup 1 <> if
    dup . 1- recurse 2 . endif ;

?runtest [IF]
\ foo0
\ foo1
$10 foo2 .
[THEN]

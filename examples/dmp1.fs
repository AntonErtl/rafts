123 constant c0

: foo0 ( -- n n n n )
  c0 456 +
  dup + dup dup 321 ;

: foo1 ( -- n )
  1 c0 +
  2 + ;

: foo2 ( n n n n n n n n -- n )
  + + + + + + + ;

: foo3 ( n -- n n )
  dup ;

: foo4 ( n -- )
  drop ;

?runtest [IF]
foo0 . . . . cr
foo1 . cr
1 2 3 4 5 6 7 8 foo2 . cr
1 foo3 . . cr
1 foo4
[THEN]

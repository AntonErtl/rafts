hex

: foo0 ( -- )
  1 . ;

: foo1 ( -- )
  2 . ; immediate

: foo2 ( -- )
  r@ . ;
' foo2 . cr
?runtest [IF]
foo2 cr
[THEN]

: foo3 ( -- )
  foo2 ;
' foo3 . cr
?runtest [IF]
foo3 cr
[THEN]

: foo4 ( n1 n2 -- n2 )
  >r dup . r> ;
' foo4 . cr
?runtest [IF]
1 2 foo4 . . cr
[THEN]

: foo0 ( flag -- n )
  123 456 + swap if
    321 654 + swap + else
    456 - endif
  123 456 + + ;

: foo1 ( flag -- n )
  123 456 + swap if
    321 654 + swap + endif
  123 456 + + ;

: foo2 ( flag -- n n n )
  1 swap if
    2 else
    3 endif
  4 ;

: foo3 ( flag -- n n n | n n )
  1 swap if
    2 endif
  3 ;

true [IF]
: foo4 ( -- n )
  10 begin
    1- dup . dup 0 < until ;

: foo5 ( -- n )
  0 begin
    1+ dup 10 < while
    dup . repeat ;

: foo6 ( -- n )
  10 begin
    1- dup 0 < if
      exit endif
    dup . again ;
[THEN]

?runtest [IF]
true foo0 . cr
false foo0 . cr
true foo1 . cr
false foo1 . cr
true foo2 . . . cr
false foo2 . . . cr
true foo3 . . . cr
false foo3 . . cr
[THEN]

?runtest [IF]
foo4 . cr
foo5 . cr
foo6 . cr
[THEN]

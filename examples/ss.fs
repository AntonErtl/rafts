hex
: foo0 ( -- )
  1 . ;

: foo1 ( -- )
  2 . foo0 ; immediate

: foo2 ( -- )
  postpone foo0
  postpone .
  postpone chars
  postpone foo1 ; immediate

?runtest [IF]
: foo21 ( -- )
  foo2 ;

foo0
foo1
foo2
3 foo21
[THEN]

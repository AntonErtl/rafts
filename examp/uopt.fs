\ Test for optimations
: foo0 ( -- )
  2dup + drop ;

: foo1 ( -- )
  swap swap ;

: foo2 ( -- )
  dup drop ;

: foo3 ( -- )
  >r
  r> ;

: foo4 ( -- )
  r>
  >r ;

: foo10 ( -- )
  1 2 + dup 3 + ;

?runtest [IF]
foo0
foo1
foo2
foo3
foo4
foo10 . . cr
[THEN]

bye

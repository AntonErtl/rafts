variable v0
variable v1
123 constant c0

: foo0 ( -- addr )
  v0 ;

: foo1 ( n -- addr n )
  v0 456 +
  swap ;

: foo2 ( -- addr n )
  1 v0 c0 +
  swap 2 + ;

?runtest [IF]
foo0 hex. cr
1 foo1 . hex. cr
foo2 . hex. cr
[THEN]

bye

: foo0
  1 2 + ;

: foo1
  foo0 . ;

foo1 cr

: foo2
  tuck dup nip over swap drop ;

: foo3
  2dup 2over 2swap 2drop ;

1 2 foo2 . . . cr
1 2 foo3 . . . . cr

: foo4
  cell+ 3 cells dup cell- ;

: foo5
  char+ 3 chars dup char- ;

5 cells foo4 . . . cr
5 chars foo5 . . . cr

variable x
  $10 a, $11 a, $12 a, $13 a, $14 a, $15 a, $16 a, $17 a,
  $18 a, $19 a, $1a a, $1b a, $1c a, $1d a, $1e a, $1f a,

3 x !
: foo6
  x @ 0 ?do
    x cell+ i cells + @ loop
  x @ ;

foo6 .s cr

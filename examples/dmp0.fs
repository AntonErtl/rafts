: foo0 ( -- n n n n n n n n n )
  $00000000 $000000ff $0000ffff $ffff0000 $ffff8000 $ffffffff
  $00000f0f $0f0f0000 $0f0f0f0f ;

: foo1 ( -- )
  123 456 +
  321 654 - swap + .
  123 456 + . ;

: foo20 ( n3 n2 n1 n0 -- n3 n2 n1 n0 n3 )
  foo1
  3 pick ;

: foo21 ( n3 n2 n1 n0 -- n3 n2 n1 n0 n3 )
  foo1
  pick ;

: foo30 ( n n -- flag flag flag flag flag flag )
  over over < rot rot
  over over <= rot rot
  over over = rot rot
  over over <> rot rot
  over over >= rot rot
  over over > rot rot
  drop drop ;

: foo31 ( u u -- flag flag flag flag )
  over over u< rot rot
  over over u<= rot rot
  over over u>= rot rot
  over over u> rot rot
  drop drop ;

: foo32 ( u -- flag flag flag flag flag flag )
  dup 0< swap
  dup 0<= swap
  dup 0= swap
  dup 0<> swap
  dup 0>= swap
  dup 0> swap
  drop ;

: foo33 ( n n -- flag flag flag flag flag flag )
  10 20 <
  10 20 <=
  10 20 =
  10 20 <>
  10 20 >=
  10 20 > ;

: foo4 ( n -- n )
  abs ;

: foo5 ( n -- n )
  negate ;

: foo6 ( n -- n )
  invert ;

: foo7 ( n n -- n )
  and ;

: foo8 ( n n -- n )
  or ;

: foo9 ( n -- n )
  456 + ;

?runtest [IF]
foo0 hex. hex. hex. hex. hex. hex. hex. hex. hex. cr
foo1 cr
1 2 3 4 5 foo20 . . . . . . cr
1 2 3 4 5 3 foo21 . . . . . . cr
10 20 foo30 . . . . . . cr
10 10 foo30 . . . . . . cr
20 10 foo30 . . . . . . cr
$80000000 $80000001 foo31 . . . . cr
$80000000 $80000000 foo31 . . . . cr
$80000001 $80000000 foo31 . . . . cr
-1 foo32 . . . . . . cr
0 foo32 . . . . . . cr
1 foo32 . . . . . . cr
foo33 . . . . . . cr
-20 foo4 . cr
20 foo4 . cr
20 foo5 . cr
$10203040 foo6 hex. cr
$10203040 $01020304 foo7 hex. cr
$10203040 $10203040 foo7 hex. cr
$10203040 $01020304 foo8 hex. cr
$10203040 $10203040 foo8 hex. cr
123 foo9 . cr
[THEN]

bye

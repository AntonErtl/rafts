: foo0 ( -- n n n n n n n n n )
  $00000000 $000000ff $0000ffff $ffff0000 $ffff8000 $ffffffff
  $00000f0f $0f0f0000 $0f0f0f0f ;

: foo10 ( n3 n2 n1 n0 -- n3 n2 n1 n0 n3 )
  3 pick ;

: foo11 ( n -- nn ... n0 nn )
  pick ;

: foo20 ( n3 n2 n1 n0 -- n2 n1 n0 n3 )
  3 roll ;

: foo21 ( n -- nn-1 ... n0 nn )
  roll ;

: foo30 ( -- 3 3 0 )
  3 ?dup
  0 ?dup ;

: foo31 ( n -- 0 | n n )
  ?dup ;

: foo32 ( n1 n2 -- n1 )
  nip ;

: foo33 ( n1 n2 -- n2 n1 n2 )
  tuck ;

: foo40 ( n -- n )
  abs ;

: foo41 ( n -- n )
  negate ;

: foo42 ( n -- n )
  invert ;

: foo43 ( n n -- n )
  and ;

: foo44 ( n n -- n )
  or ;

: foo45 ( n n -- n )
  xor ;

: foo50 ( -- n )
  123 456 + ;

: foo51 ( n -- n )
  456 + ;

: foo52 ( -- n )
  123 456 - ;

: foo53 ( n -- n )
  456 - ;

: foo54 ( -- n )
  123 456 * ;

: foo55 ( n -- n )
  456 * ;

: foo56 ( -- n )
  56088 123 / ;

: foo57 ( n -- n )
  123 / ;

: foo60 ( -- n )
  123 1+ ;

: foo61 ( n -- n )
  1+ ;

: foo62 ( -- n )
  123 1- ;

: foo63 ( n -- n )
  1- ;

: foo64 ( -- n )
  123 2* ;

: foo65 ( n -- n )
  2* ;

: foo66 ( -- n )
  246 2/ ;

: foo67 ( n -- n )
  2/ ;

: foo68 ( -- n )
  123 4 lshift ;

: foo69 ( n -- n )
  4 lshift ;

: foo6a ( -- n )
  1968 4 rshift ;

: foo6b ( n -- n )
  4 rshift ;

?runtest [IF]
foo0 hex. hex. hex. hex. hex. hex. hex. hex. hex. cr
1 2 3 4 5 foo10 . . . . . . cr
1 2 3 4 5 3 foo11 . . . . . . cr
1 2 3 4 5 foo20 . . . . . cr
1 2 3 4 5 3 foo21 . . . . . cr
foo30 . . . cr
0 foo31 . cr
3 foo31 . . cr
1 2 foo32 . cr
1 2 foo33 . . . cr
-20 foo40 . cr
20 foo40 . cr
20 foo41 . cr
$10203040 foo42 hex. cr
$10203040 $01020304 foo43 hex. cr
$10203040 $10203040 foo43 hex. cr
$10203040 $01020304 foo44 hex. cr
$10203040 $10203040 foo44 hex. cr
$10203040 $01020304 foo45 hex. cr
$10203040 $10203040 foo45 hex. cr
foo50 . cr
123 foo51 . cr
foo52 . cr
123 foo53 . cr
foo54 . cr
123 foo55 . cr
foo56 . cr
56088 foo57 . cr
foo60 . cr
123 foo61 . cr
foo62 . cr
123 foo63 . cr
foo64 . cr
123 foo65 . cr
foo66 . cr
246 foo67 . cr
foo68 . cr
123 foo69 . cr
foo6a . cr
1968 foo6b . cr
[THEN]

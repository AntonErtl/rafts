variable x

: foo0 ( addr -- n )
  dup @ 1 2 + rot ! ;

: foo1 ( -- n )
  x dup @ 2 4 + rot ! ;

: foo2 ( addr -- n n n )
  dup @ swap dup @ swap dup @ 3 6 + rot ! ;

: foo3 ( -- n n n )
  x dup @ swap dup @ swap dup @ 4 8 + rot ! ;

: foo4 ( addr -- n n n n n n )
  1 over ! dup @ swap dup @ swap dup @ swap
  2 over ! dup @ swap dup @ swap dup @ 3 rot ! ;

: foo5 ( -- n )
  1 x ! x @ 2 x ! ;

: foo6 ( -- n n n )
  1 x ! x @ x @ x @ 2 x ! ;

: foo7 ( -- n n n )
  x @ 1 x ! x @ x @ 2 x ! ;

: foo8 ( addr addr -- n n n )
  1 over ! dup @ swap @ rot dup @ 2 rot ! ;

: foo9 ( addr addr -- )
  1 swap ! 2 swap ! ;

?runtest [IF]
123 x !
x foo0 .
x @ . cr
456 x !
foo1 .
x @ . cr
789 x !
x foo2 . . .
x @ . cr
321 x !
foo3 . . .
x @ . cr

654 x !
x foo4 . . . . . .
x @ . cr
987 x !
foo5 .
x @ . cr
123 x !
foo6 . . .
x @ . cr
456 x !
foo7 . . .
x @ . cr
789 x !
x x foo8 . . .
x @ . cr
789 x !
x x foo9
x @ . cr
[THEN]

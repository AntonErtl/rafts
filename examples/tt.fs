variable x 0 x !
variable y 0 y !

: foo0 ( n addr -- n )
  1 2 + over !
  @ + ;

: foo1 ( n addr addr -- n )
  dup 1 2 + over ! !
  @ + ;

: foo2 ( n addr addr -- n )
  1 2 + over ! dup !
  @ + ;

?runtest [IF]
3 x foo0 hex. cr
x @ hex. cr
y @ hex. cr
3 x y foo1 hex. cr
x @ hex. cr
y @ hex. cr
3 x y foo2 hex. cr
x @ hex. cr
y @ hex. cr
' x $10 dump
' y $10 dump
[THEN]

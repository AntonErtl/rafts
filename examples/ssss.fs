: foo0 ( -- )
  ?do
    i .
    i 0 ?do
      j 1 - . loop
    cr loop ;

: foo1 ( -- )
  ?do
    i . -2 +loop ;

: foo2 ( -- )
  for
    i . next ;

: foo3 ( -- )
  0 swap ?do
    i . -1 +loop ;

?runtest [IF]
10 0 foo0
0 10 foo1
10 foo2
10 foo3
0 foo3
[THEN]

bye

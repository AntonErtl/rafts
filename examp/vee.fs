: foo0
  1 2 + . cr ;

: foo1
  foo0 1 2 + . cr ;

1 create x
  ,
does>
  @ ;

: foo2
  x . cr ;

foo0
foo1
foo2

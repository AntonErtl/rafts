hex
include search-order.fs

: func ( "name" -- xt )
  also Forth
  ." func:" order .s cr
  name sfind 0= if
    -&13 bounce endif
  previous ;

: efunc ( "name" -- )
  func execute ;

: foo
  ." foo0:" cr ;

foo

order cr
Vocabulary a
also a definitions
order cr

foo

: foo
  ." foo1:" cr ;

foo

order cr
Vocabulary b
also b definitions
order cr

foo

: foo
  ." foo2:" cr ;

foo

order cr
Vocabulary c
also c definitions
order cr

foo

: foo
  ." foo3:" cr ;

foo

' a . ' b . ' c . cr
get-order .s cr

1 2 func + .s cr
execute .s cr
efunc drop .s cr
\ execute .s cr
func execute .s cr

also Forth
order cr

bye

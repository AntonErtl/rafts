: foo ( -- n )
  $0123 $0456 + ;

foo  . cr

: foo1 ( -- )
  foo dup if
    . cr then ;

foo1 

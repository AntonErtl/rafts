hex
: foo0 ( -- )
  r> dup . >r ;

' foo0 >name $20 dump
foo0 cr

: foo1 ( -- )
  foo0 1 2 . . foo0 ;
' foo1 >name $20 dump
foo1 cr

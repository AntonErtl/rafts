hex
: dmp
  >name here over - dup . dump ;

: foo0
  1 2 + ;
' foo0 dmp

: foo1
  dup + ;
' foo1 dmp

: foo2
  >r ;
' foo2 dmp

: foo3
  create
    2 ,
  does>
    \ 1 +
    ;
' foo3 dmp

foo3 foo4
' foo4 dmp

foo3 foo5
' foo5 dmp

: foo6
  create
    \ 2 ,
    ;
' foo6 dmp

foo6 foo7
' foo7 dmp

\ $1001ee94 >name $20 dump
\ $1001ee44 >name $20 dump
\ $10001f60 $20 dump
\ $10002118 $20 dump
\ $10000ac4 $40 dump
$8208 $40 dump


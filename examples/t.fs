: postp
  bl word find 0> if
    compile, else
    postpone lit ,
    postpone compile, endif ; immediate

hex
: foo0 ( -- )
  1 . ;

: foo1 ( -- )
  2 . foo0 ; immediate

: foo2 ( -- )
  postp foo0
  postp chars
  postp foo1 ; immediate

: foo21 ( -- )
  foo2 ;

?runtest [IF]
foo0
foo1
foo2
foo21
[THEN]

' postp >name $40 dump
' foo0 >name $40 dump
' foo1 >name $40 dump
' foo2 >name $40 dump
' foo21 >name $40 dump

bye

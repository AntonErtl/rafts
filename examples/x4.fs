hex
include stdlib.fs

: foo0 ( -- )
  ;

: foo1 ( -- )
  ;

' foo0 char+ dup @ 2dup hex. hex. + 4 + hex. cr
' foo1 char+ dup @ 2dup hex. hex. + 4 + hex. cr

finish
bye

hex
include stdlib.fs

: foo0 ( -- )
  ." Hello" cr ;
code. foo0

: foo1 ( -- )
  ." stack dump: " .s cr ; immediate
code. foo1

: foo2 ( n1 n2 -- n2 n1 n1 )
  foo1 swap postpone foo0 dup postpone foo1 ;
code. foo2

1 2 .s cr
foo2 .s cr

." foo0: " bl word foo0 find . hex. cr
." foo1: " bl word foo1 find . hex. cr
." foo2: " bl word foo2 find . hex. cr

.s cr
: foo3 ( -- )
  if
    $01 foo1 if
      $11 foo1 else
      $21 foo1 endif $02 foo1 endif ;
code. foo3

: foo4 ( -- )
  begin
    $01 foo1 until ;
code. foo4

code. if
\ $03fa $40 dump
' ?branch hex. cr
' branch hex. cr
' lit hex. cr

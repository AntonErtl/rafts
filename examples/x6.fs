hex
include stdlib.fs

variable xxx
' xxx .

: foo0 ( -- )
  0 . ;
code. foo0

: foo1 ( -- )
  1 . ; immediate
code. foo1

: foo2 ( -- )
  foo0 foo1 2 . ;
code. foo2

: foo3 ( -- )
  postpone foo0 postpone foo1 3 . ; immediate
code. foo3

: foo4 ( -- )
  postpone xxx postpone foo3 4 . ; immediate
code. foo4

foo0 cr
foo1 cr
foo2 cr
foo3 cr
foo4 cr

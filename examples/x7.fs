hex
include stdlib.fs

: foo0 ( -- )
  &10 0 do
    1 . loop
  &10 &10 ?do
    2 . loop
  &10 &11 ?do
    3 . loop ;

: tdo ( -- )
  postpone swap
  postpone >r
  postpone >r
  postpone ahead
  ; immediate

: t?do ( -- )
  postpone 2dup
  postpone <>
  postpone if
  postpone swap
  postpone >r
  postpone >r
  postpone ahead
  postpone endif
  ; immediate

: tunloop ( -- )
  postpone rdrop
  postpone rdrop
  ; immediate

: tloop ( -- )
  postpone r>
  postpone 1+
  postpone dup
  postpone r@
  postpone =
  postpone swap
  postpone >r
  postpone until
  postpone tunloop
  ; immediate

: foo1 ( -- )
  &10 0 tdo
    1 . tloop
  \ &10 &11 t?do
    \ 2 . tloop
  \ &10 &11 t?do
    \ 3 . tloop
  ;

\ foo0 cr
foo1 cr

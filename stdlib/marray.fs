\ $Id: marray.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: marray.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

\ allocate an array
: marray_create ( i*u i -- )
  dup ,
  1 swap 0 ?do
    swap dup , * loop
  cells allot ;

\ limit check for an array
: mlims ( i*u a-addr1 -- a-addr2 )
  dup @
  dup depth 2 - < 0= abort" marray: dimension error (stack depth)"
  0 ?do
    i 1+ pick over i 1+ cells + @
    over > swap -1 >
    and 0= abort" marray: dimension error (index size)" loop ;

\ use an array
: marray_does ( i*u a-addr1 -- a-addr2 )
  swap over @ 1- 0 ?do
    1 i 1+ 0 ?do
      i 1+ cells 3 pick + @ * loop
    >r rot r> * + loop
  cells swap dup @ 1+ cells + + ;

: marray ( i*u i "name" -- )
  dup 1 < abort" marray: positive dimension count only"
  create
    marray_create
  does>
    \ mlims
    marray_does ;

?test $0400 [IF]
cr ." Test for marray.fs" cr

4 4 5 6 4 marray my_4d_array
5 6 2 marray my_2d_array

' my_4d_array dup hex. >name $80 dump
' my_2d_array dup hex. >name $80 dump

: marray_test
  base @ >r
  hex

  0 0 0 0 my_4d_array .
  3 3 4 5 my_4d_array .
  \ 3 3 5 6 my_4d_array .

  $30 0 0 0 0 my_4d_array !
  $31 0 0 0 1 my_4d_array !
  $32 0 0 0 2 my_4d_array !
  $33 0 0 0 5 my_4d_array !
  $34 0 0 1 0 my_4d_array !
  0 0 0 0 my_4d_array
  5 cells -
  20 cells dump

  0 0 0 0 my_4d_array .s @ . cr
  0 0 0 1 my_4d_array .s @ . cr
  0 0 1 0 my_4d_array .s @ . cr
  0 1 0 0 my_4d_array .s @ . cr

  $30 0 0 my_2d_array !
  $31 0 1 my_2d_array !
  $32 0 2 my_2d_array !
  $33 0 5 my_2d_array !
  $34 1 0 my_2d_array !
  0 0 my_2d_array
  3 cells -
  20 cells dump

  0
  5 0 ?do
    6 0 ?do
      1+ dup j i my_2d_array ! loop loop
  drop

  0 0 my_2d_array
  3 cells -
  33 cells dump

  r> base ! ;

marray_test

finish
[THEN]

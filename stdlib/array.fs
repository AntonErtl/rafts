\ $Id: array.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: array.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

: array ( u "name" -- )
  1 marray ; immediate

?test $0400 [IF]
cr ." Test for array.fs" cr

6 array my_array

: array_test
  base @ >r
  hex

  0 my_array .
  5 my_array .
  \ 6 my_array .

  $30 0 my_array !
  $31 1 my_array !
  $32 2 my_array !
  $33 5 my_array !
  0 my_array
  2 cells -
  8 cells dump

  0 my_array .s @ . cr
  1 my_array .s @ . cr

  0
  6 0 ?do
    1+ dup i my_array ! loop
  drop

  0 my_array
  2 cells -
  8 cells dump

  r> base ! ;

array_test

finish
[THEN]

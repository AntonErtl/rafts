\ marray.fs	multi array words
\
\ Copyright (C) 1995-96 Martin Anton Ertl, Christian Pirker
\
\ This file is part of RAFTS.
\
\	RAFTS is free software; you can redistribute it and/or
\	modify it under the terms of the GNU General Public License
\	as published by the Free Software Foundation; either version 2
\	of the License, or (at your option) any later version.
\
\	This program is distributed in the hope that it will be useful,
\	but WITHOUT ANY WARRANTY; without even the implied warranty of
\	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\	GNU General Public License for more details.
\
\	You should have received a copy of the GNU General Public License
\	along with this program; if not, write to the Free Software
\	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

\ allocate an array
: marray_create ( i*u i -- )
  \ i dimensions
  dup ,
  0 ?do
    , loop ;

\ limit check for an array (cells)
: mlims ( i*u a-addr1 -- a-addr2 )
  dup @
  dup depth 2 - < 0= abort" marray: dimension error (stack depth)"
  0 ?do
    i 1+ pick over i 1+ cells + @
    over > swap -1 >
    and 0= abort" marray: dimension error (index size)" loop ;

\ use an array (cells)
: marray_does ( i*u a-addr1 -- a-addr2 )
  swap over @ 1- 0 ?do
    1 i 1+ 0 ?do
      i 1+ cells 3 pick + @ * loop
    >r rot r> * + loop
  cells swap dup @ 1+ cells + + ;

: marray_noallot ( i*u i "name" -- )
  dup 1 < abort" marray: positive dimension count only"
  create
    marray_create
  does>
    \ mlims
    marray_does ;

: marray ( i*u i "name" -- )
  dup >r
  marray_noallot
  r> here over 1+ cells - 1 rot 0 ?do
    swap cell+ dup rot rot @ * loop
  cells allot drop ;

\ use an array (chars)
: mcarray_does ( i*u a-addr1 -- a-addr2 )
  swap over @ 1- 0 ?do
    1 i 1+ 0 ?do
      i 1+ cells 3 pick + @ * loop
    >r rot r> * + loop
  chars swap dup @ 1+ cells + + ;

: mcarray_noallot ( i*u i "name" -- )
  dup 1 < abort" marray: positive dimension count only"
  create
    marray_create
  does>
    \ mlims
    mcarray_does ;

: mcarray ( i*u i "name" -- )
  dup >r
  mcarray_noallot
  r> here over 1+ cells - 1 rot 0 ?do
    swap cell+ dup rot rot @ * loop
  chars allot drop ;

?test $0400 [IF]
cr ." Test for marray.fs" cr

4 4 5 6 4 marray m4data
5 6 2 mcarray m2cdata

' m4data dup hex. >name $80 dump
' m2cdata dup hex. >name $80 dump

: mdata_print ( addr -- )
  ." ( " dup hex. ." ) " ? ;

: mcdata_print ( addr -- )
  ." ( " dup hex. ." ) " c? ;

: mdata_test
  0 0 0 0 m4data mdata_print .s cr
  3 3 4 5 m4data mdata_print .s cr
  \ 3 3 5 6 m4data mdata_print .s cr

  123 0 0 0 0 m4data !
  234 0 0 0 1 m4data !
  345 0 0 0 2 m4data !
  456 0 0 0 5 m4data !
  567 0 0 1 0 m4data !
  678 0 1 0 0 m4data !
  789 0 1 1 1 m4data !
  0 0 0 0 m4data 5 cells - 4 4 * 5 * 6 * cells dump

  0 0 0 0 m4data mdata_print .s cr
  0 0 0 1 m4data mdata_print .s cr
  0 0 0 2 m4data mdata_print .s cr
  0 0 0 5 m4data mdata_print .s cr
  0 0 1 0 m4data mdata_print .s cr
  0 1 0 0 m4data mdata_print .s cr
  0 1 1 1 m4data mdata_print .s cr

  0
  4 0 ?do
    i 4 0 ?do
      i 5 0 ?do
        6 0 ?do
          2>r 1+ dup 2r@ rot 2r> j i
	  m4data ! loop loop
      .s cr drop loop
    .s cr drop loop
  drop

  0 0 0 0 m4data mdata_print .s cr
  0 0 0 1 m4data mdata_print .s cr
  0 0 1 0 m4data mdata_print .s cr
  0 1 0 0 m4data mdata_print .s cr
  1 0 0 0 m4data mdata_print .s cr

  0 0 0 0 m4data 5 cells - 4 4 * 5 * 6 * cells dump

  123 0 0 m2cdata c!
  234 0 1 m2cdata c!
  345 0 2 m2cdata c!
  456 0 5 m2cdata c!
  567 1 0 m2cdata c!
  0 0 m2cdata 3 cells - 3 cells 20 chars + dump

  0 0 m2cdata mcdata_print .s cr
  0 1 m2cdata mcdata_print .s cr
  0 2 m2cdata mcdata_print .s cr
  0 5 m2cdata mcdata_print .s cr
  1 0 m2cdata mcdata_print .s cr

  0
  5 0 ?do
    6 0 ?do
      1+ dup j i m2cdata c! loop loop
  drop

  0 0 m2cdata mcdata_print .s cr
  0 1 m2cdata mcdata_print .s cr
  1 0 m2cdata mcdata_print .s cr

  0 0 m2cdata 3 cells - 3 cells 30 chars + dump ;

mdata_test

finish
[THEN]

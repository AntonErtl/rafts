\ 2array.fs	two dimension array words
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
: 2array_create ( u u  -- )
  , , ;

\ limit check for an array (cells)
: 2lims ( u u a-addr1 -- a-addr2 )
  2dup @ >= abort" 2array: dimension error (index size)" ;

\ use an array (cells)
: 2array_does ( u u a-addr1 -- a-addr2 )
  rot over @ * rot + 2 + cells + ;

: 2array_noallot ( u u "name" -- )
  create
    2array_create
  does>
    \ 2lims
    2array_does ;

: 2array ( u u "name" -- )
  2dup 2array_noallot
  * cells allot ;

\ use an array (chars)
: c2array_does ( u u a-addr1 -- a-addr2 )
  rot over @ * rot + chars 2 cells + + ;

: c2array_noallot ( u u "name" -- )
  create
    2array_create
  does>
    \ 2lims
    c2array_does ;

: c2array ( u u "name" -- )
  2dup c2array_noallot
  * chars allot ;

?test $0400 [IF]
cr ." Test for 2array.fs" cr

3 6 2array a2data
4 8 c2array c2data

: a2data_print ( addr -- )
  ." ( " dup hex. ." ) " ? ;

: c2data_print ( addr -- )
  ." ( " dup hex. ." ) " c? ;

: a2data_test
  0 0 a2data hex. cr
  0 0 a2data a2data_print .s cr
  0 5 a2data a2data_print .s cr
  \ 0 6 a2data a2data_print .s cr

  123 0 0 a2data !
  234 0 1 a2data !
  345 0 2 a2data !
  456 2 5 a2data !
  0 0 a2data 4 cells - 22 cells dump

  0 0 a2data a2data_print .s cr
  0 1 a2data a2data_print .s cr
  0 2 a2data a2data_print .s cr
  2 5 a2data a2data_print .s cr

  0
  3 0 ?do
    6 0 ?do
      1+ dup j i a2data ! loop loop
  drop

  0 0 a2data a2data_print .s cr
  0 1 a2data a2data_print .s cr
  0 2 a2data a2data_print .s cr
  2 5 a2data a2data_print .s cr

  0 0 a2data 4 cells - 22 cells dump

  0 0 c2data c2data_print .s cr
  0 7 c2data c2data_print .s cr
  \ 0 8 c2data c2data_print .s cr

  123 0 0 c2data c!
  234 0 1 c2data c!
  345 0 2 c2data c!
  456 3 7 c2data c!
  0 0 c2data 4 cells - 4 cells 24 chars + dump

  0 0 c2data c2data_print .s cr
  0 1 c2data c2data_print .s cr
  0 2 c2data c2data_print .s cr
  3 7 c2data c2data_print .s cr

  0
  3 0 ?do
    8 0 ?do
      1+ dup j i c2data c! loop loop
  drop

  0 0 c2data c2data_print .s cr
  0 1 c2data c2data_print .s cr
  0 2 c2data c2data_print .s cr
  3 7 c2data c2data_print .s cr

  0 0 c2data 4 cells - 4 cells 24 chars + dump ;
a2data_test

finish
[THEN]

\ array.fs	simple array words
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

: array_noallot ( u "name" -- )
  1 marray_noallot ; immediate

: array ( u "name" -- )
  1 marray ; immediate

: carray_noallot ( u "name" -- )
  1 mcarray_noallot ; immediate

: carray ( u "name" -- )
  1 mcarray ; immediate

?test $0400 [IF]
cr ." Test for array.fs" cr

6 array adata
6 carray cadata

: adata_print ( addr -- )
  ." ( " dup hex. ." ) " @ . ;

: cadata_print ( addr -- )
  ." ( " dup hex. ." ) " c@ . ;

: adata_test
  0 adata adata_print .s cr
  5 adata adata_print .s cr
  \ 6 adata adata_print .s cr

  123 0 adata !
  234 1 adata !
  345 2 adata !
  456 5 adata !
  0 adata 2 cells - 8 cells dump

  0 adata adata_print .s cr
  1 adata adata_print .s cr
  2 adata adata_print .s cr
  5 adata adata_print .s cr

  6 0 ?do
    i dup adata ! loop

  0 adata adata_print .s cr
  1 adata adata_print .s cr

  0 adata 2 cells - 8 cells dump

  0 cadata cadata_print .s cr
  5 cadata cadata_print .s cr
  \ 6 cadata cadata_print .s cr

  123 0 cadata !
  234 1 cadata !
  345 2 cadata !
  456 5 cadata !
  0 cadata 2 cells - 2 cells 6 chars + dump

  0 cadata cadata_print .s cr
  1 cadata cadata_print .s cr
  2 cadata cadata_print .s cr
  5 cadata cadata_print .s cr

  6 0 ?do
    i dup cadata ! loop

  0 cadata cadata_print .s cr
  1 cadata cadata_print .s cr

  0 cadata 2 cells - 2 cells 6 chars + dump ;

adata_test

finish
[THEN]

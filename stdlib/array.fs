\ array.fs	one dimension array words
\
\ Copyright (C) 1995-97 Martin Anton Ertl, Christian Pirker
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

: array-create ( u  -- )
\ allocate an array
    , ;

: 1lims ( u a-addr -- )
\ limit check for an array (cells)
    2dup @ >= abort" array: dimension error (index size)" ;

: array-does ( u a-addr1 -- a-addr2 )
\ use an array (cells)
    cell+ swap cells + ;

: array-noallot ( u "name" -- )
    create array-create
does>
    \ 1lims
    array-does ;

: array ( u "name" -- )
    dup array-noallot
    cells allot ;

: carray-does ( u a-addr1 -- a-addr2 )
\ use an array (chars)
    cell+ swap chars + ;

: carray-noallot ( u "name" -- )
    create array-create
does>
    \ 1lims
    carray-does ;

: carray ( u "name" -- )
    dup carray-noallot
    chars allot ;

?test $4000 [IF]
cr ." Test for array.fs" cr

6 array adata
    
: adata-print ( addr -- )
    ." ( " dup hex. ." ) " ?  ;

: adata-test ( -- )
    0 adata adata-print .s cr
    5 adata adata-print .s cr
    \ 6 adata adata-print .s cr

    123 0 adata !
    234 1 adata !
    345 2 adata !
    456 5 adata !
    0 adata 3 cells - 9 cells dump

    0 adata adata-print .s cr
    1 adata adata-print .s cr
    2 adata adata-print .s cr
    5 adata adata-print .s cr

    6 0 ?do
	i dup adata !
    loop

    0 adata adata-print .s cr
    1 adata adata-print .s cr
    2 adata adata-print .s cr
    5 adata adata-print .s cr

    0 adata 3 cells - 9 cells dump ;
adata-test

8 carray cadata

: cadata-print ( addr -- )
    ." ( " dup hex. ." ) " c?  ;

: cadata-test ( -- )
    0 cadata cadata-print .s cr
    7 cadata cadata-print .s cr
    \ 8 cadata cadata-print .s cr

    123 0 cadata c!
    234 1 cadata c!
    345 2 cadata c!
    456 7 cadata c!
    0 cadata 3 cells - 3 cells 8 chars + dump

    0 cadata cadata-print .s cr
    1 cadata cadata-print .s cr
    2 cadata cadata-print .s cr
    7 cadata cadata-print .s cr

    8 0 ?do
	i dup cadata c!
    loop

    0 cadata cadata-print .s cr
    1 cadata cadata-print .s cr
    2 cadata cadata-print .s cr
    7 cadata cadata-print .s cr

    0 cadata 3 cells - 3 cells 8 chars + dump ;
cadata-test

finish
[THEN]

\ matrix.fs	two dimension array words
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

: matrix-create ( u u  -- )
\ allocate an array
    , , ;

: matrix-does ( u u a-addr1 -- a-addr2 )
\ use an array (cells)
    rot over @ * rot + 2 + cells + ;

: matrix-noallot ( u u "name" -- )
    create matrix-create
does>
    matrix-does ;

: matrix ( u u "name" -- )
    2dup matrix-noallot
    * cells allot ;

: cmatrix-does ( u u a-addr1 -- a-addr2 )
\ use an array (chars)
    rot over @ * rot + chars 2 cells + + ;

: cmatrix-noallot ( u u "name" -- )
    create matrix-create
does>
    cmatrix-does ;

: cmatrix ( u u "name" -- )
    2dup cmatrix-noallot
    * chars allot ;

?test $4000 [IF]
cr ." Test for matrix.fs" cr

3 6 matrix amatrix

: amatrix-print ( addr -- )
    ." ( " dup hex. ." ) " ? ;

: amatrix-test ( -- )
    0 0 amatrix hex. cr
    0 0 amatrix amatrix-print .s cr
    0 5 amatrix amatrix-print .s cr
    \ 0 6 amatrix amatrix-print .s cr

    123 0 0 amatrix !
    234 0 1 amatrix !
    345 0 2 amatrix !
    456 2 5 amatrix !
    0 0 amatrix 4 cells - 22 cells dump

    0 0 amatrix amatrix-print .s cr
    0 1 amatrix amatrix-print .s cr
    0 2 amatrix amatrix-print .s cr
    2 5 amatrix amatrix-print .s cr

    0
    3 0 ?do
	6 0 ?do
	    1+ dup j i amatrix !
	loop
    loop
    drop

    0 0 amatrix amatrix-print .s cr
    0 1 amatrix amatrix-print .s cr
    0 2 amatrix amatrix-print .s cr
    2 5 amatrix amatrix-print .s cr

    0 0 amatrix 4 cells - 22 cells dump ;
amatrix-test

4 8 cmatrix cmatrix

: cmatrix-print ( addr -- )
    ." ( " dup hex. ." ) " c? ;

: cmatrix-test ( -- )
    0 0 cmatrix cmatrix-print .s cr
    0 7 cmatrix cmatrix-print .s cr
    \ 0 8 cmatrix cmatrix-print .s cr

    123 0 0 cmatrix c!
    234 0 1 cmatrix c!
    345 0 2 cmatrix c!
    456 3 7 cmatrix c!
    0 0 cmatrix 4 cells - 4 cells 24 chars + dump

    0 0 cmatrix cmatrix-print .s cr
    0 1 cmatrix cmatrix-print .s cr
    0 2 cmatrix cmatrix-print .s cr
    3 7 cmatrix cmatrix-print .s cr

    0
    3 0 ?do
	8 0 ?do
	    1+ dup j i cmatrix c!
	loop
    loop
    drop

    0 0 cmatrix cmatrix-print .s cr
    0 1 cmatrix cmatrix-print .s cr
    0 2 cmatrix cmatrix-print .s cr
    3 7 cmatrix cmatrix-print .s cr

    0 0 cmatrix 4 cells - 4 cells 24 chars + dump ;
cmatrix-test

finish
[THEN]

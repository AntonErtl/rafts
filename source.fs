\ source.fs	debugging words
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

: nothing ( -- ) ;		\ a unknown bugfix
: sourcepos, ( -- )
  sourceline# vtarget_compile postpone literal vsource
  loadfilename# @ vtarget_compile postpone literal vsource ;
: print-sourcepos ( n n -- )
  2* cells included-files nothing 2@ drop + 2@ type
  ." :" 0 .r ;
: (~~) ( -- )
  cr print-sourcepos ." :"
  printdebugdata cr ;
>target_compile

: ~~ ( -- )
  sourcepos,
  ['] (~~) compile, ; immediate restrict

>source
: assertn ( -- )
  assert-level @ > if
    postpone (
  endif ;
: (endassert) ( -- )
  rot if
    2drop
  else
    cr print-sourcepos ." : failed assertion"
    true abort" assertion failed"
  endif ;
>target_compile

: assert0( ( -- )
  0 assertn ; immediate restrict
: assert1( ( -- )
  1 assertn ; immediate restrict
: assert2( ( -- )
  2 assertn ; immediate restrict
: assert3( ( -- )
  3 assertn ; immediate restrict
: assert( ( -- )
  vtarget_compile postpone assert1( vsource ; immediate restrict
: ) ( -- )
  sourcepos,
  ['] (endassert) compile, ; immediate restrict

>source

?test $0004 [IF]
cr ." Test for source.fs" cr

finish
[THEN]

\ string.fs	string words
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

: cmove_any ( c-addr1 c-addr2 u -- )
  >r 2dup >
  r> swap if
    cmove else
    cmove> endif ;

: copy ( c-addr1 u c-addr2 -- )
  2>r 2r@ char+ swap cmove_any
  2r> c! ;

?test $0200 [IF]
cr ." Test for string.fs" cr

finish
[THEN]

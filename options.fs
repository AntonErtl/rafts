\ option.fs	optional option words
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

: ?shared ( "name" -- flag )
  bl word find nip 0= ;

: ?word-mode-direct ( -- flag )
    threading-method 0= ; immediate

: ?word-mode-indirect ( -- flag )
    threading-method 0<> ; immediate

$0000 constant trace-const	\ no trace
\ $0a71 constant trace-const	\ trace what you want
\ $0001 constant trace-const	\ trace compiler
\ $0002 constant trace-const	\ trace compiler
\ $0003 constant trace-const	\ trace compiler (more)
\ $0010 constant trace-const	\ trace controll
\ $0020 constant trace-const	\ trace inst-selection
\ $0040 constant trace-const	\ trace inst-scheduling
\ $0500 constant trace-const	\ trace basics
\ $0810 constant trace-const	\ trace disasambler
\ $c000 constant trace-const	\ trace wordlists
\ $ffff constant trace-const	\ trace all

$0000 constant test-const	\ no test
\ $0040 constant test-const	\ test what you want
\ $0080 constant test-const	\ test asambler and disasambler
\ $1f00 constant test-const	\ test stdlib
\ $ffff constant test-const	\ test all

\ function to enable trace during execution
: trace ( n -- flag )
    trace-const and ;

: ?trace ( "n" -- flag )
    name snumber?  dup 0<> if
	drop trace
    endif ; immediate

\ function to enable tests on compiler-files
: test ( n -- flag )
    test-const and ;

: ?test ( "n" -- flag )
    name snumber? dup 0<> if
	drop test
    endif ; immediate

?test $0001 [IF]
cr ." Test for options.fs" cr

finish
[THEN]

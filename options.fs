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

$0001 constant func_mode_direct
$0002 constant func_mode_indirect
func_mode_direct constant func_mode_const
\ func_mode_indirect constant func_mode_const

: func_mode ( n -- flag )
  func_mode_const and ;

: ?func_mode_direct ( -- flag )
  func_mode_direct func_mode ; immediate

: ?func_mode_indirect ( -- flag )
  func_mode_indirect func_mode ; immediate

 $0000 constant trace_const	\ no trace
\ $0a71 constant trace_const	\ trace what you want
\ $0001 constant trace_const	\ trace compiler
\ $0002 constant trace_const	\ trace compiler
\ $0003 constant trace_const	\ trace compiler (more)
\ $0010 constant trace_const	\ trace controll
\ $0020 constant trace_const	\ trace inst-selection
\ $0040 constant trace_const	\ trace inst-scheduling
\ $0500 constant trace_const	\ trace basics
\ $0810 constant trace_const	\ trace disasambler
\ $c000 constant trace_const	\ trace wordlists
\ $ffff constant trace_const	\ trace all

$0000 constant test_const	\ no test
\ $0040 constant test_const	\ test what you want
\ $0080 constant test_const	\ test asambler and disasambler
\ $1f00 constant test_const	\ test stdlib
\ $ffff constant test_const	\ test all

\ function to enable trace during execution
: trace ( n -- flag )
  trace_const and ;

: ?trace ( "n" -- flag )
  name snumber?  dup 0<> if
    drop trace endif ; immediate

\ function to enable tests on compiler-files
: test ( n -- flag )
  test_const and ;

: ?test ( "n" -- flag )
  name snumber?  dup 0<> if
    drop test endif ; immediate

bl word oooppp find nip 0<> [IF]
." op.fs include" cr
include op.fs
[THEN]

?test $0001 [IF]
cr ." Test for options.fs" cr

\ finish
[THEN]

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
  bl word find nip invert ;
  \ name sfind 0= dup invert if
    \ nip endif ;

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

$0001 constant lit_mode_op
$0002 constant lit_mode_val
\ $0000 constant lit_mode_const
\ lit_mode_op constant lit_mode_const
\ lit_mode_val constant lit_mode_const
lit_mode_op lit_mode_val or constant lit_mode_const

: lit_mode ( n -- flag )
  lit_mode_const and ;

: ?lit_mode_op ( -- flag )
  lit_mode_op lit_mode ; immediate

: ?lit_mode_op_not ( -- flag )
  lit_mode_op lit_mode 0= ; immediate

: ?lit_mode_val ( -- flag )
  lit_mode_val lit_mode ; immediate

: ?lit_mode_val_not ( -- flag )
  lit_mode_val lit_mode 0= ; immediate

$0000 constant trace_const
\ $0002 constant trace_const
\ $0003 constant trace_const
\ $0800 constant trace_const
\ $c000 constant trace_const
\ $c800 constant trace_const
\ $0440 constant trace_const
\ $cf71 constant trace_const
\ $cf51 constant trace_const

$0000 constant test_const
\ $0005 constant test_const
\ $1f00 constant test_const
\ $0080 constant test_const
\ $0040 constant test_const

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

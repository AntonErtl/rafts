\ option.fs	optional option words
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

: ?shared ( "name" -- flag )
    bl word find nip 0= ;

: ?word-mode-direct ( -- flag )
    threading-method 0= ; immediate

: ?word-mode-indirect ( -- flag )
    threading-method ; immediate

\ $0    0    0    0
\  1234 5678 1234 5678
\  |||| |||| |||| ||||
\  |||| |||| |||| |||\-> compiler
\  |||| |||| |||| ||\--> compiler (more)
\  |||| |||| |||| |\---> basic 
\  |||| |||| |||| \----> basic (more)
\  |||| |||| |||\------> control
\  |||| |||| ||\-------> word
\  |||| |||| |\--------> unused
\  |||| |||| \---------> wordlists
\  |||| ||||          
\  |||| |||\-----------> inst-selection
\  |||| ||\------------> inst-scheduling
\  |||| |\-------------> register-alloction
\  |||| \--------------> code-generation (assembler, disassembler)
\  |||\----------------> stdlib functions
\  ||\-----------------> slist functions
\  |\------------------> array functions
\  \-------------------> btree functions
$0000 constant trace-const	\ no trace
$0f7d constant trace-const	\ trace what you want
\ $0001 constant trace-const	\ trace compiler
\ $0002 constant trace-const	\ trace compiler (more)
\ $0003 constant trace-const	\ trace compiler (all)
\ $0004 constant trace-const	\ trace basics
\ $0008 constant trace-const	\ trace basics (more)
\ $000c constant trace-const	\ trace basics (all)
\ $0010 constant trace-const	\ trace controll
\ $0100 constant trace-const	\ trace inst-selection
\ $0200 constant trace-const	\ trace inst-scheduling
\ $0400 constant trace-const	\ trace register-allocatiopn
\ $0800 constant trace-const	\ trace code-generation (assembler, disassembler)
\ $ffff constant trace-const	\ trace all

$0000 constant test-const	\ no test
\ $f800 constant test-const	\ test what you want
\ $0800 constant test-const	\ test assembler and disassembler
\ $f000 constant test-const	\ test stdlib
\ $ffff constant test-const	\ test all

\ function to enable trace during execution
: ?trace ( "n" -- flag )
    name snumber? dup if
	drop trace-const and
    endif ; immediate

\ function to enable tests on compiler-files
: ?test ( "n" -- flag )
    name snumber? dup if
	drop test-const and
    endif ; immediate

?test $0001 [IF]
cr ." Test for options.fs" cr

[THEN]

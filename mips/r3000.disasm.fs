\ r3000.disasm.fs	disassembler file (for MIPS R3000)
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

?trace $0800 [IF]
: @@op ( code -- n )
  $1a rshift $6 @mask and ;

: @@rs ( code -- n )
  $15 rshift $5 @mask and ;

: @@rt ( code -- n )
  $10 rshift $5 @mask and ;

: @@imm ( code -- n )
  $10 @mask and ;

: @@target ( code -- n )
  $1a @mask and ;

: @@rd ( code -- n )
  $b rshift $5 @mask and ;

: @@shamt ( code -- n )
  $6 rshift $5 @mask and ;

' @@shamt alias @@sa

: @@funct ( code -- n )
  $6 @mask and ;

: expand ( x -- x )
  dup $0000ffff > if
    $ffff0000 or endif ;

\ ***** I_types
: @@I_type2 ( addr -- )
  @ dup @@rt 2 swap hexn. @@imm 4 swap hexn. ;

: @@I_type2n ( addr -- )
  dup @ dup @@rs 2 swap hexn.
  @@imm $2 lshift expand dup 4 swap hexn. ." ( " + cell+ hex. ." ) " ;

: @@I_type3 ( addr -- )
  @ dup @@rt 2 swap hexn. dup @@rs 2 swap hexn. @@imm 4 swap hexn. ;

: @@I_type3n ( addr -- )
  dup @ dup @@rs 2 swap hexn. dup @@rt 2 swap hexn.
  @@imm $2 lshift expand dup 4 swap hexn. ." ( " + cell+ hex. ." ) " ;

: @@I_type3_offset ( addr -- )
  @ dup @@rt 2 swap hexn. dup @@imm 4 swap hexn. @@rs 2 swap hexn. ;

: @@I_type3n_offset ( addr -- )
  @ dup @@rt 2 swap hexn. dup @@imm 4 swap hexn. @@rs 2 swap hexn. ;

\ ***** regimm types
' @@I_type2n alias @@regimm2

\ ***** copz types 1
: @@copzi1 ( addr -- )
  dup @ dup @@imm $2 lshift expand dup 4 swap hexn. ." ( " rot + cell+ hex. ." ) "
  @@op 2 swap hexn. ;

: @@copzi3 ( addr -- )
  @ dup @@rt 2 swap hexn. dup @@imm 4 swap hexn.
  dup @@rs 2 swap hexn. @@op 2 swap hexn. ;

\ ***** J_types
: @@J_type1n ( addr -- )
  dup $fc000000 and swap @ @@target $2 lshift or hex. ;

\ ***** R_types
: @@R_type0 ( addr -- )
  @ hex. ;

: @@R_type1 ( addr -- )
  @ @@rd 2 swap hexn. ;

: @@R_type1n ( addr -- )
  @ @@rs 2 swap hexn. ;

: @@R_type2 ( addr -- )
  @ dup @@rd 2 swap hexn. @@rs 2 swap hexn. ;

: @@R_type2n ( addr -- )
  @ dup @@rs 2 swap hexn. @@rt 2 swap hexn. ;

: @@R_type3 ( addr -- )
  @ dup @@rd 2 swap hexn. dup @@rs 2 swap hexn. @@rt 2 swap hexn. ;

: @@R_type3n ( addr -- )
  @ dup @@rd 2 swap hexn. dup @@rt 2 swap hexn. @@rs 2 swap hexn. ;

: @@R_type3sa ( addr -- )
  @ dup @@rd 2 swap hexn. dup @@rt 2 swap hexn. @@sa 2 swap hexn. ;

\ ***** special types
' @@R_type0 alias @@special0
' @@R_type1 alias @@special1
' @@R_type1n alias @@special1n
' @@R_type2 alias @@special2
' @@R_type2n alias @@special2n
' @@R_type3 alias @@special3
' @@R_type3n alias @@special3n
' @@R_type3sa alias @@special3sa

\ ***** copz types 2
: @@cop0r0 ( addr -- )
  @ @@rs 2 swap hexn. ;

: @@copzr2 ( addr -- )
  @ dup @@rt 2 swap hexn. dup @@rd 2 swap hexn. @@op 2 swap hexn. ;

include	mips/r3000.disasm.opc.fs

?test $0080 [IF]
cr ." Test for r3000.asm.fs" cr

finish
[THEN]
[THEN]

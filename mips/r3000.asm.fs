\ r3000.asm.fs	assembler file (for MIPS R3000)
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

$00 constant @zero
$01 constant @at
$02 constant @v0
$03 constant @v1
$04 constant @a0
$05 constant @a1
$06 constant @a2
$07 constant @a3
$08 constant @t0
$09 constant @t1
$0a constant @t2
$0b constant @t3
$0c constant @t4
$0d constant @t5
$0e constant @t6
$0f constant @t7
$18 constant @t8
$19 constant @t9
$10 constant @s0
$11 constant @s1
$12 constant @s2
$13 constant @s3
$14 constant @s4
$15 constant @s5
$16 constant @s6
$17 constant @s7
$1e constant @s8
$1a constant @k0
$1b constant @k1
$1c constant @gp
$1d constant @sp
$1f constant @ra

: @mask ( n -- code )
  $1 swap lshift 1- ;

: @!op ( n -- code )
  $6 @mask and $1a lshift ;

: @!rs ( n -- code )
  $5 @mask and $15 lshift ;

: @!rt ( n -- code )
  $5 @mask and $10 lshift ;

: @!imm ( n -- code )
  $10 @mask and ;

: @!target ( n -- code )
  $1a @mask and ;

: @!rd ( n -- code )
  $5 @mask and $b lshift ;

: @!shamt ( n -- code )
  $5 @mask and $6 lshift ;

' @!shamt alias @!sa

: @!funct ( n -- code )
  $6 @mask and ;

\ ***** I_types
: @!(I_type) ( code -- )
  a, ;

: @!(I_type2) ( rt imm addr -- rt )
  @
  swap @!imm or over @!rt or a, ;

: @!I_type2
  create @!(I_type) does> @!(I_type2) ;

: @!(I_type2n) ( rs imm addr -- )
  @
  swap $2 rshift @!imm or swap @!rs or a, ;

: @!I_type2n
  create @!(I_type) does> @!(I_type2n) ;

: @!(I_type3) ( rt rs imm addr -- rt )
  @
  swap @!imm or swap @!rs or over @!rt or a, ;

: @!I_type3
  create @!(I_type) does> @!(I_type3) ;

: @!(I_type3n) ( rs rt imm addr -- )
  @
  swap $2 rshift @!imm or swap @!rt or over @!rs or a, drop ;

: @!I_type3n
  create @!(I_type) does> @!(I_type3n) ;

: @!(I_type3_offset) ( rt offset rs addr -- rt )
  @
  swap @!rs or swap @!imm or over @!rt or a, ;

: @!I_type3_offset
  create @!(I_type) does> @!(I_type3_offset) ;

: @!(I_type3n_offset) ( rt offset rs addr -- )
  @
  swap @!rs or swap @!imm or over @!rt or a, drop ;

: @!I_type3n_offset
  create @!(I_type) does> @!(I_type3n_offset) ;

\ ***** regimm types
: @!regimm2 ( funct -- )
  $01 @!op swap @!rt or @!I_type2n ;

\ ***** copz types 1
: @!(I_type1_copz) ( imm z addr -- )
  @
  swap @!op or swap $2 rshift @!imm or a, ;

: @!I_type1_copz
  create @!(I_type) does> @!(I_type1_copz) ;

: @!copzi1 ( code -- )
  $10 @!op or @!I_type1_copz ;

: @!(I_type3_copz) ( rt imm rs z addr -- rt )
  @
  swap @!op or swap @!rs or swap @!imm or over @!rt or a, ;

: @!I_type3_copz
  create @!(I_type) does> @!(I_type3_copz) ;

: @!copzi3 ( code -- )
  @!op @!I_type3_copz ;

$00 constant @copz_MF
$02 constant @copz_CF
$04 constant @copz_MT
$06 constant @copz_CT
$08 constant @copz_BC
$10 constant @copz_C0

$00 constant @copz_BCF
$01 constant @copz_BCT

\ ***** J_types
: @!(J_type) ( code -- )
  a, ;

: @!(J_type1n) ( target addr -- )
  @
  swap $2 rshift @!target or a, ;

: @!J_type1n
  create @!(J_type) does> @!(J_type1n) ;

\ ***** R_types
: @!(R_type) ( code -- )
  a, ;

: @!(R_type0) ( addr -- )
  @ a, ;

: @!R_type0
  create @!(R_type) does> @!(R_type0) ;

: @!(R_type1) ( rd addr -- rd )
  @
  over @!rd or a, ;

: @!R_type1
  create @!(R_type) does> @!(R_type1) ;

: @!(R_type1n) ( rs addr -- )
  @
  swap @!rs or a, ;

: @!R_type1n
  create @!(R_type) does> @!(R_type1n) ;

: @!(R_type2) ( rd rs addr -- rd )
  @
  swap @!rs or over @!rd or a, ;

: @!R_type2
  create @!(R_type) does> @!(R_type2) ;

: @!(R_type2n) ( rs rt addr -- )
  @
  swap @!rt or swap @!rs or a, ;

: @!R_type2n
  create @!(R_type) does> @!(R_type2n) ;

: @!(R_type3) ( rd rs rt addr -- rd )
  @
  swap @!rt or swap @!rs or over @!rd or a, ;

: @!R_type3
  create @!(R_type) does> @!(R_type3) ;

: @!(R_type3n) ( rd rt rs addr -- rd )
  @
  swap @!rs or swap @!rt or over @!rd or a, ;

: @!R_type3n
  create @!(R_type) does> @!(R_type3n) ;

: @!(R_type3sa) ( rd rt sa addr -- rd )
  @
  swap @!sa or swap @!rt or over @!rd or a, ;

: @!R_type3sa
  create @!(R_type) does> @!(R_type3sa) ;

\ ***** special types
: @!special0 ( funct -- )
  $00 @!op $00 @!rs or $00 @!rd or $00 @!rt or
  $00 @!shamt or swap @!funct or @!R_type0 ;

: @!special1 ( funct -- )
  $00 @!op $00 @!rd or $00 @!rt or
  $00 @!shamt or swap @!funct or @!R_type1 ;

: @!special1n ( funct -- )
  $00 @!op $00 @!rs or $00 @!rt or
  $00 @!shamt or swap @!funct or @!R_type1n ;

: @!special2 ( funct -- )
  $00 @!op $00 @!rt or
  $00 @!shamt or swap @!funct or @!R_type2 ;

: @!special2n ( funct -- )
  $00 @!op $00 @!rt or
  $00 @!shamt or swap @!funct or @!R_type2n ;

: @!special3 ( funct -- )
  $00 @!op
  $00 @!shamt or swap @!funct or @!R_type3 ;

: @!special3n ( funct -- )
  $00 @!op
  $00 @!shamt or swap @!funct or @!R_type3n ;

: @!special3sa ( funct -- )
  $00 @!op
  swap @!funct or @!R_type3sa ;

\ ***** copz types 2
: @!cop0r0 ( funct -- )
  $10 @!op $10 @!rs or $00 @!rd or $00 @!rt or
  $00 @!shamt or swap @!funct or @!R_type0 ;

: @!(R_type2_copz) ( rt rd z addr -- rt )
  @
  swap @!op or swap @!rd or over @!rt or a, ;

: @!R_type2_copz
  create @!(R_type) does> @!(R_type2_copz) ;

: @!copzr2 ( funct -- )
  $10 @!op or
  $00 @!shamt or $00 @!funct or @!R_type2_copz ;

: @nop ( -- )
  0 a, ;

include	mips/r3000.asm.opc.fs

?test $0080 [IF]
cr ." Test for r3000.asm.fs" cr

finish
[THEN]

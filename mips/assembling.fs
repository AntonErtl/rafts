\ assembling part of the code selector
\
\ Copyright (C) 1995-1997 Martin Anton Ertl, Christian Pirker
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

: prep-op ( ml -- rd rs rt )
    dup ml-reg @
    over ml-left @ ml-reg @
    rot ml-right @ ml-reg @ ;

: prep-opi ( ml -- rt rs val )
    dup ml-reg @
    over ml-left @ ml-reg @
    rot ml-val @ ;

: prep-uop ( ml -- rt rs )
    dup ml-reg @
    swap ml-left @ ml-reg @ ;

: prep-uopi ( ml -- rt val )
    dup ml-reg @
    swap ml-val @ ;

: asm-nop ( ml -- )
    drop nop, ;

: asm-lit ( ml -- )
    prep-uopi li, ;

: asm-move ( ml -- )
    prep-uop
    assert( 2dup <> )
    move, ;

: prep-load ( ml -- rt offset rs )
    dup ml-reg @
    over ml-val @
    rot ml-left @ ml-reg @ ;

: post-load ( ml -- )
    last-load @
    inst-size 1- inst-lists over <> if
	cell+ @
	tuck ml-left @ over =
	rot ml-right @ rot = or if
	    nop,
	endif
    else
	2drop
	nop,
    endif ;

: asm-fetchc ( ml -- )
    dup prep-load lbu,
    post-load ;

: asm-fetchi ( ml -- )
    dup prep-load lw,
    post-load ;

: prep-store ( ml -- rt offset rs )
    dup ml-left @ ml-reg @
    over ml-val @
    rot ml-right @ ml-reg @ ;

: asm-storec ( ml -- )
    prep-store sb, ;

: asm-storei ( ml -- )
    prep-store sw, ;

: asm-add ( ml -- )
    prep-op addu, ;

: asm-addi ( ml -- )
    prep-opi addiu, ;

: asm-sub ( ml -- )
    prep-op subu, ;

: asm-subi ( ml -- )
    prep-opi negate addiu, ;

: prep-muldiv ( ml -- rt rs )
    dup ml-left @ ml-reg @
    swap ml-right @ ml-reg @ ;

\ !! two instructions
: asm-mul ( ml -- )
    dup prep-muldiv multu,
    ml-reg @ mflo, ;

: asm-div ( ml -- )
    dup prep-muldiv div,
    ml-reg @ mflo, ;

: asm-mod ( ml -- )
    dup prep-muldiv div,
    ml-reg @ mfhi, ;

: asm-neg ( ml -- )
    prep-uop neg, ;

: asm-abs ( ml -- )
    prep-uop abs, ;

: asm-and ( ml -- )
    prep-op and, ;

: asm-andi ( ml -- )
    prep-opi andi, ;

: asm-or ( ml -- )
    prep-op or, ;

: asm-ori ( ml -- )
    prep-opi ori, ;

: asm-xor ( ml -- )
    prep-op xor, ;

: asm-xori ( ml -- )
    prep-opi xori, ;

: asm-not ( ml -- )
    prep-uop not, ;

: asm-lsh ( ml -- )
    prep-op sllv, ;

: asm-lshi ( ml -- )
    prep-opi sll, ;

: asm-rshu ( ml -- )
    prep-op srlv, ;

: asm-rsh ( ml -- )
    prep-op srav, ;

: asm-rshui ( ml -- )
    prep-opi srl, ;

: asm-rshi ( ml -- )
    prep-opi sra, ;

: asm-slt ( ml -- )
    prep-op slt, ;

: asm-slti ( ml -- )
    prep-opi slti, ;

: asm-sltu ( ml -- )
    prep-op sltu, ;

: asm-sltui ( ml -- )
    prep-opi sltiu, ;

: asm-sltui-one ( ml -- )
    prep-uop 1 sltiu, ;

: asm-subi-one ( ml -- )
    prep-uop -1 addi, ;

: back-patch-beq ( target-addr branch-addr -- )
    tuck - cell- 2 rshift
    over @ $ffff0000 and or swap ! ;

: branch,-info ( reg reg addr -- )
    here ['] back-patch-beq branch-info 2! ;

: asm-0branch ( ml -- )
    dup ml-left @ ml-reg @
    @zero
    rot ml-val @
    here - cell- branch,-info
    beq, ;

: asm-0branch-b ( ml -- )
    dup ml-left @ ml-reg @
    over ml-right @ ml-reg @
    rot ml-val @
    here - cell- branch,-info ;

: asm-0branch-beq ( ml -- )
    asm-0branch-b beq, ;

: asm-0branch-bne ( ml -- )
    asm-0branch-b bne, ;

: asm-0branch-b0 ( ml -- )
    dup ml-left @ ml-reg @
    swap ml-val @
    here - cell- branch,-info ;

: asm-0branch-0beq ( ml -- )
    asm-0branch-b0 @zero beq, ;

: asm-0branch-0bne ( ml -- )
    asm-0branch-b0 @zero bne, ;

: asm-0branch-bltz ( ml -- )
    asm-0branch-b0 bltz, ;

: asm-0branch-blez ( ml -- )
    asm-0branch-b0 blez, ;

: asm-0branch-bgez ( ml -- )
    asm-0branch-b0 bgez, ;

: asm-0branch-bgtz ( ml -- )
    asm-0branch-b0 bgtz, ;

: asm-branch ( ml -- )
    @zero @zero rot ml-val @
    here - cell- branch,-info
    beq, ;

: asm-call ( ml -- )
    ml-val @ jal, ;


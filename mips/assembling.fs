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

: (asm-lval@) ( ml-addr -- ml-addr )
    ml-left @ ;
: (asm-rval@) ( ml-addr -- ml-addr )
    ml-right @ ;
: asm-val@ ( ml-addr -- n )
    ml-val @ ;
: asm-lval@ ( ml-addr -- val )
    (asm-lval@) ml-val @ ;
: asm-reg@ ( ml-addr -- register )
    ml-reg @ ;
: asm-lreg@ ( ml-addr -- register )
    (asm-lval@) ml-reg @ ;
: asm-rreg@ ( ml-addr -- register )
    (asm-rval@) ml-reg @ ;

: asm-nop ( ml-addr -- )
    drop nop, ;

: asm-lit ( ml-addr -- )
    dup ml-reg @ swap ml-val @ li, ;

: prep-load ( ml-addr -- rt offset rs )
    >r
    r@ ml-reg @
    r@ ml-val @
    r> ml-left @ ml-reg @ ;

: asm-fetchc ( ml-addr -- )
    prep-load lbu, ;

: asm-fetchi ( ml-addr -- )
    prep-load lw, ;

: prep-store ( ml-addr -- rt offset rs )
    >r
    r@ ml-left @ ml-reg @
    r@ ml-val @
    r> ml-right @ ml-reg @ ;

: asm-storec ( ml-addr -- )
    prep-store sb, ;

: asm-storei ( ml-addr -- )
    prep-store sw, ;

: asm-add ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ addu, ;

: asm-addi ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ addiu, ;

: asm-sub ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ subu, ;

\ !! two instructions
: asm-mul ( ml-addr -- )
    dup asm-lreg@ over asm-rreg@ multu,
    asm-reg@ mflo, ;

: asm-div ( ml-addr -- )
    dup asm-lreg@ over asm-rreg@ div,
    asm-reg@ mflo, ;

: asm-mod ( ml-addr -- )
    dup asm-lreg@ over asm-rreg@ div,
    asm-reg@ mfhi, ;

: asm-neg ( ml-addr -- )
    dup asm-reg@ swap asm-lreg@ neg, ;

: asm-abs ( ml-addr -- )
    dup asm-reg@ swap asm-lreg@ abs, ;

: asm-and ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ and, ;

: asm-andi ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ andi, ;

: asm-or ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ or, ;

: asm-ori ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ ori, ;

: asm-xor ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ xor, ;

: asm-xori ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ xori, ;

: asm-not ( ml-addr -- )
    dup asm-reg@ swap asm-lreg@ not, ;

: asm-lsh ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ sllv, ;

: asm-lshi ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ sll, ;

: asm-rshu ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ srlv, ;

: asm-rsh ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ srav, ;

: asm-rshui ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ srl, ;

: asm-rshi ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ sra, ;

: back-patch-beq ( target-addr branch-addr -- )
    tuck - cell- 2 rshift
    over @ $ffff0000 and or swap ! ;

: beq,-info ( reg reg addr -- )
    here ['] back-patch-beq branch-info 2!
    beq, ;

: asm-0branch ( ml-addr -- )
    dup asm-lreg@ @zero rot asm-val@
    here - cell- beq,-info ;

: asm-branch ( ml-addr -- )
    @zero @zero rot asm-val@
    here - cell- beq,-info ;

: asm-beq ( ml-addr -- )
    dup asm-lreg@ over asm-rreg@ rot asm-val@
    here - cell- beq,-info ;

: asm-seq ( ml-addr -- )
    >r
    r@ asm-reg@ r@ asm-lreg@ r> asm-rreg@ xor,
    dup 1 sltiu, ;

: asm-slt ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ slt, ;

: asm-slti ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ slti, ;

: asm-sltu ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap asm-rreg@ sltu, ;

: asm-sltui ( ml-addr -- )
    dup asm-reg@ swap dup asm-lreg@ swap ml-val @ sltiu, ;

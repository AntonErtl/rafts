\ asm.fs	assembler file (for MIPS R3000)
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

: asm-bitmask ( n -- code )
    $1 swap lshift 1- ;

: asm-expand ( x -- x )
    dup $0000ffff > if
	$ffff0000 or
    endif ;

: asm-op ( n -- code )
    $6 asm-bitmask and $1a lshift ;

: asm-rs ( n -- code )
    $5 asm-bitmask and $15 lshift ;

: asm-rt ( n -- code )
    $5 asm-bitmask and $10 lshift ;

: asm-imm ( n -- code )
    $10 asm-bitmask and ;

: asm-target ( n -- code )
    $1a asm-bitmask and ;

: asm-rd ( n -- code )
    $5 asm-bitmask and $b lshift ;

: asm-shamt ( n -- code )
    $5 asm-bitmask and $6 lshift ;

' asm-shamt alias asm-sa

: asm-funct ( n -- code )
    $6 asm-bitmask and ;

\ ***** I-types
: (asm-I-type) ( code -- )
    a, ;

: (asm-I-type2) ( rt imm addr -- )
    @
    swap asm-imm or over asm-rt or a, drop ;

: asm-I-type2
    create (asm-I-type)
does>
    (asm-I-type2) ;

: (asm-I-type2n) ( rs imm addr -- )
    @
    swap $2 rshift asm-imm or swap asm-rs or a, ;

: asm-I-type2n
    create (asm-I-type)
does>
    (asm-I-type2n) ;

: (asm-I-type3) ( rt rs imm addr -- )
    @
    swap asm-imm or swap asm-rs or over asm-rt or a, drop ;

: asm-I-type3
    create (asm-I-type)
does>
    (asm-I-type3) ;

: (asm-I-type3n) ( rs rt imm addr -- )
    @
    swap $2 rshift asm-imm or swap asm-rt or over asm-rs or a, drop ;

: asm-I-type3n
    create (asm-I-type)
does>
    (asm-I-type3n) ;

: (asm-I-type3-offset) ( rt offset rs addr -- )
    @
    swap asm-rs or swap asm-imm or over asm-rt or a, drop ;

: asm-I-type3-offset
    create (asm-I-type)
does>
    (asm-I-type3-offset) ;

: (asm-I-type3n-offset) ( rt offset rs addr -- )
    @
    swap asm-rs or swap asm-imm or over asm-rt or a, drop ;

: asm-I-type3n-offset
    create (asm-I-type)
does>
    (asm-I-type3n-offset) ;

\ ***** regimm types
: asm-regimm2 ( funct -- )
    $01 asm-op swap asm-rt or asm-I-type2n ;

\ ***** copz types 1
: (asm-I-type1-copz) ( imm z addr -- )
    @
    swap asm-op or swap $2 rshift asm-imm or a, ;

: asm-I-type1-copz
    create (asm-I-type)
does>
    (asm-I-type1-copz) ;

: asm-copzi1 ( code -- )
    $10 asm-op or asm-I-type1-copz ;

: (asm-I-type3-copz) ( rt imm rs z addr -- )
    @
    swap asm-op or swap asm-rs or swap asm-imm or over asm-rt or a, drop ;

: asm-I-type3-copz
    create (asm-I-type)
does>
    (asm-I-type3-copz) ;

: asm-copzi3 ( code -- )
    asm-op asm-I-type3-copz ;

$00 constant asm-copz-MF
$02 constant asm-copz-CF
$04 constant asm-copz-MT
$06 constant asm-copz-CT
$08 constant asm-copz-BC
$10 constant asm-copz-C0

$00 constant asm-copz-BCF
$01 constant asm-copz-BCT

\ ***** J-types
: (asm-J-type) ( code -- )
    a, ;

: (asm-J-type1n) ( target addr -- )
    @
    swap $2 rshift asm-target or a, ;

: asm-J-type1n
    create (asm-J-type)
does>
    (asm-J-type1n) ;

\ ***** R-types
: (asm-R-type) ( code -- )
    a, ;

: (asm-R-type0n) ( addr -- )
    @ a, ;

: asm-R-type0n
    create (asm-R-type)
does>
    (asm-R-type0n) ;

: (asm-R-type1) ( rd addr -- )
    @
    over asm-rd or a, drop ;

: asm-R-type1
    create (asm-R-type)
does>
    (asm-R-type1) ;

: (asm-R-type1n) ( rs addr -- )
    @
    swap asm-rs or a, ;

: asm-R-type1n
    create (asm-R-type)
does>
    (asm-R-type1n) ;

: (asm-R-type2) ( rd rs addr -- )
    @
    swap asm-rs or over asm-rd or a, drop ;

: asm-R-type2
    create (asm-R-type)
does>
    (asm-R-type2) ;

: (asm-R-type2n) ( rs rt addr -- )
    @
    swap asm-rt or swap asm-rs or a, ;

: asm-R-type2n
    create (asm-R-type)
does>
    (asm-R-type2n) ;

: (asm-R-type3) ( rd rs rt addr -- )
    @
    swap asm-rt or swap asm-rs or over asm-rd or a, drop ;

: asm-R-type3
    create (asm-R-type)
does>
    (asm-R-type3) ;

: (asm-R-type3s) ( rd rt rs addr -- )
    @
    swap asm-rs or swap asm-rt or over asm-rd or a, drop ;

: asm-R-type3s
    create (asm-R-type)
does>
    (asm-R-type3s) ;

: (asm-R-type3sa) ( rd rt sa addr -- )
    @
    swap asm-sa or swap asm-rt or over asm-rd or a, drop ;

: asm-R-type3sa
    create (asm-R-type)
does>
    (asm-R-type3sa) ;

\ ***** special types
: asm-special0n ( funct -- )
    $00 asm-op $00 asm-rs or $00 asm-rd or $00 asm-rt or
    $00 asm-shamt or swap asm-funct or asm-R-type0n ;

: asm-special1 ( funct -- )
    $00 asm-op $00 asm-rd or $00 asm-rt or
    $00 asm-shamt or swap asm-funct or asm-R-type1 ;

: asm-special1n ( funct -- )
    $00 asm-op $00 asm-rs or $00 asm-rt or
    $00 asm-shamt or swap asm-funct or asm-R-type1n ;

: asm-special2 ( funct -- )
    $00 asm-op $00 asm-rt or
    $00 asm-shamt or swap asm-funct or asm-R-type2 ;

: asm-special2n ( funct -- )
    $00 asm-op $00 asm-rt or
    $00 asm-shamt or swap asm-funct or asm-R-type2n ;

: asm-special3 ( funct -- )
    $00 asm-op
    $00 asm-shamt or swap asm-funct or asm-R-type3 ;

: asm-special3s ( funct -- )
    $00 asm-op
    $00 asm-shamt or swap asm-funct or asm-R-type3s ;

: asm-special3sa ( funct -- )
    $00 asm-op
    swap asm-funct or asm-R-type3sa ;

\ ***** copz types 2
: asm-cop0r0 ( funct -- )
    $10 asm-op $10 asm-rs or $00 asm-rd or $00 asm-rt or
    $00 asm-shamt or swap asm-funct or asm-R-type0n ;

: (asm-R-type2-copz) ( rt rd z addr -- )
    @
    swap asm-op or swap asm-rd or over asm-rt or a, drop ;

: asm-R-type2-copz
    create (asm-R-type)
does>
    (asm-R-type2-copz) ;

: asm-copzr2 ( funct -- )
    $10 asm-op or
    $00 asm-shamt or $00 asm-funct or asm-R-type2-copz ;

: nop, ( -- )
    0 a, ;

include regs.fs

$04 asm-op asm-I-type3n			beq,
$05 asm-op asm-I-type3n			bne,
$06 asm-op $00 asm-rt or asm-I-type2n	blez,
$07 asm-op $00 asm-rt or asm-I-type2n	bgtz,
$08 asm-op asm-I-type3			addi,
$09 asm-op asm-I-type3			addiu,
$0a asm-op asm-I-type3			slti,
$0b asm-op asm-I-type3			sltiu,
$0c asm-op asm-I-type3			andi,
$0d asm-op asm-I-type3			ori,
$0e asm-op asm-I-type3			xori,
$0f asm-op asm-I-type2			lui,
$20 asm-op asm-I-type3-offset		lb,
$21 asm-op asm-I-type3-offset		lh,
$22 asm-op asm-I-type3-offset		lwl,
$23 asm-op asm-I-type3-offset		lw,
$24 asm-op asm-I-type3-offset		lbu,
$25 asm-op asm-I-type3-offset		lhu,
$26 asm-op asm-I-type3-offset		lwr,
$28 asm-op asm-I-type3n-offset		sb,
$29 asm-op asm-I-type3n-offset		sh,
$2a asm-op asm-I-type3n-offset		swl,
$2b asm-op asm-I-type3n-offset		sw,
$2e asm-op asm-I-type3n-offset		swr,

$02 asm-op asm-J-type1n			j,
$03 asm-op asm-J-type1n			jal,

$00 asm-special3sa			sll,
$02 asm-special3sa			srl,
$03 asm-special3sa			sra,
$04 asm-special3s			sllv,
$06 asm-special3s			srlv,
$07 asm-special3s			srav,
$08 asm-special1n			jr,
$09 asm-special2			jalr,
$0c asm-special0n			syscall,
$0d asm-special0n			break,
$10 asm-special1			mfhi,
$11 asm-special1n			mthi,
$12 asm-special1			mflo,
$13 asm-special1n			mtlo,
$18 asm-special2n			mult,
$19 asm-special2n			multu,
$1a asm-special2n			div,
$1b asm-special2n			divu,
$20 asm-special3			add,
$21 asm-special3			addu,
$22 asm-special3			sub,
$23 asm-special3			subu,
$24 asm-special3			and,
$25 asm-special3			or,
$26 asm-special3			xor,
$27 asm-special3			nor,
$2a asm-special3			slt,
$2b asm-special3			sltu,

$00 asm-regimm2				bltz,
$01 asm-regimm2				bgez,
$10 asm-regimm2				bltzal,
$11 asm-regimm2				bgezal,

$30 asm-copzi3				lwcz,
$38 asm-copzi3				swcz,
asm-copz-MF asm-rs asm-copzr2		mfcz,
asm-copz-CF asm-rs asm-copzr2		cfcz,
asm-copz-MT asm-rs asm-copzr2		mtcz,
asm-copz-CT asm-rs asm-copzr2		ctcz,
asm-copz-BC asm-rs asm-copz-BCF asm-rt or asm-copzi1 bczf,
asm-copz-BC asm-rs asm-copz-BCT asm-rt or asm-copzi1 bczt,
$01 asm-cop0r0				tlbr,
$02 asm-cop0r0				tlbwi,
$06 asm-cop0r0				tlbwr,
$08 asm-cop0r0				tlbl,

: move, ( rd rs -- )
    @zero addu, ;

: abs, ( rd rs -- )
    dup $0008 bgez,
    2dup move,
    @zero swap subu, ;

: neg, ( rd rs -- )
    @zero swap subu, ;

: negu, ( rd rs -- )
    @zero swap subu, ;

: not, ( rd rs -- )
    @zero nor, ;

: li, ( rd imm -- )
    dup 0= if
	drop dup @zero = if
	    drop
	else
	    @zero move,
	endif
    else
	dup $8000 u< if
	    @zero swap addiu,
	else
	    dup $10000 u< if
		@zero swap ori,
	    else
		dup $ffff and 0= if
		    $10 rshift lui,
		else
		    dup $ffff8000 and $ffff8000 = if
			@zero swap addiu,
		    else
			2dup $10 rshift lui,
			over swap ori,
		    endif
		endif
	    endif
	endif
    endif ;

: law, ( rt rs imm -- )
    regs-get dup >r rot rot li,
    0 r> lw, ;

: saw, ( rt rs imm -- )
    regs-get dup >r rot rot li,
    0 r> sw, ;

: lab, ( rt imm rs -- )
    regs-get dup >r rot rot li,
    0 r> lb, ;

: sab, ( rt imm rs -- )
    regs-get dup >r rot rot li,
    0 r> sb, ;

: blt, ( rs rt imm -- )		\ <
    >r @at rot rot slt,
    @at @zero r> bne, ;

: ble, ( rs rt imm -- )		\ <=
    >r @at rot rot swap slt,
    @at @zero r> beq, ;

: bgt, ( rs rt imm -- )		\ >
    >r @at rot rot swap slt,
    @at @zero r> bne, ;

: bge, ( rs rt imm -- )		\ >=
    >r @at rot rot slt,
    @at @zero r> beq, ;

: bltu, ( rs rt imm -- )	\ < unsigned
    >r @at rot rot sltu,
    @at @zero r> bne, ;

: bleu, ( rs rt imm -- )	\ <= unsigned
    >r @at rot rot swap sltu,
    @at @zero r> beq, ;

: bgtu, ( rs rt imm -- )	\ > unsigned
    >r @at rot rot swap sltu,
    @at @zero r> bne, ;

: bgeu, ( rs rt imm -- )	\ >= unsigned
    >r @at rot rot sltu,
    @at @zero r> beq, ;

?test $0080 [IF]
cr ." Test for asm.fs" cr

: exec ( u -- )
    >r execute r> 1+ 1 ?do
	here i cells - @
    loop ;

: same ( valn ... val0 u -- flag )
    true swap dup 2 + 2 ?do
	i pick over i + 1+ pick = rot and swap
    loop
    swap >r 2* 0 ?do
	drop
    loop
    r> ;

variable asm-exec
variable asm-xt
variable asm-u
variable asm-z

: save ( xt u exec -- )
    asm-exec ! asm-u !
    dup name. asm-xt ! ;

: check ( orz -- )
    asm-xt @ asm-u @ asm-exec @ execute asm-u @ same if
	." OK "
    else
	." NOK "
    endif ;

: asm-t0 ( val xt u xt -- )
    save
    check cr ;
: asm-test0 ( val xt u -- ) ['] exec asm-t0 ;

: asm-t2 ( val val xt u xt -- )
    save
    $ffffffff check
    $1 check cr ;
: asm-test2 ( val val xt u -- ) ['] exec asm-t2 ;

: asm-t2i ( val val xt u xt -- )
    save
    $fffffffc check
    $4 check cr ;
: asm-test2i ( val val xt u -- ) ['] exec asm-t2i ;

: asm-t2-copzi ( val val z xt u xt -- )
    save asm-z !
    $fffffffc asm-z @ check
    $4 asm-z @ check cr ;
: asm-test2-copzi ( val val z xt u -- ) ['] exec asm-t2-copzi ;

: asm-t4 ( val val val val xt u xt -- )
    save
    $ffffffff $ffffffff check
    $0 $1 check
    $1 $0 check
    $1 $1 check cr ;
: asm-test4 ( val val val val xt u -- ) ['] exec asm-t4 ;

: asm-t4i ( val val val val xt u xt -- )
    save
    $ffffffff $fffffffc check
    $0 $4 check
    $1 $0 check
    $1 $4 check cr ;
: asm-test4i ( val val val val xt u -- ) ['] exec asm-t4i ;

: asm-t4-copz ( val val val val z xt u xt -- )
    save asm-z !
    $ffffffff $ffffffff asm-z @ check
    $0 $1 asm-z @ check
    $1 $0 asm-z @ check
    $1 $1 asm-z @ check cr ;
: asm-test4-copz ( val val val val z xt u -- ) ['] exec asm-t4-copz ;

: asm-t5 ( val val val val val xt u xt -- )
    save
    $ffffffff $ffffffff $ffffffff check
    $0 $0 $1 check
    $0 $1 $0 check
    $1 $0 $0 check
    $1 $1 $1 check cr ;
: asm-test5 ( val val val val val xt u -- ) ['] exec asm-t5 ;

: asm-t5i ( val val val val val xt u xt -- )
    save
    $ffffffff $ffffffff $fffffffc check
    $0 $0 $4 check
    $0 $1 $0 check
    $1 $0 $0 check
    $1 $1 $4 check cr ;
: asm-test5i ( val val val val val xt u -- ) ['] exec asm-t5i ;

: asm-t5-copz ( val val val val val z xt u xt -- )
    save asm-z !
    $ffffffff $ffffffff $ffffffff asm-z @ check
    $0 $0 $1 asm-z @ check
    $0 $1 $0 asm-z @ check
    $1 $0 $0 asm-z @ check
    $1 $1 $1 asm-z @ check cr ;
: asm-test5-copz ( val val val val val z xt u -- ) ['] exec asm-t5-copz ;

$00210820 $00000820 $00200020 $00010020 $03fff820 ' add, 1 asm-test5
$20210001 $20010000 $20200000 $20000001 $23ffffff ' addi, 1 asm-test5
$24210001 $24010000 $24200000 $24000001 $27ffffff ' addiu, 1 asm-test5
$00210821 $00000821 $00200021 $00010021 $03fff821 ' addu, 1 asm-test5
$00210824 $00000824 $00200024 $00010024 $03fff824 ' and, 1 asm-test5
$30210001 $30010000 $30200000 $30000001 $33ffffff ' andi, 1 asm-test5
$45000001 $4500ffff 1 ' bczf, 1 asm-test2-copzi
$45010001 $4501ffff 1 ' bczt, 1 asm-test2-copzi
$10210001 $10200000 $10010000 $10000001 $13ffffff ' beq, 1 asm-test5i
$04210001 $04210000 $04010001 $07e1ffff ' bgez, 1 asm-test4i
$04310001 $04310000 $04110001 $07f1ffff ' bgezal, 1 asm-test4i
$1c200001 $1c200000 $1c000001 $1fe0ffff ' bgtz, 1 asm-test4i
$18200001 $18200000 $18000001 $1be0ffff ' blez, 1 asm-test4i
$04200001 $04200000 $04000001 $07e0ffff ' bltz, 1 asm-test4i
$04300001 $04300000 $04100001 $07f0ffff ' bltzal, 1 asm-test4i
$14210001 $14200000 $14010000 $14000001 $17ffffff ' bne, 1 asm-test5i
$0000000d ' break, 1 asm-test0
$44410800 $44410000 $44400800 $445ff800 1 ' cfcz, 1 asm-test4-copz
$44c10800 $44c10000 $44c00800 $44dff800 1 ' ctcz, 1 asm-test4-copz
$0021001a $0020001a $0001001a $03ff001a ' div, 1 asm-test4
$0021001b $0020001b $0001001b $03ff001b ' divu, 1 asm-test4
$08000001 $0bffffff ' j, 1 asm-test2i
$0c000001 $0fffffff ' jal, 1 asm-test2i
$00200809 $00000809 $00200009 $03e0f809 ' jalr, 1 asm-test4
$00200008 $03e00008 ' jr, 1 asm-test2
$80210001 $80010000 $80000001 $80200000 $83ffffff ' lb, 1 asm-test5
$90210001 $90010000 $90000001 $90200000 $93ffffff ' lbu, 1 asm-test5
$84210001 $84010000 $84000001 $84200000 $87ffffff ' lh, 1 asm-test5
$94210001 $94010000 $94000001 $94200000 $97ffffff ' lhu, 1 asm-test5
$3c010001 $3c010000 $3c000001 $3c1fffff ' lui, 1 asm-test4
$8c210001 $8c010000 $8c000001 $8c200000 $8fffffff ' lw, 1 asm-test5
$c4210001 $c4010000 $c4000001 $c4200000 $c7ffffff 1 ' lwcz, 1 asm-test5-copz
$88210001 $88010000 $88000001 $88200000 $8bffffff ' lwl, 1 asm-test5
$98210001 $98010000 $98000001 $98200000 $9bffffff ' lwr, 1 asm-test5
$44010800 $44010000 $44000800 $441ff800 1 ' mfcz, 1 asm-test4-copz
$00000810 $0000f810 ' mfhi, 1 asm-test2
$00000812 $0000f812 ' mflo, 1 asm-test2
$44810800 $44810000 $44800800 $449ff800 1 ' mtcz, 1 asm-test4-copz
$00200011 $03e00011 ' mthi, 1 asm-test2
$00200013 $03e00013 ' mtlo, 1 asm-test2
$00210018 $00200018 $00010018 $03ff0018 ' mult, 1 asm-test4
$00210019 $00200019 $00010019 $03ff0019 ' multu, 1 asm-test4
$00210827 $00000827 $00200027 $00010027 $03fff827 ' nor, 1 asm-test5
$00210825 $00000825 $00200025 $00010025 $03fff825 ' or, 1 asm-test5
$34210001 $34010000 $34200000 $34000001 $37ffffff ' ori, 1 asm-test5
$a0210001 $a0010000 $a0000001 $a0200000 $a3ffffff ' sb, 1 asm-test5
$a4210001 $a4010000 $a4000001 $a4200000 $a7ffffff ' sh, 1 asm-test5
$0021082a $0000082a $0020002a $0001002a $03fff82a ' slt, 1 asm-test5
$28210001 $28010000 $28200000 $28000001 $2bffffff ' slti, 1 asm-test5
$2c210001 $2c010000 $2c200000 $2c000001 $2fffffff ' sltiu, 1 asm-test5
$0021082b $0000082b $0020002b $0001002b $03fff82b ' sltu, 1 asm-test5
$00210822 $00000822 $00200022 $00010022 $03fff822 ' sub, 1 asm-test5
$00210823 $00000823 $00200023 $00010023 $03fff823 ' subu, 1 asm-test5
$ac210001 $ac010000 $ac000001 $ac200000 $afffffff ' sw, 1 asm-test5
$e4210001 $e4010000 $e4000001 $e4200000 $e7ffffff 1 ' swcz, 1 asm-test5-copz
$a8210001 $a8010000 $a8000001 $a8200000 $abffffff ' swl, 1 asm-test5
$b8210001 $b8010000 $b8000001 $b8200000 $bbffffff ' swr, 1 asm-test5
$0000000c ' syscall, 1 asm-test0
$42000008 ' tlbl, 1 asm-test0
$42000001 ' tlbr, 1 asm-test0
$42000002 ' tlbwi, 1 asm-test0
$42000006 ' tlbwr, 1 asm-test0
$00210826 $00000826 $00200026 $00010026 $03fff826 ' xor, 1 asm-test5
$38210001 $38010000 $38200000 $38000001 $3bffffff ' xori, 1 asm-test5

$00200821 $00000821 $00200021 $03e0f821 ' move, 1 asm-test4
$00010823 $00200821 $04210002
$00000823 $00000821 $04010002
$00010023 $00200021 $04210002
$001ff823 $03e0f821 $07e10002 ' abs, 3 asm-test4
$00010823 $00000823 $00010023 $001ff823 ' neg, 1 asm-test4
$00010823 $00000823 $00010023 $001ff823 ' negu, 1 asm-test4
$00200827 $00000827 $00200027 $03e0f827 ' not, 1 asm-test4
$14200001 $0021082a $14200000 $0020082a $14200000
$0001082a $14200001 $0000082a $1420ffff $03ff082a ' blt, 2 asm-test5i
$10200001 $0021082a $10200000 $0001082a $10200000
$0020082a $10200001 $0000082a $1020ffff $03ff082a ' ble, 2 asm-test5i
$14200001 $0021082a $14200000 $0001082a $14200000
$0020082a $14200001 $0000082a $1420ffff $03ff082a ' bgt, 2 asm-test5i
$10200001 $0021082b $10200000 $0020082b $10200000
$0001082b $10200001 $0000082b $1020ffff $03ff082b ' bgeu, 2 asm-test5i
$14200001 $0021082b $14200000 $0020082b $14200000
$0001082b $14200001 $0000082b $1420ffff $03ff082b ' bltu, 2 asm-test5i
$10200001 $0021082b $10200000 $0001082b $10200000
$0020082b $10200001 $0000082b $1020ffff $03ff082b ' bleu, 2 asm-test5i
$14200001 $0021082b $14200000 $0001082b $14200000
$0020082b $14200001 $0000082b $1420ffff $03ff082b ' bgtu, 2 asm-test5i
$10200001 $0021082b $10200000 $0020082b $10200000
$0001082b $10200001 $0000082b $1020ffff $03ff082b ' bgeu, 2 asm-test5i

finish
[THEN]

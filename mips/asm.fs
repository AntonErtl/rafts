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

\ ***** I-types
: @!(I-type) ( code -- )
    a, ;

: @!(I-type2) ( rt imm addr -- rt )
    @
    swap @!imm or over @!rt or a, ;

: @!I-type2
    create @!(I-type)
does>
    @!(I-type2) ;

: @!(I-type2n) ( rs imm addr -- )
    @
    swap $2 rshift @!imm or swap @!rs or a, ;

: @!I-type2n
    create @!(I-type)
does>
    @!(I-type2n) ;

: @!(I-type3) ( rt rs imm addr -- rt )
    @
    swap @!imm or swap @!rs or over @!rt or a, ;

: @!I-type3
    create @!(I-type)
does>
    @!(I-type3) ;

: @!(I-type3n) ( rs rt imm addr -- )
    @
    swap $2 rshift @!imm or swap @!rt or over @!rs or a, drop ;

: @!I-type3n
    create @!(I-type)
does>
    @!(I-type3n) ;

: @!(I-type3-offset) ( rt offset rs addr -- rt )
    @
    swap @!rs or swap @!imm or over @!rt or a, ;

: @!I-type3-offset
    create @!(I-type)
does>
    @!(I-type3-offset) ;

: @!(I-type3n-offset) ( rt offset rs addr -- )
    @
    swap @!rs or swap @!imm or over @!rt or a, drop ;

: @!I-type3n-offset
    create @!(I-type)
does>
    @!(I-type3n-offset) ;

\ ***** regimm types
: @!regimm2 ( funct -- )
    $01 @!op swap @!rt or @!I-type2n ;

\ ***** copz types 1
: @!(I-type1-copz) ( imm z addr -- )
    @
    swap @!op or swap $2 rshift @!imm or a, ;

: @!I-type1-copz
    create @!(I-type)
does>
    @!(I-type1-copz) ;

: @!copzi1 ( code -- )
    $10 @!op or @!I-type1-copz ;

: @!(I-type3-copz) ( rt imm rs z addr -- rt )
    @
    swap @!op or swap @!rs or swap @!imm or over @!rt or a, ;

: @!I-type3-copz
    create @!(I-type)
does>
    @!(I-type3-copz) ;

: @!copzi3 ( code -- )
    @!op @!I-type3-copz ;

$00 constant @copz-MF
$02 constant @copz-CF
$04 constant @copz-MT
$06 constant @copz-CT
$08 constant @copz-BC
$10 constant @copz-C0

$00 constant @copz-BCF
$01 constant @copz-BCT

\ ***** J-types
: @!(J-type) ( code -- )
    a, ;

: @!(J-type1n) ( target addr -- )
    @
    swap $2 rshift @!target or a, ;

: @!J-type1n
    create @!(J-type)
does>
    @!(J-type1n) ;

\ ***** R-types
: @!(R-type) ( code -- )
    a, ;

: @!(R-type0n) ( addr -- )
    @ a, ;

: @!R-type0n
    create @!(R-type)
does>
    @!(R-type0n) ;

: @!(R-type1) ( rd addr -- rd )
    @
    over @!rd or a, ;

: @!R-type1
    create @!(R-type)
does>
    @!(R-type1) ;

: @!(R-type1n) ( rs addr -- )
    @
    swap @!rs or a, ;

: @!R-type1n
    create @!(R-type)
does>
    @!(R-type1n) ;

: @!(R-type2) ( rd rs addr -- rd )
    @
    swap @!rs or over @!rd or a, ;

: @!R-type2
    create @!(R-type)
does>
    @!(R-type2) ;

: @!(R-type2n) ( rs rt addr -- )
    @
    swap @!rt or swap @!rs or a, ;

: @!R-type2n
    create @!(R-type)
does>
    @!(R-type2n) ;

: @!(R-type3) ( rd rs rt addr -- rd )
    @
    swap @!rt or swap @!rs or over @!rd or a, ;

: @!R-type3
    create @!(R-type)
does>
    @!(R-type3) ;

: @!(R-type3s) ( rd rt rs addr -- rd )
    @
    swap @!rs or swap @!rt or over @!rd or a, ;

: @!R-type3s
    create @!(R-type)
does>
    @!(R-type3s) ;

: @!(R-type3sa) ( rd rt sa addr -- rd )
    @
    swap @!sa or swap @!rt or over @!rd or a, ;

: @!R-type3sa
    create @!(R-type)
does>
    @!(R-type3sa) ;

\ ***** special types
: @!special0n ( funct -- )
    $00 @!op $00 @!rs or $00 @!rd or $00 @!rt or
    $00 @!shamt or swap @!funct or @!R-type0n ;

: @!special1 ( funct -- )
    $00 @!op $00 @!rd or $00 @!rt or
    $00 @!shamt or swap @!funct or @!R-type1 ;

: @!special1n ( funct -- )
    $00 @!op $00 @!rs or $00 @!rt or
    $00 @!shamt or swap @!funct or @!R-type1n ;

: @!special2 ( funct -- )
    $00 @!op $00 @!rt or
    $00 @!shamt or swap @!funct or @!R-type2 ;

: @!special2n ( funct -- )
    $00 @!op $00 @!rt or
    $00 @!shamt or swap @!funct or @!R-type2n ;

: @!special3 ( funct -- )
    $00 @!op
    $00 @!shamt or swap @!funct or @!R-type3 ;

: @!special3s ( funct -- )
    $00 @!op
    $00 @!shamt or swap @!funct or @!R-type3s ;

: @!special3sa ( funct -- )
    $00 @!op
    swap @!funct or @!R-type3sa ;

\ ***** copz types 2
: @!cop0r0 ( funct -- )
    $10 @!op $10 @!rs or $00 @!rd or $00 @!rt or
    $00 @!shamt or swap @!funct or @!R-type0n ;

: @!(R-type2-copz) ( rt rd z addr -- rt )
    @
    swap @!op or swap @!rd or over @!rt or a, ;

: @!R-type2-copz
    create @!(R-type)
does>
    @!(R-type2-copz) ;

: @!copzr2 ( funct -- )
    $10 @!op or
    $00 @!shamt or $00 @!funct or @!R-type2-copz ;

: @nop ( -- )
    0 a, ;

include regs.fs

$04 @!op @!I-type3n		@beq
$05 @!op @!I-type3n		@bne
$06 @!op $00 @!rt or @!I-type2n	@blez
$07 @!op $00 @!rt or @!I-type2n	@bgtz
$08 @!op @!I-type3		@addi
$09 @!op @!I-type3		@addiu
$0a @!op @!I-type3		@slti
$0b @!op @!I-type3		@sltiu
$0c @!op @!I-type3		@andi
$0d @!op @!I-type3		@ori
$0e @!op @!I-type3		@xori
$0f @!op @!I-type2		@lui
$20 @!op @!I-type3-offset	@lb
$21 @!op @!I-type3-offset	@lh
$22 @!op @!I-type3-offset	@lwl
$23 @!op @!I-type3-offset	@lw
$24 @!op @!I-type3-offset	@lbu
$25 @!op @!I-type3-offset	@lhu
$26 @!op @!I-type3-offset	@lwr
$28 @!op @!I-type3n-offset	@sb
$29 @!op @!I-type3n-offset	@sh
$2a @!op @!I-type3n-offset	@swl
$2b @!op @!I-type3n-offset	@sw
$2e @!op @!I-type3n-offset	@swr

$02 @!op @!J-type1n		@j
$03 @!op @!J-type1n		@jal

$00 @!special3sa		@sll
$02 @!special3sa		@srl
$03 @!special3sa		@sra
$04 @!special3s			@sllv
$06 @!special3s			@srlv
$07 @!special3s			@srav
$08 @!special1n			@jr
$09 @!special2			@jalr
$0c @!special0n			@syscall
$0d @!special0n			@break
$10 @!special1			@mfhi
$11 @!special1n			@mthi
$12 @!special1			@mflo
$13 @!special1n			@mtlo
$18 @!special2n			@mult
$19 @!special2n			@multu
$1a @!special2n			@div
$1b @!special2n			@divu
$20 @!special3			@add
$21 @!special3			@addu
$22 @!special3			@sub
$23 @!special3			@subu
$24 @!special3			@and
$25 @!special3			@or
$26 @!special3			@xor
$27 @!special3			@nor
$2a @!special3			@slt
$2b @!special3			@sltu

$00 @!regimm2			@bltz
$01 @!regimm2			@bgez
$10 @!regimm2			@bltzal
$11 @!regimm2			@bgezal

$30 @!copzi3			@lwcz
$38 @!copzi3			@swcz
@copz-MF @!rs @!copzr2		@mfcz
@copz-CF @!rs @!copzr2		@cfcz
@copz-MT @!rs @!copzr2		@mtcz
@copz-CT @!rs @!copzr2		@ctcz
@copz-BC @!rs @copz-BCF @!rt or @!copzi1 @bczf
@copz-BC @!rs @copz-BCT @!rt or @!copzi1 @bczt
$01 @!cop0r0			@tlbr
$02 @!cop0r0			@tlbwi
$06 @!cop0r0			@tlbwr
$08 @!cop0r0			@tlbl

: @move ( rd rs -- rd )
    @zero @addu ;

: @abs ( rd rs -- rd )
    dup $0008 @bgez
    2dup @move drop
    @zero swap @subu ;

: @neg ( rd rs -- rd )
    @zero swap @subu ;

: @negu ( rd rs -- rd )
    @zero swap @subu ;

: @not ( rd rs -- rd )
    @zero @nor ;

: @li ( rd imm -- rd )
    dup 0= if
	drop dup @zero = if
	    drop @zero
	else
	    @zero @move
	endif
    else
	dup $8000 u< if
	    @zero swap @addiu
	else
	    dup $10000 u< if
		@zero swap @ori
	    else
		dup $ffff and 0= if
		    $10 rshift @lui
		else
		    dup $ffff8000 and $ffff8000 = if
			@zero swap @addiu
		    else
			tuck $10 rshift @lui
			dup rot @ori
		    endif
		endif
	    endif
	endif
    endif ;

: @law ( rt rs imm -- rt )
    regs-get rot rot @li
    0 swap @lw ;

: @saw ( rt rs imm -- )
    regs-get rot rot @li
    0 swap @sw ;

: @lab ( rt imm rs -- rt )
    regs-get rot rot @li
    0 swap @lb ;

: @sab ( rt imm rs -- )
    regs-get rot rot @li
    0 swap @sb ;

: @blt ( rs rt imm -- )		\ <
    >r @at rot rot @slt
    @zero r> @bne ;

: @ble ( rs rt imm -- )		\ <=
    >r @at rot rot swap @slt
    @zero r> @beq ;

: @bgt ( rs rt imm -- )		\ >
    >r @at rot rot swap @slt
    @zero r> @bne ;

: @bge ( rs rt imm -- )		\ >=
    >r @at rot rot @slt
    @zero r> @beq ;

: @bltu ( rs rt imm -- )	\ < unsigned
    >r @at rot rot @sltu
    @zero r> @bne ;

: @bleu ( rs rt imm -- )	\ <= unsigned
    >r @at rot rot swap @sltu
    @zero r> @beq ;

: @bgtu ( rs rt imm -- )	\ > unsigned
    >r @at rot rot swap @sltu
    @zero r> @bne ;

: @bgeu ( rs rt imm -- )	\ >= unsigned
    >r @at rot rot @sltu
    @zero r> @beq ;

?test $0080 [IF]
cr ." Test for asm.fs" cr

: execd ( u -- )
  >r execute drop r> 1+ 1 ?do
    here i cells - @
  loop ;

: exece ( u -- )
  >r execute r> 1+ 1 ?do
    here i cells - @
  loop ;

: same ( valn ... val0 u -- flag )
  true swap dup 2 + 2 ?do
    i pick over i + 1+ pick = rot and swap
  loop
  swap >r 2* ndrop r> ;

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
: asm-test0d ( val xt u -- ) ['] execd asm-t0 ;
: asm-test0e ( val xt u -- ) ['] exece asm-t0 ;

: asm-t2 ( val val xt u xt -- )
  save
  $ffffffff check
  $1 check cr ;
: asm-test2d ( val val xt u -- ) ['] execd asm-t2 ;
: asm-test2e ( val val xt u -- ) ['] exece asm-t2 ;

: asm-t2i ( val val xt u xt -- )
  save
  $fffffffc check
  $4 check cr ;
: asm-test2di ( val val xt u -- ) ['] execd asm-t2i ;
: asm-test2ei ( val val xt u -- ) ['] exece asm-t2i ;

: asm-t2-copzi ( val val z xt u xt -- )
  save asm-z !
  $fffffffc asm-z @ check
  $4 asm-z @ check cr ;
: asm-test2d-copzi ( val val z xt u -- ) ['] execd asm-t2-copzi ;
: asm-test2e-copzi ( val val z xt u -- ) ['] exece asm-t2-copzi ;

: asm-t4 ( val val val val xt u xt -- )
  save
  $ffffffff $ffffffff check
  $0 $1 check
  $1 $0 check
  $1 $1 check cr ;
: asm-test4d ( val val val val xt u -- ) ['] execd asm-t4 ;
: asm-test4e ( val val val val xt u -- ) ['] exece asm-t4 ;

: asm-t4i ( val val val val xt u xt -- )
  save
  $ffffffff $fffffffc check
  $0 $4 check
  $1 $0 check
  $1 $4 check cr ;
: asm-test4di ( val val val val xt u -- ) ['] execd asm-t4i ;
: asm-test4ei ( val val val val xt u -- ) ['] exece asm-t4i ;

: asm-t4-copz ( val val val val z xt u xt -- )
  save asm-z !
  $ffffffff $ffffffff asm-z @ check
  $0 $1 asm-z @ check
  $1 $0 asm-z @ check
  $1 $1 asm-z @ check cr ;
: asm-test4d-copz ( val val val val z xt u -- ) ['] execd asm-t4-copz ;
: asm-test4e-copz ( val val val val z xt u -- ) ['] exece asm-t4-copz ;

: asm-t5 ( val val val val val xt u xt -- )
  save
  $ffffffff $ffffffff $ffffffff check
  $0 $0 $1 check
  $0 $1 $0 check
  $1 $0 $0 check
  $1 $1 $1 check cr ;
: asm-test5d ( val val val val val xt u -- ) ['] execd asm-t5 ;
: asm-test5e ( val val val val val xt u -- ) ['] exece asm-t5 ;

: asm-t5i ( val val val val val xt u xt -- )
  save
  $ffffffff $ffffffff $fffffffc check
  $0 $0 $4 check
  $0 $1 $0 check
  $1 $0 $0 check
  $1 $1 $4 check cr ;
: asm-test5di ( val val val val val xt u -- ) ['] execd asm-t5i ;
: asm-test5ei ( val val val val val xt u -- ) ['] exece asm-t5i ;

: asm-t5-copz ( val val val val val z xt u xt -- )
  save asm-z !
  $ffffffff $ffffffff $ffffffff asm-z @ check
  $0 $0 $1 asm-z @ check
  $0 $1 $0 asm-z @ check
  $1 $0 $0 asm-z @ check
  $1 $1 $1 asm-z @ check cr ;
: asm-test5d-copz ( val val val val val z xt u -- ) ['] execd asm-t5-copz ;
: asm-test5e-copz ( val val val val val z xt u -- ) ['] exece asm-t5-copz ;

$00210820 $00000820 $00200020 $00010020 $03fff820 ' @add 1 asm-test5d
$20210001 $20010000 $20200000 $20000001 $23ffffff ' @addi 1 asm-test5d
$24210001 $24010000 $24200000 $24000001 $27ffffff ' @addiu 1 asm-test5d
$00210821 $00000821 $00200021 $00010021 $03fff821 ' @addu 1 asm-test5d
$00210824 $00000824 $00200024 $00010024 $03fff824 ' @and 1 asm-test5d
$30210001 $30010000 $30200000 $30000001 $33ffffff ' @andi 1 asm-test5d
$45000001 $4500ffff 1 ' @bczf 1 asm-test2e-copzi
$45010001 $4501ffff 1 ' @bczt 1 asm-test2e-copzi
$10210001 $10200000 $10010000 $10000001 $13ffffff ' @beq 1 asm-test5ei
$04210001 $04210000 $04010001 $07e1ffff ' @bgez 1 asm-test4ei
$04310001 $04310000 $04110001 $07f1ffff ' @bgezal 1 asm-test4ei
$1c200001 $1c200000 $1c000001 $1fe0ffff ' @bgtz 1 asm-test4ei
$18200001 $18200000 $18000001 $1be0ffff ' @blez 1 asm-test4ei
$04200001 $04200000 $04000001 $07e0ffff ' @bltz 1 asm-test4ei
$04300001 $04300000 $04100001 $07f0ffff ' @bltzal 1 asm-test4ei
$14210001 $14200000 $14010000 $14000001 $17ffffff ' @bne 1 asm-test5ei
$0000000d ' @break 1 asm-test0e
$44410800 $44410000 $44400800 $445ff800 1 ' @cfcz 1 asm-test4d-copz
$44c10800 $44c10000 $44c00800 $44dff800 1 ' @ctcz 1 asm-test4d-copz
$0021001a $0020001a $0001001a $03ff001a ' @div 1 asm-test4e
$0021001b $0020001b $0001001b $03ff001b ' @divu 1 asm-test4e
$08000001 $0bffffff ' @j 1 asm-test2ei
$0c000001 $0fffffff ' @jal 1 asm-test2ei
$00200809 $00000809 $00200009 $03e0f809 ' @jalr 1 asm-test4d
$00200008 $03e00008 ' @jr 1 asm-test2e
$80210001 $80010000 $80000001 $80200000 $83ffffff ' @lb 1 asm-test5d
$90210001 $90010000 $90000001 $90200000 $93ffffff ' @lbu 1 asm-test5d
$84210001 $84010000 $84000001 $84200000 $87ffffff ' @lh 1 asm-test5d
$94210001 $94010000 $94000001 $94200000 $97ffffff ' @lhu 1 asm-test5d
$3c010001 $3c010000 $3c000001 $3c1fffff ' @lui 1 asm-test4d
$8c210001 $8c010000 $8c000001 $8c200000 $8fffffff ' @lw 1 asm-test5d
$c4210001 $c4010000 $c4000001 $c4200000 $c7ffffff 1 ' @lwcz 1 asm-test5d-copz
$88210001 $88010000 $88000001 $88200000 $8bffffff ' @lwl 1 asm-test5d
$98210001 $98010000 $98000001 $98200000 $9bffffff ' @lwr 1 asm-test5d
$44010800 $44010000 $44000800 $441ff800 1 ' @mfcz 1 asm-test4d-copz
$00000810 $0000f810 ' @mfhi 1 asm-test2d
$00000812 $0000f812 ' @mflo 1 asm-test2d
$44810800 $44810000 $44800800 $449ff800 1 ' @mtcz 1 asm-test4d-copz
$00200011 $03e00011 ' @mthi 1 asm-test2e
$00200013 $03e00013 ' @mtlo 1 asm-test2e
$00210018 $00200018 $00010018 $03ff0018 ' @mult 1 asm-test4e
$00210019 $00200019 $00010019 $03ff0019 ' @multu 1 asm-test4e
$00210827 $00000827 $00200027 $00010027 $03fff827 ' @nor 1 asm-test5d
$00210825 $00000825 $00200025 $00010025 $03fff825 ' @or 1 asm-test5d
$34210001 $34010000 $34200000 $34000001 $37ffffff ' @ori 1 asm-test5d
$a0210001 $a0010000 $a0000001 $a0200000 $a3ffffff ' @sb 1 asm-test5e
$a4210001 $a4010000 $a4000001 $a4200000 $a7ffffff ' @sh 1 asm-test5e
$0021082a $0000082a $0020002a $0001002a $03fff82a ' @slt 1 asm-test5d
$28210001 $28010000 $28200000 $28000001 $2bffffff ' @slti 1 asm-test5d
$2c210001 $2c010000 $2c200000 $2c000001 $2fffffff ' @sltiu 1 asm-test5d
$0021082b $0000082b $0020002b $0001002b $03fff82b ' @sltu 1 asm-test5d
$00210822 $00000822 $00200022 $00010022 $03fff822 ' @sub 1 asm-test5d
$00210823 $00000823 $00200023 $00010023 $03fff823 ' @subu 1 asm-test5d
$ac210001 $ac010000 $ac000001 $ac200000 $afffffff ' @sw 1 asm-test5e
$e4210001 $e4010000 $e4000001 $e4200000 $e7ffffff 1 ' @swcz 1 asm-test5d-copz
$a8210001 $a8010000 $a8000001 $a8200000 $abffffff ' @swl 1 asm-test5e
$b8210001 $b8010000 $b8000001 $b8200000 $bbffffff ' @swr 1 asm-test5e
$0000000c ' @syscall 1 asm-test0e
$42000008 ' @tlbl 1 asm-test0e
$42000001 ' @tlbr 1 asm-test0e
$42000002 ' @tlbwi 1 asm-test0e
$42000006 ' @tlbwr 1 asm-test0e
$00210826 $00000826 $00200026 $00010026 $03fff826 ' @xor 1 asm-test5d
$38210001 $38010000 $38200000 $38000001 $3bffffff ' @xori 1 asm-test5d

$00200821 $00000821 $00200021 $03e0f821 ' @move 1 asm-test4d
$00010823 $00200821 $04210002
$00000823 $00000821 $04010002
$00010023 $00200021 $04210002
$001ff823 $03e0f821 $07e10002 ' @abs 3 asm-test4d
$00010823 $00000823 $00010023 $001ff823 ' @neg 1 asm-test4d
$00010823 $00000823 $00010023 $001ff823 ' @negu 1 asm-test4d
$00200827 $00000827 $00200027 $03e0f827 ' @not 1 asm-test4d
$14200001 $0021082a $14200000 $0020082a $14200000
$0001082a $14200001 $0000082a $1420ffff $03ff082a ' @blt 2 asm-test5ei
$10200001 $0021082a $10200000 $0001082a $10200000
$0020082a $10200001 $0000082a $1020ffff $03ff082a ' @ble 2 asm-test5ei
$14200001 $0021082a $14200000 $0001082a $14200000
$0020082a $14200001 $0000082a $1420ffff $03ff082a ' @bgt 2 asm-test5ei
$10200001 $0021082b $10200000 $0020082b $10200000
$0001082b $10200001 $0000082b $1020ffff $03ff082b ' @bgeu 2 asm-test5ei
$14200001 $0021082b $14200000 $0020082b $14200000
$0001082b $14200001 $0000082b $1420ffff $03ff082b ' @bltu 2 asm-test5ei
$10200001 $0021082b $10200000 $0001082b $10200000
$0020082b $10200001 $0000082b $1020ffff $03ff082b ' @bleu 2 asm-test5ei
$14200001 $0021082b $14200000 $0001082b $14200000
$0020082b $14200001 $0000082b $1420ffff $03ff082b ' @bgtu 2 asm-test5ei
$10200001 $0021082b $10200000 $0020082b $10200000
$0001082b $10200001 $0000082b $1020ffff $03ff082b ' @bgeu 2 asm-test5ei

finish
[THEN]

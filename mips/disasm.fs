\ disasm.fs	disassembler file (for MIPS R3000)
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

\ ?trace $0800 [IF]
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
	$ffff0000 or
    endif ;

\ ***** I-types
: @@I-type2 ( addr -- )
    @ dup @@rt 2 swap hexn. @@imm 4 swap hexn. ;

: @@I-type2n ( addr -- )
    dup @ dup @@rs 2 swap hexn.
    @@imm $2 lshift expand dup 4 swap hexn. ." ( " + cell+ hex. ." ) " ;

: @@I-type3 ( addr -- )
    @ dup @@rt 2 swap hexn. dup @@rs 2 swap hexn. @@imm 4 swap hexn. ;

: @@I-type3n ( addr -- )
    dup @ dup @@rs 2 swap hexn. dup @@rt 2 swap hexn.
    @@imm $2 lshift expand dup 4 swap hexn. ." ( " + cell+ hex. ." ) " ;

: @@I-type3-offset ( addr -- )
    @ dup @@rt 2 swap hexn. dup @@imm 4 swap hexn. @@rs 2 swap hexn. ;

: @@I-type3n-offset ( addr -- )
    @ dup @@rt 2 swap hexn. dup @@imm 4 swap hexn. @@rs 2 swap hexn. ;

\ ***** regimm types
' @@I-type2n alias @@regimm2

\ ***** copz types 1
: @@copzi1 ( addr -- )
    dup @ dup @@imm $2 lshift expand dup 4 swap hexn. ." ( " rot + cell+ hex. ." ) "
    @@op 2 swap hexn. ;

: @@copzi3 ( addr -- )
    @ dup @@rt 2 swap hexn. dup @@imm 4 swap hexn.
    dup @@rs 2 swap hexn. @@op 2 swap hexn. ;

\ ***** J-types
: @@J-type1n ( addr -- )
    dup $fc000000 and swap @ @@target $2 lshift or hex. ;

\ ***** R-types
: @@R-type0 ( addr -- )
    @ hex. ;

: @@R-type1 ( addr -- )
    @ @@rd 2 swap hexn. ;

: @@R-type1n ( addr -- )
    @ @@rs 2 swap hexn. ;

: @@R-type2 ( addr -- )
    @ dup @@rd 2 swap hexn. @@rs 2 swap hexn. ;

: @@R-type2n ( addr -- )
    @ dup @@rs 2 swap hexn. @@rt 2 swap hexn. ;

: @@R-type3 ( addr -- )
    @ dup @@rd 2 swap hexn. dup @@rs 2 swap hexn. @@rt 2 swap hexn. ;

: @@R-type3n ( addr -- )
    @ dup @@rd 2 swap hexn. dup @@rt 2 swap hexn. @@rs 2 swap hexn. ;

: @@R-type3sa ( addr -- )
    @ dup @@rd 2 swap hexn. dup @@rt 2 swap hexn. @@sa 2 swap hexn. ;

\ ***** special types
' @@R-type0 alias @@special0
' @@R-type1 alias @@special1
' @@R-type1n alias @@special1n
' @@R-type2 alias @@special2
' @@R-type2n alias @@special2n
' @@R-type3 alias @@special3
' @@R-type3n alias @@special3n
' @@R-type3sa alias @@special3sa

\ ***** copz types 2
: @@cop0r0 ( addr -- )
    @ @@rs 2 swap hexn. ;

: @@copzr2 ( addr -- )
    @ dup @@rt 2 swap hexn. dup @@rd 2 swap hexn. @@op 2 swap hexn. ;

$40 2 matrix disasm-opc
$40 2 matrix disasm-opc-spezial
$20 2 matrix disasm-opc-regimm
$20 2 matrix disasm-opc-copzrs
$20 2 matrix disasm-opc-copzrt
$40 2 matrix disasm-opc-cop0

: (disasm-print) ( addr addr -- )
    >r dup 1 r@ execute @ rot swap execute
    0 r> execute @ name. ;

: disasm-print ( addr -- )
    dup @ 0= if
	drop ['] @nop name.
    else
	dup @ @@op
	dup 0 disasm-opc @ NIL <> if
	    ['] disasm-opc (disasm-print)
	else
	    1 disasm-opc @ execute
	endif
    endif ;

: disasm-dump ( to from -- )
    cr swap ?do
	i dup hex. ." ( " dup @ hex. ." ) : " disasm-print cr
    4 +loop ;

: (disasm-gen) ( name func n addr -- )
    >r tuck 1 r@ execute !
    0 r> execute ! ;

: disasm-gen ( name func n -- )
    ['] disasm-opc (disasm-gen) ;

: disasm-print-spezial ( addr -- )
    dup @ @@funct ['] disasm-opc-spezial (disasm-print) ;

: disasm-gen-spezial ( name func n -- )
    ['] disasm-opc-spezial (disasm-gen) ;

: disasm-print-regimm ( addr -- )
    dup @ @@rt ['] disasm-opc-regimm (disasm-print) ;

: disasm-gen-regimm ( name func n -- )
    ['] disasm-opc-regimm (disasm-gen) ;

: disasm-print-copzrs ( addr -- )
    dup @ @@rs
    dup 0 disasm-opc-copzrs @ NIL <> if
	['] disasm-opc-copzrs (disasm-print)
    else
	1 disasm-opc-copzrs @ execute
    endif ;

: disasm-gen-copzrs ( name func n -- )
    ['] disasm-opc-copzrs (disasm-gen) ;

: disasm-print-copzrt ( addr -- )
    dup @ @@rt ['] disasm-opc-copzrt (disasm-print) ;

: disasm-gen-copzrt ( name func n -- )
    ['] disasm-opc-copzrt (disasm-gen) ;

: disasm-print-copzi ( addr -- )
    dup @ @@rs ['] disasm-opc-copzrs (disasm-print) ;

: disasm-gen-copzi ( name func n -- )
    >r 2dup r@ 1+ disasm-gen
    2dup r@ 2 + disasm-gen
    r> 3 + disasm-gen ;

: disasm-print-cop0 ( addr -- )
    dup @ @@funct ['] disasm-opc-cop0 (disasm-print) ;

: disasm-gen-cop0 ( name func n -- )
    ['] disasm-opc-cop0 (disasm-gen) ;

: illegal-code ( -- ) ;

: @@nop ( code -- )
    @ 8 swap ." ( " hexn. ." ) " ;

: disasm-init ( xt n -- )
    0 ?do
	['] illegal-code ['] @@nop i 3 pick execute
    loop
    drop ;
' disasm-gen $40 disasm-init
' disasm-gen-spezial $40 disasm-init
' disasm-gen-regimm $20 disasm-init
' disasm-gen-copzrs $20 disasm-init
' disasm-gen-copzrt $20 disasm-init
' disasm-gen-cop0 $40 disasm-init
NIL ' disasm-print-spezial $00 disasm-gen
NIL ' disasm-print-regimm $01 disasm-gen
NIL ' disasm-print-cop0 $10 disasm-gen
NIL ' disasm-print-copzrs $11 disasm-gen
NIL ' disasm-print-copzrs $12 disasm-gen
NIL ' disasm-print-copzrs $13 disasm-gen
NIL ' disasm-print-copzrt @copz-BC disasm-gen-copzrs

' @beq		' @@I-type3n $04 disasm-gen
' @bne		' @@I-type3n $05 disasm-gen
' @blez		' @@I-type2n $06 disasm-gen
' @bgtz		' @@I-type2n $07 disasm-gen
' @addi		' @@I-type3 $08 disasm-gen
' @addiu	' @@I-type3 $09 disasm-gen
' @slti		' @@I-type3 $0a disasm-gen
' @sltiu	' @@I-type3 $0b disasm-gen
' @andi		' @@I-type3 $0c disasm-gen
' @ori		' @@I-type3 $0d disasm-gen
' @xori		' @@I-type3 $0e disasm-gen
' @lui		' @@I-type2 $0f disasm-gen
' @lb		' @@I-type3-offset $20 disasm-gen
' @lh		' @@I-type3-offset $21 disasm-gen
' @lwl		' @@I-type3-offset $22 disasm-gen
' @lw		' @@I-type3-offset $23 disasm-gen
' @lbu		' @@I-type3-offset $24 disasm-gen
' @lhu		' @@I-type3-offset $25 disasm-gen
' @lwr		' @@I-type3-offset $26 disasm-gen
' @sb		' @@I-type3n-offset $28 disasm-gen
' @sh		' @@I-type3n-offset $29 disasm-gen
' @swl		' @@I-type3n-offset $2a disasm-gen
' @sw		' @@I-type3n-offset $2b disasm-gen
' @swr		' @@I-type3n-offset $2e disasm-gen

' @j		' @@J-type1n $02 disasm-gen
' @jal		' @@J-type1n $03 disasm-gen

' @sll		' @@special3sa $00 disasm-gen-spezial
' @srl		' @@special3sa $02 disasm-gen-spezial
' @sra		' @@special3sa $03 disasm-gen-spezial
' @sllv		' @@special3n $04 disasm-gen-spezial
' @srlv		' @@special3n $06 disasm-gen-spezial
' @srav		' @@special3n $07 disasm-gen-spezial
' @jr		' @@special1n $08 disasm-gen-spezial
' @jalr		' @@special2 $09 disasm-gen-spezial
' @syscall	' @@special0 $0c disasm-gen-spezial
' @break	' @@special0 $0d disasm-gen-spezial
' @mfhi		' @@special1 $10 disasm-gen-spezial
' @mthi		' @@special1n $11 disasm-gen-spezial
' @mflo		' @@special1 $12 disasm-gen-spezial
' @mtlo		' @@special1n $13 disasm-gen-spezial
' @mult		' @@special2n $18 disasm-gen-spezial
' @multu	' @@special2n $19 disasm-gen-spezial
' @div		' @@special2n $1a disasm-gen-spezial
' @divu		' @@special2n $1b disasm-gen-spezial
' @add		' @@special3 $20 disasm-gen-spezial
' @addu		' @@special3 $21 disasm-gen-spezial
' @sub		' @@special3 $22 disasm-gen-spezial
' @subu		' @@special3 $23 disasm-gen-spezial
' @and		' @@special3 $24 disasm-gen-spezial
' @or		' @@special3 $25 disasm-gen-spezial
' @xor		' @@special3 $26 disasm-gen-spezial
' @nor		' @@special3 $27 disasm-gen-spezial
' @slt		' @@special3 $2a disasm-gen-spezial
' @sltu		' @@special3 $2b disasm-gen-spezial

' @bltz		' @@regimm2 $00 disasm-gen-regimm
' @bgez		' @@regimm2 $01 disasm-gen-regimm
' @bltzal	' @@regimm2 $10 disasm-gen-regimm
' @bgezal	' @@regimm2 $11 disasm-gen-regimm

' @lwcz		' @@copzi3 $30 disasm-gen-copzi
' @swcz		' @@copzi3 $38 disasm-gen-copzi
' @mfcz		' @@copzr2 @copz-MF disasm-gen-copzrs
' @cfcz		' @@copzr2 @copz-CF disasm-gen-copzrs
' @mtcz		' @@copzr2 @copz-MT disasm-gen-copzrs
' @ctcz		' @@copzr2 @copz-CT disasm-gen-copzrs
' @bczf		' @@copzi1 @copz-BCF disasm-gen-copzrt
' @bczt		' @@copzi1 @copz-BCT disasm-gen-copzrt
' @tlbr		' @@cop0r0 $01 disasm-gen-cop0
' @tlbwi	' @@cop0r0 $02 disasm-gen-cop0
' @tlbwr	' @@cop0r0 $06 disasm-gen-cop0
' @tlbl		' @@cop0r0 $08 disasm-gen-cop0

?test $0080 [IF]
cr ." Test for disasm..fs" cr

: gen ( coden ... code0 n -- )
    0 ?do
	a,
    loop ;

here
$00210820 $00000820 $00200020 $00010020 $03fff820 5 gen
$20210001 $20010000 $20200000 $20000001 $23ffffff 5 gen
$24210001 $24010000 $24200000 $24000001 $27ffffff 5 gen
$00210821 $00000821 $00200021 $00010021 $03fff821 5 gen
$00210824 $00000824 $00200024 $00010024 $03fff824 5 gen
$30210001 $30010000 $30200000 $30000001 $33ffffff 5 gen
$45000001 $4500ffff 2 gen
$45010001 $4501ffff 2 gen
$10210001 $10200000 $10010000 $10000001 $13ffffff 5 gen
$04210001 $04210000 $04010001 $07e1ffff 4 gen
$04310001 $04310000 $04110001 $07f1ffff 4 gen
$1c200001 $1c200000 $1c000001 $1fe0ffff 4 gen
$18200001 $18200000 $18000001 $1be0ffff 4 gen
$04200001 $04200000 $04000001 $07e0ffff 4 gen
$04300001 $04300000 $04100001 $07f0ffff 4 gen
$14210001 $14200000 $14010000 $14000001 $17ffffff 5 gen
$0000000d 1 gen
$44410800 $44410000 $44400800 $445ff800 4 gen
$44c10800 $44c10000 $44c00800 $44dff800 4 gen
$0021001a $0020001a $0001001a $03ff001a 4 gen
$0021001b $0020001b $0001001b $03ff001b 4 gen
$08000001 $0bffffff 2 gen
$0c000001 $0fffffff 2 gen
$00200809 $00000809 $00200009 $03e0f809 4 gen
$00200008 $03e00008 2 gen
$80210001 $80010000 $80000001 $80200000 $83ffffff 5 gen
$90210001 $90010000 $90000001 $90200000 $93ffffff 5 gen
$84210001 $84010000 $84000001 $84200000 $87ffffff 5 gen
$94210001 $94010000 $94000001 $94200000 $97ffffff 5 gen
$3c010001 $3c010000 $3c000001 $3c1fffff 4 gen
$8c210001 $8c010000 $8c000001 $8c200000 $8fffffff 5 gen
$c4210001 $c4010000 $c4000001 $c4200000 $c7ffffff 5 gen
$88210001 $88010000 $88000001 $88200000 $8bffffff 5 gen
$98210001 $98010000 $98000001 $98200000 $9bffffff 5 gen
$44010800 $44010000 $44000800 $441ff800 4 gen
$00000810 $0000f810 2 gen
$00000812 $0000f812 2 gen
$44810800 $44810000 $44800800 $449ff800 4 gen
$00200011 $03e00011 2 gen
$00200013 $03e00013 2 gen
$00210018 $00200018 $00010018 $03ff0018 4 gen
$00210019 $00200019 $00010019 $03ff0019 4 gen
$00210827 $00000827 $00200027 $00010027 $03fff827 5 gen
$00210825 $00000825 $00200025 $00010025 $03fff825 5 gen
$34210001 $34010000 $34200000 $34000001 $37ffffff 5 gen
$a0210001 $a0010000 $a0000001 $a0200000 $a3ffffff 5 gen
$a4210001 $a4010000 $a4000001 $a4200000 $a7ffffff 5 gen
$0021082a $0000082a $0020002a $0001002a $03fff82a 5 gen
$28210001 $28010000 $28200000 $28000001 $2bffffff 5 gen
$2c210001 $2c010000 $2c200000 $2c000001 $2fffffff 5 gen
$0021082b $0000082b $0020002b $0001002b $03fff82b 5 gen
$00210822 $00000822 $00200022 $00010022 $03fff822 5 gen
$00210823 $00000823 $00200023 $00010023 $03fff823 5 gen
$ac210001 $ac010000 $ac000001 $ac200000 $afffffff 5 gen
$e4210001 $e4010000 $e4000001 $e4200000 $e7ffffff 5 gen
$a8210001 $a8010000 $a8000001 $a8200000 $abffffff 5 gen
$b8210001 $b8010000 $b8000001 $b8200000 $bbffffff 5 gen
$0000000c 1 gen
$42000008 1 gen
$42000001 1 gen
$42000002 1 gen
$42000006 1 gen
$00210826 $00000826 $00200026 $00010026 $03fff826 5 gen
$38210001 $38010000 $38200000 $38000001 $3bffffff 5 gen

$00200821 $00000821 $00200021 $03e0f821 4 gen
$00010822 $00200821 $04210002 $00000822 $00000821 $04010002
$00010022 $00200021 $04210002 $001ff822 $03e0f821 $07e10002 12 gen
$00010822 $00000822 $00010022 $001ff822 4 gen
$00010823 $00000823 $00010023 $001ff823 4 gen
$00200827 $00000827 $00200027 $03e0f827 4 gen
$14200001 $0021082a $14200000 $0020082a $14200000 $0001082a
$14200001 $0000082a $1420ffff $03ff082a 10 gen
$10200001 $0021082a $10200000 $0001082a $10200000 $0020082a
$10200001 $0000082a $1020ffff $03ff082a 10 gen
$14200001 $0021082a $14200000 $0001082a $14200000 $0020082a
$14200001 $0000082a $1420ffff $03ff082a 10 gen
$10200001 $0021082b $10200000 $0020082b $10200000 $0001082b
$10200001 $0000082b $1020ffff $03ff082b 10 gen
$14200001 $0021082b $14200000 $0020082b $14200000 $0001082b
$14200001 $0000082b $1420ffff $03ff082b 10 gen
$10200001 $0021082b $10200000 $0001082b $10200000 $0020082b
$10200001 $0000082b $1020ffff $03ff082b 10 gen
$14200001 $0021082b $14200000 $0001082b $14200000 $0020082b
$14200001 $0000082b $1420ffff $03ff082b 10 gen
$10200001 $0021082b $10200000 $0020082b $10200000 $0001082b
$10200001 $0000082b $1020ffff $03ff082b 10 gen
here disasm-dump

finish
[THEN]
\ [THEN]

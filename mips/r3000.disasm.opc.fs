\ $Id: r3000.disasm.opc.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: r3000.disasm.opc.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

$40 2 2 marray disasm_opc
$40 2 2 marray disasm_opc_spezial
$20 2 2 marray disasm_opc_regimm
$20 2 2 marray disasm_opc_copzrs
$20 2 2 marray disasm_opc_copzrt
$40 2 2 marray disasm_opc_cop0

: (disasm_print) ( addr addr -- )
  >r dup 1 r@ execute @ rot swap execute
  0 r> execute @ name. ;

: disasm_print ( addr -- )
  dup @ 0= if
    drop ['] @nop name. else
    dup @ @@op
    dup 0 disasm_opc @ NIL <> if
      ['] disasm_opc (disasm_print) else
      1 disasm_opc @ execute endif endif ;

: disasm_dump ( to from -- )
  swap ?do
    i dup hex. ." : " disasm_print cr 4 +loop ;

: (disasm_gen) ( name func n addr -- )
  >r tuck 1 r@ execute !
  0 r> execute ! ;

: disasm_gen ( name func n -- )
  ['] disasm_opc (disasm_gen) ;

: disasm_print_spezial ( addr -- )
  dup @ @@funct ['] disasm_opc_spezial (disasm_print) ;

: disasm_gen_spezial ( name func n -- )
  ['] disasm_opc_spezial (disasm_gen) ;

: disasm_print_regimm ( addr -- )
  dup @ @@rt ['] disasm_opc_regimm (disasm_print) ;

: disasm_gen_regimm ( name func n -- )
  ['] disasm_opc_regimm (disasm_gen) ;

: disasm_print_copzrs ( addr -- )
  dup @ @@rs
  dup 0 disasm_opc_copzrs @ NIL <> if
    ['] disasm_opc_copzrs (disasm_print) else
    1 disasm_opc_copzrs @ execute endif ;

: disasm_gen_copzrs ( name func n -- )
  ['] disasm_opc_copzrs (disasm_gen) ;

: disasm_print_copzrt ( addr -- )
  dup @ @@rt ['] disasm_opc_copzrt (disasm_print) ;

: disasm_gen_copzrt ( name func n -- )
  ['] disasm_opc_copzrt (disasm_gen) ;

: disasm_print_copzi ( addr -- )
  dup @ @@rs ['] disasm_opc_copzrs (disasm_print) ;

: disasm_gen_copzi ( name func n -- )
  >r 2dup r@ 1+ disasm_gen
  2dup r@ 2 + disasm_gen
  r> 3 + disasm_gen ;

: disasm_print_cop0 ( addr -- )
  dup @ @@funct ['] disasm_opc_cop0 (disasm_print) ;

: disasm_gen_cop0 ( name func n -- )
  ['] disasm_opc_cop0 (disasm_gen) ;

: illegal-code ( -- )
  ;

: @@nop ( code -- )
  @ 8 swap ." ( " hexn. ." ) " ;

: disasm_init ( xt n -- )
  0 ?do
    ['] illegal-code ['] @@nop i 3 pick execute loop
  drop ;
' disasm_gen $40 disasm_init
' disasm_gen_spezial $40 disasm_init
' disasm_gen_regimm $20 disasm_init
' disasm_gen_copzrs $20 disasm_init
' disasm_gen_copzrt $20 disasm_init
' disasm_gen_cop0 $40 disasm_init
NIL ['] disasm_print_spezial $00 disasm_gen
NIL ['] disasm_print_regimm $01 disasm_gen
NIL ['] disasm_print_cop0 $10 disasm_gen
NIL ['] disasm_print_copzrs $11 disasm_gen
NIL ['] disasm_print_copzrs $12 disasm_gen
NIL ['] disasm_print_copzrs $13 disasm_gen
NIL ['] disasm_print_copzrt @copz_BC disasm_gen_copzrs

' @beq		' @@I_type3n $04 disasm_gen
' @bne		' @@I_type3n $05 disasm_gen
' @blez		' @@I_type2n $06 disasm_gen
' @bgtz		' @@I_type2n $07 disasm_gen
' @addi		' @@I_type3 $08 disasm_gen
' @addiu	' @@I_type3 $09 disasm_gen
' @slti		' @@I_type3 $0a disasm_gen
' @sltiu	' @@I_type3 $0b disasm_gen
' @andi		' @@I_type3 $0c disasm_gen
' @ori		' @@I_type3 $0d disasm_gen
' @xori		' @@I_type3 $0e disasm_gen
' @lui		' @@I_type2 $0f disasm_gen
' @lb		' @@I_type3_offset $20 disasm_gen
' @lh		' @@I_type3_offset $21 disasm_gen
' @lwl		' @@I_type3_offset $22 disasm_gen
' @lw		' @@I_type3_offset $23 disasm_gen
' @lbu		' @@I_type3_offset $24 disasm_gen
' @lhu		' @@I_type3_offset $25 disasm_gen
' @lwr		' @@I_type3_offset $26 disasm_gen
' @sb		' @@I_type3n_offset $28 disasm_gen
' @sh		' @@I_type3n_offset $29 disasm_gen
' @swl		' @@I_type3n_offset $2a disasm_gen
' @sw		' @@I_type3n_offset $2b disasm_gen
' @swr		' @@I_type3n_offset $2e disasm_gen

' @j		' @@J_type1n $02 disasm_gen
' @jal		' @@J_type1n $03 disasm_gen

' @sll		' @@special3sa $00 disasm_gen_spezial
' @srl		' @@special3sa $02 disasm_gen_spezial
' @sra		' @@special3sa $03 disasm_gen_spezial
' @sllv		' @@special3n $04 disasm_gen_spezial
' @srlv		' @@special3n $06 disasm_gen_spezial
' @srav		' @@special3n $07 disasm_gen_spezial
' @jr		' @@special1n $08 disasm_gen_spezial
' @jalr		' @@special2 $09 disasm_gen_spezial
' @syscall	' @@special0 $0c disasm_gen_spezial
' @break	' @@special0 $0d disasm_gen_spezial
' @mfhi		' @@special1 $10 disasm_gen_spezial
' @mthi		' @@special1n $11 disasm_gen_spezial
' @mflo		' @@special1 $12 disasm_gen_spezial
' @mtlo		' @@special1n $13 disasm_gen_spezial
' @mult		' @@special2n $18 disasm_gen_spezial
' @multu	' @@special2n $19 disasm_gen_spezial
' @div		' @@special2n $1a disasm_gen_spezial
' @divu		' @@special2n $1b disasm_gen_spezial
' @add		' @@special3 $20 disasm_gen_spezial
' @addu		' @@special3 $21 disasm_gen_spezial
' @sub		' @@special3 $22 disasm_gen_spezial
' @subu		' @@special3 $23 disasm_gen_spezial
' @and		' @@special3 $24 disasm_gen_spezial
' @or		' @@special3 $25 disasm_gen_spezial
' @xor		' @@special3 $26 disasm_gen_spezial
' @nor		' @@special3 $27 disasm_gen_spezial
' @slt		' @@special3 $2a disasm_gen_spezial
' @sltu		' @@special3 $2b disasm_gen_spezial

' @bltz		' @@regimm2 $00 disasm_gen_regimm
' @bgez		' @@regimm2 $01 disasm_gen_regimm
' @bltzal	' @@regimm2 $10 disasm_gen_regimm
' @bgezal	' @@regimm2 $11 disasm_gen_regimm

' @lwcz		' @@copzi3 $30 disasm_gen_copzi
' @swcz		' @@copzi3 $38 disasm_gen_copzi
' @mfcz		' @@copzr2 @copz_MF disasm_gen_copzrs
' @cfcz		' @@copzr2 @copz_CF disasm_gen_copzrs
' @mtcz		' @@copzr2 @copz_MT disasm_gen_copzrs
' @ctcz		' @@copzr2 @copz_CT disasm_gen_copzrs
' @bczf		' @@copzi1 @copz_BCF disasm_gen_copzrt
' @bczt		' @@copzi1 @copz_BCT disasm_gen_copzrt
' @tlbr		' @@cop0r0 $01 disasm_gen_cop0
' @tlbwi	' @@cop0r0 $02 disasm_gen_cop0
' @tlbwr	' @@cop0r0 $06 disasm_gen_cop0
' @tlbl		' @@cop0r0 $08 disasm_gen_cop0

?test $0080 [IF]
cr ." Test for r3000.disasm.opc.fs" cr

: gen ( coden ... code0 n -- )
  0 ?do
    a, loop ;

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
here disasm_dump

finish
[THEN]

\ $Id: r3000.asm.opc.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: r3000.asm.opc.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

include regs.fs

$04 @!op @!I_type3n		@beq
$05 @!op @!I_type3n		@bne
$06 @!op $00 @!rt or @!I_type2n	@blez
$07 @!op $00 @!rt or @!I_type2n	@bgtz
$08 @!op @!I_type3		@addi
$09 @!op @!I_type3		@addiu
$0a @!op @!I_type3		@slti
$0b @!op @!I_type3		@sltiu
$0c @!op @!I_type3		@andi
$0d @!op @!I_type3		@ori
$0e @!op @!I_type3		@xori
$0f @!op @!I_type2		@lui
$20 @!op @!I_type3_offset	@lb
$21 @!op @!I_type3_offset	@lh
$22 @!op @!I_type3_offset	@lwl
$23 @!op @!I_type3_offset	@lw
$24 @!op @!I_type3_offset	@lbu
$25 @!op @!I_type3_offset	@lhu
$26 @!op @!I_type3_offset	@lwr
$28 @!op @!I_type3n_offset	@sb
$29 @!op @!I_type3n_offset	@sh
$2a @!op @!I_type3n_offset	@swl
$2b @!op @!I_type3n_offset	@sw
$2e @!op @!I_type3n_offset	@swr

$02 @!op @!J_type1n		@j
$03 @!op @!J_type1n		@jal

$00 @!special3sa		@sll
$02 @!special3sa		@srl
$03 @!special3sa		@sra
$04 @!special3n			@sllv
$06 @!special3n			@srlv
$07 @!special3n			@srav
$08 @!special1n			@jr
$09 @!special2			@jalr
$0c @!special0			@syscall
$0d @!special0			@break
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
@copz_MF @!rs @!copzr2		@mfcz
@copz_CF @!rs @!copzr2		@cfcz
@copz_MT @!rs @!copzr2		@mtcz
@copz_CT @!rs @!copzr2		@ctcz
@copz_BC @!rs @copz_BCF @!rt or @!copzi1 @bczf
@copz_BC @!rs @copz_BCT @!rt or @!copzi1 @bczt
$01 @!cop0r0			@tlbr
$02 @!cop0r0			@tlbwi
$06 @!cop0r0			@tlbwr
$08 @!cop0r0			@tlbl

: @move ( rd rs -- rd )
  @zero @addu ;

: @abs ( rd rs -- rd )
  dup $0008 @bgez
  2dup @move drop
  @zero swap @sub ;

: @neg ( rd rs -- rd )
  @zero swap @sub ;

: @negu ( rd rs -- rd )
  @zero swap @subu ;

: @not ( rd rs -- rd )
  @zero @nor ;

: @li ( rd imm -- rd )
  dup 0= if
    drop @zero @move else
    dup $8000 u< if
      @zero swap @addiu else
      dup $10000 u< if
        @zero swap @ori else
        dup $ffff and 0= if
          $10 rshift @lui else
          dup $ffff8000 and $ffff8000 = if
          @zero swap @addiu else
          tuck $10 rshift @lui
          dup rot @ori endif endif endif endif endif ;

: @law ( rt rs imm -- rt )
  regs_get rot rot @li
  0 swap @lw ;

: @saw ( rt rs imm -- )
  regs_get rot rot @li
  0 swap @sw ;

: @lab ( rt imm rs -- rt )
  regs_get rot rot @li
  0 swap @lb ;

: @sab ( rt imm rs -- )
  regs_get rot rot @li
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
cr ." Test for r3000.asm.opc.fs" cr

: execd ( u -- )
  >r execute drop r> 1+ 1 ?do
    here i cells - @ loop ;

: exece ( u -- )
  >r execute r> 1+ 1 ?do
    here i cells - @ loop ;

: same ( valn ... val0 u -- flag )
  true swap dup 2 + 2 ?do
    i pick over i + 1+ pick = rot and swap loop
  swap >r 2* ndrop r> ;

variable asm_exec
variable asm_xt
variable asm_u
variable asm_z

: save ( xt u exec -- )
  asm_exec ! asm_u !
  dup name. asm_xt ! ;

: check ( orz -- )
  asm_xt @ asm_u @ asm_exec @ execute asm_u @ same if
    ." OK " else
    ." NOK " endif ;

: asm_t0 ( val xt u xt -- )
  save
  check cr ;
: asm_test0d ( val xt u -- ) ['] execd asm_t0 ;
: asm_test0e ( val xt u -- ) ['] exece asm_t0 ;

: asm_t2 ( val val xt u xt -- )
  save
  $ffffffff check
  $1 check cr ;
: asm_test2d ( val val xt u -- ) ['] execd asm_t2 ;
: asm_test2e ( val val xt u -- ) ['] exece asm_t2 ;

: asm_t2i ( val val xt u xt -- )
  save
  $fffffffc check
  $4 check cr ;
: asm_test2di ( val val xt u -- ) ['] execd asm_t2i ;
: asm_test2ei ( val val xt u -- ) ['] exece asm_t2i ;

: asm_t2_copzi ( val val z xt u xt -- )
  save asm_z !
  $fffffffc asm_z @ check
  $4 asm_z @ check cr ;
: asm_test2d_copzi ( val val z xt u -- ) ['] execd asm_t2_copzi ;
: asm_test2e_copzi ( val val z xt u -- ) ['] exece asm_t2_copzi ;

: asm_t4 ( val val val val xt u xt -- )
  save
  $ffffffff $ffffffff check
  $0 $1 check
  $1 $0 check
  $1 $1 check cr ;
: asm_test4d ( val val val val xt u -- ) ['] execd asm_t4 ;
: asm_test4e ( val val val val xt u -- ) ['] exece asm_t4 ;

: asm_t4i ( val val val val xt u xt -- )
  save
  $ffffffff $fffffffc check
  $0 $4 check
  $1 $0 check
  $1 $4 check cr ;
: asm_test4di ( val val val val xt u -- ) ['] execd asm_t4i ;
: asm_test4ei ( val val val val xt u -- ) ['] exece asm_t4i ;

: asm_t4_copz ( val val val val z xt u xt -- )
  save asm_z !
  $ffffffff $ffffffff asm_z @ check
  $0 $1 asm_z @ check
  $1 $0 asm_z @ check
  $1 $1 asm_z @ check cr ;
: asm_test4d_copz ( val val val val z xt u -- ) ['] execd asm_t4_copz ;
: asm_test4e_copz ( val val val val z xt u -- ) ['] exece asm_t4_copz ;

: asm_t5 ( val val val val val xt u xt -- )
  save
  $ffffffff $ffffffff $ffffffff check
  $0 $0 $1 check
  $0 $1 $0 check
  $1 $0 $0 check
  $1 $1 $1 check cr ;
: asm_test5d ( val val val val val xt u -- ) ['] execd asm_t5 ;
: asm_test5e ( val val val val val xt u -- ) ['] exece asm_t5 ;

: asm_t5i ( val val val val val xt u xt -- )
  save
  $ffffffff $ffffffff $fffffffc check
  $0 $0 $4 check
  $0 $1 $0 check
  $1 $0 $0 check
  $1 $1 $4 check cr ;
: asm_test5di ( val val val val val xt u -- ) ['] execd asm_t5i ;
: asm_test5ei ( val val val val val xt u -- ) ['] exece asm_t5i ;

: asm_t5_copz ( val val val val val z xt u xt -- )
  save asm_z !
  $ffffffff $ffffffff $ffffffff asm_z @ check
  $0 $0 $1 asm_z @ check
  $0 $1 $0 asm_z @ check
  $1 $0 $0 asm_z @ check
  $1 $1 $1 asm_z @ check cr ;
: asm_test5d_copz ( val val val val val z xt u -- ) ['] execd asm_t5_copz ;
: asm_test5e_copz ( val val val val val z xt u -- ) ['] exece asm_t5_copz ;

$00210820 $00000820 $00200020 $00010020 $03fff820 ' @add 1 asm_test5d
$20210001 $20010000 $20200000 $20000001 $23ffffff ' @addi 1 asm_test5d
$24210001 $24010000 $24200000 $24000001 $27ffffff ' @addiu 1 asm_test5d
$00210821 $00000821 $00200021 $00010021 $03fff821 ' @addu 1 asm_test5d
$00210824 $00000824 $00200024 $00010024 $03fff824 ' @and 1 asm_test5d
$30210001 $30010000 $30200000 $30000001 $33ffffff ' @andi 1 asm_test5d
$45000001 $4500ffff 1 ' @bczf 1 asm_test2e_copzi
$45010001 $4501ffff 1 ' @bczt 1 asm_test2e_copzi
$10210001 $10200000 $10010000 $10000001 $13ffffff ' @beq 1 asm_test5ei
$04210001 $04210000 $04010001 $07e1ffff ' @bgez 1 asm_test4ei
$04310001 $04310000 $04110001 $07f1ffff ' @bgezal 1 asm_test4ei
$1c200001 $1c200000 $1c000001 $1fe0ffff ' @bgtz 1 asm_test4ei
$18200001 $18200000 $18000001 $1be0ffff ' @blez 1 asm_test4ei
$04200001 $04200000 $04000001 $07e0ffff ' @bltz 1 asm_test4ei
$04300001 $04300000 $04100001 $07f0ffff ' @bltzal 1 asm_test4ei
$14210001 $14200000 $14010000 $14000001 $17ffffff ' @bne 1 asm_test5ei
$0000000d ' @break 1 asm_test0e
$44410800 $44410000 $44400800 $445ff800 1 ' @cfcz 1 asm_test4d_copz
$44c10800 $44c10000 $44c00800 $44dff800 1 ' @ctcz 1 asm_test4d_copz
$0021001a $0020001a $0001001a $03ff001a ' @div 1 asm_test4e
$0021001b $0020001b $0001001b $03ff001b ' @divu 1 asm_test4e
$08000001 $0bffffff ' @j 1 asm_test2ei
$0c000001 $0fffffff ' @jal 1 asm_test2ei
$00200809 $00000809 $00200009 $03e0f809 ' @jalr 1 asm_test4d
$00200008 $03e00008 ' @jr 1 asm_test2e
$80210001 $80010000 $80000001 $80200000 $83ffffff ' @lb 1 asm_test5d
$90210001 $90010000 $90000001 $90200000 $93ffffff ' @lbu 1 asm_test5d
$84210001 $84010000 $84000001 $84200000 $87ffffff ' @lh 1 asm_test5d
$94210001 $94010000 $94000001 $94200000 $97ffffff ' @lhu 1 asm_test5d
$3c010001 $3c010000 $3c000001 $3c1fffff ' @lui 1 asm_test4d
$8c210001 $8c010000 $8c000001 $8c200000 $8fffffff ' @lw 1 asm_test5d
$c4210001 $c4010000 $c4000001 $c4200000 $c7ffffff 1 ' @lwcz 1 asm_test5d_copz
$88210001 $88010000 $88000001 $88200000 $8bffffff ' @lwl 1 asm_test5d
$98210001 $98010000 $98000001 $98200000 $9bffffff ' @lwr 1 asm_test5d
$44010800 $44010000 $44000800 $441ff800 1 ' @mfcz 1 asm_test4d_copz
$00000810 $0000f810 ' @mfhi 1 asm_test2d
$00000812 $0000f812 ' @mflo 1 asm_test2d
$44810800 $44810000 $44800800 $449ff800 1 ' @mtcz 1 asm_test4d_copz
$00200011 $03e00011 ' @mthi 1 asm_test2e
$00200013 $03e00013 ' @mtlo 1 asm_test2e
$00210018 $00200018 $00010018 $03ff0018 ' @mult 1 asm_test4e
$00210019 $00200019 $00010019 $03ff0019 ' @multu 1 asm_test4e
$00210827 $00000827 $00200027 $00010027 $03fff827 ' @nor 1 asm_test5d
$00210825 $00000825 $00200025 $00010025 $03fff825 ' @or 1 asm_test5d
$34210001 $34010000 $34200000 $34000001 $37ffffff ' @ori 1 asm_test5d
$a0210001 $a0010000 $a0000001 $a0200000 $a3ffffff ' @sb 1 asm_test5e
$a4210001 $a4010000 $a4000001 $a4200000 $a7ffffff ' @sh 1 asm_test5e
$0021082a $0000082a $0020002a $0001002a $03fff82a ' @slt 1 asm_test5d
$28210001 $28010000 $28200000 $28000001 $2bffffff ' @slti 1 asm_test5d
$2c210001 $2c010000 $2c200000 $2c000001 $2fffffff ' @sltiu 1 asm_test5d
$0021082b $0000082b $0020002b $0001002b $03fff82b ' @sltu 1 asm_test5d
$00210822 $00000822 $00200022 $00010022 $03fff822 ' @sub 1 asm_test5d
$00210823 $00000823 $00200023 $00010023 $03fff823 ' @subu 1 asm_test5d
$ac210001 $ac010000 $ac000001 $ac200000 $afffffff ' @sw 1 asm_test5e
$e4210001 $e4010000 $e4000001 $e4200000 $e7ffffff 1 ' @swcz 1 asm_test5d_copz
$a8210001 $a8010000 $a8000001 $a8200000 $abffffff ' @swl 1 asm_test5e
$b8210001 $b8010000 $b8000001 $b8200000 $bbffffff ' @swr 1 asm_test5e
$0000000c ' @syscall 1 asm_test0e
$42000008 ' @tlbl 1 asm_test0e
$42000001 ' @tlbr 1 asm_test0e
$42000002 ' @tlbwi 1 asm_test0e
$42000006 ' @tlbwr 1 asm_test0e
$00210826 $00000826 $00200026 $00010026 $03fff826 ' @xor 1 asm_test5d
$38210001 $38010000 $38200000 $38000001 $3bffffff ' @xori 1 asm_test5d

$00200821 $00000821 $00200021 $03e0f821 ' @move 1 asm_test4d
$00010822 $00200821 $04210002
$00000822 $00000821 $04010002
$00010022 $00200021 $04210002
$001ff822 $03e0f821 $07e10002 ' @abs 3 asm_test4d
$00010822 $00000822 $00010022 $001ff822 ' @neg 1 asm_test4d
$00010823 $00000823 $00010023 $001ff823 ' @negu 1 asm_test4d
$00200827 $00000827 $00200027 $03e0f827 ' @not 1 asm_test4d
$14200001 $0021082a $14200000 $0020082a $14200000
$0001082a $14200001 $0000082a $1420ffff $03ff082a ' @blt 2 asm_test5ei
$10200001 $0021082a $10200000 $0001082a $10200000
$0020082a $10200001 $0000082a $1020ffff $03ff082a ' @ble 2 asm_test5ei
$14200001 $0021082a $14200000 $0001082a $14200000
$0020082a $14200001 $0000082a $1420ffff $03ff082a ' @bgt 2 asm_test5ei
$10200001 $0021082b $10200000 $0020082b $10200000
$0001082b $10200001 $0000082b $1020ffff $03ff082b ' @bgeu 2 asm_test5ei
$14200001 $0021082b $14200000 $0020082b $14200000
$0001082b $14200001 $0000082b $1420ffff $03ff082b ' @bltu 2 asm_test5ei
$10200001 $0021082b $10200000 $0001082b $10200000
$0020082b $10200001 $0000082b $1020ffff $03ff082b ' @bleu 2 asm_test5ei
$14200001 $0021082b $14200000 $0001082b $14200000
$0020082b $14200001 $0000082b $1420ffff $03ff082b ' @bgtu 2 asm_test5ei
$10200001 $0021082b $10200000 $0020082b $10200000
$0001082b $10200001 $0000082b $1020ffff $03ff082b ' @bgeu 2 asm_test5ei

finish
[THEN]

\ disasm.fs
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

\ 80386 Disassembler

\ ported to gforth by M. Anton Ertl
\ ported to rafts by Christian Pirker

\ Andrew McKewan, April 1994
\ Tom Zimmer,  05/18/94 port to Win32f
\ Modified to word in decimal 08/03/94 10:04 tjz
\ 06-??-95 SMuB NEXT sequence defined in FKERNEL
\ 06-21-95 SMuB removed redundant COUNT calls from txb, lxs.

: strcat ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 )
    >r >r 2dup + r> swap r@ cmove
    r> + 2dup swap char- c! ;

\ gforth compatibility

\ require gforth-compat.fs
: h.   ( u -- )
    base @ hex  swap u.  base ! ;
: ?.name ( cfa -- )
    \ print name of cfa
    dup look if
	name>string type drop
    else
	drop dup 0< if
	    ." -" negate
	endif
	h.
    endif ;
: abs>rel ( abs -- rel )
    \ convert absolute address to relative address in win32forth
    \ gforth does not use relative addresses
;
: col ( n -- )
    \ move forward to column n
    drop space ;
4 value right-margin
: ?line ( n1 -- )
    drop ;
: h.r           ( n1 n2 -- )    \ display n1 as a hex number right
                                \ justified in a field of n2 characters
    base @ >r hex >r
    0 <# #s #> r> over - spaces type
    r> base ! ;
: h.n           ( n1 n2 -- )    \ display n1 a s a hex number of n2 digits
    base @ >r hex >r
    0 <# r> 0 ?do # loop #> type
    r> base ! ;

: h.2           ( n1 -- ) 2 h.n ;       \ two digit HEX number
: h.4           ( n1 -- ) 4 h.n ;       \ four digit HEX number
: h.8           ( n1 -- ) 8 h.n ;       \ eight digit HEX number
: upc ( c1 -- c2 )
    toupper ;
: not 0= ;
: next-seq ( addr -- f )
    \ is the code at addr next?
    false ;
0 value tabing?        \ are we tabing, default to no
: tabing-on     ( -- )
                true to tabing? ;

: tabing-off    ( -- )
                false to tabing? ;
 0 value tab-margin
: 0tab          ( -- )          \ left margin goes to left edge of screen
                0 to tab-margin ;
: start/stop
    \ if a key is pressed, wait for another one
;

\ end of gforth compatibility

\ only forth also definitions
\ vocabulary disassembler
\ disassembler also definitions

\ hex

\ 0 value ip  ( points to current instruction )

\ : size  ip c@ 1 and ;  ( operand size, 0=byte, 1=dword )

0 value size
0 value 16-bit-data
0 value 16-bit-addr
0 value prefix-op

: @+  ( addr -- addr n )
    dup cell+ swap @ ;
: w@+ ( addr -- addr n )
    dup 2 + swap w@ ;

: sext  ( byte -- n )
    dup $80 and if $FFFFFF00 or endif ;

: ???   ( n1 -- )
    ." ??? " drop ;

: s. ( n adr len w )
    >r drop  swap r@ * +  r> type space ;

: reg8  ( n -- )
    7 and s" alcldlblahchdhbh" 2 s. ;
: sreg  ( n -- )
    7 and s" escsssdsfsgs????" 2 s. ;
: reg16 ( n -- )
    7 and s" axcxdxbxspbpsidi" 2 s. ;
: reg32 ( n -- )
    7 and s" eaxecxedxebxespebpesiedi" 3 s. ;
: [reg] ( n -- )
    7 and s" [eax][ecx][edx][ebx][esp][ebp][esi][edi]"
    5 s. space ;

: reg  ( a n -- a )  size
    if
	16-bit-data if
	    reg16
	else
	    reg32
	endif
    else
	reg8
    endif ;

: disp8  ( adr -- adr' )
    count h. ;

: disp32 ( adr -- adr' )
    16-bit-addr if
	w@+
    else
	@+
    endif
    body> ?.name ;

: .,     ( -- )
    ." , " ;

: .#
    ., ." # " ;

: imm8   ( adr -- adr' )
    .# count h. ;

: imm16  ( adr -- adr' )
    .# w@+ h. ;

: imm32  ( adr -- adr' )
    16-bit-data if
	imm16
    else
	.# @+ h.
    endif ;

: [*]  ( sib --  )
    ." sib = " h. ;

: sib ( adr ext -- adr' )
    swap count >r swap  6 rshift 3 and ?dup if
	1 = if
	    disp8
	else
	    disp32
	endif
    endif
    r> dup 7 and dup 5 = if
	drop [*]
    else
	[reg]
	dup $38 and $20 = if
	    drop
	else
	    ." [" dup 3 rshift reg32 8 emit
	    5 rshift 6 and
	    dup 6 = if
		2 +
	    endif
	    ?dup if
		." *" 0 .r
	    endif
	    ." ] "
	endif
    endif ;

: mod-r/m  ( adr ext -- adr' )
    dup $C7 and 5 = if
	drop disp32 ." [] " exit
    endif
    dup $C0 and $C0 < over 7 and 4 = and if
	sib exit
    endif
    dup 6 rshift case
	0 of  ." 0 " [reg]  endof
	1 of  swap disp8  swap [reg]  endof
	2 of  swap disp32 swap [reg]  endof
	3 of  reg  endof
    endcase ;

: r/m8
    0 to size mod-r/m ;
: r/m32
    1 to size mod-r/m ;

: r,r/m  ( adr -- adr' )
    count dup 3 rshift reg ., mod-r/m ;

: r/m,r  ( adr -- adr' )
    count dup >r mod-r/m ., r> 3 rshift reg ;

: r/m  ( adr op -- adr' )
    2 and if
	r,r/m else r/m,r
    endif ;

\ -------------------- Simple Opcodes --------------------

: inh   ( -<name>- )
    create
    bl word count here place
    here c@ 1+ allot
does>
    count type space drop ;

inh clc  clc
inh stc  stc
inh cld  cld
inh std  std
inh rpnz repnz
inh repz repz
inh cbw  cbw
inh cdq  cdq
inh daa  daa
inh das  das
inh aaa  aaa
inh aas  aas
inh lock lock
inh inb  insb
inh osb  outsb
inh sah  sahf
inh lah  lahf
inh aam  aam
inh aad  aad
inh hlt  hlt
inh cmc  cmc
inh xlt  xlat
inh cli  cli
inh sti  sti

inh d16: d16:
inh a16: a16:
\ inh es:  es:
\ inh cs:  cs:
\ inh ds:  ds:
\ inh fs:  fs:
\ inh gs:  gs:

: d16   ( adr code -- adr' )
    d16:
    true to 16-bit-data
    true to prefix-op ;

: a16   ( adr code -- adr' )
    a16:
    true to 16-bit-addr
    true to prefix-op ;

: rpz	( adr code -- adr' )
    rpnz
    true to prefix-op ;

: rep   ( adr code -- adr' )
    repz
    true to prefix-op ;

: lok   ( adr code -- adr' )  \ This should have error checking added
    drop ." lock "
    true to prefix-op ;

: cs:	( adr code -- adr' )
    drop ." cs: "
    true to prefix-op ;

: ds:	( adr code -- adr' )
    drop ." ds: "
    true to prefix-op ;

: ss:	( adr code -- adr' )
    drop ." ss: "
    true to prefix-op ;

: es:	( adr code -- adr' )
    drop ." es: "
    true to prefix-op ;

: gs:	( adr code -- adr' )
    drop ." gs: "
    true to prefix-op ;

: fs:	( adr code -- adr' )
    drop ." fs: "
    true to prefix-op ;

: isd   ( adr code -- adr' )
    drop 16-bit-data if
	." insw    "
    else
	." insd    "
    endif ;

: osd	( adr code -- adr' )
    drop 16-bit-data if
	." outsw    "
    else
	." outsd    "
    endif ;

: inp	( addr code -- addr' )
    ." in      " 1 and if
	16-bit-data if
	    ." ax, "
	else
	    ." eax, "
	endif
    else
	." al, "
    endif
    count h. ;

: otp	( addr code -- addr' )
    ." out     " 1 and if
	count h. 16-bit-data if
	    ." , ax "
	else
	    ." , eax "
	endif
    else
	count h. ." , al "
    endif ;

: ind	( addr code -- addr' )
    ." in      " 1 and if
	16-bit-data if
	    ." ax, dx "
	else
	    ." eax, dx "
	endif
    else
	." al, dx "
    endif ;

: otd	( addr code -- addr' )
    ." out     " 1 and if
	16-bit-data if
	    ." dx, ax "
	else
	    ." dx, eax "
	endif
    else
	." dx, al "
    endif ;

\ -------------------- ALU Opcodes --------------------

: .alu  ( n -- )
    7 and s" addor adcsbbandsubxorcmp" 3 s. 4 spaces ;

: alu  ( adr op -- adr' )
    dup 3 rshift .alu r/m ;

: ali ( adr op -- adr' )
    >r count
    dup 3 rshift .alu
    mod-r/m
    r> 3 and ?dup if
	1 = if
	    imm32
	else
	    .# count sext .
	endif
    else
	imm8
    endif ;

: ala  ( adr op -- adr' )
    dup 3 rshift .alu
    1 and if
	0 reg imm32
    else
	0 reg8 imm8
    endif ;


\ -------------------- Test/Xchg --------------------

: txb	( addr op -- addr' )
    dup 3 and s" testtestxchgxchg" 4 s. 4 spaces
    1 and if
	1 to size r,r/m     \ SMuB removed COUNT
    else
	0 to size r,r/m     \ SMuB removed COUNT
    endif ;

: tst	( addr op -- addr' )
    ." test    " 1 and if
	16-bit-data if
	    ." ax, " imm16
	else
	    ." eax, " imm32
	endif
    else
	." al, " imm8
    endif ;

\ -------------------- Inc/Dec ----------------------

: inc  ( addr op -- addr' )
    ." inc     " reg32 ;

: dec  ( addr op -- addr' )
    ." dec     " reg32 ;


\ -------------------- Push/Pop --------------------

: psh  	( addr op -- addr' )
    ." push    " reg32 ;

: pop  	( addr op -- addr' )
    ." pop     " reg32 ;

: pss   ( addr op -- addr' )
    ." push    " 3 rshift 3 and sreg ;

: pps	( addr op -- addr' )
    ." pop     " 3 rshift 3 and sreg ;

: psa	( addr op -- addr' )
    drop 16-bit-data if
	." pusha   "
    else
	." pushad  "
    endif ;

: ppa	( addr op -- addr' )
    drop 16-bit-data if
	." popa    "
    else
	." popad   "
    endif ;

: psi	( addr op -- addr' )
    ." push    " 2 and if
	imm8
    else
	imm32
    endif ;

: psf	( addr op -- addr' )
    drop 16-bit-data if
	." pushf   "
    else
	." pushfd  "
    endif ;

: ppf	( addr op -- addr' )
    drop 16-bit-data if
	." popf    "
    else
	." popfd   "
    endif ;

: 8F.  	( addr op -- addr' )
    drop count ." pop     " r/m32 ;

\ -------------------- Move --------------------

: mov  ( addr op -- addr' )
    ." mov,    " r/m ;

: mri  ( addr op -- addr' ) ( mov register, imm )
    ." mov,    " dup 8 and if
	reg32 imm32
    else
	reg8 imm8
    endif ;

: mvi  ( adr op -- adr' )   ( mov mem, imm )
    ." mov,    " drop count mod-r/m
    size if
	imm32
    else
	imm8
    endif ;

: mrs	( addr op -- addr' )
    16-bit-data if
	." mov,    " drop
	1 to size
	count dup >r mod-r/m .,
	r> 3 rshift sreg
    else
	???
    endif ;

: msr	( addr op -- addr' )
    16-bit-data if
	." mov,    " drop
	1 to size
	count dup 3 rshift sreg .,
	mod-r/m
    else
	???
    endif ;

: mv1	( addr op -- addr' )
    ." mov,    " 1 and if
	16-bit-data if
	    ." ax, word "
	else
	    ." eax, dword "
	endif
    else
	." al, byte "
    endif
    16-bit-addr if
	w@+
    else
	@+
    endif
    abs>rel body> ?.name ;

: mv2	( addr op -- addr' )
    ." mov,    " 1 and if
	16-bit-data if
	    ." word "  disp32 ., ."  ax "
	else
	    ." dword " disp32 ., ."  eax "
	endif
    else
	." byte "  disp32 ., ."  al "
    endif ;

: lea  ( addr op -- addr' )
    ." lea     " drop  1 to size r,r/m ;

: lxs	( addr op -- addr' )
    1 and if
	." lds     "
    else
	." les     "
    endif
    r,r/m ;   \ SMuB removed COUNT

: bnd  ( addr op -- addr' )
    ." bound   " drop  1 to size r,r/m ;

: arp	( addr op -- addr' )
    ." arpl    " drop
    1 to size
    true to 16-bit-data
    r,r/m ;

: mli	( addr op -- addr' )
    1 to size
    ." imul    " $69 = if
	r,r/m imm32
    else
	r,r/m imm8
    endif ;

\ -------------------- Jumps and Calls --------------------

: rel8  ( addr op -- addr' )
    count sext over + h. ;

: rel32 ( addr op -- addr' )
    @+ over + ?.name ;
			
: jsr  ( addr op -- addr' )
    ." call    " drop rel32 ;

: jmp  ( addr op -- addr' )
    ." jmp     " 2 and if rel8 else rel32 endif ;

: .jxx  ( addr op -- addr' )
    $0F and
    S" jo jnojb jaeje jnejbeja js jnsjpejpojl jgejlejg "
    3 s.  5 spaces ;

: bra  ( addr op -- addr' )
    .jxx rel8 ;

: lup  ( addr op -- addr' )
    3 and S" loopnzloopz loop  jecxz " 6 s. 2 spaces rel8 ;

: lbr  ( addr op -- addr' )
    .jxx rel32 ;

: rtn  ( addr op -- addr' )
    ." ret     near " 1 and 0= if
	w@+ h.
    endif ;

: rtf  ( addr op -- addr' )
    ." ret     far " 1 and 0= if
	w@+ h.
    endif ;

: ent  ( addr op -- addr' )
    ." enter   " w@+ . ., count h. ;

: cis	( addr op -- addr' )
    $9a = if
	." call    "
    else
	." jmp     "
    endif
    16-bit-data if
	." ptr16:16 "
    else
	." ptr16:32 "
    endif
    count mod-r/m ;

: nt3	( addr op -- addr' )
    drop ." int     3 " ;

: int	( addr op -- addr' )
    drop ." int     "
    count h. ;

inh lev leave
inh irt  iret
inh nto  into

\ -------------------- string ops --------------------

: str   inh does> count type  1 and if ." d" else ." b" endif ;

str mvs movs
str cps cmps
str sts stos
str lds lods
str scs scas

\ -------------------- Exchange --------------------

: xga  ( addr op -- addr' )
    ." xchg     eax, " reg32 ;

\ : xch  ( addr op -- addr' )
\	." xchg    " drop r,r/m ;


\ -------------------- Shifts & Rotates --------------------

: .shift ( n -- )
    7 and S" rolrorrclrcrshlshrxxxsar" 3 s.  5 spaces ;

: shf  ( addr op -- addr' )
    >r count
    dup 3 rshift .shift
    mod-r/m .,
    r> $D2 and case
	$C0 of count h. endof
	$D0 of 1 h.     endof
	$D2 of 1 reg8   endof
    endcase ;

\ -------------------- Extended Opcodes --------------------

: wf1  ( addr -- addr' )
    1+ count dup
    $0c0 < if
	dup
	3 rshift 7 and case
	    6 of	  ." fstenv  "      mod-r/m endof
	    7 of	  ." fstcw   word " mod-r/m endof
	    2drop 2 - dup ." fwait   "
	endcase
    else
	drop 2 - ." fwait   "
    endif ;
	
: wf2  ( addr -- addr' )
    1+ count case
	$e2 of  ." fclex   " endof
	$e3 of	." finit   " endof
	2 -	." fwait   "
    endcase ;
	
: wf3  ( addr -- addr' )
    1+ count dup 3 rshift 7 and case
	6 of     ." fsave   "      mod-r/m endof
	7 of     ." fstsw   word " mod-r/m endof
	drop 2 - ." fwait   "
    endcase ;
		
: wf4  ( addr -- addr' )
    1+ count $e0 = if
	." fstsw   ax "
    else
	2 - ." fwait   "
    endif ;

: fwaitops   ( addr op -- addr' )
    case
	$d9 of wf1 endof
	$db of wf2 endof
	$dd of wf3 endof
	$df of wf4 endof
	." fwait   "
    endcase ;
	
: w8f	( addr op -- addr' )
    drop dup c@ dup $f8 and $d8 = if
	fwaitops
    else
	drop ." wait    "
    endif ;	

: falu1   ( xopcode -- )
    3 rshift 7 and
    S" fadd fmul fcom fcompfsub fsubrfdiv fdivr"
    5 S. 2 spaces ;

: falu5   ( xopcode -- )
    3 rshift 7 and
    s" fadd fmul ???? ???? fsubrfsub fdivrfdiv "
    5 S. 2 spaces ;

: sti.   ( op -- )
    7 and ." ST(" 1 .r ." )";

: sti.st   ( op -- )
    7 and
    ." ST(" 1 .r ." )" ."  ST " ;

: fd8   ( addr opcode -- addr' )
    drop count dup falu1
    dup $c0 < if
	." float " mod-r/m
    else
	dup $f0 and $d0 = if
	    sti.
	else
	    ." ST, " sti.
	endif
    endif ;

: fdc   ( addr opcode -- addr' )
    drop count
    dup $c0 < if
	dup falu1 ." double " mod-r/m
    else
	dup falu5 dup $f0 and $d0 = if
	    sti.  ."  ??? "
	else
	    sti.st
	endif
    endif ;

: fnullary-f   ( op -- )
    $0f and dup 8 < if	
	S" f2xm1  fyl2x  fptan  fpatan fxtractfprem1 fdecstpfincstp"
    else
	8 -	
	S" fprem  fyl2xp1fsqrt  fsincosfrndintfscale fsin   fcos   "
    endif	
    7 s. ;

: fnullary-e   ( op -- )
    $0f and dup 8 < if
	S" fchs   fabs   ???    ???    ftst   fxam   ???    ???    "
    else  8 -
	S" fld1   fldl2t fldl2e fldpi  fldlg2 fldln2 fldz   ???    "
    endif
    7 s. ;

: fnullary   ( op -- )
    dup $ef > if
	fnullary-f EXIT
    endif
    dup $e0 < if
	$d0 = if
	    ." fnop "
	else
	    0 ???
	endif
	EXIT
    endif
    fnullary-e ;

: falu2   ( op -- )
    3 rshift 7 and
    S" fld    ???    fst    fstp   fldenv fldcw  fnstenvfnstcw "
    7 S. ;

: fd9   ( addr op -- addr' )
    drop count dup $c0 < if
	dup falu2 dup $38 and CASE
	    $00 OF ." float " mod-r/m       endof
	    $08 OF ." float " mod-r/m 0 ??? endof
	    $10 OF ." float " mod-r/m       endof
	    $18 OF ." float " mod-r/m       endof
	    $20 OF mod-r/m		    endof
	    $28 OF ." word " mod-r/m        endof
	    $30 OF mod-r/m	            endof
	    $38 OF ." word " mod-r/m        endof
	ENDCASE
    else
	dup $d0 < if
	    dup $c8 < if
		." fld     "
	    else
		." fxch    "
	    endif
	    sti.
	else
	    fnullary
	endif
    endif ;

: falu3   ( op -- )
    3 rshift 7 and
    S" fiadd fimul ficom ficompfisub fisubrfidiv fidivr"
    6 s. 1 spaces ;

: fda   ( addr op -- )
    drop count dup $c0 < if
	dup falu3 ." dword " mod-r/m
    else
	$e9 = if
	    ." fucompp "
	else
	    0 ???
	endif
    endif ;

: fde   ( addr op -- addr' )
    drop count dup $c0 < if
	dup falu3 ." word " mod-r/m
    else
	dup $f8 and CASE
	    $c0 OF ." faddp   " sti.st	endof
	    $c8 OF ." fmulp   " sti.st	endof
	    $d0 OF ???			endof
	    $d8 OF $d9 = if
		." fcompp "
	    else
		0 ???
	    endif			endof
	    $e0 OF ." fsubrp  " sti.st	endof
	    $e8 OF ." fsubp   " sti.st	endof
	    $f0 OF ." fdivrp  " sti.st	endof
	    $f8 OF ." fdivp   " sti.st	endof
	ENDCASE
    endif ;

: fdb   ( addr op -- addr' )
    drop count dup $c0 < if
	dup $38 and CASE
	    $00 OF ." fild    dword "    mod-r/m endof
	    $10 OF ." fist    dword "    mod-r/m endof
	    $18 OF ." fistp   dword "    mod-r/m endof
	    $28 OF ." fld     extended " mod-r/m endof
	    $38 OF ." fstp    extended " mod-r/m endof
	    ( else ) ???
	ENDCASE
    else	
	CASE
	    $e2 OF ." fnclex " endof
	    $e3 OF ." fninit " endof
	    ( else ) 0 ???
	ENDCASE
    endif ;

: fdd   ( addr op -- addr' )
    drop count dup $c0 < if
	dup $38 and CASE
	    $00 OF ." fld     double " mod-r/m endof
	    $10 OF ." fst     double " mod-r/m endof
	    $18 OF ." fstp    double " mod-r/m endof
	    $20 OF ." frstor  "        mod-r/m endof
	    $30 OF ." fnsave  "        mod-r/m endof
	    $38 OF ." fnstsw  word   " mod-r/m endof
	    ( else ) ???
	ENDCASE
    else
	dup $f8 and CASE
	    $c0 OF ." ffree   " sti. endof
	    $d0 OF ." fst     " sti. endof
	    $d8 OF ." fstp    " sti. endof
	    $e0 OF ." fucom   " sti. endof
	    $e8 OF ." fucomp  " sti. endof
	    ( else ) 0 ???
	ENDCASE
    endif ;

: fdf   ( addr op -- addr' )
    drop count dup $c0 < if
	dup $38 and CASE
	    $00 OF ." fild    word "  mod-r/m endof
	    $10 OF ." fist    word "  mod-r/m endof
	    $18 OF ." fistp   word "  mod-r/m endof
	    $20 OF ." fbld    tbyte " mod-r/m endof
	    $28 OF ." fild    qword " mod-r/m endof
	    $30 OF ." fbstp   tbyte " mod-r/m endof
	    $38 OF ." fistp   qword " mod-r/m endof
	    ( else ) ???
	ENDCASE
    else
	$f8 and $e0 = if
	    ." fnstsw  ax "
	else
	    0 ???
	endif
    endif ;

: grp6   ( addr -- addr' )
    count dup 3 rshift
    7 and S" sldtstr lldtltr verrverw??? ???" 4 s. 4 spaces
    r/m32 ;

: 0F.  ( addr op -- addr' )
    drop count case
	$00 of   grp6				endof
	$02 of   ." lar     " 1 to size r,r/m   endof
	$03 of   ." lsl     " 1 to size r,r/m   endof
	$06 of   ." clts    "                   endof
	$08 of   ." invd    "                   endof
	$09 of   ." wbinvd  "                   endof
	$30 of   ." wrmsr   "                   endof
	$31 of   ." rdtsc   "                   endof
	$32 of   ." rdmsr   "                   endof
	$80 of   ." jo      " rel32             endof
	$81 of   ." jno     " rel32             endof
	$82 of   ." jc      " rel32             endof
	$83 of   ." jnc     " rel32             endof
	$84 of   ." jz      " rel32             endof
	$85 of   ." jne     " rel32             endof
	$86 of   ." jbe     " rel32             endof
	$87 of   ." ja      " rel32             endof
	$88 of   ." js      " rel32             endof
	$89 of   ." jns     " rel32             endof
	$8A of   ." jpe     " rel32             endof
	$8B of   ." jpo     " rel32             endof
	$8C of   ." jnge    " rel32             endof
	$8D of   ." jge     " rel32             endof
	$8E of   ." jng     " rel32             endof
	$8F of   ." jg      " rel32             endof
	$90 of   ." seto    byte " r/m8	        endof
	$91 of   ." setno   byte " r/m8	        endof
	$92 of   ." setc    byte " r/m8	        endof
	$93 of   ." setnc   byte " r/m8	        endof
	$94 of   ." setz    byte " r/m8	        endof
	$95 of   ." setnz   byte " r/m8	        endof
	$96 of   ." setbe   byte " r/m8	        endof
	$97 of   ." seta    byte " r/m8	        endof
	$98 of   ." sets    byte " r/m8	        endof
	$99 of   ." setns   byte " r/m8	        endof
	$9A of   ." setp    byte " r/m8	        endof
	$9B of   ." setnp   byte " r/m8	        endof
	$9C of   ." setl    byte " r/m8	        endof
	$9D of   ." setge   byte " r/m8	        endof
	$9E of   ." setle   byte " r/m8	        endof
	$9F of   ." setg    byte " r/m8	        endof
	$A0 of   ." push    fs "                endof
	$A1 of   ." pop     fs "                endof
	$A2 of   ." cpuid      "		endof
	$A3 of   ." bt      " r/m,r             endof
	$A4 of   ." shld    " r/m,r imm8        endof
	$A5 of   ." shld    " r/m,r ." , cl"    endof
	$A8 of   ." push    gs "                endof
	$A9 of   ." pop     gs "                endof
	$AA of   ." rsm     "                   endof
	$AB of   ." bts     " 1 to size r/m,r   endof
	$AC of   ." shrd    " r/m,r imm8        endof
	$AD of   ." shrd    " r/m,r ." , cl"    endof
	$AF of   ." imul    " r,r/m             endof
	$B0 of   ." cmpxch  " 0 to size r/m,r   endof
	$B1 of   ." cmpxch  " 1 to size r/m,r   endof
	$B2 of   ." lss     " 1 to size r,r/m   endof
	$B3 of   ." btr     " 1 to size r/m,r   endof
	$B4 of   ." lfs     " 1 to size r,r/m   endof
	$B5 of   ." lgs     " 1 to size r,r/m   endof
	$B6 of   ." movzx   " 0 to size r,r/m   endof
	$B7 of   ." movzx   " 1 to size r,r/m   endof
	$BB of   ." btc     " 1 to size r/m,r   endof
	$BC of   ." bsf     " 1 to size r,r/m   endof
	$BD of   ." bsr     " 1 to size r,r/m   endof
	$BE of   ." movsx   " 0 to size r,r/m   endof
	$BF of   ." movsx   " 1 to size r,r/m   endof
	$C0 of   ." xadd    " 0 to size r/m,r   endof
	$C1 of   ." xadd    " 1 to size r/m,r   endof
	$C7 of   ." cmpxchg8b " r/m32           endof
	$C8 of   ." bswap   eax "               endof
	$C9 of   ." bswap   ecx "               endof
	$CA of   ." bswap   edx "               endof
	$CB of   ." bswap   ebx "               endof
	$CC of   ." bswap   esp "               endof
	$CD of   ." bswap   ebp "               endof
	$CE of   ." bswap   esi "               endof
	$CF of   ." bswap   edi "               endof
	( else )  dup ???
    endcase ;

: F6.  ( addr op -- addr' )
    drop count
    dup 3 rshift 7 and S" testXXXXnot neg mul imuldiv idiv" 4 s.
    4 spaces mod-r/m ;

: FE.  ( addr op -- addr' )
    drop count
    dup 3 rshift 7 and case
	0 of ." inc     " r/m8 endof
	1 of ." dec     " r/m8 endof
	dup ???
    endcase ;

: FF.  ( addr op -- addr' )
    drop count
    dup 3 rshift 7 and case
	0 of ." inc     "  r/m32 endof
	1 of ." dec     "  r/m32 endof
	2 of ." call    "  r/m32 endof
	4 of ." jmp     "  r/m32 endof
	6 of ." push    "  r/m32 endof
	dup ???
    endcase ;

\ -------------------- Opcode Table --------------------

: ops $10 0 do ' , loop ;

create op-table

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

ops  alu alu alu alu  ala ala pss pps  alu alu alu alu  ala ala pss 0F.
ops  alu alu alu alu  ala ala pss pps  alu alu alu alu  ala ala pss pps
ops  alu alu alu alu  ala ala es: daa  alu alu alu alu  ala ala cs: das
ops  alu alu alu alu  ala ala ss: aaa  alu alu alu alu  ala ala ds: aas

ops  inc inc inc inc  inc inc inc inc  dec dec dec dec  dec dec dec dec
ops  psh psh psh psh  psh psh psh psh  pop pop pop pop  pop pop pop pop
ops  psa ppa bnd arp  fs: gs: d16 a16  psi mli psi mli  inb isd osb osd
ops  bra bra bra bra  bra bra bra bra  bra bra bra bra  bra bra bra bra

ops  ali ali ??? ali  txb txb txb txb  mov mov mov mov  mrs lea msr 8F.
ops  xga xga xga xga  xga xga xga xga  cbw cdq cis w8f  psf ppf sah lah
ops  mv1 mv1 mv2 mv2  mvs mvs cps cps  tst tst sts sts  lds lds scs scs
ops  mri mri mri mri  mri mri mri mri  mri mri mri mri  mri mri mri mri

ops  shf shf rtn rtn  lxs lxs mvi mvi  ent lev rtf rtf  nt3 int nto irt
ops  shf shf shf shf  aam aad ??? xlt  fd8 fd9 fda fdb  fdc fdd fde fdf
ops  lup lup lup lup  inp inp otp otp  jsr jmp cis jmp  ind ind otd otd
ops  lok ??? rpz rep  hlt cmc F6. F6.  clc stc cli sti  cld std FE. FF.

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

: disasm-op  ( adr -- adr' )
    false to prefix-op              \ SMuB
    count
    dup 1 and to size
    dup cells op-table +  @ execute
    prefix-op 0= if
	false to 16-bit-data
	false to 16-bit-addr
    endif ;

0 value next-inst

: inst  ( adr -- adr' )
    ." ( " dup hex. ." ) "
    dup to next-inst
    cols $29 < if
	disasm-op
    else
	dup disasm-op
	$1D col 9 right-margin - ?line
	."  \ "
	dup rot
	2dup - $10 u> abort" decompiler error"
	do i c@ h.2 loop
    endif
    dup to next-inst ;
    
: disasm-db
    cr ." db " count h. ;
: disasm-dw
    cr ." dw " W@+ h. ;
: disasm-dd
    cr ." dd " @+ h. ;
: disasm-ds
    cr ." string " $22 emit count 2dup type + $22 emit ;

: disasm  ( adr -- )
    begin
	dup
	cr inst
	key upc dup $1B = over [char] Q = or not
    while
	case
	    'B' of drop disasm-db endof
	    'W' of drop disasm-dw endof
	    'D' of drop disasm-dd endof
	    'S' of drop disasm-ds endof
	    rot drop
	endcase
    repeat
    2drop drop ;

\ create stopcode 8 C,                           \ count of length
\                 $AD C, $8B C, $0C C, $38 C,
\                 $03 C, $CF C, $FF C, $E1 C,

: next?         ( a1 -- f1 )
    next-seq count tuck compare 0= ;

: rest          ( adr -- )
    begin
	tabing-on 0tab
	cr
	dup next-seq c@ - next? 0=      \ NEXT, behind us?
    while
	dup next? if
	    $1D col $D right-margin - ?line
	    ." \ NEXT, MACRO"
	    cr
	endif
	inst
	tabing-off
	start/stop
    repeat
    drop ." END-CODE  "
    tabing-off ;

\ hidden

\ ' rest is discode       \ link into high level decompiler

\ decimal

\ forth definitions

: dismore ( -- )
    tabing-on 0tab
    next-inst
    cr inst
    rest ;

: disasm-dump ( addr n -- )
    over + swap
    begin
	2dup >
    while
	cr inst
    repeat
    2drop ;

\ only forth also definitions

?test $0002 [if]
cr ." Test for disasm.fs" cr

finish
[endif]

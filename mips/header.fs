\ header.fs
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

@s0 constant #ip
@s1 constant #sp
@s2 constant #rp
\ @s3 constant #fp
\ @s4 constant #lp
@s4 constant #cfa
\ @s6 constant #up
@v0 constant #tos
\ @s5 constant #tos
\ @s8 constant #ftos

$2 constant tos-#register

\ initialize freeable registers
\ $0300FFF8 constant regs-freeable-set
$0300FFFC
tos-#register 0 > [IF]
    tos-#register asm-bitmask #tos lshift invert and
[THEN]
constant regs-freeable-set

: (word-init) ( -- )
    here ih-size tuck + a,
    ['] compile,-native a,
    0 a, 0 a,
    word-good a,
    ['] compile,-nativext a,
    6cells ?do
	0 a,
    cell +loop ;

: (word-exit) ( -- )
    @ra jr,
    nop, ;

: (word-lwexit) ( -- )
    @ra 0 #rp lw, ;

: tos-load ( n to from -- )
    ?do
	i over #sp lw,
	cell+
    loop ;

: tos-store ( n to from -- )
    ?do
	i over #sp sw,
	cell+
    loop ;
    
?shared docode: [IF]

create docode:
    \ load top of stack elements
    tos-#register 0 > [IF]
	$0000 #tos tos-#register + #tos tos-load drop
    [THEN]
    #cfa 2cells over lw,
    #ip -1cells #rp sw,
    @ra #cfa jalr,
    #rp dup -1cells addiu,

    \ save top of stack elements (without tos)
    tos-#register 0 > [IF]
	$0000 #tos tos-#register + 1- #tos tos-store
    [THEN]
    #ip 0 #rp lw,
    #rp dup 1cells addiu,
    #cfa 0 #ip lw,
    ?word-mode-direct [IF]
	#ip dup 1cells addiu,
	#cfa jr,
    [ELSE]
	@t0 0 #cfa lw,
	#ip dup 1cells addiu,
	@t0 jr,
    [THEN]
    \ save tos element
    tos-#register 0 > [IF]
	#tos tos-#register + 1- swap #sp sw,
    [ELSE]
	nop,
    [THEN]

?trace $0001 [IF]
    ." docode:" lastxt here over - disasm-dump
[THEN]

create dodata:
    #cfa -1cells #sp sw,
    #cfa 2cells over lw,
    #sp dup -1cells addiu,
    #cfa jr,
    nop,
    
?trace $0001 [IF]
    ." dodata:" lastxt here over - disasm-dump
[THEN]

create dodoes:
    ?word-mode-indirect [IF]
	@ra 0 #cfa lw,
	nop,
	@ra dup 2cells addiu,
    [THEN]
    tos-#register 0 > [IF]
	#tos #cfa ih-cfsize addiu,
    [ELSE]
	#cfa dup ih-cfsize addiu,
	#cfa -1cells #sp sw,
    [THEN]
    \ load top of stack elements
    tos-#register 0 > [IF]
	$0000 #tos tos-#register + #tos 1+ tos-load drop
    [THEN]
    #sp dup -1cells addiu,
    #cfa 0 @ra lw,
    #ip -1cells #rp sw,
    @ra #cfa jalr,
    #rp dup -1cells addiu,

    \ save top of stack elements (without tos)
    tos-#register 0 > [IF]
	$0000 #tos tos-#register + 1- #tos tos-store
    [THEN]
    #ip 0 #rp lw,
    #rp dup 1cells addiu,
    #cfa 0 #ip lw,
    ?word-mode-direct [IF]
	#ip dup 1cells addiu,
	#cfa jr,
    [ELSE]
	@t0 0 #cfa lw,
	#ip dup 1cells addiu,
	@t0 jr,
    [THEN]
    \ save tos element
    tos-#register 0 > [IF]
	#tos tos-#register + 1- swap #sp sw,
    [ELSE]
	nop,
    [THEN]
    
?trace $0001 [IF]
    ." dodoes:" lastxt here over - disasm-dump
[THEN]

[THEN]

here docode: cfa, @
constant j,-docode:

?trace $0001 [IF]
    hex.s cr
    ." DOCODE:" docode: hex. cr
    ." DODATA:" dodata: hex. cr
    ." DODOES:" dodoes: hex. cr
[THEN]

?test $0002 [IF]
cr ." Test for header.fs" cr

finish
[THEN]

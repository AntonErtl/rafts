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
\ @s5 constant #tos
\ @s8 constant #ftos

@at constant #tos
\ @s5 constant #tos
@v0 constant #sos

\ initialize freeable registers
\ $0300FFF8 constant regs-freeable-set
$0300FFF8 constant regs-freeable-set

: (word-init) ( -- )
    here ih-size tuck + a,
    ['] compile,-native a,
    2cells ?do
	0 a,
    cell +loop ;

: (word-exit) ( -- )
    @ra jr,
    nop, ;

: (word-lwexit) ( -- )
    @ra 0 #rp lw, ;

?shared docode: [IF]

create docode:
    #tos 0 #sp lw,         \ load top of stack element
    #sos 1cells #sp lw,    \ load second of stack element
    #cfa 2cells over lw,
    #ip -1cells #rp sw,
    @ra #cfa jalr,
    #rp dup -1cells addiu,

    #sos 1cells #sp sw,    \ save second of stack element
    #tos 0 #sp sw,         \ save top of stack element
    #ip 0 #rp lw,
    #rp dup 1cells addiu,
    #cfa 0 #ip lw,
    #ip dup 1cells addiu,
    ?word-mode-direct [IF]
	#cfa jr,
    [THEN]
    ?word-mode-indirect [IF]
	@t0 0 #cfa lw,
	nop,
	@t0 jr,
    [THEN]
    nop,

?trace $0001 [IF]
    ." docode" lastxt here over - disasm-dump
[THEN]

create dodata:
    #tos #cfa @zero addiu,          \ load top of stack element
    #sos 0 #sp lw,                  \ load top of stack element0
    #cfa -1cells #sp sw,
    nop,
    #cfa 2cells over lw,
    #cfa jr,
    #sp dup -1cells addiu,
    
?trace $0001 [IF]
    ." dodata" lastxt here over - disasm-dump
[THEN]

create dodoes:
    ?word-mode-indirect [IF]
	@ra 0 #cfa lw,
	nop,
	@ra dup 2cells addiu,
    [THEN]
    #cfa dup ih-cfsize addiu,
    \ #cfa -1cells #sp sw,
    #tos #cfa @zero addiu,          \ load top of stack element
    #sos 0 #sp lw,                  \ load second of stack element
    #sp dup -1cells addiu,
    #cfa 0 @ra lw,
    #ip -1cells #rp sw,
    @ra #cfa jalr,
    #rp dup -1cells addiu,

    #sos 1cells #sp sw,    \ save second of stack element
    #tos 0 #sp sw,         \ save top of stack element
    #ip 0 #rp lw,
    #rp dup 1cells addiu,
    #cfa 0 #ip lw,
    #ip dup 1cells addiu,
    ?word-mode-direct [IF]
	#cfa jr,
    [THEN]
    ?word-mode-indirect [IF]
	@t0 0 #cfa lw,
	nop,
	@t0 jr,
    [THEN]
    nop,

?trace $0001 [IF]
    ." dodoes" lastxt here over - disasm-dump
[THEN]

[THEN]

\ hex.s cr
\ ." DOCODE:" docode: hex. cr
\ ." DODATA:" dodata: hex. cr
\ ." DODOES:" dodoes: hex. cr

?test $0002 [IF]
cr ." Test for header.fs" cr

finish
[THEN]

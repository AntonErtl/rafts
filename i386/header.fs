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

?shared docode: [IF]

create docode:
    #cfa 2cells over lw,
    #ip -1cells #rp sw,
    @ra #cfa jalr,
    #rp dup -1cells addiu,

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

create dodoes:
    @t0 0 #cfa lw,
    #cfa dup 2cells addiu,
    #cfa -1cells #sp sw,
    #sp dup -1cells addiu,
    ?word-mode-direct [IF]
	@t1 $03ffffff li,
	@t1 @t0 tuck and,
	@t0 dup $02 sll,
	@t1 $fc000000 li,
	@t1 #cfa tuck and,
	@t0 #cfa tuck or,
    [THEN]
    #cfa 2cells over lw,
    #ip -1cells #rp sw,
    @ra #cfa jalr,
    #rp dup -1cells addiu,

    #ip 0 #rp lw,
    #rp dup cells addiu,
    #cfa 0 #ip lw,
    #ip dup cells addiu,
    ?word-mode-direct [IF]
	#cfa jr,
    [THEN]
    ?word-mode-indirect [IF]
	@t0 0 #cfa lw,
	nop,
	@t0 jr,
    [THEN]
    nop,

[THEN]

: (word-init) ( -- )
    here ih-size tuck + a,
    ['] compile,-native a,
    2 ?do
	0 a,
    loop ;

: (word-exit) ( -- )
    @ra jr,
    nop, ;

: (word-lwexit) ( -- )
    @ra 0 #rp lw, ;

?test $0002 [IF]
cr ." Test for header.fs" cr

finish
[THEN]

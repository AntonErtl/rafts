\ control.fs	control structur words
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

>target-compile
: if ( flag -- ) ( C: -- orig )
    vtarget-compile vsource
    data> I_0BRANCH uop 0 over il-reg !
    NULL over il-val !
    inst-btrees-insert-end
    basic-exit
    here 2 cells - >control
    ?trace $0010 [IF]
	." if " .cs cr
    [THEN]
    basic-init ; immediate compile-only

: ahead ( -- ) ( C: -- orig )
    vtarget-compile vsource
    0 0 I_BRANCH terminal 0 over il-reg !
    NULL over il-val !
    inst-btrees-insert-end
    basic-exit
    here 2 cells - >control
    ?trace $0010 [IF]
	." ahead " .cs cr
    [THEN]
    basic-init ; immediate compile-only

: then ( -- ) ( C: orig -- )
    basic-exit
    ?trace $0010 [IF]
	." then " hex.s .cs cr
    [THEN]
    control>
    ?trace $0010 [IF]
	dup hex. dup hex? cr
    [THEN]
    dup here swap - cell- 2 rshift
    ?trace $0010 [IF]
	hex.s cr
	dup hex. cr
    [THEN]
    over @ $ffff0000 and or swap !
    basic-init ; immediate compile-only
lastxt alias endif immediate compile-only

: else ( -- ) ( C: orig1 -- orig2 )
    vtarget-compile
    postpone ahead 1 cs-roll postpone then
    vsource ; immediate compile-only

: begin ( -- ) ( C: -- dest )
    basic-exit
    here >control
    ?trace $0010 [IF]
	." begin " .cs cr
    [THEN]
    basic-init ; immediate compile-only

: again ( -- ) ( C: dest -- )
    ?trace $0010 [IF]
	." again " .cs cr
    [THEN]
    vtarget-compile vsource
    0 0 I_BRANCH terminal 0 over il-reg !
    control> over il-val ! inst-btrees-insert-end
    basic-exit
    basic-init ; immediate compile-only

: until ( flag -- ) ( C: dest -- )
    ?trace $0010 [IF]
	." until " .cs cr
    [THEN]
    vtarget-compile vsource
    data> I_0BRANCH uop 0 over il-reg !
    control> over il-val ! inst-btrees-insert-end
    basic-exit
    basic-init ; immediate compile-only

: while ( flag -- ) ( C: dest -- \rig dest )
    vtarget-compile
    postpone if 1 cs-roll
    vsource ; immediate compile-only

: repeat ( -- ) ( C: orig dest -- )
    vtarget-compile
    postpone again postpone then
    vsource ; immediate compile-only

: exit ( -- ) ( C: -- )
    check-ra
    basic-exit
    (word-exit)
    basic-init ; immediate compile-only

>source
$20 constant ls-size
variable ls-tos
0 ls-tos !
ls-size array ls-data

: >leave ( orig -- ) ( L: -- orig )
    ls-tos @ ls-data !
    1 ls-tos +! ;

: leave> ( -- orig ) ( L: orig -- )
    -1 ls-tos +!
    ls-tos @ ls-data @ ;
>target-compile

: leave ( -- ) ( C: -- ) ( L: -- orig )
    vtarget-compile
    postpone ahead
    vsource
    control> >leave ; immediate compile-only

: do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
    vtarget-compile
    postpone swap postpone >r postpone >r
    vsource
    0 >leave
    vtarget-compile
    postpone begin
    vsource ; immediate compile-only

: unloop ( -- ) ( C: -- ) ( L: -- ) ( R: to from -- )
    vtarget-compile
    postpone rdrop postpone rdrop
    vsource ; immediate compile-only

: loop ( -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- | 0 destu ... dest0 ) ( R: to from -- | to from )
    vtarget-compile
    postpone r> postpone 1+ postpone dup postpone r@ postpone = postpone swap postpone >r postpone until
    vsource
    begin
	leave> dup 0<>
    while
	>control
	vtarget-compile
	postpone then
	vsource
    repeat
    drop
    vtarget-compile
    postpone unloop
    vsource ; immediate compile-only

: ?do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
    vtarget-compile
    postpone swap postpone >r postpone >r
    vsource
    0 >leave
    vtarget-compile
    postpone 2r@ postpone = postpone if
    postpone leave postpone endif
    postpone begin
    vsource ; immediate compile-only

: +loop ( n -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- | 0 destu ... dest0 ) ( R: to from -- | to from )
    vtarget-compile
    postpone dup postpone r> postpone dup postpone r@ postpone - postpone swap postpone rot postpone + postpone >r
    postpone 2dup postpone tuck postpone + postpone xor postpone 0<=
    postpone rot postpone rot postpone xor postpone 0<= postpone and postpone until
    vsource
    begin
	leave> dup 0<>
    while
	>control
	vtarget-compile
	postpone then
	vsource
    repeat
    drop
    vtarget-compile
    postpone unloop
    vsource ; immediate compile-only

: i ( -- n ) ( C: -- ) ( L: -- ) ( R: to from -- to from )
    vtarget-compile
    postpone r@
    vsource ; immediate compile-only

: i' ( -- n ) ( C: -- ) ( L: -- ) ( R: to from -- to from )
    vtarget-compile
    postpone r> postpone r@ postpone swap postpone >r
    vsource ; immediate compile-only

: j ( -- n ) ( C: -- ) ( L: -- ) ( R: to1 from1 to0 from0 -- to1 from1 to0 from0 )
    vtarget-compile
    postpone r> postpone r> postpone r@ postpone swap postpone >r postpone swap postpone >r
    vsource ; immediate compile-only

: for ( count -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- 0 from )
    vtarget-compile
    0 postpone literal postpone swap postpone ?do
    vsource ; immediate compile-only

: next ( -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- | 0 destu ... dest0 ) ( R: 0 from -- | 0 from )
    vtarget-compile
    -1 postpone literal postpone +loop
    vsource ; immediate compile-only

: case ( -- ) ( C: -- 0 )
    0 >control ; immediate compile-only

: of ( x1 x2 -- | x1 ) ( C: u -- orig u+1 )
    control> 1+ >r
    ?trace $0010 [IF]
	." of:" hex.s cr
    [THEN]
    vtarget-compile
    postpone over postpone = postpone if postpone drop
    vsource
    r> >control ; immediate compile-only

: endof ( -- ) ( C: orig1 u -- orig2 u )
    control> >r
    vtarget-compile
    postpone else
    vsource
    r> >control ; immediate compile-only

: endcase ( x -- ) ( C: destu ... dest0 u -- )
    vtarget-compile
    postpone drop
    vsource
    control> 0 ?do
	vtarget-compile
	postpone then
	vsource
    loop ; immediate compile-only

: recurse ( -- )
    basic-exit basic-init lastxt compile, ; immediate compile-only

: postpone ( "name" -- )
    ?trace $0010 [IF]
	." POSTPONE:" order cr
    [THEN]
    name sfind case
	2 of
	?trace $0010 [IF]
	    ." postpone (compile-only&immediate):" dup hex. dup >code-address hex. cr
	[THEN]
	compile, endof
	-2 of
	?trace $0010 [IF]
	    ." postpone (compile-only):" dup hex. dup >code-address hex. cr
	[THEN]
	vtarget-compile postpone literal vsource ['] compile, compile, endof
	1 of
	?trace $0010 [IF]
	    ." postpone (immediate):" dup hex. dup >code-address hex. cr
	[THEN]
	compile, endof
	-1 of
	?trace $0010 [IF]
	    ." postpone:" dup hex. dup >code-address hex. cr
	[THEN]
	vtarget-compile postpone literal vsource ['] compile, compile, endof
	0 of
	-13 throw endof
    endcase ; immediate compile-only
>source

: !does ( addr -- )
    ?word-mode-direct [IF]
	2 rshift $1a @mask and $08000000 or
    [THEN]
    lastcfa @ !
    here last @ tuck - flush-icache ;

: dodoes, ( -- )
    dodoes cfa, ;

: ;dodoes ( -- )
    r@ 4 cells +
    dup cell- @ 0= over cell+ @ 0= and invert if
	?word-mode-direct [IF]
	    1
	[THEN]
	?word-mode-indirect [IF]
	    2
	[THEN]
	cells +
    endif
    !does ;
>target-compile

: does> ( -- )
    state @ if
	['] ;dodoes compile,
	check-ra
	basic-exit
	(word-exit)
	dodoes,
    else
	here !does dodoes,
	vtarget ] vsource
    endif
    (word-init)
    basic-init
    0 @ra I_REG terminal >return ; immediate compile-only
\ !! make it visible in target to get interpretation semantics?
>source

?test $0010 [IF]
cr ." Test for control.fs" cr

finish
[THEN]

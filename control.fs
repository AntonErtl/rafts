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

: compile-forward-branch ( il -- )
    0 over il-reg !
    NULL over il-val !
    inst-btrees-insert-end
    basic-exit
    branch-info 2@ drop >control
    ?trace $0010 [IF]
	." forward branch " .cs cr
    [THEN]
    basic-init ;

: compile,-if ( flag -- ) ( C: -- orig )
    data> I_0BRANCH uop
    compile-forward-branch ;

: compile,-ahead ( -- ) ( C: -- orig )
    0 0 I_BRANCH terminal
    compile-forward-branch ;

: compile,-then ( -- ) ( C: orig -- )
    basic-exit
    ?trace $0010 [IF]
	." then " hex.s .cs cr
    [THEN]
    here control> back-patch-beq
    basic-init ;
>target

: if ( flag -- )
    ['] compile,-if gforth-compile, ; immediate compile-only

: ahead ( -- )
    ['] compile,-ahead gforth-compile, ; immediate compile-only

: then ( -- )
    ['] compile,-then gforth-compile, ; immediate compile-only

: endif ( -- )
    vtarget
    postpone then
    vsource ; immediate compile-only

: else ( -- )
    vtarget
    postpone ahead
    1 cs-roll
    postpone then
    vsource ; immediate compile-only

>source
: compile,-begin ( -- ) ( C: -- dest )
    basic-exit
    here >control
    ?trace $0010 [IF]
	." begin " .cs cr
    [THEN]
    basic-init ;

: compile,-again ( -- ) ( C: dest -- )
    ?trace $0010 [IF]
	." again " .cs cr
    [THEN]
    0 0 I_BRANCH terminal 0 over il-reg !
    control> over il-val ! inst-btrees-insert-end
    basic-exit
    basic-init ;

: compile,-until ( flag -- ) ( C: dest -- )
    ?trace $0010 [IF]
	." until " .cs cr
    [THEN]
    data> I_0BRANCH uop 0 over il-reg !
    control> over il-val ! inst-btrees-insert-end
    basic-exit
    basic-init ;
>target

: begin ( -- )
    ['] compile,-begin gforth-compile, ; immediate compile-only

: again ( -- )
    ['] compile,-again gforth-compile, ; immediate compile-only

: until ( flag -- )
    ['] compile,-until gforth-compile, ; immediate compile-only

: while ( flag -- ) ( C: dest -- \rig dest )
    vtarget
    postpone if 1 cs-roll
    vsource ; immediate compile-only

: repeat ( -- ) ( C: orig dest -- )
    vtarget
    postpone again postpone then
    vsource ; immediate compile-only

>source
: compile,-exit ( -- ) ( C: -- )
    check-ra
    basic-exit
    (word-exit)
    basic-init ;
>target

: exit ( -- )
    ['] compile,-exit gforth-compile, ; immediate compile-only

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

: compile,-leave ( -- ) ( C: -- ) ( L: -- orig )
    control> >leave ;

: compile,-leave-init ( -- )
    0 >leave ;

: compile,-leave-exit ( -- )
    begin
	leave> dup 0<>
    while
	>control
	compile,-then
    repeat
    drop ;
>target

: leave ( -- ) ( C: -- ) ( L: -- orig )
    vtarget
    postpone ahead
    vsource
    ['] compile,-leave gforth-compile, ; immediate compile-only

: do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
    vtarget
    postpone swap postpone >r postpone >r
    vsource
    ['] compile,-leave-init gforth-compile,
    vtarget
    postpone begin
    vsource ; immediate compile-only

: unloop ( -- ) ( C: -- ) ( L: -- ) ( R: to from -- )
    vtarget
    postpone rdrop postpone rdrop
    vsource ; immediate compile-only

: loop ( -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- ) ( R: to from -- )
    vtarget
    postpone r> 1 postpone literal postpone + postpone dup postpone r@ postpone = postpone swap postpone >r postpone until
    vsource
    ['] compile,-leave-exit gforth-compile,
    vtarget
    postpone unloop
    vsource ; immediate compile-only

: ?do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
    vtarget
    postpone over postpone >r postpone dup postpone >r
    vsource
    ['] compile,-leave-init gforth-compile,
    vtarget
    postpone = postpone if
    postpone leave postpone then
    postpone begin
    vsource ; immediate compile-only

: +loop ( n -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- ) ( R: to from -- )
    vtarget
    postpone dup postpone r> postpone dup postpone r@ postpone - postpone swap postpone rot postpone + postpone >r
    postpone over postpone over postpone swap postpone over postpone + postpone xor postpone 0<=
    postpone rot postpone rot postpone xor postpone 0<= postpone and postpone until
    vsource
    ['] compile,-leave-exit gforth-compile,
    vtarget
    postpone unloop
    vsource ; immediate compile-only

: i ( -- n ) ( C: -- ) ( L: -- ) ( R: to from -- to from )
    vtarget
    postpone r@
    vsource ; immediate compile-only

: i' ( -- n ) ( C: -- ) ( L: -- ) ( R: to from -- to from )
    vtarget
    postpone r> postpone r@ postpone swap postpone >r
    vsource ; immediate compile-only

: j ( -- n ) ( C: -- ) ( L: -- ) ( R: to1 from1 to0 from0 -- to1 from1 to0 from0 )
    vtarget
    postpone r> postpone r> postpone r@ postpone swap postpone >r postpone swap postpone >r
    vsource ; immediate compile-only

: case ( -- ) ( C: -- 0 )
    0 >control ; immediate compile-only

: of ( x1 x2 -- | x1 ) ( C: u -- orig u+1 )
    control> 1+ >r
    ?trace $0010 [IF]
	." of:" hex.s cr
    [THEN]
    vtarget
    postpone over postpone = postpone if postpone drop
    vsource
    r> >control ; immediate compile-only

: endof ( -- ) ( C: orig1 u -- orig2 u )
    control> >r
    vtarget
    postpone else
    vsource
    r> >control ; immediate compile-only

: endcase ( x -- ) ( C: destu ... dest0 u -- )
    vtarget
    postpone drop
    vsource
    control> 0 ?do
	vtarget
	postpone then
	vsource
    loop ; immediate compile-only

>source
: compile,-recurse ( -- )
    basic-exit basic-init
    lastxt info-cfhead-size - imm-compile, ;
>target

: recurse ( -- )
    ['] compile,-recurse gforth-compile, ; immediate compile-only

>source
$200 constant does-size
create does-addr
    cell a,
    does-size cells allot

: does-addr-inc ( -- )
    cell does-addr +! ;

: !does ( addr -- )
    ?word-mode-direct [IF]
	2 rshift $1a asm-bitmask and $08000000 or
    [THEN]
    lastcfa @ tuck !
    3cells + ['] compile,-does swap !
    here last @ tuck - flush-icache ;

: dodoes, ( -- )
    dodoes:
    ?word-mode-direct [IF]
	jal,
    [ELSE]
	j,
    [THEN]
    nop, ;

: ;dodoes ( n -- )
    does-addr + @ !does ;
>target

also vtarget :word ;dodoes previous

>source
: compile,-does> ( -- )
    does-addr @ compile,-literal
    vtarget ['] ;dodoes vsource imm-compile,
    compile,-word-exit-check
    here does-addr dup @ + ! does-addr-inc
    dodoes,
    compile,-word-init-check ;
>target

: does> ( -- )
    ['] compile,-does> gforth-compile, ; immediate compile-only

>source

: postpone, ( w xt -- )
    dup ['] execute = if
	drop compile,
    else
	swap postpone literal compile,
    endif ;

?test $0010 [IF]
cr ." Test for control.fs" cr

finish
[THEN]

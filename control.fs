\ control.fs	control structur words
\
\ Copyright (C) 1995-97 Martin Anton Ertl, Christian Pirker
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

variable dead-code
dead-code off

: unreachable ( -- )
    dead-code on ;

: assume-live ( -- )
    dead-code off ;

: compile-forward-branch ( il -- )
    0 over il-reg !
    NULL over il-val !
    inst-ils-insert
    basic-exit
    branch-info 2@ drop 0 0 >control
    ?trace $0010 [IF]
	." forward branch " .cs cr
    [THEN]
    basic-init ;

: compile-if ( flag -- ) ( C: -- orig )
    data> I_0BRANCH uop
    compile-forward-branch ;

: intermediate-if ( flag -- )
    0 -1 word-good word-regs-adjust
    word-regs-get >control
    ?trace $0010 [IF]
	." IF: "
	word-regs-print .cs cr
    [THEN]
    ;

: compile-ahead ( -- ) ( C: -- orig )
    0 0 I_BRANCH terminal
    compile-forward-branch ;

: intermediate-ahead ( -- )
    word-regs-get >control
    unreachable
    ?trace $0010 [IF]
	." AHEAD: "
	word-regs-print .cs cr
    [THEN]
    ;

: compile-then ( -- ) ( C: orig -- )
    basic-exit
    ?trace $0010 [IF]
	." then " hex.s .cs cr
    [THEN]
    here control> 2drop back-patch-beq
    basic-init ;

: intermediate-then ( -- )
    control>
    dead-code @ if
	?trace $0010 [IF]
	    ." UNREACHABLE (then):" cr
	[THEN]
	dead-code off
    else
	>r word-regs-get >r
	2over 2over + rot rot +
	?trace $0010 [IF]
	    ." REACHABLE (then):"
	    2dup . . cr
	[THEN]
	<> if
	    ?trace $0010 [IF]
		." stackdepth different (then) !!!" cr
	    [THEN]
	endif
	word-regs-max
	2r> or
    endif
    word-regs-put
    ?trace $0010 [IF]
	." THEN: "
	word-regs-print .cs cr
    [THEN]
    ;
>target

: if ( flag -- )
    intermediate-if
    ['] compile-if gforth-compile, ; immediate compile-only

: ahead ( -- )
    intermediate-ahead
    ['] compile-ahead gforth-compile, ; immediate compile-only

: then ( -- )
    intermediate-then
    ['] compile-then gforth-compile, ; immediate compile-only

: endif ( -- )
    vtarget
    postpone then
    vsource ; immediate compile-only

: else ( -- )
    vtarget
    postpone ahead 1 cs-roll postpone then
    vsource ; immediate compile-only

>source
: compile-begin ( -- ) ( C: -- dest )
    basic-exit
    here 0 0 >control
    ?trace $0010 [IF]
	." begin " .cs cr
    [THEN]
    basic-init ;

: intermediate-begin ( -- )
    word-regs-get >control
    ?trace $0010 [IF]
	." BEGIN: "
	word-regs-print .cs cr
    [THEN]
    ;

: compile-again ( -- ) ( C: dest -- )
    ?trace $0010 [IF]
	." again " .cs cr
    [THEN]
    0 0 I_BRANCH terminal 0 over il-reg !
    control> 2drop over il-val ! inst-ils-insert
    basic-exit
    basic-init ;

: intermediate-again ( -- )
    control>
    dead-code @ if
	?trace $0010 [IF]
	    ." UNREACHABLE (again):" cr
	[THEN]
	dead-code off
    else
	>r word-regs-get >r
	2over 2over + rot rot +
	?trace $0010 [IF]
	    ." REACHABLE (again):"
	    2dup . . cr
	[THEN]
	<> if
	    ?trace $0010 [IF]
		." stackdepth different (again) !!!" cr
	    [THEN]
	endif
	word-regs-max
	2r> or
    endif
    word-regs-put
    ?trace $0010 [IF]
	." AGAIN: "
	word-regs-print .cs cr
    [THEN]
    unreachable
    ;

: compile-until ( flag -- ) ( C: dest -- )
    ?trace $0010 [IF]
	." until " .cs cr
    [THEN]
    data> I_0BRANCH uop 0 over il-reg !
    control> 2drop over il-val ! inst-ils-insert
    basic-exit
    basic-init ;

: intermediate-until ( flag -- )
    0 -1 word-good word-regs-adjust
    control>
    dead-code @ if
	?trace $0010 [IF]
	    ." UNREACHABLE (until):" cr
	[THEN]
	dead-code off
    else
	>r word-regs-get >r
	2over 2over + rot rot +
	?trace $0010 [IF]
	    ." REACHABLE (until):"
	    2dup . . cr
	[THEN]
	<> if
	    ?trace $0010 [IF]
		." stackdepth different (until) !!!" cr
	    [THEN]
	endif
	word-regs-max
	2r> or
    endif
    word-regs-put
    ?trace $0010 [IF]
	." UNTIL: "
	word-regs-print .cs cr
    [THEN]
    ;
>target

: begin ( -- )
    intermediate-begin
    ['] compile-begin gforth-compile, ; immediate compile-only

: again ( -- )
    intermediate-again
    ['] compile-again gforth-compile, ; immediate compile-only

: until ( flag -- )
    intermediate-until
    ['] compile-until gforth-compile, ; immediate compile-only

: while ( flag -- ) ( C: dest -- \rig dest )
    ?trace $0010 [IF]
	." WHILE: "
	word-regs-print .cs cr
    [THEN]
    vtarget
    postpone if 1 cs-roll
    vsource ; immediate compile-only

: repeat ( -- ) ( C: orig dest -- )
    ?trace $0010 [IF]
	." REPEAT: "
	word-regs-print .cs cr
    [THEN]
    vtarget
    postpone again postpone then
    vsource ; immediate compile-only

>source
: compile-exit ( -- ) ( C: -- )
    check-ra
    basic-exit
    (word-exit)
    basic-init ;
>target

: exit ( -- )
    unreachable
    ['] compile-exit gforth-compile, ; immediate compile-only

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

: compile-leave ( -- ) ( C: -- ) ( L: -- orig )
    control> 2drop >leave ;

: compile-leave-init ( -- )
    0 >leave ;

: intermediate-leave-init ( -- )
    0 >leave ;

: compile-leave-exit ( -- )
    begin
	leave> dup
    while
	0 0 >control
	compile-then
    repeat
    drop ;

: intermediate-leave-exit ( -- )
    begin
	leave> dup
    while
	drop
	intermediate-then
    repeat
    drop ;
>target

: leave ( -- ) ( C: -- ) ( L: -- orig )
    here >leave
    vtarget
    postpone ahead
    vsource
    ['] compile-leave gforth-compile, ; immediate compile-only

: leave-init ( -- )
    intermediate-leave-init
    ['] compile-leave-init gforth-compile, ; immediate compile-only

: leave-exit ( -- )
    intermediate-leave-exit
    ['] compile-leave-exit gforth-compile, ; immediate compile-only

: do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
    vtarget
    postpone swap postpone >r postpone >r
    postpone leave-init
    postpone begin
    vsource ; immediate compile-only

: unloop ( -- ) ( C: -- ) ( L: -- ) ( R: to from -- )
    vtarget
    postpone rdrop postpone rdrop
    vsource ; immediate compile-only

: loop ( -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- ) ( R: to from -- )
    vtarget
    postpone r> 1 postpone literal postpone + postpone dup postpone r@ postpone = postpone swap postpone >r postpone until
    postpone leave-exit
    postpone unloop
    vsource ; immediate compile-only

: ?do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
    vtarget
    postpone over postpone >r postpone dup postpone >r
    postpone leave-init
    postpone = postpone if
    postpone leave postpone then
    postpone begin
    vsource ; immediate compile-only

: +loop ( n -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- ) ( R: to from -- )
    vtarget
    postpone dup postpone r> postpone dup postpone r@ postpone - postpone swap postpone rot postpone + postpone >r
    postpone over postpone over postpone swap postpone over postpone + postpone xor
    postpone rot postpone rot postpone xor postpone and postpone 0< postpone until
    postpone leave-exit
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
    0 0 0 >control ; immediate compile-only

: of ( x1 x2 -- | x1 ) ( C: u -- orig u+1 )
    control> rot 1+ rot rot 2>r >r
    ?trace $0010 [IF]
	." of:" hex.s cr
    [THEN]
    vtarget
    postpone over postpone = postpone if postpone drop
    vsource
    r> 2r> >control ; immediate compile-only

: endof ( -- ) ( C: orig1 u -- orig2 u )
    control> 2>r >r
    vtarget
    postpone else
    vsource
    r> 2r> >control ; immediate compile-only

: endcase ( x -- ) ( C: destu ... dest0 u -- )
    vtarget
    postpone drop
    vsource
    control> 2drop 0 ?do
	vtarget
	postpone then
	vsource
    loop ; immediate compile-only

>source
: compile-recurse ( -- )
    basic-code-ptr @ NIL I_CALL terminal inst-ils-insert
    basic-exit
    basic-init
;
>target

: recurse ( -- )
    ['] compile-recurse gforth-compile, ; immediate compile-only

>source
$200 constant does-size
create does-addr
    cell a,
    does-size cells allot

: does-addr-inc ( -- )
    cell does-addr +! ;

: !does ( addr -- )
    word-regs-init dup word-regs-read lastih word-regs-write
    ?word-mode-direct [IF]
	j,-docode:
    [THEN]
    \ insert comments !!!
    lastxt tuck !
    \ check later !!!
    \ ['] compile,-nonative-xt over ih-compile-xt !
    over swap ih-does-xt !
    here basic-code-sav !
    basic-code-ptr @ dp !
    basic-init
    lastxt ih-cfsize + compile,-literal
    ih-cfsize + 0 I_BRANCH terminal inst-ils-insert
    basic-exit
    basic-code-ptr @ dup here over - flush-icache
    here basic-code-ptr !
    basic-code-sav @ dp !
    lastxt 2cells + !
    ['] compile,-does lastxt 3cells + !
    here lastxt tuck - flush-icache ;

: dodoes, ( -- )
    dodoes:
    ?word-mode-direct [IF]
	jal,
    [ELSE]
	j,
    [THEN]
    nop, ;

: ;dodoes ( n -- )
    does-addr + @
    !does ;
>target

also vtarget
word-good 0 0 :word ;dodoes
previous

>source
: compile-does> ( -- )
    does-addr @
    compile,-literal
    vtarget ['] ;dodoes vsource compile,-now
    compile-word-exit-does
    here does-addr dup @ + ! does-addr-inc
    lastih-init
    dodoes,
    compile-word-init-does ;
>target

: does> ( -- )
    lastih word-regs-write
    word-regs-init
    1 0 word-good word-regs-adjust
    ['] compile-does> gforth-compile, ; immediate compile-only

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

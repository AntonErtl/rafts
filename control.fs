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

>target_compile
: if ( flag -- ) ( C: -- orig )
  vtarget_compile vsource
  data> I_0BRANCH uop 0 over node_reg ! true over node_delay ! NULL over node_val ! inst_btrees_insert_end
  basic_exit
  here 2 cells - >control
?trace $0010 [IF]
  ." if " .cs cr
[THEN]
  basic_init ; immediate restrict

: ahead ( -- ) ( C: -- orig )
  NULL vtarget_compile postpone literal vsource data>
  I_0BRANCH uop 0 over node_reg ! true over node_delay ! NULL over node_val ! inst_btrees_insert_end
  basic_exit
  here 2 cells - >control
?trace $0010 [IF]
  ." ahead " .cs cr
[THEN]
  basic_init ; immediate restrict

: then ( -- ) ( C: orig -- )
  basic_exit
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
  basic_init ; immediate restrict
lastxt alias endif immediate restrict

: else ( -- ) ( C: orig1 -- orig2 )
  vtarget_compile
  postpone ahead 1 cs-roll postpone then
  vsource ; immediate restrict

: begin ( -- ) ( C: -- dest )
  basic_exit
  here >control
?trace $0010 [IF]
  ." begin " .cs cr
[THEN]
  basic_init ; immediate restrict

: again ( -- ) ( C: dest -- )
?trace $0010 [IF]
  ." again " .cs cr
[THEN]
  NULL vtarget_compile postpone literal vsource data>
  I_0BRANCH uop 0 over node_reg ! true over node_delay ! control> over node_val ! inst_btrees_insert_end
  basic_exit
  basic_init ; immediate restrict

: until ( flag -- ) ( C: dest -- )
?trace $0010 [IF]
  ." until " .cs cr
[THEN]
  vtarget_compile vsource
  data> I_0BRANCH uop 0 over node_reg ! true over node_delay ! control> over node_val ! inst_btrees_insert_end
  basic_exit
  basic_init ; immediate restrict

: while ( flag -- ) ( C: dest -- \rig dest )
  vtarget_compile
  postpone if 1 cs-roll
  vsource ; immediate restrict

: repeat ( -- ) ( C: orig dest -- )
  vtarget_compile
  postpone again postpone then
  vsource ; immediate restrict

: exit ( -- ) ( C: -- )
  check-ra
  basic_exit
  (func_exit)
  basic_init ; immediate restrict

>source
$20 constant ls_size
variable ls_tos
0 ls_tos !
ls_size array ls_data

: >leave ( orig -- ) ( L: -- orig )
  ls_tos @ ls_data !
  1 ls_tos +! ;

: leave> ( -- orig ) ( L: orig -- )
  -1 ls_tos +!
  ls_tos @ ls_data @ ;
>target_compile

: leave ( -- ) ( C: -- ) ( L: -- orig )
  vtarget_compile
  postpone ahead
  vsource
  control> >leave ; immediate restrict

: do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
  vtarget_compile
  postpone swap postpone >r postpone >r
  vsource
  0 >leave
  vtarget_compile
  postpone begin
  vsource ; immediate restrict

: unloop ( -- ) ( C: -- ) ( L: -- ) ( R: to from -- )
  vtarget_compile
  postpone rdrop postpone rdrop
  vsource ; immediate restrict

: loop ( -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- | 0 destu ... dest0 ) ( R: to from -- | to from )
  vtarget_compile
  postpone r> postpone 1+ postpone dup postpone r@ postpone = postpone swap postpone >r postpone until
  vsource
  begin
    leave> dup 0<> while
    >control
    vtarget_compile
    postpone then
    vsource
    repeat
  drop
  vtarget_compile
  postpone unloop
  vsource ; immediate restrict

: ?do ( to from -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- to from )
  vtarget_compile
  postpone swap postpone >r postpone >r
  vsource
  0 >leave
  vtarget_compile
  postpone 2r@ postpone = postpone if
    postpone leave postpone endif
  postpone begin
  vsource ; immediate restrict

: +loop ( n -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- | 0 destu ... dest0 ) ( R: to from -- | to from )
  vtarget_compile
  postpone dup postpone r> postpone dup postpone r@ postpone - postpone swap postpone rot postpone + postpone >r
  postpone 2dup postpone tuck postpone + postpone xor postpone 0<=
  postpone rot postpone rot postpone xor postpone 0<= postpone and postpone until
  vsource
  begin
    leave> dup 0<> while
    >control
    vtarget_compile
    postpone then
    vsource
    repeat
  drop
  vtarget_compile
  postpone unloop
  vsource ; immediate restrict

: i ( -- n ) ( C: -- ) ( L: -- ) ( R: to from -- to from )
  vtarget_compile
  postpone r@
  vsource ; immediate restrict

: i' ( -- n ) ( C: -- ) ( L: -- ) ( R: to from -- to from )
  vtarget_compile
  postpone r> postpone r@ postpone swap postpone >r
  vsource ; immediate restrict

: j ( -- n ) ( C: -- ) ( L: -- ) ( R: to1 from1 to0 from0 -- to1 from1 to0 from0 )
  vtarget_compile
  postpone r> postpone r> postpone r@ postpone swap postpone >r postpone swap postpone >r
  vsource ; immediate restrict

: for ( count -- ) ( C: -- dest ) ( L: -- 0 ) ( R: -- 0 from )
  vtarget_compile
  0 postpone literal postpone swap postpone ?do
  vsource ; immediate restrict

: next ( -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- | 0 destu ... dest0 ) ( R: 0 from -- | 0 from )
  vtarget_compile
  -1 postpone literal postpone +loop
  vsource ; immediate restrict

: case ( -- ) ( C: -- 0 )
  0 >control ; immediate restrict

: of ( x1 x2 -- | x1 ) ( C: u -- orig u+1 )
  control> 1+ >r
?trace $0010 [IF]
  ." of:" hex.s cr
[THEN]
  vtarget_compile
  postpone over postpone = postpone if postpone drop
  vsource
  r> >control ; immediate restrict

: endof ( -- ) ( C: orig1 u -- orig2 u )
  control> >r
  vtarget_compile
  postpone else
  vsource
  r> >control ; immediate restrict

: endcase ( x -- ) ( C: destu ... dest0 u -- )
  vtarget_compile
  postpone drop
  vsource
  control> 0 ?do
    vtarget_compile
    postpone then
    vsource
    loop ; immediate restrict

: recurse ( -- )
  basic_exit basic_init lastxt compile, ; immediate restrict

: postpone ( "name" -- )
?trace $0010 [IF]
  ." POSTPONE:" order cr
[THEN]
  name sfind case
    2 of
?trace $0010 [IF]
      ." postpone (restrict&immediate):" dup hex. dup >code-address hex. cr
[THEN]
      compile, endof
    -2 of
?trace $0010 [IF]
      ." postpone (restrict):" dup hex. dup >code-address hex. cr
[THEN]
      vtarget_compile postpone literal vsource ['] compile, compile, endof
    1 of
?trace $0010 [IF]
      ." postpone (immediate):" dup hex. dup >code-address hex. cr
[THEN]
      compile, endof
    -1 of
?trace $0010 [IF]
      ." postpone:" dup hex. dup >code-address hex. cr
[THEN]
      vtarget_compile postpone literal vsource ['] compile, compile, endof
    0 of
      -13 throw endof
  endcase ; immediate restrict
>source

: !does ( addr -- )
?func_mode_direct [IF]
  2 rshift $1a @mask and $08000000 or
[THEN]
  lastcfa @ !
  here last @ tuck - flush-icache ;

: dodoes, ( -- )
  dodoes cfa, ;

: ;dodoes ( -- )
  r@ 4 cells +
  dup cell- @ 0= over cell+ @ 0= and invert if
?func_mode_direct [IF]
    1
[THEN]
?func_mode_indirect [IF]
    2
[THEN]
    cells + endif
  !does ;
>target_compile

: does> ( -- )
  state @ if
    ['] ;dodoes compile,
    check-ra
    basic_exit
    (func_exit)
    dodoes, else
    here !does dodoes,
    vtarget ] vsource endif
  (func_init)
  basic_init
  0 @ra I_REG terminal dup inst_done >return ; immediate restrict
\ !! make it visible in target to get interpretation semantics?
>source

?test $0010 [IF]
cr ." Test for control.fs" cr

finish
[THEN]

\ $Id: control.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: control.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

\ control structures
: @if ( -- )
  @t0 0 #sp @lw
  #sp dup 4 @addiu drop
  @zero $0000 @beq
  @nop ;

: @ahead ( -- )
  @zero @zero $0000 @beq
  @nop ;

: @again ( -- )
  @zero @zero rot @beq
  @nop ;

: @until ( -- )
  @t0 0 #sp @lw
  #sp dup 4 @addiu drop
  @zero rot @beq
  @nop ;

>target_compile
: if ( flag -- ) ( C: -- orig )
  basic_exit
  @if
  here 2 cells - >control
?trace $0010 [IF]
  ." if " .cs cr
[THEN]
  basic_init ; immediate restrict

: ahead ( -- ) ( C: -- orig )
  basic_exit
  @ahead
  here 2 cells - >control
?trace $0010 [IF]
  ." ahead " .cs cr
[THEN]
  basic_init ; immediate restrict

: then ( -- ) ( C: orig -- )
  basic_exit
?trace $0010 [IF]
  ." then " .s .cs cr
[THEN]
  control>
?trace $0010 [IF]
  dup hex. dup @ hex. cr
[THEN]
  dup here swap - cell- 2 rshift
?trace $0010 [IF]
  dup hex. cr
[THEN]
  over @ or swap !
  basic_init ; immediate restrict
vtarget_compile ' then alias endif immediate restrict vsource

: else ( -- ) ( C: orig1 -- orig2 )
  vtarget_compile
  postpone ahead 1 postpone cs-roll postpone then
  vsource ; immediate restrict

: begin ( -- ) ( C: -- dest )
  basic_exit
  here >control
?trace $0010 [IF]
  ." begin " .cs cr
[THEN]
  basic_init ; immediate restrict

: again ( -- ) ( C: dest -- )
  basic_exit
?trace $0010 [IF]
  ." again " .cs cr
[THEN]
  control> here - 1 cells -
?trace $0010 [IF]
  dup hex. cr
[THEN]
  @again
  basic_init ; immediate restrict

: until ( flag -- ) ( C: dest -- )
  basic_exit
?trace $0010 [IF]
  ." until " .cs cr
[THEN]
  control> here - 3 cells -
?trace $0010 [IF]
  dup hex. cr
[THEN]
  @until
  basic_init ; immediate restrict

: while ( flag -- ) ( C: dest -- orig dest )
  vtarget_compile
  postpone if 1 postpone cs-roll
  vsource ; immediate restrict

: repeat ( -- ) ( C: orig dest -- )
  vtarget_compile
  postpone again postpone then
  vsource ; immediate restrict

: exit ( -- ) ( C: -- )
  basic_exit
  @exit
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
  0 postpone lit postpone swap postpone do
  vsource ; immediate restrict

: next ( -- ) ( C: dest -- ) ( L: 0 destu ... dest0 -- | 0 destu ... dest0 ) ( R: 0 from -- | 0 from )
  vtarget_compile
  -1 postpone lit postpone +loop
  vsource ; immediate restrict

: case ( -- ) ( C: -- 0 )
  0 >control ; immediate restrict

: of ( x1 x2 -- | x1 ) ( C: u -- orig u+1 )
  control> 1+ >r
?trace $0010 [IF]
  ." of:" .s cr
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
  basic_exit
  lastxt compile,
  basic_init ; immediate restrict

: postpone ( "name" -- )
?trace $0010 [IF]
  ." POSTPONE:" order cr
[THEN]
  name sfind case
    2 of
?trace $0010 [IF]
      ." postpone (restrict&immediate):" dup hex. dup get_do hex. cr
[THEN]
      ?compile endof
    -2 of
?trace $0010 [IF]
      ." postpone (restrict):" dup hex. dup get_do hex. cr
[THEN]
      postpone lit basic_exit
      ['] compile, compile,
      basic_init endof
    1 of
?trace $0010 [IF]
      ." postpone (immediate):" dup hex. dup get_do hex. cr
[THEN]
      ?compile endof
    -1 of
?trace $0010 [IF]
      ." postpone:" dup hex. dup get_do hex. cr
[THEN]
      postpone lit basic_exit
      ['] compile, compile,
      basic_init endof
    0 of
      notfound endof
  endcase ; immediate restrict
>source

: !does ( addr -- )
?func_mode_direct [IF]
  2 rshift $1a @mask and $08000000 or
[THEN]
  lastcfa @ !
  here last @ tuck - cacheflush ;

: dodoes, ( -- )
  :dodoes a, 0 a, ;

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
    basic_exit
    ['] ;dodoes compile,
    @exit
    dodoes, else
    here !does dodoes,
    vtarget ] vsource endif
  @init
  basic_init ; immediate restrict
vtarget_compile ' does>
>source
vsource alias does>

?test $0010 [IF]
cr ." Test for control.fs" cr

finish
[THEN]

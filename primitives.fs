\ $Id: primitives.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: primitives.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

: ['Forth] ( "name" -- )
  postpone vForth postpone ['] postpone vsource ; immediate restrict

>target_compile

\ stack primitives
: drop ( x -- )
  1 data_stackel link- vForth drop vsource ; immediate restrict

: dup ( x -- x x )
  1 data_stackel vForth dup vsource link+ ; immediate restrict

: over ( x1 x2 -- x1 x2 x1 )
  2 data_stackel vForth over vsource link+ ; immediate restrict

: rot ( x1 x2 x3 -- x2 x3 x1 )
  3 data_stackel vForth rot vsource ; immediate restrict

: swap ( x1 x2 -- x2 x1 )
  2 data_stackel vForth swap vsource ; immediate restrict

: pick ( xu ... x1 x0 u -- xu ... x1 x0 xu ) \ PROBLEM
  1 data_stackel dup btree_data @ dup node_type @ ['] n_literal = if
    nip node_val @ >r r@ 1+ data_stackel r> pick link+ else
    drop basic_exit ['Forth] pick compile, basic_init endif ; immediate restrict

: roll ( xu xu-1 ... x0 u -- xu-1 ... x0 xu ) \ PROBLEM
  1 data_stackel dup btree_data @ dup node_type @ ['] n_literal = if
    nip node_val @ >r r@ 1+ data_stackel r> roll else
    drop basic_exit ['Forth] roll compile, basic_init endif ; immediate restrict

: 2drop ( x1 x2 -- )
  vtarget_compile postpone drop postpone drop vsource ; immediate restrict

: 2dup ( x1 x2 -- x1 x2 x1 x2 )
  vtarget_compile postpone over postpone over vsource ; immediate restrict

: 2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
  vtarget_compile 3 postpone lit postpone pick 3 postpone lit postpone pick vsource ; immediate restrict

: 2rot ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
  vtarget_compile 5 postpone lit postpone roll 5 postpone lit postpone roll vsource ; immediate restrict

: 2swap ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
  vtarget_compile 3 postpone lit postpone roll 3 postpone lit postpone roll vsource ; immediate restrict

: ?dup ( x -- 0 | x x ) \ PROBLEM
  1 data_stackel dup btree_data @ dup node_type @ ['] n_literal = if
    node_val @ 0<> if
      dup link+ endif else
    drop basic_exit ['Forth] ?dup compile, basic_init endif ; immediate restrict

: nip ( x1 x2 -- x2 )
  2 data_stackel vForth swap link- swap nip vsource ; immediate restrict

: tuck ( x1 x2 -- x2 x1 x2 )
  2 data_stackel vForth tuck vsource link+ ; immediate restrict

\ primitives
: literal ( addr -- addr )
  1 data_stackel postpone lit ; immediate restrict

: + ( addr addr -- addr )
  2 data_stackel_literal if
    vForth + vsource postpone lit else
    @zero @zero ['] @add postpone op endif ; immediate restrict

: - ( addr addr -- addr )
  2 data_stackel_literal if
    vForth - vsource postpone lit else
    @zero @zero ['] @sub postpone op endif ; immediate restrict

>source
: @1+ ( rt rs -- rt )
  $0001 @addiu ;
: @1- ( rt rs -- rt )
  $ffff @addiu ;
>target_compile
: 1+ ( addr -- addr )
  1 data_stackel_literal if
    vForth 1+ vsource postpone lit else
    @zero @zero ['] @1+ postpone uop endif ; immediate restrict

: 1- ( addr -- addr )
  1 data_stackel_literal if
    vForth 1- vsource postpone lit else
    @zero @zero ['] @1- postpone uop endif ; immediate restrict

>source
: @* ( rd rs rt -- rd )
  @mult
  @mflo ;
: @/ ( rd rs rt -- rd )
  @div
  @mflo ;
: @mod ( rd rs rt -- rd )
  @div
  @mfhi ;
: @2* ( rd rt -- rd )
  $01 @sll ;
: @2/ ( rd rt -- rd )
  $01 @srl ;
>target_compile
: * ( addr addr -- addr )
  2 data_stackel_literal if
    vForth * vsource postpone lit else
    @zero @zero ['] @* postpone op endif ; immediate restrict

: / ( addr addr -- addr )
  2 data_stackel_literal if
    vForth / vsource postpone lit else
    @zero @zero ['] @/ postpone op endif ; immediate restrict

: mod ( addr addr -- addr )
  2 data_stackel_literal if
    vForth mod vsource postpone lit else
    @zero @zero ['] @mod postpone op endif ; immediate restrict

: /mod ( addr addr -- addr addr )
  2 data_stackel_literal if
    vForth /mod vsource postpone lit vForth swap vsource
    postpone lit vForth swap vsource else
    vForth 2dup vsource vtarget_compile postpone mod vsource
    vForth rot rot vsource vtarget_compile postpone / vsource endif ; immediate restrict

: 2* ( addr -- addr )
  1 data_stackel_literal if
    vForth 2* vsource postpone lit else
    @zero @zero ['] @2* postpone uop endif ; immediate restrict

: 2/ ( addr -- addr )
  1 data_stackel_literal if
    vForth 2/ vsource postpone lit else
    @zero @zero ['] @2/ postpone uop endif ; immediate restrict

: lshift ( addr addr -- addr )
  2 data_stackel_literal if
    vForth lshift vsource postpone lit else
    @zero @zero ['] @sllv postpone op endif ; immediate restrict

: rshift ( addr addr -- addr )
  2 data_stackel_literal if
    vForth rshift vsource postpone lit else
    @zero @zero ['] @srlv postpone op endif ; immediate restrict

: abs ( addr -- addr )
  1 data_stackel_literal if
    vForth + vsource postpone lit else
    @zero @zero ['] @abs postpone uop endif ; immediate restrict

: negate ( addr -- addr )
  1 data_stackel_literal if
    vForth negate vsource postpone lit else
    @zero @zero ['] @neg postpone uop endif ; immediate restrict

: invert ( addr -- addr )
  1 data_stackel_literal if
    vForth invert vsource postpone lit else
    @zero @zero ['] @not postpone uop endif ; immediate restrict

: and ( addr addr -- addr )
  2 data_stackel_literal if
    vForth and vsource postpone lit else
    @zero @zero ['] @and postpone op endif ; immediate restrict

: or ( addr addr -- addr )
  2 data_stackel_literal if
    vForth or vsource postpone lit else
    @zero @zero ['] @or postpone op endif ; immediate restrict

: xor ( addr addr -- addr )
  2 data_stackel_literal if
    vForth or vsource postpone lit else
    @zero @zero ['] @xor postpone op endif ; immediate restrict

>source
: @= ( rd rs rt -- rd )
  @xor
  dup $0001 @sltiu
  dup @zero swap @sub ;
: @<> ( rd rs rt -- rd )
  @xor
  @zero over @sltu
  dup @zero swap @sub ;
: @< ( rd rs rt -- rd )
  @slt
  dup @zero swap @sub ;
: @<= ( rd rs rt -- rd )
  swap @slt
  dup $ffff @addiu ;
: @> ( rd rs rt -- rd )
  swap @slt
  dup @zero swap @sub ;
: @>= ( rd rs rt -- rd )
  @slt
  dup $ffff @addiu ;
: @0= ( rd rs -- rd )
  $0001 @sltiu
  dup @zero swap @sub ;
: @0<> ( rd rs -- rd )
  @zero swap @sltu
  dup @zero swap @sub ;
: @0< ( rd rs -- rd )
  @zero @slt
  dup @zero swap @sub ;
: @0<= ( rd rs -- rd )
  @zero swap @slt
  dup $ffff @addiu ;
: @0> ( rd rs -- rd )
  @zero swap @slt
  dup @zero swap @sub ;
: @0>= ( rd rs -- rd )
  @zero @slt
  dup $ffff @addiu ;
: @u< ( rd rs rt -- rd )
  @sltu
  dup @zero swap @sub ;
: @u<= ( rd rs rt -- rd )
  swap @sltu
  dup $ffff @addiu ;
: @u> ( rd rs rt -- rd )
  swap @sltu
  dup @zero swap @sub ;
: @u>= ( rd rs rt -- rd )
  @sltu
  dup $ffff @addiu ;
>target_compile
: = ( addr addr -- addr )
  2 data_stackel_literal if
    vForth = vsource postpone lit else
    @zero @zero ['] @= postpone op endif ; immediate restrict

: <> ( addr addr -- addr )
  2 data_stackel_literal if
    vForth <> vsource postpone lit else
    @zero @zero ['] @<> postpone op endif ; immediate restrict

: < ( addr addr -- addr )
  2 data_stackel_literal if
    vForth < vsource postpone lit else
    @zero @zero ['] @< postpone op endif ; immediate restrict

: <= ( addr addr -- addr )
  2 data_stackel_literal if
    vForth <= vsource postpone lit else
    @zero @zero ['] @<= postpone op endif ; immediate restrict

: > ( addr addr -- addr )
  2 data_stackel_literal if
    vForth > vsource postpone lit else
    @zero @zero ['] @> postpone op endif ; immediate restrict

: >= ( addr addr -- addr )
  2 data_stackel_literal if
    vForth >= vsource postpone lit else
    @zero @zero ['] @>= postpone op endif ; immediate restrict

: 0= ( addr -- addr )
  1 data_stackel_literal if
    vForth 0= vsource postpone lit else
    @zero @zero ['] @0= postpone uop endif ; immediate restrict

: 0<> ( addr -- addr )
  1 data_stackel_literal if
    vForth 0<> vsource postpone lit else
    @zero @zero ['] @0<> postpone uop endif ; immediate restrict

: 0< ( addr -- addr )
  1 data_stackel_literal if
    vForth 0< vsource postpone lit else
    @zero @zero ['] @0< postpone uop endif ; immediate restrict

: 0<= ( addr -- addr )
  1 data_stackel_literal if
    vForth 0<= vsource postpone lit else
    @zero @zero ['] @0<= postpone uop endif ; immediate restrict

: 0> ( addr -- addr )
  1 data_stackel_literal if
    vForth 0> vsource postpone lit else
    @zero @zero ['] @0> postpone uop endif ; immediate restrict

: 0>= ( addr -- addr )
  1 data_stackel_literal if
    vForth 0>= vsource postpone lit else
    @zero @zero ['] @0>= postpone uop endif ; immediate restrict

: u< ( addr addr -- addr )
  2 data_stackel_literal if
    vForth u< vsource postpone lit else
    @zero @zero ['] @u< postpone op endif ; immediate restrict

: u<= ( addr addr -- addr )
  2 data_stackel_literal if
    vForth u<= vsource postpone lit else
    @zero @zero ['] @u<= postpone op endif ; immediate restrict

: u> ( addr addr -- addr )
  2 data_stackel_literal if
    vForth u> vsource postpone lit else
    @zero @zero ['] @u> postpone op endif ; immediate restrict

: u>= ( addr addr -- addr )
  2 data_stackel_literal if
    vForth u>= vsource postpone lit else
    @zero @zero ['] @u>= postpone op endif ; immediate restrict

>source
: @@ ( rt rs -- rt )
  @zero swap @lw
  @nop ;
: @! ( rt rs -- rt )
  @zero swap @sw ;
: @c@ ( rt rs -- rt )
  @zero swap @lb
  @nop ;
: @c! ( rt rs -- rt )
  @zero swap @sb ;
>target_compile
: @ ( addr -- addr )
  1 data_stackel @zero @zero ['] @@ postpone uop
  node_!_list @ over btree_data @ node_depends !
  dup node_@_list @ slist_insert drop ; immediate restrict

: ! ( addr addr -- )
  2 data_stackel @zero @zero ['] @! postpone opn
  node_@_list @ over btree_data @ node_depends !
  slist_init node_!_list !
  slist_init node_@_list !
  dup node_!_list @ slist_insert drop
  dup node_@_list @ slist_insert drop
  inst_insert ; immediate restrict

: c@ ( addr -- addr )
  1 data_stackel @zero @zero ['] @c@ postpone uop
  node_!_list @ over btree_data @ node_depends !
  dup node_@_list @ slist_insert drop ; immediate restrict

: c! ( addr addr -- )
  2 data_stackel @zero @zero ['] @c! postpone opn
  node_@_list @ over btree_data @ node_depends !
  slist_init node_!_list !
  slist_init node_@_list !
  dup node_!_list @ slist_insert drop
  dup node_@_list @ slist_insert drop
  inst_insert ; immediate restrict

: rdrop ( addr -- )
  return_stackel_get drop ; immediate restrict

: >r ( addr -- )
  1 data_stackel
  link- return_stackel_put ; immediate restrict

: r@ ( -- addr )
  0 return_stackel_fetch link+ ; immediate restrict

: r> ( -- addr )
  return_stackel_get link+ ; immediate restrict

: 2>r ( addr addr -- )
  2 data_stackel swap
  link- return_stackel_put
  link- return_stackel_put ; immediate restrict

: 2r@ ( -- addr addr )
  1 return_stackel_fetch link+
  0 return_stackel_fetch link+ ; immediate restrict

: 2r> ( -- addr addr )
  return_stackel_get link+
  return_stackel_get link+ swap ; immediate restrict

: ['] ( "name" -- )
  ' postpone lit ; immediate restrict

: [char] ( 'char' -- n )
  char postpone lit ; immediate restrict

: cells ( addr -- addr )
  1 data_stackel_literal if
    4 vForth * vsource postpone lit else
    4 postpone lit vtarget_compile postpone * vsource endif ; immediate restrict

: cell+ ( addr -- addr )
  1 data_stackel_literal if
    4 vForth + vsource postpone lit else
    4 postpone lit vtarget_compile postpone + vsource endif ; immediate restrict

: cell- ( addr -- addr )
  1 data_stackel_literal if
    4 vForth - vsource postpone lit else
    4 postpone lit vtarget_compile postpone - vsource endif ; immediate restrict

: chars ( addr -- addr )
  ; immediate restrict

: char+ ( addr -- addr )
  vtarget_compile postpone 1+ vsource ; immediate restrict

: char- ( addr -- addr )
  vtarget_compile postpone 1- vsource ; immediate restrict

>source
\ Variablen fuer Stringbehandlung
$2000 constant text_size	\ 16 kByte Textbuffer
create text_data
  here cell+ ,
  text_size allot

: text_pointer ( -- addr )
  text_data @ ;

: text_print ( -- )
  text_data dup @ over - dump ;

: ," ( "string"<"> -- n )
  text_pointer dup
  [char] " parse
  rot 2dup + char+ dup aligned swap over swap ?do
    bl i c! loop
  2dup text_data !
  2>r place
  2r> tuck - cacheflush ;

: (.")
  count type ;
: (s")
  count ;
: (abort")
  "error ! if
    -2 throw endif ;
>target_compile

: ." ( "string"<"> -- )
  ," postpone lit
  basic_exit ['] (.") compile, basic_init ; immediate restrict

: s" ( "string"<"> -- )
  ," postpone lit
  basic_exit ['] (s") compile, basic_init ; immediate restrict

: abort" ( "string"<"> -- )
  ," postpone lit
  basic_exit ['] (abort") compile, basic_init ; immediate restrict

\ : ( ( -- )
  \ postpone ( ; immediate restrict
  
\ : \ ( -- )
  \ postpone \ ; immediate restrict

>source

: (dostruc) ( addr u - addr )
  vtarget_compile
  postpone +
  vsource ; restrict
' (dostruc) dostruc !

\ here allot , c, A,                                   17dec92py

: dp    ( -- addr )  dpp @ ;
: here  ( -- here )  dp @ ;
: allot ( n -- )     dp +! ;
: c,    ( c -- )     here 1 chars allot c! ;
: ,     ( x -- )     here cell allot  ! ;
: 2,    ( w1 w2 -- ) \ general
    here 2 cells allot 2! ;

: aligned ( addr -- addr' )
  [ cell 1- ] Literal + [ -1 cells ] Literal and ;
: align ( -- )          here dup aligned swap ?DO  bl c,  LOOP ;

: faligned ( addr -- f-addr )
  [ 1 floats 1- ] Literal + [ -1 floats ] Literal and ;

: falign ( -- )
  here dup faligned swap
  ?DO
      bl c,
  LOOP ;

\ !! this is machine-dependent, but works on all but the strangest machines
' faligned Alias maxaligned
' falign Alias maxalign

\ the code field is aligned if its body is maxaligned
\ !! machine-dependent and won't work if "0 >body" <> "0 >body maxaligned"
' maxaligned Alias cfaligned
' maxalign Alias cfalign

: chars ; immediate

: A!    ( addr1 addr2 -- )  dup relon ! ;
: A,    ( addr -- )     here cell allot A! ;

\ on off                                               23feb93py

: on  ( addr -- )  true  swap ! ;
: off ( addr -- )  false swap ! ;

?test $0004 [IF]
cr ." Test for primitives.fs" cr

finish
[THEN]

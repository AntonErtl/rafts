." START: " here hex . decimal cr
\ $Id: compiler.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: compiler.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

bl word vocabulary find nip 0= [IF]
include search-order.fs
\ include wordinfo.fs
\ include float.fs
\ include see.fs
[THEN]

vocabulary voc_source
vocabulary voc_target
vocabulary voc_target_compile

voc_source also definitions

include options.fs
include stdlib/stdlib.fs

: vForth ( -- )
  Forth ; immediate

: vsource ( -- )
  voc_source ; immediate

: >source ( -- )
  also voc_source definitions previous ;

: source> ( -- )
  voc_source also ;

: vtarget ( -- )
  voc_target ; immediate

: >target ( -- )
  also voc_target definitions previous ;

: target> ( -- )
  voc_target also ;

: vtarget_compile ( -- )
  voc_target_compile ; immediate

: >target_compile ( -- )
  also voc_target_compile definitions previous ;

: target_compile> ( -- )
  also voc_target_compile also ;

include mips/r3000.asm.fs
include mips/r3000.disasm.fs
include basic.fs

: interpreter ( c-addr u -- )
?trace $0002 [IF]
  ." INTER:" 2dup type cr
[THEN]
  2dup sfind
?trace $0001 [IF]
  order .s cr
[THEN]
  1 and if nip nip
?trace $0001 [IF]
    ." interpreter:" dup name.
    dup hex. dup get_do hex. cr
[THEN]
    execute else
    snumber? 0= if
      notfound endif endif ;

: (compile,) ( xt n -- )
  case
    :docol of
?trace $0001 [IF]
      ." func_interpreter:" hex.s cr
[THEN]
      func_interpreter endof
    :docode of
?trace $0001 [IF]
      ." func_native:" hex.s cr
[THEN]
      func_native endof
    >r
?trace $0001 [IF]
      ." func_interpreter (default):" hex.s cr
[THEN]
      func_interpreter
    r>
  endcase ;

: compile, ( xt -- )
  dup forthstart u> if
    dup get_do (compile,) else
?trace $0001 [IF]
    ." func_interpreter (forth):" hex.s cr
[THEN]
    func_interpreter endif ;

: ?compile ( xt -- )
  dup forthstart u> if
    dup get_do
    case
      :docon of
?trace $0001 [IF]
        ." constant:" hex.s
[THEN]
        execute
?trace $0001 [IF]
        dup hex. cr
[THEN]
	postpone lit endof
      :dovar of
?trace $0001 [IF]
        ." variable:" hex.s cr
[THEN]
        execute
?trace $0001 [IF]
        dup hex. cr
[THEN]
	postpone lit endof
      :douser of
?trace $0001 [IF]
        ." user:" hex.s cr
[THEN]
        execute postpone lit endof
      :docol of
?trace $0001 [IF]
        ." FUNC_INTERPRETER:" hex.s cr
[THEN]
        >r basic_exit r>
        compile,
        basic_init endof
      :dodefer of
?trace $0001 [IF]
        ." FUNC_DEFER:" hex.s cr
[THEN]
	2 cells + @
        recurse endof
      :dostruc of
?trace $0001 [IF]
        ." FUNC_STRUC:" hex.s cr
[THEN]
	2 cells + @
	\ ." -> " dup . ." <-" cr
	\ ." BEFORE: " .s
	postpone lit
	\ ." BETWEEN: " .s
	dostruc @ execute
	\ ." AFTER: " .s cr
	endof
      :docode of
?trace $0001 [IF]
        ." FUNC_NATIVE:" hex.s cr
[THEN]
        >r basic_exit r>
        compile,
        basic_init endof
      dup >r
?func_mode_direct [IF]
      $03ffffff and 2 lshift
      over $fc000000 and +
[THEN]
      dup @ :dodoes = if
?trace $0001 [IF]
        ." FUNC_NATIVE (DOES>):" hex.s cr
[THEN]
	>r 2 cells + postpone lit basic_exit r>
        :docode (compile,)
        basic_init else
	drop
?trace $0001 [IF]
        ." FUNC_INTERPRETER (DOES>):" hex.s cr
[THEN]
        >r basic_exit r>
        :docol (compile,)
        basic_init endif
      r>
    endcase else
?trace $0001 [IF]
    ." FUNC_INTERPRETER (FORTH):" hex.s cr
[THEN]
    >r basic_exit r>
    compile,
    basic_init endif ;

: compiler ( c-addr u -- )
?trace $0002 [IF]
  ." COMP:" 2dup type cr
[THEN]
  2dup sfind
?trace $0001 [IF]
  order .s cr
[THEN]
  case
    2 of nip nip
?trace $0001 [IF]
      ." compiler (restrict&immediate):" dup name.
      dup hex. dup get_do hex. cr
[THEN]
      execute endof
    -2 of nip nip
?trace $0001 [IF]
      ." compiler (restrict):" dup name.
      dup hex. dup get_do hex. cr
[THEN]
      execute endof
    1 of nip nip
?trace $0001 [IF]
      ." compiler (immediate):" dup name.
      dup hex. dup get_do hex. cr
[THEN]
      >r basic_exit r>
      execute
      basic_init endof
    -1 of nip nip
?trace $0001 [IF]
      ." compiler:" dup name.
      dup hex. dup get_do hex. cr
[THEN]
      ?compile endof
    0 of
      snumber? 0<> if
?trace $0001 [IF]
        ." number:" dup hex. cr
[THEN]
        postpone lit else
        notfound endif endof
  endcase ;

: interpret ( -- )
  begin
    \ ?stack
    name dup 0<> while
    state @ 0= if
      interpreter else
      compiler endif repeat
  2drop ;

: printok ( -- )
  ."  ok"
?trace $0001 [IF]
  hex.s ." ay"
[THEN]
  ;

: printcompile ( -- )
  ."  com"
?trace $0001 [IF]
  hex.s
[THEN]
  ." piled" ;

include dataflow.fs

: quit ( -- )
  loadfile off blk off
  begin
    cr refill while
    interpret
    state @ 0= if
      printok else
      printcompile endif
    repeat ;

: cold ( -- )
  argc @ 2 > if			\ 2, because compiler.fs is 1 in testmod !!!
    begin
      argc @ 2 > while
      2 arg
      -1 argc +!
      1 cells argv +!
      included repeat
    endif
  quit ;

>target
: bye ( -- )
?trace $8000 [IF]
  finish
  regs_print
  text_print
  order cr
  \ ['] Root list
  \ ['] Forth list
  ['] voc_source list
  ['] voc_target list
  ['] voc_target_compile list
[THEN]

  finish

?trace $4000 [IF]
  depth 0 ?do
    . loop
  cr
[THEN]
  bye ;

?test $0001 [IF]
cr ." Test for compiler.fs" cr

finish
[THEN]

\ ." START AGAIN: " here hex . decimal cr

target>
\ order .s cr
cold

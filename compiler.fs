." START: " here hex . decimal cr

\ compiler.fs	compiler main load file
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

vocabulary voc_source
\ basically the host-residing words, but may be called from target words
vocabulary voc_target
\ all words compiled by the target, and a few more
vocabulary voc_target_compile
\ compilation semantics of "primitives" and other words visible in "compile state"

voc_source also definitions

include options.fs
include stdlib/stdlib.fs

: vForth ( -- )
  Forth ; restrict immediate

: vsource ( -- )
  voc_source ; restrict immediate

: >source ( -- )
  also voc_source definitions previous ;

: source> ( -- )
  voc_source also ;

: vtarget ( -- )
  voc_target ; restrict immediate

: >target ( -- )
  also voc_target definitions previous ;

: target> ( -- )
  voc_target also ;

: vtarget_compile ( -- )
  voc_target_compile ; restrict immediate

: >target_compile ( -- )
  also voc_target_compile definitions previous ;

: target_compile> ( -- )
  also voc_target_compile also ;

include mips/r3000.asm.fs
include mips/r3000.disasm.fs
include basic.fs

: interpreter ( c_addr u -- )
?trace $0002 [IF]
  ." INTER:" 2dup type cr
[THEN]
  2dup sfind
?trace $0001 [IF]
  order cr
  hex.s cr
[THEN]
  1 and if nip nip
?trace $0001 [IF]
    ." interpreter:" dup name.
    dup hex. dup >code-address hex. cr
[THEN]
    execute 
  else
    snumber? 0= if
      interpreter-notfound endif endif ;

: compile, ( xt -- )
  dup forthstart u> if
    dup >code-address
    case
      docon: of
?trace $0001 [IF]
        ." constant:" hex.s
[THEN]
        execute vtarget_compile postpone literal vsource endof
      dovar: of
?trace $0001 [IF]
        ." variable:" hex.s cr
[THEN]
        execute vtarget_compile postpone literal vsource endof
      douser: of
?trace $0001 [IF]
        ." user:" hex.s cr
[THEN]
        execute vtarget_compile postpone literal vsource endof
      docol: of
?trace $0001 [IF]
        ." FUNC_INTERPRETER:" hex.s cr
[THEN]
        basic_exit
        func_interpreter
        basic_init endof
      dodefer: of
?trace $0001 [IF]
        ." FUNC_DEFER:" hex.s cr
[THEN]
	basic_exit
        func_interpreter
        basic_init endof
\	2 cells +
\        vtarget_compile postpone literal vsource
\	['] @ recurse
\        ['] execute recurse endof
      dofield: of
?trace $0001 [IF]
        ." FUNC_STRUC:" hex.s cr
[THEN]
	2 cells + @ vtarget_compile postpone literal vsource dostruc @
	execute
	endof
      docode: of
?trace $0001 [IF]
        ." FUNC_NATIVE:" hex.s cr
[THEN]
        basic_exit
        func_native
        basic_init endof
      dup >r
      over >does-code 0= if \ !! defaults to native-code does> handler, interpreter would be better
?trace $0001 [IF]
        ." FUNC_NATIVE (DOES>):" hex.s cr
[THEN]
	swap 2 cells + vtarget_compile postpone literal vsource basic_exit
        func_native
        basic_init else
	drop
?trace $0001 [IF]
        ." FUNC_INTERPRETER (DOES>):" hex.s cr
[THEN]
        basic_exit
        func_interpreter
        basic_init endif
      r>
    endcase else
?trace $0001 [IF]
    ." FUNC_INTERPRETER (FORTH):" hex.s cr
[THEN]
    basic_exit
    func_interpreter
    basic_init endif ;

: compiler ( c_addr u -- )
?trace $0002 [IF]
  ." COMP:" 2dup type cr
[THEN]
  2dup sfind
?trace $0001 [IF]
  order cr hex.s cr
[THEN]
  case
    2 of nip nip
?trace $0001 [IF]
      ." compiler (restrict&immediate):" dup name.
      dup hex. dup >code-address hex. cr
[THEN]
      execute endof
    -2 of nip nip
?trace $0001 [IF]
      ." compiler (restrict):" dup name.
      dup hex. dup >code-address hex. cr
[THEN]
      compile, endof
    1 of nip nip
?trace $0001 [IF]
      ." compiler (immediate):" dup name.
      dup hex. dup >code-address hex. cr
[THEN]
      execute endof
    -1 of nip nip
?trace $0001 [IF]
      ." compiler:" dup name.
      dup hex. dup >code-address hex. cr
[THEN]
      compile, endof
    0 of
      snumber? 0<> if
?trace $0001 [IF]
        ." number:" dup hex. cr
[THEN]
        vtarget_compile postpone literal vsource else
        compiler-notfound endif endof
  endcase ;

: interpret ( -- )
  begin
    ?stack
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

\ dataflow.fs	dataflow words
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
: [ ( -- )
  state off previous previous ; immediate restrict

>target
: ] ( -- )
  state on target_compile> ;

: :noname ( -- )
  vtarget ] vsource
  true noname_state !
  func_init_noname ;

: : ( "name" -- )
  vtarget ] vsource
  false noname_state !
  func_init ;

>target_compile
: ; ( -- )
?trace $0008 [IF]
  hex.s cr
[THEN]
  noname_state @ if
    func_exit_noname else
    func_exit endif
  vtarget_compile postpone [ vsource
?trace $0800 [IF]
  noname_state @ 0<> if
    lastcfa @ else
    last @ endif
  here 2dup over - hex.s dump		\ Hexdump vom generierten Maschinencode
  noname_state @ 0= if
    swap name>int swap endif
  hex.s swap				\ disassemblierter Dump vom generierten Maschienencode
  dup 2 cells + tuck disasm_dump
  \ dup 2 cells + tuck disasm_dump
  \ swap 4 cells - tuck disasm_dump
  swap disasm_dump
  .cs cr
  regs_print
[THEN]
  cs_depth 0<> abort" unstructured"
  noname_state @ 0<> if
    lastcfa @ else
    last @ endif
  here over - flush-icache
  noname_state @ 0<> if
    lastcfa @ endif
  ; immediate restrict
>source

?test $0008 [IF]
cr ." Test for dataflow.fs" cr

finish
[THEN]

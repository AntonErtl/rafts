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

>target
: : ( "name" -- )
  vtarget ] vsource
  func_init
  basic_init ;

>target_compile
: ; ( -- )
?trace $0008 [IF]
  .s cr
[THEN]
  basic_exit
  vtarget_compile postpone [ vsource
  func_exit
?trace $0800 [IF]
  last @ here 2dup over - hex.s dump		\ Hexdump vom generierten Maschinencode
  swap name>
  swap hex.s disasm_dump			\ disasemblierter Dump vom generierten Maschienencode
  .cs cr
  regs_print
[THEN]
  cs_depth 0<> abort" unstructured"
  last @ here over - flush-icache ; immediate restrict
>source

include primitives.fs
include control.fs

include primitives_ext.fs

?test $0008 [IF]
cr ." Test for dataflow.fs" cr

finish
[THEN]

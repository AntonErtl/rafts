\ func.fs	function words
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

include header.fs
docode constant docode:

: func_init_noname ( -- )
  docode cfa,
  (func_init)
  basic_init
  0 @ra I_REG terminal
  >return ;

: check-ra ( -- )
  return>
  dup node_reg @ @ra <> if
    @ra over node_reg ! inst_btrees_insert else
    drop endif ;

: func_exit_noname ( -- )
  check-ra
  basic_exit
  (func_exit) ;

: func_init ( -- )
  header
  func_init_noname ;

: func_exit ( -- )
  func_exit_noname
  reveal ;

: func_call ( pfa -- )
  @jal
  @nop ;

: func_native ( cfa -- )
?trace $0002 [IF]
  dup >name hex. ." func_native:" dup name. cr
[THEN]
  2 cells + func_call ;

: func_interpreter ( cfa -- )
?trace $0002 [IF]
  dup >name hex. ." func_interpreter:" dup name. cr
[THEN]
  #cfa swap @li
?func_mode_direct [IF]
  #ip here 4 cells + @li drop
[THEN]
?func_mode_indirect [IF]
  #ip here 5 cells + @li drop
  @t0 0 rot @lw
[THEN]
  @jr
  @nop
  here cell+ a,
?func_mode_indirect [IF]
  here cell+ a,
[THEN]
  ;

' func_init alias :code
' func_exit alias ;code

variable dostruc
variable noname_state
false noname_state !

?test $0008 [IF]
cr ." Test for func.fs" cr

' docode >name &164 dump

docol: hex.
docon: hex.
dovar: hex.
douser: hex.
dodefer: hex.
dofield: hex.
dodoes: hex.
docode: hex. cr

here
func_init foo
$1234 func_native
$5678 func_interpreter
func_exit
here 2dup over - dump
swap cell+ dup c@ $1f and + char+ aligned swap disasm_dump

finish
[THEN]

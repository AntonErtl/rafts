\ header.fs
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

: get_do ( addr -- addr )
  dup c@ $e8 = if
    char+ dup @ + 4 + else
    @ endif ;

?shared docode [IF]
create docode
  #cfa dup 8 @addiu
  #ip -4 #rp @sw
  @ra swap @jalr drop
  #rp dup -4 @addiu

  #ip 0 rot @lw
  #rp dup 4 @addiu drop
  #cfa 0 rot @lw
  #ip dup 4 @addiu drop
?func_mode_indirect [IF]
  @t0 0 rot @lw
  @nop
[THEN]
  @jr
  @nop

create dodoes
  @t0 0 #cfa @lw
  #cfa dup 8 @addiu
  dup -4 #sp @sw
  #sp dup -4 @addiu drop
?func_mode_direct [IF]
  @t1 $03ffffff @li
  rot tuck @and
  dup $02 @sll
  @t1 $fc000000 @li
  rot tuck @and
  tuck @or
[THEN]
?func_mode_indirect [IF]
  drop
[THEN]
  #ip -4 #rp @sw
  dup 8 @addiu
  @ra swap @jalr drop
  #rp dup -4 @addiu

  #ip 0 rot @lw
  #rp dup 4 @addiu drop
  #cfa 0 rot @lw
  #ip dup 4 @addiu drop
?func_mode_indirect [IF]
  @t0 0 rot @lw
  @nop
[THEN]
  @jr
  @nop
[THEN]

: (func_init) ( -- )
  \ \\ @ra -4 #rp @sw
  \ \\ #rp #rp -4 @addiu drop
  \ -4 regs_unused LITI node
  \ 0 #rp VREGP node
  \ ADDU op #rp over node_reg ! inst_btrees_insert
  \ 0 @ra VREGP node dup inst_done
  \ -4 #rp id! inst_btrees_insert
  ;

: (func_exit) ( -- )
  \ \\ @ra 0 #rp @lw
  \ \\ #rp #rp 4 @addiu drop
  @ra @jr
  @nop
  ;

: (func_lwexit) ( -- )
  @ra 0 #rp @lw drop ;

true constant ?runtest

?test $0002 [IF]
cr ." Test for header.fs" cr

finish
[THEN]

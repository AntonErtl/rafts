\ stack.fs	stack words
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

\ data allocation and definitions
' slist_struct alias stack_struct
' slist_next alias stack_next ( addr -- addr )

\ init function
' slist alias stack ( addr -- addr )

\ push an element
: stack_push ( val-addr addr -- )
  slist_insert drop ;

\ pop an element
: stack_pop ( addr -- val-addr )
  dup slist_next @ swap
  slist_delete ;

\ exit function
: stack_exit ( addr -- )
  slist_exit ;

\ depth function
' slist_size alias stack_depth ( addr -- u )

\ empty function
: stack_empty ( addr -- flag )
  stack_depth 0= ;

\ print function
' slist_print alias stack_print ( addr -- )

' slist_forall alias stack_forall ( xt addr -- )

?test $0800 [IF]
cr ." Test for stack.fs" cr

stack_struct
  1 cells: field tdata_value
end-struct tdata_struct

: tdata ( -- addr )
  tdata_struct struct-allot stack
  NIl over tdata_value ! ;

: tdata_init ( x -- addr )
  tdata
  tuck tdata_value ! ;

: tdata_print_func ( addr -- )
  ." ( " dup hex. ." ) " tdata_value @ . ;

: tdata_print ( addr -- )
  ['] tdata_print_func swap stack_forall ;

variable tdata_head

." tdata: " tdata tdata_head ! .s cr

." stack_push: " 123 tdata_init tdata_head @ stack_push
  tdata_head @ stack_next @ tdata_print_func .s cr
." stack_push: " 456 tdata_init tdata_head @ stack_push
  tdata_head @ stack_next @ tdata_print_func .s cr
." stack_push: " 789 tdata_init tdata_head @ stack_push
  tdata_head @ stack_next @ tdata_print_func .s cr
." tdata_print: " tdata_head @ tdata_print .s cr

." stack_pop: " tdata_head @ dup stack_next @ tdata_print_func
  stack_pop tdata_print_func .s cr
." stack_pop: " tdata_head @ dup stack_next @ tdata_print_func
  stack_pop tdata_print_func .s cr
." stack_depth: " tdata_head @ stack_depth . .s cr
." stack_empty: " tdata_head @ stack_empty . false . .s cr
." tdata_print: " tdata_head @ tdata_print .s cr
." stack_pop: " tdata_head @ dup stack_next @ tdata_print_func
  stack_pop tdata_print_func .s cr
." stack_depth: " tdata_head @ stack_depth . .s cr
." stack_empty: " tdata_head @ stack_empty . true . .s cr
." tdata_print: " tdata_head @ tdata_print .s cr

." stack_exit: " tdata_head @ stack_exit .s cr

finish
[THEN]

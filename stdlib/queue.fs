\ queue.fs	queue words
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
' dlist_prev alias queue_prev
' dlist_next alias queue_next

\ init function
' dlist_init alias queue_init ( addr addr -- addr addr )

\ put an element
: queue_put ( x addr addr -- addr )
  drop dlist_insert_prev ;

\ get an element
: queue_get ( addr addr -- x )
  nip dlist_next @ dup swap
  dlist_delete ;

\ exit function
: queue_exit ( addr -- )
  dlist_exit ;

\ depth function
: queue_depth ( addr -- u )
  dlist_size ;

\ empty function
: queue_empty ( addr -- flag )
  queue_depth 0= ;

\ print function
: queue_print ( addr -- )
  dlist_print ;

?test $0800 [IF]
cr ." Test for queue.fs" cr

dlist_struct
  1 cells: field qdata_value
end-struct qdata_struct

: qdata ( -- addr )
  qdata_struct struct-allot dlist
  NIL over qdata_value ! ;

: qdata_init ( x -- addr )
  qdata
  tuck qdata_value ! ;

: qdata_print_func ( addr -- )
  ." ( " dup hex. ." ) " qdata_value ? ;

: qdata_print ( addr -- )
  ['] qdata_print_func swap dlist_forall ;

variable qdata_head
variable qdata_tail

." queue_init: " qdata qdata queue_init qdata_head ! qdata_tail ! .s cr

." queue_put: " 123 qdata_init qdata_tail @ qdata_head @ queue_put
  qdata_print_func .s cr
." queue_put: " 456 qdata_init qdata_tail @ qdata_head @ queue_put
  qdata_print_func .s cr
." queue_put: " 789 qdata_init qdata_tail @ qdata_head @ queue_put
  qdata_print_func .s cr
." queue_depth: " qdata_head @ queue_depth . .s cr
." queue_empty: " qdata_head @ queue_empty . false . .s cr
." qdata_print: " qdata_head @ qdata_print .s cr

." queue_get: " qdata_tail @ qdata_head @ dup queue_next @ qdata_print_func
  queue_get qdata_print_func .s cr
." queue_get: " qdata_tail @ qdata_head @ dup queue_next @ qdata_print_func
  queue_get qdata_print_func .s cr
." queue_depth: " qdata_head @ queue_depth . .s cr
." queue_empty: " qdata_head @ queue_empty . false . .s cr
." qdata_print: " qdata_head @ qdata_print .s cr

." queue_get: " qdata_tail @ qdata_head @ dup queue_next @ qdata_print_func
  queue_get qdata_print_func .s cr
." queue_depth: " qdata_head @ queue_depth . .s cr
." queue_empty: " qdata_head @ queue_empty . true . .s cr
." qdata_print: " qdata_head @ qdata_print .s cr

." queue_exit: " qdata_head @ queue_exit .s cr
." qdata_print: " qdata_head @ qdata_print .s cr

finish
[THEN]

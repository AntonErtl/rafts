\ node.fs	node structur and words
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

7 constant MAX-NT \ !! machine dependent

btree-struct
    1 cells: field il-op	\ operation (intermediate language)
    1 cells: field il-slabel	\ state label
    1 cells: field il-val	\ literal value (alias: il-offset)
    1 cells: field il-reg	\ used register
    1 cells: field il-depends	\ list of dependencies
    -1 cells: field il-unpad	\ next field starts at offset 1
    MAX-NT cells: field il-nt-insts \ for each nt, the ml for this node
end-struct il-struct

' btree-left alias il-left
' btree-right alias il-right
' il-val alias il-offset	\ for stack elements: offset (in bytes) from stack pointer at start of basic block

NIL inst constant il-depends-init

btree-struct \ !! machine dep: is it really a *binary* tree?
    1 cells: field ml-asm	\ xt of the assembler word for the instruction
    1 cells: field ml-count	\ use count (# of parents)
    1 cells: field ml-val	\ literal value
    1 cells: field ml-done	\ scheduling done flag
    1 cells: field ml-reg	\ used register
    1 cells: field ml-node-dependences \ dependences of ml's node
    1 cells: field ml-depends	\ list of dependencies
    1 cells: field ml-delay	\ true: create a delay slot nop for the instruction
    1 cells: field ml-cost	\ path length to end of bb
end-struct ml-struct

' btree-left alias ml-left
' btree-right alias ml-right

: flag. ( f -- )
    dup 0= if
	." true " drop EXIT
    endif
    dup true = if
	." false" drop EXIT
    endif
    hex. assert( FALSE ) ;

: inst-print-depends-func ( inst-addr -- )
    inst-node hex? ;

: inst-print-depends ( inst-addr -- )
    dup il-depends-init <> if
	['] inst-print-depends-func swap slist-forall
    else
	drop ." no "
    endif ;

: ml-print-depends ( inst-addr -- )
    ['] inst-print-depends-func maplist ;

: print-ml ( ml -- )
    dup hex.
    dup ml-left hex?
    dup ml-right hex?
    dup ml-asm @ name.
    ." count=" dup ml-count @ .
    ." val=" dup ml-val @ .
    ." done=" dup ml-done @ flag.
    ." reg=" dup ml-reg @ .
    ." delay=" dup ml-delay @ flag.
    ." cost=" dup ml-cost @ .
    ." depends=" dup ml-depends @ ml-print-depends
    drop cr ;

: asm ( il-addr -- )
    drop ;

\ reset node values
: il-reset ( il-addr -- )
    0 over il-slabel !
    -1 over il-reg !
    il-depends-init over il-depends !
    drop ;

\ allocate and initialize a node
: make-il ( val reg op -- il-addr )
    il-struct struct-allot	\ allocate
    btree
    dup il-reset
    dup il-nt-insts cell+ max-nt 1- cells erase \ !! or just reset the mls in the nts
    tuck il-op !		\ initial values
    tuck il-reg !
    tuck il-val ! ;

?test $0002 [IF]
cr ." Test for node.fs" cr

finish
[THEN]

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

7 constant max-NT \ !! machine dependent

btree-struct
    1 cells: field il-op	\ operation (intermediate language)
    1 cells: field il-slabel	\ state label
    1 cells: field il-val	\ literal value (alias: il-offset)
    1 cells: field il-reg	\ used register
    1 cells: field il-depends	\ list of dependencies
    -1 cells: field il-unpad	\ next field starts at offset 1
    max-NT cells: field il-nt-insts \ for each nt, the ml for this node
end-struct il-struct

' btree-left alias il-left
' btree-right alias il-right

btree-struct \ !! machine dep: is it really a *binary* tree?
    1 cells: field ml-asm	\ xt of the assembler word for the instruction
    1 cells: field ml-count	\ use count (# of parents)
    1 cells: field ml-val	\ literal value
    1 cells: field ml-reg	\ used register
    1 cells: field ml-done	\ scheduling done flag
    1 cells: field ml-node-dependences \ dependences of ml's node
    1 cells: field ml-depends	\ list of dependencies
    1 cells: field ml-delay	\ true: create a delay slot nop for the instruction
    1 cells: field ml-cost	\ path length to end of bb
end-struct ml-struct

' btree-left alias ml-left
' btree-right alias ml-right

?test $0002 [IF]
cr ." Test for node.fs" cr

finish
[THEN]

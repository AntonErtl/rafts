\ vcc.fs	compiler main load file
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

vocabulary voc_source2
vocabulary voc_target2
vocabulary voc_target_compile2

: vsource ( -- )
  voc_source2 ; immediate

: >source ( -- )
  also voc_source2 definitions previous ;

: source> ( -- )
  voc_source2 also ;

: vtarget ( -- )
  voc_target2 ; immediate

: >target ( -- )
  also voc_target2 definitions previous ;

: target> ( -- )
  voc_target2 also ;

: vtarget_compile ( -- )
  voc_target_compile2 ; immediate

: >target_compile ( -- )
  also voc_target_compile2 definitions previous ;

: target_compile> ( -- )
  >target_compile
  get-current 1 set-order also ;

order cr

source>
\ voc_source2 also
>source
\ also voc_source2 definitions previous

order cr

include stdlib/stdlib.fs

: foo ( -- )
  [ order cr ]
  1 2 + . cr ;

' Root list
' Forth list
' voc_source2 list
' voc_target2 list
' voc_target_compile2 list

foo
$40 cell- hex.

\ primitive_ext.fs	extended primitive words
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

: read-loop ( i*x -- j*x )
  begin
    refill while
    interpret repeat ;

: include-file ( i*x fid -- j*x )
  push-file loadfile !
  0 loadline ! blk off ['] read-loop catch
  loadfile @ close-file swap 2dup or
  pop-file drop throw throw ;

: included ( i*x addr u -- j*x )
  loadfilename 2@ >r >r
  open-path-file ( file-id c-addr2 u2 )
  dup allocate throw over loadfilename 2! ( file-id c-addr2 u2 )
  drop loadfilename 2@ move
  ['] include-file catch
  \ don't free filenames; they don't take much space
  \ and are used for debugging
  r> r> loadfilename 2! throw ;

: include ( "name" -- )
  name included ;

?test $0004 [IF]
cr ." Test for primitives_ext.fs" cr

finish
[THEN]

\ $Id: primitives_ext.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: primitives_ext.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

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

?test $2000 [IF]
cr ." Test for primitives_ext.fs" cr

finish
[THEN]

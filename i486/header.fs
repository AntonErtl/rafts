\ $Id: header.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: header.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

: get_do ( addr -- addr )
  dup c@ $e9 = if
    char+ dup @ + 4 + else
    @ endif ;

false constant ?runtest

?test $0002 [IF]
cr ." Test for header.fs" cr

finish
[THEN]

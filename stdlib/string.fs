\ $Id: string.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: string.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

: cmove_any ( c-addr1 c-addr2 u -- )
  >r 2dup >
  r> swap if
    cmove else
    cmove> endif ;

: copy ( c-addr1 u c-addr2 -- )
  2>r 2r@ char+ swap cmove_any
  2r> c! ;

?test $0200 [IF]
cr ." Test for string.fs" cr

finish
[THEN]

\ $Id: dataflow.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: dataflow.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

>target_compile
: [ ( -- )
  state off previous previous ; immediate restrict

>target
: ] ( -- )
  state on target_compile> ;

>target
: : ( "name" -- )
  vtarget ] vsource
  func_init
  basic_init ;

>target_compile
: ; ( -- )
?trace $0008 [IF]
  .s cr
[THEN]
  basic_exit
  vtarget_compile postpone [ vsource
  func_exit
?trace $0800 [IF]
  last @ here 2dup over - hex.s dump		\ Hexdump vom generierten Maschinencode
  swap name>
  swap hex.s disasm_dump			\ disasemblierter Dump vom generierten Maschienencode
  .cs cr
  regs_print
[THEN]
  cs_depth 0<> abort" unstructured"
  last @ here over - cacheflush ; immediate restrict
>source

include primitives.fs
include control.fs

include primitives_ext.fs

?test $0008 [IF]
cr ." Test for dataflow.fs" cr

finish
[THEN]

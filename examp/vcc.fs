\ $Id: vcc.fs,v 1.1 1995/10/06 18:12:55 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: vcc.fs,v $
\ Revision 1.1  1995/10/06 18:12:55  anton
\ Initial revision
\

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

bye

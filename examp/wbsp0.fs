: sum ( n -- n )
  dup 1+ * 2/ ;

: printsum ( n -- )
  ." sum: " dup . ." = " sum . ;

: printsum-while ( to from -- )
  begin
    2dup > while
    dup 10 mod 2 < if
      cr endif
    dup printsum
    2 + repeat
  2drop ;

: printsum-until ( to from -- )
  begin
    dup 10 mod 2 < if
      cr endif
    dup printsum
    2 +
    2dup < until
  2drop ;

100 1 printsum-while cr
50 1 printsum-while cr
100 1 printsum-until cr
50 1 printsum-until cr
.s cr

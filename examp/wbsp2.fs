: foo ( to from -- )
  ?do
    i 1+ 0 ?do
      j . i .
      i 10 >= if
        leave endif
      loop
    cr
    2 +loop ;

10 0 foo cr
20 0 foo cr
.s cr
bye

: case0 ( to from -- )
  do
    i case
      15 of
        i . cr endof
      endcase
    loop ;

?runtest [IF]
$1000000 $0 case0
[THEN]
bye

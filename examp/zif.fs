: if0 ( to from -- )
  do
    i 15 = if
      i . cr endif
    loop ;

?runtest [IF]
$1000000 $0 if0
[THEN]

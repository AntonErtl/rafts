: ifelsef0 ( to from -- )
  do
    i 15 <> if
      else
      i . cr endif
    loop ;

?runtest [IF]
$1000000 $0 ifelsef0
[THEN]
bye

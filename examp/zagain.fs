: again0 ( to from -- )
  begin
    1+
    2dup = if
      2drop exit endif
    again ;

?runtest [IF]
$1000000 $0 again0
[THEN]
bye

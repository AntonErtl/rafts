: while0 ( to from -- )
  1+
  begin
    2dup <> while
    1+ repeat
  2drop ;

?runtest [IF]
$1000000 $0 while0
[THEN]

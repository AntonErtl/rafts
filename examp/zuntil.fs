: until0 ( to from -- )
  begin
    1+
    2dup = until
  2drop ;

?runtest [IF]
$1000000 $0 until0
[THEN]

: recurse0 ( to from -- )
  1+
  2dup <> if
    recurse else
    2drop endif ;

: recurse1 ( to from -- )
  do
    $10000 $0 recurse0 loop ;

?runtest [IF]
$100 $0 recurse1
[THEN]
bye

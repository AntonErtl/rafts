: loop0 ( to from -- )
  do
    loop ;

?runtest [IF]
$1000000 $0 loop0
[THEN]

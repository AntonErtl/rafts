: func0 ( to from -- )
  ;

: func1 ( to from -- )
  do
    func0 loop ;

?runtest [IF]
$1000000 $0 func1
[THEN]

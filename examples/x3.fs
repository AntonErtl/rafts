hex

: foo
  2r@ . . 2dup <> if
    recurse endif ;

foo
' foo .
' foo $20 dump
$0800007e $20 dump
cr
bye

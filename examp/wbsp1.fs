variable x

: foo ( addr -- 1 1 1 2 2 3 )
  1 over !
  dup @ swap dup @ swap dup @
  swap 2 over !
  dup @ swap dup @
  swap 3 over !
  @ ;

x foo . . . . . . cr
.s cr

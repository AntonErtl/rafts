: ['Forth] ( "name" -- )
  postpone vForth postpone ['] postpone vtarget_compile ; immediate restrict

: foo
  ['] pick
  ['Forth] pick
  ;

bye

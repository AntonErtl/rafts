hex
\ true constant dd
false constant dd

: foo0 ( n n -- )
  , , ;

: foo1 ( addr -- n n )
  dup cell+ @ swap @ ;

: str
  create
    , ,
  does>
    dup cell+ @
    swap @
    ;

?runtest [IF]
1 2 str x
[THEN]
dd [IF]
' x >name $18 dump
' x $10 over + disasm_dump
[THEN]
?runtest [IF]
x .s . . cr
: f
  1 2 + x ;
f . . . cr
[THEN]

1 2
create xx
  , ,
does>
  dup cell+ @
  swap @
  ;
?runtest [IF]
xx . . cr
[THEN]

create xxx
  3 , 4 ,
dd [IF]
' xxx >name $18 dump
' xxx $10 over + disasm_dump
.s cr
[THEN]

bye

: foo0 ( -- )
  ." Das ist auch ein Test00" cr ;

: foo1 ( -- )
  10 0 do
    ." Das ist ein LOOP" cr loop ;

: foo2 ( -- )
  s" Das ist auch ein Test00" type cr ;

?runtest [IF]
." Das ist ein Test" cr
s" Das ist ein Test" type cr
foo0
foo1
foo2
[THEN]

create x
  1 , 2 ,
does>
  dup hex.
  dup @ swap cell+ @ + ;

' x here over - dump

: foo3 ( -- )
  ." foo3:" ['] x hex. x . ;

?runtest [IF]
x . cr
foo3 cr
[THEN]

false [IF]
: foo4 ( -- )
  ." foo4:" 1 2 3 4 @beq . ;
[THEN]

: foo5 ( addr -- )
  26 0 ?do
    i over + i $61 + swap c! loop
  drop ;

: foo6 ( addr -- )
  26 0 ?do
    i over + c@ emit loop
  drop ;

create xx
  26 chars allot

?runtest [IF]
false [IF]
foo4 cr
[THEN]
xx foo5 cr
xx foo6 cr
[THEN]

' xx here over - dump

: foo7
  26 0 ?do
    i .
    i 13 = abort" SHIT" loop ;

?runtest [IF]
false [IF]
foo7 cr
[THEN]
[THEN]

: foo8 ( -- n )
  [char] a ;

?runtest [IF]
foo8 emit cr
[THEN]

bye

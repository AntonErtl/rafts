\ Test for conditional branches
\ Test for if
: if0 ( flag -- n )
  123 456 + swap if
    321 654 + swap + else
    456 - endif
  123 456 + + ;

: if1 ( flag -- n )
  123 456 + swap if
    321 654 + swap + endif
  123 456 + + ;

: if2 ( flag -- n n n )
  1 swap if
    2 else
    3 endif
  4 ;

: if3 ( flag -- n n n | n n )
  1 swap if
    2 endif
  3 ;

\ Test for case
: case0 ( n -- )
  case
    0 of
      0 . endof
    1 of
      10 . endof
    >r 20 . r>
  endcase ;

?runtest [IF]
true if0 . cr
false if0 . cr
true if1 . cr
false if1 . cr
true if2 . . . cr
false if2 . . . cr
true if3 . . . cr
false if3 . . cr
0 case0 cr
1 case0 cr
2 case0 cr
[THEN]

\ Test for conditional loops
\ Test for until
: until0 ( -- n )
  10 begin
    1- dup . dup 0 < until ;

: until1 ( n -- n )
  begin
    1- dup . dup 0 < until ;

\ Test for while
: while0 ( -- n )
  0 begin
    1+ dup 10 < while
    dup . repeat ;

: while1 ( n -- n )
  0 begin
    1+ 2dup swap < while
    dup . repeat
  swap drop ;

\ Test for again
: again0 ( -- n )
  10 begin
    1- dup 0 < if
      exit endif
    dup . again ;

: again1 ( n -- n )
  begin
    1- dup 0 < if
      exit endif
    dup . again ;

?runtest [IF]
until0 . cr
10 until1 . cr
while0 . cr
10 while1 . cr
again0 . cr
10 again1 . cr
[THEN]

\ Test for counted loops
: do0 ( -- )
  10 0 do
    i . loop ;

: do1 ( n n -- )
  do
    i . loop ;

: do2 ( n n -- )
  do
    20 15 do
      i . j . loop
    cr loop ;

: do3 ( n n -- )
  do
    20 15 do
      i 17 = if
        leave endif
      i . j . loop
    cr loop ;

: do4 ( n n -- )
  do
    i 7 = if
      leave endif
    20 15 do
      i 17 = if
        leave endif
      i . j . loop
    cr loop ;

: ?do0 ( -- )
  10 0 ?do
    i . loop ;

: ?do1 ( n n -- )
  ?do
    i . loop ;

: ?do2 ( n n -- )
  ?do
    20 15 ?do
      i . j . loop
    cr loop ;

: ?do3 ( n n -- )
  ?do
    20 15 ?do
      i 17 = if
        leave endif
      i . j . loop
    cr loop ;

: ?do4 ( n n -- )
  ?do
    i 7 = if
      leave endif
    20 15 ?do
      i 17 = if
        leave endif
      i . j . loop
    cr loop ;

: +loop0 ( n -- )
  10 0 ?do
    i . dup +loop ;

: +loop1 ( n n n -- )
  ?do
    i . dup +loop ;

: for0 ( -- )
  10 for
    i . next ;

: for1 ( n -- )
  for
    i . next ;

?runtest [IF]
do0
10 0 do1
10 0 do2
10 0 do3
10 0 do4
?do0
0 0 ?do1
10 0 ?do1
10 0 ?do2
10 0 ?do3
10 0 ?do4
2 +loop0 .
2 10 0 +loop1 .
3 10 0 +loop1 .
-2 0 10 +loop1 .
-3 0 10 +loop1 .
for0
0 for1
1 for1
2 for1
[THEN]

\ Test for immediate, postpone
: immediate_postpone0 ( -- )
  1 . ;

: immediate_postpone1 ( -- )
  2 . immediate_postpone0 ; immediate

: immediate_postpone2 ( -- )
  postpone immediate_postpone0
  postpone chars
  postpone .
  postpone immediate_postpone1 ; immediate

?runtest [IF]
: immediate_postpone3 ( -- )
  immediate_postpone2 ;

immediate_postpone0
immediate_postpone1
3 immediate_postpone3
[THEN]

\ Test for recurse
: recurse2 ( n -- )
  dup 1 <> if
    dup . 1- recurse 0 . endif ;

?runtest [IF]
10 recurse2 .
[THEN]

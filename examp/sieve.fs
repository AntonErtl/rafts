#! /usr/stud/paysan/bin/forth

DECIMAL
: SECS TIME&DATE  2DROP DROP  60 * + 60 * + ;
CREATE FLAGS $4000 ALLOT
FLAGS $4000 + CONSTANT EFLAG

: PRIMES  ( -- n )  FLAGS $4000 1 FILL  0 3  EFLAG FLAGS
  DO   I C@
       IF  DUP I + DUP EFLAG <
           IF    EFLAG SWAP
                 DO  0 I C! DUP  +LOOP
           ELSE  DROP  THEN  SWAP 1+ SWAP
           THEN  2 +
       LOOP  DROP ;

: BENCHMARK  0 100 0 DO  PRIMES NIP  LOOP ;

?runtest [IF]
SECS BENCHMARK . SECS SWAP - CR . .( secs)
[THEN]

\ HPPA/720, 50 MHz: user 3.90s

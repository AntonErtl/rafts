variable v0
variable v1
123 constant c0

: foo0 ( -- n n )
  v0 @
  v1 @ swap ;

: foo1 ( -- n )
  1 1 v0 !
  2 + ;

: foo2 ( -- n n )
  v0 @
  2 v0 !
  v0 @ ;

?runtest [IF]
$01020304 v0 !
$04030201 v1 !
' v0 cell+ cell+ dup hex. @ hex. cr

foo0 hex. hex. cr
foo1 . cr
' v0 cell+ cell+ dup hex. @ hex. cr

foo2 hex. hex. cr
[THEN]

: <<
    :noname ;
: >>
    POSTPONE ; execute ; immediate

: !! ( f -- )
    0= depth 1 <> or if
	sourcefilename type ." :"
	sourceline# . ." :" .s true abort" result wrong"
    endif ;

<< 0 >> 0= !!
<< 1 >> 1 = !!
<< 5 >> 5 = !!
<< -1 >> -1 = !!
<< $fffff >> $fffff = !!
create x 1 c, 2 c, 3 c, 4 c, 5 c, 6 c, 7 c, 8 c, $12345678 ,
x << @ >> x @ = !!
x << c@ >> x c@ = !!
x << 4 + @ >> x 4 + @ = !!
x << 1 + c@ >> x 1 + c@ = !!
x << 4 swap + @ >> x 4 swap + @ = !!
x << 2 swap + c@ >> x 2 swap + c@ = !!
1 2 << + >> 3 = !!
1 << 2 + >> 3 = !!
1 << 2 swap + >> 3 = !!
3 1 << - >> 2 = !!
3 << 1 - >> 2 = !!
1 << 3 swap - >> 2 = !!
3 5 << * >> 15 = !!
7 3 << / >> 2 = !!
-7 -3 << / >> 2 = !!
7 3 << mod >> 1 = !!
1 << negate >> -1 = !!

3 5 << and >> 1 = !!
3 << 5 and >> 1 = !!
3 << 5 swap and >> 1 = !!
3 5 << or >> 7 = !!
3 << 5 or >> 7 = !!
3 << 5 swap or >> 7 = !!
3 5 << xor >> 6 = !!
3 << 5 xor >> 6 = !!
3 << 5 swap xor >> 6 = !!
5 << invert >> 5 invert = !!
3 5 << lshift >> 3 5 lshift = !!
3 << 5 lshift >> 3 5 lshift = !!
5 << 3 swap lshift >> 3 5 lshift = !!
5 << 2* >> 10 = !!
$aa 3 << rshift >> $aa 3 rshift = !!
$aa << 2/ >> $55 = !!
-$aa 3 << rshifta >> -22 = !!
$aa << 3 rshift >> $aa 3 rshift = !!
-$aa << 3 rshifta >> -22 = !!
3 << $aa swap rshift >> $aa 3 rshift = !!
3 << -$aa swap rshifta >> -22 = !!

1 2 << < >> !!
-1 0 << < >> !!
2 1 << < >> 0= !!
0 -1 << < >> 0= !!
1 << 2 < >> !!
1 << 2 swap < >> 0= !!
1 2 << u< >> !!
-1 0 << u< >> 0= !!
-1 << 0 u< >> 0= !!
1 << 2 u< >> !!
2 << 1 swap u< >> !!

5 x << c! >> x c@ 5 = !!
6 x << 1 + c! >> x 1 + c@ 6 = !!
1000 x << ! >> x @ 1000 = !!
2000 x << 4 + ! >> x 4 + @ 2000 = !!

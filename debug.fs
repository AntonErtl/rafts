\ debug.fs	print all possible debug informations
\
\ Copyright (C) 1995-97 Martin Anton Ertl, Christian Pirker
\
\ This file is part of RAFTS.
\
\	RAFTS is free software; you can redistribute it and/or
\	modify it under the terms of the GNU General Public License
\	as published by the Free Software Foundation; either version 2
\	of the License, or (at your option) any later version.
\
\	This program is distributed in the hope that it will be useful,
\	but WITHOUT ANY WARRANTY; without even the implied warranty of
\	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\	GNU General Public License for more details.
\
\	You should have received a copy of the GNU General Public License
\	along with this program; if not, write to the Free Software
\	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

0 constant il-print-flag
1 constant ml-print-flag
variable print-flag
il-print-flag print-flag !

: (print-.-) ( flag -- )
    if
	." -"
    else
	." _"
    endif ;

: (print-.name) ( flag -- )
    print-flag @ il-print-flag = if
	." il"
    else
	." ml" print-flag @ 0 0 d.r
    endif
    dup (print-.-)
    lastnfa
    name>string 0 ?do
	2dup c@
	swap if
	    emit
	else
	    dup '- = if
		drop '_ emit
	    else
		dup 'a 'z 1+ within
		over 'A 'Z 1+ within or
		over '0 '9 1+ within or
		over '_ = or if
		    emit
		else
		    0 0 d.r
		endif
	    endif
	endif
	char+
    loop
    drop
    (print-.-) ;

: (print-name) ( flag -- )
    dup (print-.name) basic-block @ 0 0 d.r (print-.-) ;

: print-name ( -- )
    true (print-name) ;

: print-cname ( -- )
    false (print-name) ;

: print-number ( n -- )
    [ 2 cells ] literal swap hexn. ;

: ?print-number ( addr -- )
    @ print-number ;

: print-number-alone ( n -- )
    [ 2 cells ] literal swap (hexn.) ;

: ?print-number-alone ( addr -- )
    @ print-number-alone ;

: print-bool ( n -- )
    if
	." true"
    else
	." false"
    endif ;

: ?print-bool ( addr -- )
    @ print-bool ;

: print-register ( n -- )
    dup regs-unallocated = if
	drop
	." unallocated"
    else
	2 swap hexn.
    endif ;

: ?print-register ( addr -- )
    @ print-register ;

: print-node-name ( addr -- )
    print-cname hexnum. ;

: print-edge ( addr addr -- )
    dup if
	print-cname hexnum.
	." -> "
	print-cname hexnum. ." ;" cr
    else
	2drop
    endif ;

: print-edge-dotted ( addr addr -- )
    dup if
	print-cname hexnum.
	." -> "
	print-cname hexnum. ." [ style = dotted ];" cr
    else
	2drop
    endif ;

: il-print-depends-func ( il inst -- il )
    over swap inst-node @ print-edge-dotted cr ;

: il-print-depends ( il inst -- il )
    ?dup if
	['] il-print-depends-func swap slist-forall
    endif ;

: il-print ( il -- )
    dup il-struct %size dump
    dup dup il-left @ print-edge
    dup dup il-right @ print-edge

    dup print-node-name
    ." [ "
    ." label = " '" emit
    dup burm-OP-LABEL @ burm-opname
    ." (" dup il-slabel ?print-number ." )\l"
    ." [" dup print-cname print-number-alone ." ]\l"
    ." val: " dup il-val ?print-number ." \l"
    ." reg: " dup il-reg ?print-register ." \l"
    '" emit
    ."  ];" cr
    dup il-depends @ il-print-depends
    drop ;

: il-print-all ( il -- )
    ['] il-print swap btree-postorder cr ;

: ml-print-rule ( cfa -- )
    rafts->head cell+
    count $1f and cell- swap cell+ swap snumber? if
	burm-string
    else
	true burm-assert" invalid rule"
    endif ;

defer ml-print-all

: ml-print-depends-func ( ml inst -- ml )
    inst-node @
    dup ml-count @ 1 = if
	dup ml-print-all
    endif
    over swap print-edge-dotted cr ;

: ml-print-depends ( ml inst-addr -- ml )
    ?dup if
	['] ml-print-depends-func swap slist-forall
    endif ;

: ml-print ( ml -- )
    dup ml-struct %size dump
    dup dup ml-left @ print-edge
    dup dup ml-right @ print-edge

    dup print-node-name
    ." [ "
    ." label = " '" emit
    dup ml-asm @ ml-print-rule ." \l"
    ." [" dup print-cname print-number-alone ." ]\l"
    ." count: " dup ml-count ?print-number ." \l"
    ." val: " dup ml-val ?print-number ." \l"
    ." reg: " dup ml-reg ?print-register ." \l"
    ." delay: " dup ml-delay ?print-bool ." \l"
    ." latency: " dup ml-latency ?print-number ." \l"
    ." pathlength: " dup ml-pathlength ?print-number ." \l"
    ." let: " dup ml-let ?print-number ." \l"
    '" emit
    ."  ];" cr
    dup ml-depends @ ml-print-depends
    drop ;

: print-edge-asm-ml ( addr -- )
    dup if
	print-cname dup hexnum.
	." -> "
	1 print-flag !
	print-cname
	3 print-flag !
	hexnum. ." ;" cr
    else
	2drop
    endif ;

: ml-asm-print ( ml ml -- )
    dup ml-struct %size dump
    swap ?dup if
	over swap print-edge
    endif
    dup print-edge-asm-ml

    dup print-node-name
    ." [ "
    ." label = " '" emit
    dup ml-asm @ ml-print-rule ." \l"
    ." [" dup print-cname print-number-alone ." ]\l"
    ." count: " dup ml-count ?print-number ." \l"
    ." val: " dup ml-val ?print-number ." \l"
    ." reg: " dup ml-reg ?print-register ." \l"
    ." delay: " dup ml-delay ?print-bool ." \l"
    ." latency: " dup ml-latency ?print-number ." \l"
    ." pathlength: " dup ml-pathlength ?print-number ." \l"
    ." let: " dup ml-let ?print-number ." \l"
    '" emit
    ."  ];" cr ;

:noname ( il -- )
    ['] ml-print swap btree-postorder cr ;
is  ml-print-all

: inst-ils-print ( -- )
    ." BTREE PRINT" hex.s cr
    il-print-flag print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " '" emit print-name stamp '" emit ." ;" cr
    ['] il-print-all 0 inst-ils inst-ils-end @ inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: inst-mls-print ( -- )
    ." NODE PRINT" hex.s cr
    ml-print-flag print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " '" emit print-name stamp '" emit ." ;" cr
    ['] ml-print-all 0 inst-mls inst-mls-end @ inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: inst-lists-print ( -- )
    ." LISTS PRINT" hex.s cr
    ml-print-flag 2 + print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " '" emit print-name stamp '" emit ." ;" cr
    NIL ['] ml-asm-print 0 inst-lists
    inst-lists-end @ 1+ tuck cells + inst-size rot - inst-sequence drop
    ." /* cluster_" print-cname ." */ }" cr ;

?test $0010 [IF]
cr ." Test for debug.fs" cr

finish
[THEN]

base @ hex
." START: " here . cr
decimal

\ compiler.fs	compiler main load file
\
\ Copyright (C) 1995-96 Martin Anton Ertl, Christian Pirker
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

vocabulary voc-source
\ basically the host-residing words, but may be called from target words

voc-source also definitions

vocabulary voc-target
\ all words compiled by the target, and a few more

include options.fs

$0000 constant word-good
$0001 constant word-bad

variable word-regs-depth
variable word-regs-current
variable word-regs-flag

?shared wword-regs-adjust [IF]
: wword-regs-print ( -- )
    ;

: wword-regs-adjust ( regs-out regs-in -- )
    2drop ;
[THEN]

: word-regs-put ( regs-out regs-in regs-flag -- )
    word-regs-flag !
    dup word-regs-depth !
    + word-regs-current ! ;

: word-regs-get ( -- regs-out regs-in regs-flag )
    word-regs-depth @ dup
    word-regs-current @ swap - swap
    word-regs-flag @ ;

: word-regs-init ( -- )
    0 0 0 word-regs-put ;

: word-regs-print ( -- )
    ." word-regs: "
    word-regs-get . . . cr ;

: word-regs-adjust ( regs-out reg-in regs-flag -- )
    word-regs-flag @ or word-regs-flag !
    word-regs-current +!
    word-regs-depth @ word-regs-current @ tuck < if
	drop
    else
	word-regs-depth !
    endif
    word-regs-current +! ;

: word-regs-max ( x1 x2 x3 x4 -- x5 x6 )
    2over 2over nip rot drop < if
	2drop
    else
	2nip
    endif ;

include stdlib/stdlib.fs

: vsource ( -- )
    voc-source ; immediate
: >source ( -- )
    also voc-source definitions previous ;
: source> ( -- )
    voc-source also ;

: vtarget ( -- )
    voc-target ; immediate
: >target ( -- )
    also voc-target definitions previous ;
: target> ( -- )
    >target get-current 1 set-order also ;

>target
' voc-source alias vvv
>source

: gforth-compile, ( xt -- )
    , ;

: 'Forth ( "name" -- cfa )
    also Forth ' previous ; immediate
: ['Forth] ( "name" -- cfa )
    also Forth ' previous postpone literal ; immediate compile-only
: (')Forth ( "name" -- nfa )
    also Forth (') previous ; immediate

struct
    2 cells: field ih-cfa
    1 cells: field ih-interpreter
    1 cells: field ih-compiler
    1 cells: field ih-regs-in
    1 cells: field ih-regs-out
    1 cells: field ih-regs-flag
    1 cells: field ih-compile-xt
    1 cells: field ih-xt-addr
    1 cells: field ih-#xt
    1 cells: field ih-#access
    4 cells: field ih-buffer1
    1 cells: field ih-does-xt
    1 cells: field ih-null
    1 cells: field ih-status
end-struct ih-struct
ih-struct drop constant ih-cfsize
ih-cfsize 2cells - constant ih-size

defer compile,-constant-gforth
defer compile,-variable-gforth
defer compile,-user-gforth
defer compile,-field-gforth
defer compile,-defer-gforth
defer compile,-interpreter

defer compile,-constant
defer compile,-2constant
defer compile,-variable
defer compile,-user
defer compile,-field
defer compile,-native
defer compile,-does

variable noname-state
noname-state off

2variable branch-info \ contains the address of the last assembled branch
		\ and the method ( target-addr branch-addr -- ) for patching it
		\ so, for patching the last branch to jump to x one would do:
		\ x branch-info 2@ execute

variable (lastih)
: lastih ( -- addr )
    (lastih) @ ;

: lastih-init ( -- )
    here (lastih) ! ;

variable (lastnfa)
: lastnfa ( -- addr )
    (lastnfa) @ ;

: lastnfa-init ( -- )
    lastih look drop (lastnfa) ! ;

\ variables for compile-xt stack
$40 constant xt-size
variable xt-tos
\ compile-xt stack pointer
xt-size array xt-data

\ functions for handling the compile-xt stack
: #xt@ ( n -- x )
    xt-data @ ;

: #xt! ( x n -- )
    xt-data ! ;

: >xt ( x -- ) ( xt: -- x )
    xt-tos @ #xt!
    1 xt-tos +! ;

: xt> ( -- x ) ( xt: x -- )
    -1 xt-tos +!
    xt-tos @  #xt@ ;

: .xt ( -- )
    ." <xt:" xt-tos @ 0 .r ." > "
    xt-tos @ 0 ?do
	i #xt@ hex.
    loop ;

: xt-init ( -- )
    0 xt-tos !
    0 xt-data xt-size cells NULL fill ;

: xt-write ( cfa -- )
    here over ih-xt-addr !
    xt-tos @ tuck swap ih-#xt !
    0 ?do
	i #xt@ ,
    loop ;

: compile,-nativext ( xt -- xt )
    ?trace $0001 [IF]
	dup hex.
	dup name.
    [THEN]
    dup >xt
    1 over ih-#access +! ;

: compile,-nonativext ( xt -- xt )
    ;

include machine/asm.fs
include machine/disasm.fs
include basic.fs

: compile,-now ( xt -- )
    ?trace $0001 [IF]
	\ ~~
	\ dup >name .name
	\ dup $10 - $40 dump
    [THEN]
    dup ih-compiler @ execute ;

include primitives.fs
include control.fs

>target
word-bad 0 -1 also vtarget :word execute previous
>source

:noname ( xt -- )
    ?trace $0001 [IF]
	." constant (gforth): " dup name. hex.s cr
    [THEN]
    2cells + @ execute compile,-literal ;
is compile,-constant-gforth

:noname ( xt -- )
    ?trace $0001 [IF]
	." [2]variable (gforth): " dup name. hex.s cr
    [THEN]
    2cells + @ execute compile,-literal ;
is compile,-variable-gforth

:noname ( xt -- )
    ?trace $0001 [IF]
	." [2]user (gforth): " dup name. hex.s cr
    [THEN]
    2cells + @ execute compile,-literal ;
is compile,-user-gforth

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-field (gforth): " dup name. hex.s cr
    [THEN]
    2cells + @
    2cells + @ compile,-literal 0 compile-+ ;
is compile,-field-gforth

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-defer (gforth): " dup name. hex.s cr
    [THEN]
    basic-exit
    2cells + @ word-interpreter
    basic-init ;
is compile,-defer-gforth

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-interpreter: " dup name. hex.s cr
    [THEN]
    basic-exit
    2cells + @ word-interpreter
    basic-init ;
is compile,-interpreter

:noname ( xt -- )
    ?trace $0001 [IF]
	." constant: " dup name. hex.s cr
    [THEN]
    ih-cfsize + @ compile,-literal ;
is compile,-constant

:noname ( xt -- )
    ?trace $0001 [IF]
	." 2constant: " dup name. hex.s cr
    [THEN]
    dup ih-cfsize + cell+ @ compile,-literal
    ih-cfsize + @ compile,-literal ;
is compile,-2constant

:noname ( xt -- )
    ?trace $0001 [IF]
	." [2]variable: " dup name. hex.s cr
    [THEN]
    ih-cfsize + compile,-literal ;
is compile,-variable

:noname ( xt -- )
    ?trace $0001 [IF]
	." [2]user: " dup name. hex.s cr
    [THEN]
    ih-cfsize + compile,-literal ;
is compile,-user

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-field: " dup name. hex.s cr
    [THEN]
    ih-cfsize + @ compile,-literal 0 compile-+ ;
is compile,-field

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-native: " dup name. hex.s cr
    [THEN]
    NIL I_CALL terminal inst-btrees-insert-end
    basic-exit
    basic-init ;
is compile,-native

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-native (does>): " hex.s cr
    [THEN]
    dup ih-cfsize + compile,-literal
    ih-does-xt @ NIL I_CALL terminal inst-btrees-insert-end
    basic-exit
    \ dup @ 2 lshift $1a asm-bitmask and swap $1a asm-bitmask invert and or
    \ ih-cfsize + word-call
    basic-init ;
is compile,-does

: compile, ( xt -- )
    ?trace $0001 [IF]
	\ ~~
	dup hex.
	dup >name .name
	\ dup $10 - $40 dump
    [THEN]
    dup ih-compile-xt @ execute
    dup word-regs-read
    ?trace $0001 [IF]
	\ word-regs-print
    [THEN]
    [ also Forth ' lit previous ] literal gforth-compile, ,
    ['] compile,-now gforth-compile, ;

: wword-regs-print ( -- )
    word-regs-print ;
: wword-regs-adjust ( regs-out regs-in -- )
    word-good word-regs-adjust ;

>target
word-good 0 0 also vtarget :word compile, previous
>source

: name>comp ( nt -- w xt ) \ gforth
    \ @var{w xt} is the compilation token wor the word @var{nt}.
    (name>x) >r dup interpret/compile? if
	interpret/compile-comp @
    endif
    r> immediate-mask and if
	['] execute
    else
	vtarget ['] compile, vsource
    endif ;

: vlist ( wid -- )
    body>
    ." Vocabulary: "
    dup name.
    >body
    begin
	@ dup
    while
	dup name>int hex.
	dup .name
	dup (name>x) swap ." ( " dup >code-address case
	    docon: of
	    ." gforth (docon) " endof
	    dovar: of
	    ." gforth (dovar) " endof
	    douser: of
	    ." gforth (douser) " endof
	    dofield: of
	    ." gforth (dofield) " endof
	    dodefer: of
	    ." gforth (dodefer) " endof
	    docol: of
	    ." gforth (docol) " endof
	    docode: of
	    ." docode "
	    \ dup $20 - $20 ih-cfsize + dump
	    dup ih-xt-addr @
	    over ih-#xt @ 0 ?do
		dup @ hex.
		cell+
	    loop
	    drop
	    endof
	    dodata: of
	    ." dodata "
	    \ dup $20 - $20 ih-cfsize + dump
	    endof
	    ." gforth "
	endcase
	drop ." )"
	dup $40 and if
	    ." [imm]"
	endif
	$20 and if
	    ." [conly]"
	endif
	cr
    repeat
    drop cr ;

: >body ( cfa -- pfa )
    dup >code-address case
	docode: of
	dup 2cells + @ dup
	>code-address dodefer: = if
	    nip 2cells
	else
	    drop
	    ih-cfsize
	endif
	endof
	dodata: of
	ih-cfsize
	endof
	docol: of
	2cells
	endof
	dup >r 2cells r>
    endcase
    + ;

: body> ( pfa -- cfa )
    2cells - dup @ 0= if
	ih-size -
    endif ;

: replace-word ( xt nfa -- )
    dup ((name>)) swap
    cell+ c@ alias-mask and if
	\ replace word at cfa with xt. !! This is quite general-purpose
	\ and should migrate elsewhere.
	dodefer: over code-address!
	\ >body
	8 +                            \ bugfix! always the GFORTH offset
	!
    else
	!
    endif ;
>target

also vtarget
word-good 0 0 :word bye
word-good 0 0 :word also
word-good 0 0 :word previous
word-good 0 0 :word Forth
word-good 0 0 :word vsource
word-good 0 0 :word vtarget
previous

?test $0001 [IF]
cr ." Test for compiler.fs" cr

finish
[THEN]

\ BBBUUUGGGYYY !!!
' also name>comp 2drop
' also >body body> drop

also
vsource     ' name>comp    (')Forth name>comp replace-word
vsource     ' body>        (')Forth body>     replace-word
vsource     ' >body        (')Forth >body     replace-word
vtarget     ' compile,     (')Forth compile,  replace-word
vsource     ' postpone,    (')Forth postpone, replace-word
vtarget comp' literal drop (')Forth literal   replace-word
previous

target>

also vsource also Forth

\ gforth word useable in target
\ word-flag out-regs in-regs
 word-good 1 0 :word true
 word-good 1 0 :word false
 word-good 1 0 :word bl
 word-good 1 0 :word base
 word-good 1 0 :word last
 word-good 1 0 :word state
 word-good 0 0 :word printdebugdata
 word-good 0 -1 :word .name
 word-good 1 -2 :word name>string
 word-good 0 0 :word .s
 word-good 0 -2 :word .r
 word-good 0 -1 :word ,
 word-good 0 0 :word ;s
 word-good 0 -2 :word 2,
 word-good 1 0 :word '
 word-good 0 0 :word (')
word-good 0 0 :word "error
 word-good 0 0 :word [
 word-good 0 0 :word ]
 word-good 0 0 :word \
 word-good 0 0 :word (
 word-good 0 0 :word <#
 word-good 1 -1 :word >body
 word-good 1 -1 :word body>
word-good 0 0 :word >code-address
word-good 0 0 :word >does-code
word-good 0 0 :word >in
word-good 0 0 :word >number
word-good 0 0 :word >name
word-good 0 0 :word ((name>))
 word-good 2 -1 :word (name>x)
 word-good 2 -2 :word #
 word-good 2 -2 :word #s
 word-good 2 -2 :word #>
 word-good 1 0 :word #bs
 word-good 1 0 :word #cr
 word-good 1 0 :word #ff
 word-good 1 0 :word #lf
word-good 0 0 :word /mod
word-good 0 0 :word */
word-good 0 0 :word */mod
 word-good 0 -1 :word a,
 word-good 1 -1 :word abs
 word-good 0 -1 :word allot
word-good 0 0 :word align
word-good 0 0 :word alias
 word-good 1 0 :word alias-mask
\ word-good 0 0 :word also
word-good 0 0 :word aligned
word-good 0 0 :word assert-level
 word-good 0 -1 :word c,
word-good 0 0 :word cells:
word-good 0 0 :word char
word-good 0 0 :word cfa,
word-good 0 0 :word code-address!
word-good 0 0 :word comp'
word-good 0 0 :word compile-only
word-good 0 0 :word context
 word-good 2 -1 :word count
word-good 0 0 :word decimal
word-good 0 0 :word definitions
 word-good 1 0 :word depth
word-good 0 0 :word dp
 word-good 1 0 :word docon:
 word-good 1 0 :word docol:
 word-good 1 0 :word dodefer:
 word-good 1 0 :word dofield:
 word-good 1 0 :word douser:
 word-good 1 0 :word dovar:
 word-good 0 -2 :word dump
 word-good 0 -2 :word d.
 word-good 0 -2 :word d.r
 word-good 0 -1 :word emit
word-good 0 0 :word erase
word-good 0 0 :word evaluate
word-good 0 0 :word find
word-good 0 0 :word find-name
word-good 0 0 :word fill
word-good 0 0 :word flush-icache
word-good 0 0 :word fm/mod
 word-good 0 0 :word forth
word-good 0 0 :word forthstart
word-good 0 0 :word get-current
 word-good 0 0 :word header
 word-good 1 0 :word here
 word-good 0 0 :word hex
 word-good 0 -1 :word hold
 word-good 0 0 :word include
word-good 0 0 :word included-files
 word-good 0 0 :word immediate
 word-good 1 0 :word immediate-mask
word-good 0 0 :word interpret/compile?
word-good 0 0 :word interpret/compile-comp
word-good 0 0 :word loadfilename#
 word-good 1 0 :word lastcfa
 word-good 1 0 :word lastxt
 word-good 1 0 :word lit
 word-good 1 0 :word literal
 word-good 1 0 :word look
 word-good 1 -2 :word max
 word-good 1 0 :word maxdepth-.s
 word-good 1 -2 :word min
word-good 0 0 :word move
word-good 0 0 :word m*
word-good 0 0 :word nalign
 word-good 2 0 :word name
word-good 0 0 :word name>comp
word-good 0 0 :word name>int
 word-good 0 0 :word noop
word-good 0 0 :word nothing
word-good 0 0 :word order
word-good 0 0 :word parse
word-good 0 0 :word place
word-good 0 0 :word postpone
 word-good 1 0 :word restrict-mask
 word-good 0 0 :word reveal
 word-good 0 0 :word root
 word-good 1 0 :word rp@
 word-good 1 0 :word r0
 word-good 0 0 :word savesystem
word-good 0 0 :word see
word-good 0 0 :word set-order
word-good 0 0 :word sfind
word-good 0 0 :word sign
 word-bad 1 -2 :word snumber?
word-good 0 0 :word sourcefilename
word-good 0 0 :word sourceline#
 word-good 1 0 :word sp@
 word-good 1 0 :word s0
 word-good 0 0 :word space
 word-good 0 -1 :word spaces
word-good 0 0 :word struct-allot
 word-good 2 -1 :word s>d
word-good 0 0 :word sm/rem
word-good 0 0 :word source
word-good 0 0 :word threading-method
word-good 0 0 :word throw
 word-good 0 -1 :word u.
word-good 0 0 :word um/mod
word-good 0 0 :word um*
word-good 0 0 :word within
word-good 0 0 :word word
word-good 0 0 :word wordlist
word-good 0 -1 :word [IF]
word-good 0 0 :word [ELSE]
word-good 0 0 :word [ENDIF]
word-good 0 0 :word [THEN]

 word-bad 1 -1 :word ?dup

previous

\ rafts word useable in target
 word-good 1 0 :word [']
 word-good 1 0 :word s"
 word-good 0 0 :word ."
 word-good 0 -1 :word abort"
 word-good 0 0 :word ~~
 word-good 0 0 :word :
 word-good 0 0 :word :noname
 word-good 0 0 :word ;
 word-good 0 0 :word ?trace
 word-good 0 -1 :word constant
 word-good 0 0 :word create
 word-good 0 -2 :word disasm-dump
 word-good 1 -1 :word field
 word-good 0 0 :word finish
 word-good 0 -1 :word hex.
 word-good 0 0 :word hex.s
 word-good 0 0 :word hex.rs
 word-good 0 0 :word text-print
 word-good 0 -1 :word vlist
 word-good 0 0 :word variable

 word-good 0 0 :word wword-regs-print
 word-good 0 -3 :word wword-regs-adjust

previous

: . ( n -- )
    s>d d. ;

: cr ( -- )
    #lf emit ;

: vocabulary ( "name" -- )
    create wordlist drop
does>
    context ! ;

: defer ( xt "name" -- )
    create 0 ,
does>
    @ execute ;

: is ( xt "name" -- )
    ' >body ! ;

: bye ( -- )
    finish
    ?trace $4000 [IF]
	depth 0 ?do
	    . loop
	cr
    [THEN]
    ?trace $4000 [IF]
	\ vsource ['] voc-target vtarget >body vlist
	\ vsource ['] vvv vtarget >body vlist
    [THEN]
    bye ;

\ hex.s cr
base !

\ ." START AGAIN: " here hex. cr

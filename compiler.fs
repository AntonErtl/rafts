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

: 'Forth ( "name" -- )
    also Forth (') previous ; immediate

defer compile,-defer
defer compile,-field
defer compile,-interpreter
defer compile,-native
defer compile,-does
defer compile-native
variable dostruc
variable noname-state
false noname-state !

: compile,-constant-gforth ( xt -- )
    ?trace $0001 [IF]
	." constant (gforth): " dup name. hex.s cr
    [THEN]
    2cells + @ execute postpone literal ;

: compile,-variable-gforth ( xt -- )
    ?trace $0001 [IF]
	." [2]variable (gforth): " dup name. hex.s cr
    [THEN]
    2cells + @ execute postpone literal ;

: compile,-user-gforth ( xt -- )
    ?trace $0001 [IF]
	." [2]user (gforth): " dup name. hex.s cr
    [THEN]
    2cells + @ execute postpone literal ;

: compile,-constant ( xt -- )
    ?trace $0001 [IF]
	." constant: " dup name. hex.s cr
    [THEN]
    dup 2cells + @ execute postpone literal ;

: compile,-2constant ( xt -- )
    ?trace $0001 [IF]
	." 2constant: " dup name. hex.s cr
    [THEN]
    dup 2cells + @ execute swap postpone literal postpone literal ;

: compile,-variable ( xt -- )
    ?trace $0001 [IF]
	." [2]variable: " dup name. hex.s cr
    [THEN]
    dup 2cells + @ execute postpone literal ;

: compile,-user ( xt -- )
    ?trace $0001 [IF]
	." [2]user: " dup name. hex.s cr
    [THEN]
    dup 2cells + @ execute postpone literal ;

include machine/asm.fs
include machine/disasm.fs
include basic.fs

include primitives.fs
include control.fs

: (compile,-field) ( xt -- )
    ?trace $0001 [IF]
	." compile,-field: " dup name. hex.s cr
    [THEN]
    2cells + info-head-size + @ postpone literal vtarget postpone + vsource ;
' (compile,-field) is compile,-field

: (compile,-interpreter) ( xt -- )
    ?trace $0001 [IF]
	." compile,-interpreter: " dup name. hex.s cr
    [THEN]
    basic-exit
    2cells + @
    word-interpreter
    basic-init ;
' (compile,-interpreter) is compile,-interpreter

: (compile,-defer) ( xt -- )
    ?trace $0001 [IF]
	." compile,-defer: " dup name. hex.s cr
    [THEN]
    2cells + @ postpone literal
    basic-exit
    ['] execute word-interpreter
    basic-init ;
' (compile,-defer) is compile,-defer

: (compile,-native) ( xt -- )
    ?trace $0001 [IF]
	." compile,-native: " dup name. hex.s cr
    [THEN]
    dup $10 - $40 dump
    2cells + @
    compile-native ;
' (compile,-native) is compile,-native

: (compile-native) ( xt -- )
    ?trace $0001 [IF]
	." compile-native: " dup name. hex.s cr
    [THEN]
    basic-exit
    word-native
    basic-init ;
' (compile-native) is compile-native

: (compile,-does) ( xt -- )
    ?trace $0001 [IF]
	." compile,-native (does>): " hex.s cr
    [THEN]
    dup 2cells + info-head-size + postpone literal basic-exit
    dup @ 2 lshift $1a asm-bitmask and swap $1a asm-bitmask invert and or
    word-native
    basic-init ;
' (compile,-does) is compile,-does

: compile, ( xt -- )
    dup 3cells + @ execute ;

>target

also vtarget :word compile, previous

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
    ." Vocabulary: " dup name. dup hex. cr
    >body begin
	@ dup 0<>
    while
	dup .name
	dup (name>x) swap ." ( " dup >code-address case
	    docode: of
	    ." docode " endof
	    ." gforth "
	endcase
	>body hex. ." )"
	dup $40 and if
	    ." [imm]"
	endif
	$20 and if
	    ." [conly]"
	endif
	\ space
	cr
    repeat
    drop cr ;

: >body ( cfa -- pfa )
    dup >code-address case
	docode: of
	2cells info-head-size +
	endof
	dodata: of
	2cells info-head-size +
	endof
	dup >r
	>code-address case
	    dodoes: of
	    2cells info-head-size +
	    endof
	    >r 2cells r>
	endcase 
	r>
    endcase
    + ;

: body> ( pfa -- cfa )
    dup hex.
    2cells - dup @ 0= if
	info-head-size -
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
:word also
:word previous
:word Forth
:word vsource
:word vtarget
previous

?test $0001 [IF]
cr ." Test for compiler.fs" cr

finish
[THEN]

also
vsource ' name>comp 'Forth name>comp replace-word
vsource ' body> 'Forth body> replace-word
vsource ' >body 'Forth >body replace-word
vtarget ' compile, 'Forth compile, replace-word
vsource ' postpone, 'Forth postpone, replace-word
vtarget comp' literal drop 'Forth literal replace-word
previous

target>

also vsource also Forth

\ gforth word useable in target
:word true
:word false
:word bl
:word base
:word last
:word state
:word type
:word printdebugdata
:word .name
:word .s
:word .r
:word ,
:word 2,
:word '
:word (')
:word "error
:word [
:word ]
:word \
:word (
:word <#
:word >body
:word >code-address
:word >in
:word >number
:word >name
:word ((name>))
:word (name>x)
:word #
:word #s
:word #>
:word #cr
:word #ff
:word #lf
:word /mod
:word */
:word */mod
:word a,
:word abs
:word allot
:word align
:word alias
:word alias-mask
:word also
:word aligned
:word assert-level
:word bye
:word c,
:word cells:
:word char
:word cfa,
:word code-address!
:word comp'
:word compile-only
:word context
:word count
:word decimal
:word definitions
:word depth
:word dp
:word docon:
:word docol:
:word dodefer:
:word douser:
:word dovar:
:word dump
:word d.
:word emit
:word erase
:word evaluate
:word execute
:word find
:word find-name
:word fill
:word flush-icache
:word fm/mod
:word forth
:word forthstart
:word get-current
:word header
:word here
:word hex
:word hold
:word include
:word included-files
:word immediate
:word immediate-mask
:word interpret/compile?
:word interpret/compile-comp
:word loadfilename#
:word lastcfa
:word lastxt
:word literal
:word look
:word max
:word maxdepth-.s
:word min
:word move
:word m*
:word nalign
:word name
:word name>int
:word noop
:word nothing
:word order
:word parse
:word place
:word postpone
:word restrict-mask
:word reveal
:word root
:word rp@
:word see
:word set-order
:word sfind
:word sign
:word snumber?
:word sourcefilename
:word sourceline#
:word space
:word spaces
:word struct-allot
:word s>d
:word sm/rem
:word source
:word threading-method
:word throw
:word u.
:word um/mod
:word um*
:word word
:word wordlist
:word [IF]
:word [ELSE]
:word [ENDIF]
:word [THEN]

:word ?dup
previous

\ rafts word useable in target
:word ~~
:word :
:word :noname
:word ;
:word ?trace
:word constant
:word create
:word defer
:word disasm-dump
:word info-head-size
:word field
:word finish
:word hex.
:word hex.s
:word text-print
:word vlist
:word variable

previous

: . ( n -- )
    s>d d. ;

: cr ( -- )
    #lf emit ;

: vocabulary ( "name" -- )
    create wordlist drop
does>
    context ! ;

: is
    ' >body ! ;

: bye ( -- )
    finish
    ?trace $4000 [IF]
	depth 0 ?do
	    . loop
	cr
    [THEN]
    bye ;

\ hex.s cr
base !
\ order hex.s cr
\ also vsource
\ ' voc-source
\ ' voc-target
\ previous
\ vlist
\ vlist

\ ." START AGAIN: " here hex . decimal cr

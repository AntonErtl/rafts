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

: gforth-compile, ( xt -- )
    , ;

: 'Forth ( "name" -- cfa )
    also Forth ' previous ; immediate
: ['Forth] ( "name" -- cfa )
    also Forth ' previous postpone literal ; immediate compile-only
: (')Forth ( "name" -- nfa )
    also Forth (') previous ; immediate

struct
    1 cells: field info-head-interpreter
    1 cells: field info-head-compiler
    8 cells: field info-head-buffer1
    1 cells: field info-head-null
    1 cells: field info-head-buffer2
end-struct info-head
info-head drop constant info-head-size
info-head-size 2cells + constant info-cfhead-size

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
defer compile,-defer
defer compile,-native
defer compile,-does

variable noname-state
false noname-state !

include machine/asm.fs
include machine/disasm.fs
include basic.fs

: imm-compile, ( xt -- )
    \ ~~
    \ dup >name .name
    \ dup $10 - $40 dump
    dup 3cells + @ execute ;

include primitives.fs
include control.fs

>target
also vtarget :word execute previous
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
    2cells + @ compile,-literal 0 compile,-+ ;
is compile,-field-gforth

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-defer (gforth): " dup name. hex.s cr
    [THEN]
    basic-exit
    2cells + @
    word-interpreter
    basic-init
    
    \ 2cells + @
    \ 2cells +
    \ postpone literal
    \ vtarget postpone @ vsource 
    \ vtarget postpone execute vsource
;
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
    info-cfhead-size + @ compile,-literal ;
is compile,-constant

:noname ( xt -- )
    ?trace $0001 [IF]
	." 2constant: " dup name. hex.s cr
    [THEN]
    dup info-cfhead-size + cell+ @ compile,-literal
    info-cfhead-size + @ compile,-literal ;
is compile,-2constant

:noname ( xt -- )
    ?trace $0001 [IF]
	." [2]variable: " dup name. hex.s cr
    [THEN]
    info-cfhead-size + compile,-literal ;
is compile,-variable

:noname ( xt -- )
    ?trace $0001 [IF]
	." [2]user: " dup name. hex.s cr
    [THEN]
    info-cfhead-size + compile,-literal ;
is compile,-user

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-field: " dup name. hex.s cr
    [THEN]
    info-cfhead-size + @ compile,-literal 0 compile,-+ ;
is compile,-field

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-defer: " dup name. hex.s cr
    [THEN]
    info-cfhead-size + compile,-literal 0 compile,-@
    basic-exit
    ['] execute word-interpreter
    basic-init ;
is compile,-defer

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-native: " dup name. hex.s cr
    [THEN]
    basic-exit
    word-native
    basic-init ;
is compile,-native

:noname ( xt -- )
    ?trace $0001 [IF]
	." compile,-native (does>): " hex.s cr
    [THEN]
    dup info-cfhead-size + compile,-literal
    basic-exit
    dup @ 2 lshift $1a asm-bitmask and swap $1a asm-bitmask invert and or
    info-cfhead-size + word-call
    basic-init ;
is compile,-does

: compile, ( xt -- )
    \ ~~
    \ dup >name .name
    \ dup $10 - $40 dump
    dup [ also Forth ' lit previous ] literal gforth-compile, ,
    3cells + @ gforth-compile, ;

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
	info-cfhead-size
	endof
	dodata: of
	info-cfhead-size
	endof
	dup >r
	>code-address case
	    dodoes: of
	    info-cfhead-size
	    endof
	    >r 2cells r>
	endcase 
	r>
    endcase
    + ;

: body> ( pfa -- cfa )
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
vsource ' name>comp (')Forth name>comp replace-word
vsource ' body> (')Forth body> replace-word
vsource ' >body (')Forth >body replace-word
vtarget ' compile, (')Forth compile, replace-word
vsource ' postpone, (')Forth postpone, replace-word
vtarget comp' literal drop (')Forth literal replace-word
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
:word ;s
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
:word body>
:word >code-address
:word >does-code
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
:word dofield:
:word douser:
:word dovar:
:word dump
:word d.
:word emit
:word erase
:word evaluate
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
:word lit
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
\ :word ;
:word ?trace
:word constant
:word create
:word defer
:word disasm-dump
:word info-head-size
:word info-cfhead-size
:word field
:word finish
:word hex.
:word hex.s
:word hex.rs
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

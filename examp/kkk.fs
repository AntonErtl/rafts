: "tib ( -- )
  ." tib:" tib . ;

\ include-file                                         07apr93py

: push-file  ( -- )  r>
  loadline @ >r loadfile @ >r
  blk @ >r >tib @ >r  #tib @ dup >r  >tib +!  >in @ >r  >r ;

: pop-file   ( throw-code -- throw-code )
  dup IF
         source >in @ loadline @ loadfilename 2@
	 error-stack dup @ dup 1+
	 max-errors 1- min error-stack !
	 6 * cells + cell+
	 5 cells bounds swap DO
	                    I !
	 -1 cells +LOOP
  THEN
  r>
  r> >in !  r> #tib !  r> >tib !  r> blk !
  r> loadfile ! r> loadline !  >r ;

: read-loop ( i*x -- j*x )
  BEGIN  refill  WHILE  parse type  REPEAT ;

: include-file ( i*x fid -- j*x )
  push-file
.s "tib cr
  loadfile !
.s cr
  0 loadline ! blk off  ['] read-loop catch
  loadfile @ close-file swap 2dup or
  pop-file  drop throw throw ;

create pathfilenamebuf 256 chars allot \ !! make this grow on demand

: open-path-file ( c-addr1 u1 -- file-id c-addr2 u2 )
    \ opens a file for reading, searching in the path for it; c-addr2
    \ u2 is the full filename (valid until the next call); if the file
    \ is not found (or in case of other errors for each try), -38
    \ (non-existant file) is thrown. Opening for other access modes
    \ makes little sense, as the path will usually contain dirs that
    \ are only readable for the user
    \ !! check for "/", "./", "../" in original filename; check for "~/"?
    pathdirs 2@ 0
    ?DO ( c-addr1 u1 dirnamep )
	dup >r 2@ dup >r pathfilenamebuf swap cmove ( addr u )
	2dup pathfilenamebuf r@ chars + swap cmove ( addr u )
	pathfilenamebuf over r> + dup >r r/o open-file 0=
	if ( addr u file-id )
	    nip nip r> rdrop 0 leave
	then
	rdrop drop r> cell+ cell+
    LOOP
    0<> -&38 and throw ( file-id u2 )
    pathfilenamebuf swap ;

: included ( i*x addr u -- j*x )
    loadfilename 2@ >r >r
    open-path-file ( file-id c-addr2 u2 )
.s 2dup type cr
    dup allocate throw over loadfilename 2! ( file-id c-addr2 u2 )
    drop loadfilename 2@ move
.s loadfilename 2@ type cr
    ['] include-file catch
    \ don't free filenames; they don't take much space
    \ and are used for debugging
    r> r> loadfilename 2!  throw ;


\ INCLUDE                                               9may93jaw

: include  ( "file" -- )
  name
.s 2dup type cr
  included ;

hex
"tib
.s cr
include options.fs
bye

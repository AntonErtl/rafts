
\ mips.gr	mips BURG grammar
\
\ Copyright (C) 1995-96 Martin Anton Ertl, Christian Pirker
\
\ This file is part of RAFTS.
\
\	RAFTS is free software you can redistribute it and/or
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

: node_op@ ( node_addr -- op )
  node_op @ ;
: node_op! ( op node_addr -- )
  node_op ! ;

: node_slabel@ ( node_addr -- slabel )
  node_slabel @ ;
: node_slabel! ( slabel node_addr -- )
  node_slabel ! ;

: node_left@ ( node_addr -- left )
  node_left @ ;
: node_left! ( left node_addr -- )
  node_left ! ;

: node_right@ ( node_addr -- right )
  node_right @ ;
: node_right! ( right node_addr -- )
  node_right ! ;

' node_op alias OP_LABEL
' node_op@ alias OP_LABEL@
' node_op! alias OP_LABEL!
' node_slabel alias STATE_LABEL
' node_slabel@ alias STATE_LABEL@
' node_slabel! alias STATE_LABEL!
' node_left alias LEFT_CHILD
' node_left@ alias LEFT_CHILD@
' node_left! alias LEFT_CHILD!
' node_right alias RIGHT_CHILD
' node_right@ alias RIGHT_CHILD@
' node_right! alias RIGHT_CHILD!

: val ( node_addr -- node_addr )
  ;
: lval ( node_addr -- node_addr )
  LEFT_CHILD ;
: llval ( node_addr -- node_addr )
  LEFT_CHILD@ LEFT_CHILD ;
: lrval ( node_addr -- node_addr )
  LEFT_CHILD@ RIGHT_CHILD ; 
: rval ( node_addr -- node_addr )
  RIGHT_CHILD ;
: rlval ( node_addr -- node_addr )
  RIGHT_CHILD@ LEFT_CHILD ; 
: rrval ( node_addr -- node_addr ) 
  RIGHT_CHILD@ RIGHT_CHILD ;

: val@ ( node_addr -- node_addr )
  val @ ;
: lval@ ( node_addr -- node_addr )
  lval @ ;
: llval@ ( node_addr -- node_addr )
  llval @ ;
: lrval@ ( node_addr -- node_addr )
  lrval @ ;
: rval@ ( node_addr -- node_addr )
  rval @ ;
: rlval@ ( node_addr -- node_addr )
  rlval @ ;
: rrval@ ( node_addr -- node_addr ) 
  rrval @ ;

' ." alias burm_PANIC" immediate

0 constant none
1 constant unary
2 constant binary


bl word burm_PANIC" find nip 0= [IF]
: burm_PANIC" ( -- )
  ." PANIC" ; immediate
[ENDIF]
\ ' abort" alias burm_assert" immediate
: burm_assert"
  postpone abort" ; immediate restrict

create burm_r1_nts
  3 , 0 ,
create burm_r2_nts
  3 , 4 , 0 ,
create burm_r3_nts
  0 ,
create burm_r6_nts
  4 , 0 ,
create burm_r7_nts
  5 , 0 ,
create burm_r8_nts
  2 , 0 ,
create burm_r12_nts
  3 , 2 , 0 ,
create burm_r13_nts
  3 , 3 , 0 ,
create burm_r20_nts
  6 , 0 ,
create burm_r23_nts
  7 , 0 ,
create burm_r49_nts
  4 , 3 , 0 ,
88 array_noallot burm_nts
  0 ,
  burm_r1_nts ,
  burm_r2_nts ,
  burm_r3_nts ,
  burm_r3_nts ,
  burm_r3_nts ,
  burm_r6_nts ,
  burm_r7_nts ,
  burm_r8_nts ,
  burm_r1_nts ,
  burm_r8_nts ,
  burm_r1_nts ,
  burm_r12_nts ,
  burm_r13_nts ,
  burm_r12_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r20_nts ,
  burm_r20_nts ,
  burm_r13_nts ,
  burm_r23_nts ,
  burm_r23_nts ,
  burm_r1_nts ,
  burm_r1_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r1_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r2_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r2_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r13_nts ,
  burm_r2_nts ,
  burm_r49_nts ,
  burm_r8_nts ,
  burm_r12_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r1_nts ,
  burm_r1_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
  burm_r13_nts ,
: burm_nts@ ( i -- x )
  burm_nts @ ;

2 3 2 mcarray_noallot burm_ADDI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,     5 c,     4 c,	\ row 1
: burm_ADDI_transition@ ( i j -- x )
  burm_ADDI_transition c@ ;

2 2 2 mcarray_noallot burm_ADDF_transition
     0 c,     0 c,	\ row 0
     0 c,     3 c,	\ row 1
: burm_ADDF_transition@ ( i j -- x )
  burm_ADDF_transition c@ ;

2 2 2 mcarray_noallot burm_SUBI_transition
     0 c,     0 c,	\ row 0
     0 c,    82 c,	\ row 1
: burm_SUBI_transition@ ( i j -- x )
  burm_SUBI_transition c@ ;

2 2 2 mcarray_noallot burm_SUBF_transition
     0 c,     0 c,	\ row 0
     0 c,    81 c,	\ row 1
: burm_SUBF_transition@ ( i j -- x )
  burm_SUBF_transition c@ ;

2 2 2 mcarray_noallot burm_MUL_transition
     0 c,     0 c,	\ row 0
     0 c,    62 c,	\ row 1
: burm_MUL_transition@ ( i j -- x )
  burm_MUL_transition c@ ;

2 2 2 mcarray_noallot burm_MULF_transition
     0 c,     0 c,	\ row 0
     0 c,    63 c,	\ row 1
: burm_MULF_transition@ ( i j -- x )
  burm_MULF_transition c@ ;

2 2 2 mcarray_noallot burm_DIV_transition
     0 c,     0 c,	\ row 0
     0 c,    47 c,	\ row 1
: burm_DIV_transition@ ( i j -- x )
  burm_DIV_transition c@ ;

2 2 2 mcarray_noallot burm_DIVF_transition
     0 c,     0 c,	\ row 0
     0 c,    48 c,	\ row 1
: burm_DIVF_transition@ ( i j -- x )
  burm_DIVF_transition c@ ;

2 3 2 mcarray_noallot burm_BANDU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,     6 c,     7 c,	\ row 1
: burm_BANDU_transition@ ( i j -- x )
  burm_BANDU_transition c@ ;

2 3 2 mcarray_noallot burm_BORU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    43 c,    44 c,	\ row 1
: burm_BORU_transition@ ( i j -- x )
  burm_BORU_transition c@ ;

2 3 2 mcarray_noallot burm_BXORU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    46 c,    45 c,	\ row 1
: burm_BXORU_transition@ ( i j -- x )
  burm_BXORU_transition c@ ;

2 3 2 mcarray_noallot burm_LSHU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    59 c,    60 c,	\ row 1
: burm_LSHU_transition@ ( i j -- x )
  burm_LSHU_transition c@ ;

2 3 2 mcarray_noallot burm_LSHI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    58 c,    57 c,	\ row 1
: burm_LSHI_transition@ ( i j -- x )
  burm_LSHI_transition c@ ;

2 3 2 mcarray_noallot burm_RSHU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    71 c,    70 c,	\ row 1
: burm_RSHU_transition@ ( i j -- x )
  burm_RSHU_transition c@ ;

2 3 2 mcarray_noallot burm_RSHI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    68 c,    69 c,	\ row 1
: burm_RSHI_transition@ ( i j -- x )
  burm_RSHI_transition c@ ;

2 3 2 mcarray_noallot burm_STOREC_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    76 c,    77 c,	\ row 1
: burm_STOREC_transition@ ( i j -- x )
  burm_STOREC_transition c@ ;

2 3 2 mcarray_noallot burm_STOREI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    80 c,    79 c,	\ row 1
: burm_STOREI_transition@ ( i j -- x )
  burm_STOREI_transition c@ ;

2 2 2 mcarray_noallot burm_STOREF_transition
     0 c,     0 c,	\ row 0
     0 c,    78 c,	\ row 1
: burm_STOREF_transition@ ( i j -- x )
  burm_STOREF_transition c@ ;

2 3 2 mcarray_noallot burm_SLTI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    72 c,    73 c,	\ row 1
: burm_SLTI_transition@ ( i j -- x )
  burm_SLTI_transition c@ ;

2 3 2 mcarray_noallot burm_SLTU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    74 c,    75 c,	\ row 1
: burm_SLTU_transition@ ( i j -- x )
  burm_SLTU_transition c@ ;

2 2 2 mcarray_noallot burm_BEQI_transition
     0 c,     0 c,	\ row 0
     0 c,     9 c,	\ row 1
: burm_BEQI_transition@ ( i j -- x )
  burm_BEQI_transition c@ ;

2 2 2 mcarray_noallot burm_BEQF_transition
     0 c,     0 c,	\ row 0
     0 c,     8 c,	\ row 1
: burm_BEQF_transition@ ( i j -- x )
  burm_BEQF_transition c@ ;

3 3 2 mcarray_noallot burm_BGEI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    13 c,    11 c,	\ row 1
     0 c,    12 c,    11 c,	\ row 2
: burm_BGEI_transition@ ( i j -- x )
  burm_BGEI_transition c@ ;

3 3 2 mcarray_noallot burm_BGEU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    16 c,    15 c,	\ row 1
     0 c,    14 c,    15 c,	\ row 2
: burm_BGEU_transition@ ( i j -- x )
  burm_BGEU_transition c@ ;

2 2 2 mcarray_noallot burm_BGEF_transition
     0 c,     0 c,	\ row 0
     0 c,    10 c,	\ row 1
: burm_BGEF_transition@ ( i j -- x )
  burm_BGEF_transition c@ ;

3 3 2 mcarray_noallot burm_BGTI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    19 c,    18 c,	\ row 1
     0 c,    20 c,    18 c,	\ row 2
: burm_BGTI_transition@ ( i j -- x )
  burm_BGTI_transition c@ ;

3 3 2 mcarray_noallot burm_BGTU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    21 c,    22 c,	\ row 1
     0 c,    23 c,    22 c,	\ row 2
: burm_BGTU_transition@ ( i j -- x )
  burm_BGTU_transition c@ ;

2 2 2 mcarray_noallot burm_BGTF_transition
     0 c,     0 c,	\ row 0
     0 c,    17 c,	\ row 1
: burm_BGTF_transition@ ( i j -- x )
  burm_BGTF_transition c@ ;

3 3 2 mcarray_noallot burm_BLEI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    26 c,    27 c,	\ row 1
     0 c,    28 c,    27 c,	\ row 2
: burm_BLEI_transition@ ( i j -- x )
  burm_BLEI_transition c@ ;

3 3 2 mcarray_noallot burm_BLEU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    30 c,    31 c,	\ row 1
     0 c,    29 c,    31 c,	\ row 2
: burm_BLEU_transition@ ( i j -- x )
  burm_BLEU_transition c@ ;

2 2 2 mcarray_noallot burm_BLEF_transition
     0 c,     0 c,	\ row 0
     0 c,    25 c,	\ row 1
: burm_BLEF_transition@ ( i j -- x )
  burm_BLEF_transition c@ ;

3 3 2 mcarray_noallot burm_BLTI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    35 c,    33 c,	\ row 1
     0 c,    34 c,    33 c,	\ row 2
: burm_BLTI_transition@ ( i j -- x )
  burm_BLTI_transition c@ ;

3 3 2 mcarray_noallot burm_BLTU_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    36 c,    37 c,	\ row 1
     0 c,    38 c,    37 c,	\ row 2
: burm_BLTU_transition@ ( i j -- x )
  burm_BLTU_transition c@ ;

2 2 2 mcarray_noallot burm_BLTF_transition
     0 c,     0 c,	\ row 0
     0 c,    32 c,	\ row 1
: burm_BLTF_transition@ ( i j -- x )
  burm_BLTF_transition c@ ;

3 3 2 mcarray_noallot burm_BNEI_transition
     0 c,     0 c,     0 c,	\ row 0
     0 c,    41 c,    40 c,	\ row 1
     0 c,    42 c,    40 c,	\ row 2
: burm_BNEI_transition@ ( i j -- x )
  burm_BNEI_transition c@ ;

2 2 2 mcarray_noallot burm_BNEF_transition
     0 c,     0 c,	\ row 0
     0 c,    39 c,	\ row 1
: burm_BNEF_transition@ ( i j -- x )
  burm_BNEF_transition c@ ;

0 constant burm_f0
1 constant burm_f1
2 constant burm_f2
3 constant burm_f3
4 constant burm_f4
5 constant burm_f5
6 constant burm_f6
7 constant burm_f7
8 constant burm_f8
9 constant burm_f9
10 constant burm_f10
84 11 2 marray_noallot burm_plank_0
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,	\ row 0
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    1 ,   22 ,	\ row 1
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   23 ,   22 ,	\ row 2
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    6 ,   22 ,	\ row 3
    1 ,    1 ,    0 ,    0 ,    2 ,    1 ,    1 ,    0 ,    0 ,   32 ,   22 ,	\ row 4
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   33 ,   22 ,	\ row 5
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   22 ,   22 ,	\ row 6
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   21 ,   22 ,	\ row 7
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   31 ,	\ row 8
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   30 ,	\ row 9
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   36 ,	\ row 10
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   28 ,	\ row 11
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   27 ,	\ row 12
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   29 ,	\ row 13
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   24 ,	\ row 14
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   25 ,	\ row 15
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   26 ,	\ row 16
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   37 ,	\ row 17
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   21 ,	\ row 18
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   23 ,	\ row 19
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   19 ,	\ row 20
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   18 ,	\ row 21
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   17 ,	\ row 22
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   16 ,	\ row 23
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   16 ,   22 ,	\ row 24
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   38 ,	\ row 25
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   15 ,	\ row 26
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   14 ,	\ row 27
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   13 ,	\ row 28
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   10 ,	\ row 29
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   12 ,	\ row 30
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   11 ,	\ row 31
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   39 ,	\ row 32
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    8 ,	\ row 33
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    7 ,	\ row 34
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    9 ,	\ row 35
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    6 ,	\ row 36
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    5 ,	\ row 37
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    4 ,	\ row 38
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   40 ,	\ row 39
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    2 ,	\ row 40
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    3 ,	\ row 41
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    1 ,	\ row 42
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   20 ,   22 ,	\ row 43
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   19 ,   22 ,	\ row 44
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   17 ,   22 ,	\ row 45
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   18 ,   22 ,	\ row 46
    0 ,    0 ,    0 ,    1 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,	\ row 47
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    3 ,   22 ,	\ row 48
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   26 ,   22 ,	\ row 49
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   35 ,   22 ,	\ row 50
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   34 ,   22 ,	\ row 51
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    8 ,   22 ,	\ row 52
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   36 ,   22 ,	\ row 53
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   38 ,   22 ,	\ row 54
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    1 ,    0 ,   30 ,   22 ,	\ row 55
    1 ,    2 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    1 ,   27 ,   22 ,	\ row 56
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   12 ,   22 ,	\ row 57
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   14 ,   22 ,	\ row 58
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   15 ,   22 ,	\ row 59
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   13 ,   22 ,	\ row 60
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   25 ,   22 ,	\ row 61
    0 ,    0 ,    1 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,	\ row 62
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    4 ,   22 ,	\ row 63
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   28 ,   22 ,	\ row 64
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   29 ,   22 ,	\ row 65
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    2 ,   22 ,	\ row 66
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   24 ,   22 ,	\ row 67
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   10 ,   22 ,	\ row 68
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    7 ,   22 ,	\ row 69
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    9 ,   22 ,	\ row 70
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   11 ,   22 ,	\ row 71
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   42 ,   22 ,	\ row 72
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   41 ,   22 ,	\ row 73
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   39 ,   22 ,	\ row 74
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   37 ,   22 ,	\ row 75
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   33 ,	\ row 76
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   32 ,	\ row 77
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   20 ,	\ row 78
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   34 ,	\ row 79
    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,   35 ,	\ row 80
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,    5 ,   22 ,	\ row 81
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   31 ,   22 ,	\ row 82
    1 ,    1 ,    0 ,    0 ,    1 ,    2 ,    0 ,    0 ,    0 ,   40 ,   22 ,	\ row 83
: burm_plank_0@ ( i j -- x )
  burm_plank_0 @ ;

89 array_noallot burm_eruleMap
     0 ,     2 ,     0 ,     5 ,     0 ,     4 ,     0 ,    22 ,     0 ,    19 ,	\ 0-9
     0 ,    81 ,    80 ,    79 ,    78 ,    77 ,    76 ,    41 ,    74 ,    40 ,	\ 10-19
    39 ,    38 ,    37 ,    36 ,    35 ,    34 ,    33 ,    32 ,    31 ,    30 ,	\ 20-29
    29 ,    28 ,    27 ,    26 ,    25 ,    24 ,    23 ,     6 ,    21 ,    20 ,	\ 30-39
     7 ,    18 ,    17 ,    16 ,     8 ,     9 ,    10 ,    45 ,    11 ,    44 ,	\ 40-49
     3 ,    43 ,    42 ,     0 ,    73 ,    72 ,    71 ,    70 ,    69 ,    68 ,	\ 50-59
    67 ,    66 ,    65 ,    64 ,    63 ,    62 ,    61 ,    60 ,    59 ,    58 ,	\ 60-69
    57 ,    56 ,    55 ,    75 ,    54 ,     1 ,    53 ,    52 ,    51 ,    50 ,	\ 70-79
    49 ,    48 ,    47 ,    46 ,    82 ,    12 ,    13 ,    14 ,    15 ,    83 ,	\ 80-89
    84 ,    85 ,    86 ,    87 ,
: burm_eruleMap@ ( i -- x )
  burm_eruleMap @ ;

: burm_stmt_rule ( state -- rule )
  burm_f10 burm_plank_0@ dup 0<> if 53 + endif burm_eruleMap@ ;
: burm_addr_rule ( state -- rule )
  burm_f6 burm_plank_0@ dup 0<> if 0 + endif burm_eruleMap@ ;
: burm_reg_rule ( state -- rule )
  burm_f9 burm_plank_0@ dup 0<> if 10 + endif burm_eruleMap@ ;
: burm_cons_rule ( state -- rule )
  burm_f8 burm_plank_0@ dup 0<> if 4 + endif burm_eruleMap@ ;
: burm_con_rule ( state -- rule )
  burm_f7 burm_plank_0@ dup 0<> if 2 + endif burm_eruleMap@ ;
: burm_lmul_rule ( state -- rule )
  burm_f2 burm_plank_0@ dup 0<> if 8 + endif burm_eruleMap@ ;
: burm_ldiv_rule ( state -- rule )
  burm_f3 burm_plank_0@ dup 0<> if 6 + endif burm_eruleMap@ ;
: burm_rule ( state goalnt -- rule )
  over dup 0 < swap 84 >= and burm_assert" Bad state passed to burm_rule"
  case
    1 of burm_stmt_rule endof
    2 of burm_addr_rule endof
    3 of burm_reg_rule endof
    4 of burm_cons_rule endof
    5 of burm_con_rule endof
    6 of burm_lmul_rule endof
    7 of burm_ldiv_rule endof
    >r true burm_assert" Unknown nonterminal in burm_rule" r> endcase ;

: burm_LITS_state ( left right -- state )
  2drop 56 ;
: burm_LITI_state ( left right -- state )
  2drop 55 ;
: burm_LITF_state ( left right -- state )
  2drop 0 ;
: burm_BINVU_state ( left right -- state )
  drop burm_f0 burm_plank_0@ dup 0<> if 23 + endif ;
: burm_NEGI_state ( left right -- state )
  drop burm_f0 burm_plank_0@ dup 0<> if 66 + endif ;
: burm_NEGF_state ( left right -- state )
  drop burm_f0 burm_plank_0@ dup 0<> if 65 + endif ;
: burm_ABSI_state ( left right -- state )
  drop burm_f0 burm_plank_0@ dup 0<> if 1 + endif ;
: burm_ABSF_state ( left right -- state )
  drop burm_f0 burm_plank_0@ dup 0<> if 0 + endif ;
: burm_ADDI_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_ADDI_transition@ ;
: burm_ADDF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_ADDF_transition@ ;
: burm_SUBI_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_SUBI_transition@ ;
: burm_SUBF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_SUBF_transition@ ;
: burm_MUL_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_MUL_transition@ ;
: burm_MULIL_state ( left right -- state )
  drop burm_f2 burm_plank_0@ dup 0<> if 64 + endif ;
: burm_MULIH_state ( left right -- state )
  drop burm_f2 burm_plank_0@ dup 0<> if 63 + endif ;
: burm_MULF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_MULF_transition@ ;
: burm_DIV_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_DIV_transition@ ;
: burm_DIVI_state ( left right -- state )
  drop burm_f3 burm_plank_0@ dup 0<> if 48 + endif ;
: burm_DIVF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_DIVF_transition@ ;
: burm_MODI_state ( left right -- state )
  drop burm_f3 burm_plank_0@ dup 0<> if 60 + endif ;
: burm_BANDU_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BANDU_transition@ ;
: burm_BORU_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BORU_transition@ ;
: burm_BXORU_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BXORU_transition@ ;
: burm_LSHU_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_LSHU_transition@ ;
: burm_LSHI_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_LSHI_transition@ ;
: burm_RSHU_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_RSHU_transition@ ;
: burm_RSHI_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_RSHI_transition@ ;
: burm_FETCHC_state ( left right -- state )
  drop burm_f4 burm_plank_0@ dup 0<> if 49 + endif ;
: burm_FETCHI_state ( left right -- state )
  drop burm_f5 burm_plank_0@ dup 0<> if 52 + endif ;
: burm_FETCHF_state ( left right -- state )
  drop burm_f6 burm_plank_0@ dup 0<> if 51 + endif ;
: burm_STOREC_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f4 burm_plank_0@ burm_STOREC_transition@ ;
: burm_STOREI_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f4 burm_plank_0@ burm_STOREI_transition@ ;
: burm_STOREF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f6 burm_plank_0@ burm_STOREF_transition@ ;
: burm_SLTI_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_SLTI_transition@ ;
: burm_SLTU_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_SLTU_transition@ ;
: burm_SLTF_state ( left right -- state )
  2drop 0 ;
: burm_BEQI_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_BEQI_transition@ ;
: burm_BEQF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_BEQF_transition@ ;
: burm_BGEI_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BGEI_transition@ ;
: burm_BGEU_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BGEU_transition@ ;
: burm_BGEF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_BGEF_transition@ ;
: burm_BGTI_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BGTI_transition@ ;
: burm_BGTU_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BGTU_transition@ ;
: burm_BGTF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_BGTF_transition@ ;
: burm_BLEI_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BLEI_transition@ ;
: burm_BLEU_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BLEU_transition@ ;
: burm_BLEF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_BLEF_transition@ ;
: burm_BLTI_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BLTI_transition@ ;
: burm_BLTU_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BLTU_transition@ ;
: burm_BLTF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_BLTF_transition@ ;
: burm_BNEI_state ( left right -- state )
  swap burm_f1 burm_plank_0@ swap burm_f1 burm_plank_0@ burm_BNEI_transition@ ;
: burm_BNEF_state ( left right -- state )
  swap burm_f0 burm_plank_0@ swap burm_f0 burm_plank_0@ burm_BNEF_transition@ ;
: burm_VREGP_state ( left right -- state )
  2drop 83 ;
: burm_NOP_state ( left right -- state )
  2drop 0 ;
201 array [burm_state]
  ' burm_LITS_state 1 [burm_state] !
  ' burm_LITI_state 2 [burm_state] !
  0 4 [burm_state] !
  ' burm_BINVU_state 11 [burm_state] !
  ' burm_NEGI_state 18 [burm_state] !
  ' burm_NEGF_state 20 [burm_state] !
  ' burm_ABSI_state 22 [burm_state] !
  ' burm_ABSF_state 24 [burm_state] !
  ' burm_ADDI_state 26 [burm_state] !
  ' burm_ADDF_state 28 [burm_state] !
  ' burm_SUBI_state 98 [burm_state] !
  ' burm_SUBF_state 100 [burm_state] !
  ' burm_MUL_state 81 [burm_state] !
  ' burm_MULIL_state 82 [burm_state] !
  ' burm_MULIH_state 83 [burm_state] !
  ' burm_MULF_state 84 [burm_state] !
  ' burm_DIV_state 56 [burm_state] !
  ' burm_DIVI_state 58 [burm_state] !
  ' burm_DIVF_state 60 [burm_state] !
  ' burm_MODI_state 74 [burm_state] !
  ' burm_BANDU_state 35 [burm_state] !
  ' burm_BORU_state 43 [burm_state] !
  ' burm_BXORU_state 51 [burm_state] !
  ' burm_LSHU_state 66 [burm_state] !
  ' burm_LSHI_state 67 [burm_state] !
  ' burm_RSHU_state 90 [burm_state] !
  ' burm_RSHI_state 91 [burm_state] !
  ' burm_FETCHC_state 105 [burm_state] !
  ' burm_FETCHI_state 106 [burm_state] !
  ' burm_FETCHF_state 108 [burm_state] !
  ' burm_STOREC_state 113 [burm_state] !
  ' burm_STOREI_state 114 [burm_state] !
  ' burm_STOREF_state 116 [burm_state] !
  ' burm_SLTI_state 120 [burm_state] !
  ' burm_SLTU_state 121 [burm_state] !
  0 122 [burm_state] !
  ' burm_BEQI_state 130 [burm_state] !
  ' burm_BEQF_state 132 [burm_state] !
  ' burm_BGEI_state 138 [burm_state] !
  ' burm_BGEU_state 139 [burm_state] !
  ' burm_BGEF_state 140 [burm_state] !
  ' burm_BGTI_state 146 [burm_state] !
  ' burm_BGTU_state 147 [burm_state] !
  ' burm_BGTF_state 148 [burm_state] !
  ' burm_BLEI_state 154 [burm_state] !
  ' burm_BLEU_state 155 [burm_state] !
  ' burm_BLEF_state 156 [burm_state] !
  ' burm_BLTI_state 162 [burm_state] !
  ' burm_BLTU_state 163 [burm_state] !
  ' burm_BLTF_state 164 [burm_state] !
  ' burm_BNEI_state 170 [burm_state] !
  ' burm_BNEF_state 172 [burm_state] !
  ' burm_VREGP_state 182 [burm_state] !
  0 200 [burm_state] !
: burm_state ( left right op -- state )
  [burm_state] @ execute ;

1 constant LITS
2 constant LITI
4 constant LITF
11 constant BINVU
18 constant NEGI
20 constant NEGF
22 constant ABSI
24 constant ABSF
26 constant ADDI
28 constant ADDF
35 constant BANDU
43 constant BORU
51 constant BXORU
56 constant DIV
58 constant DIVI
60 constant DIVF
66 constant LSHU
67 constant LSHI
74 constant MODI
81 constant MUL
82 constant MULIL
83 constant MULIH
84 constant MULF
90 constant RSHU
91 constant RSHI
98 constant SUBI
100 constant SUBF
105 constant FETCHC
106 constant FETCHI
108 constant FETCHF
113 constant STOREC
114 constant STOREI
116 constant STOREF
120 constant SLTI
121 constant SLTU
122 constant SLTF
130 constant BEQI
132 constant BEQF
138 constant BGEI
139 constant BGEU
140 constant BGEF
146 constant BGTI
147 constant BGTU
148 constant BGTF
154 constant BLEI
155 constant BLEU
156 constant BLEF
162 constant BLTI
163 constant BLTU
164 constant BLTF
170 constant BNEI
172 constant BNEF
182 constant VREGP
200 constant NOP

7 constant burm_ldiv_NT
6 constant burm_lmul_NT
5 constant burm_con_NT
4 constant burm_cons_NT
3 constant burm_reg_NT
2 constant burm_addr_NT
1 constant burm_stmt_NT
7 constant burm_NT

: burm_opname1 ." LITS " ;
: burm_opname2 ." LITI " ;
: burm_opname4 ." LITF " ;
: burm_opname11 ." BINVU " ;
: burm_opname18 ." NEGI " ;
: burm_opname20 ." NEGF " ;
: burm_opname22 ." ABSI " ;
: burm_opname24 ." ABSF " ;
: burm_opname26 ." ADDI " ;
: burm_opname28 ." ADDF " ;
: burm_opname35 ." BANDU " ;
: burm_opname43 ." BORU " ;
: burm_opname51 ." BXORU " ;
: burm_opname56 ." DIV " ;
: burm_opname58 ." DIVI " ;
: burm_opname60 ." DIVF " ;
: burm_opname66 ." LSHU " ;
: burm_opname67 ." LSHI " ;
: burm_opname74 ." MODI " ;
: burm_opname81 ." MUL " ;
: burm_opname82 ." MULIL " ;
: burm_opname83 ." MULIH " ;
: burm_opname84 ." MULF " ;
: burm_opname90 ." RSHU " ;
: burm_opname91 ." RSHI " ;
: burm_opname98 ." SUBI " ;
: burm_opname100 ." SUBF " ;
: burm_opname105 ." FETCHC " ;
: burm_opname106 ." FETCHI " ;
: burm_opname108 ." FETCHF " ;
: burm_opname113 ." STOREC " ;
: burm_opname114 ." STOREI " ;
: burm_opname116 ." STOREF " ;
: burm_opname120 ." SLTI " ;
: burm_opname121 ." SLTU " ;
: burm_opname122 ." SLTF " ;
: burm_opname130 ." BEQI " ;
: burm_opname132 ." BEQF " ;
: burm_opname138 ." BGEI " ;
: burm_opname139 ." BGEU " ;
: burm_opname140 ." BGEF " ;
: burm_opname146 ." BGTI " ;
: burm_opname147 ." BGTU " ;
: burm_opname148 ." BGTF " ;
: burm_opname154 ." BLEI " ;
: burm_opname155 ." BLEU " ;
: burm_opname156 ." BLEF " ;
: burm_opname162 ." BLTI " ;
: burm_opname163 ." BLTU " ;
: burm_opname164 ." BLTF " ;
: burm_opname170 ." BNEI " ;
: burm_opname172 ." BNEF " ;
: burm_opname182 ." VREGP " ;
: burm_opname200 ." NOP " ;
201 array [burm_opname]
  ' burm_opname1 1 [burm_opname] !
  ' burm_opname2 2 [burm_opname] !
  ' burm_opname4 4 [burm_opname] !
  ' burm_opname11 11 [burm_opname] !
  ' burm_opname18 18 [burm_opname] !
  ' burm_opname20 20 [burm_opname] !
  ' burm_opname22 22 [burm_opname] !
  ' burm_opname24 24 [burm_opname] !
  ' burm_opname26 26 [burm_opname] !
  ' burm_opname28 28 [burm_opname] !
  ' burm_opname35 35 [burm_opname] !
  ' burm_opname43 43 [burm_opname] !
  ' burm_opname51 51 [burm_opname] !
  ' burm_opname56 56 [burm_opname] !
  ' burm_opname58 58 [burm_opname] !
  ' burm_opname60 60 [burm_opname] !
  ' burm_opname66 66 [burm_opname] !
  ' burm_opname67 67 [burm_opname] !
  ' burm_opname74 74 [burm_opname] !
  ' burm_opname81 81 [burm_opname] !
  ' burm_opname82 82 [burm_opname] !
  ' burm_opname83 83 [burm_opname] !
  ' burm_opname84 84 [burm_opname] !
  ' burm_opname90 90 [burm_opname] !
  ' burm_opname91 91 [burm_opname] !
  ' burm_opname98 98 [burm_opname] !
  ' burm_opname100 100 [burm_opname] !
  ' burm_opname105 105 [burm_opname] !
  ' burm_opname106 106 [burm_opname] !
  ' burm_opname108 108 [burm_opname] !
  ' burm_opname113 113 [burm_opname] !
  ' burm_opname114 114 [burm_opname] !
  ' burm_opname116 116 [burm_opname] !
  ' burm_opname120 120 [burm_opname] !
  ' burm_opname121 121 [burm_opname] !
  ' burm_opname122 122 [burm_opname] !
  ' burm_opname130 130 [burm_opname] !
  ' burm_opname132 132 [burm_opname] !
  ' burm_opname138 138 [burm_opname] !
  ' burm_opname139 139 [burm_opname] !
  ' burm_opname140 140 [burm_opname] !
  ' burm_opname146 146 [burm_opname] !
  ' burm_opname147 147 [burm_opname] !
  ' burm_opname148 148 [burm_opname] !
  ' burm_opname154 154 [burm_opname] !
  ' burm_opname155 155 [burm_opname] !
  ' burm_opname156 156 [burm_opname] !
  ' burm_opname162 162 [burm_opname] !
  ' burm_opname163 163 [burm_opname] !
  ' burm_opname164 164 [burm_opname] !
  ' burm_opname170 170 [burm_opname] !
  ' burm_opname172 172 [burm_opname] !
  ' burm_opname182 182 [burm_opname] !
  ' burm_opname200 200 [burm_opname] !
: burm_opname ( rule -- )
  [burm_opname] @ execute ;

201 carray_noallot burm_arity
  -1 c,	\ 0
  0 c,	\ 1
  0 c,	\ 2
  -1 c,	\ 3
  -1 c,	\ 4
  -1 c,	\ 5
  -1 c,	\ 6
  -1 c,	\ 7
  -1 c,	\ 8
  -1 c,	\ 9
  -1 c,	\ 10
  1 c,	\ 11
  -1 c,	\ 12
  -1 c,	\ 13
  -1 c,	\ 14
  -1 c,	\ 15
  -1 c,	\ 16
  -1 c,	\ 17
  1 c,	\ 18
  -1 c,	\ 19
  1 c,	\ 20
  -1 c,	\ 21
  1 c,	\ 22
  -1 c,	\ 23
  1 c,	\ 24
  -1 c,	\ 25
  2 c,	\ 26
  -1 c,	\ 27
  2 c,	\ 28
  -1 c,	\ 29
  -1 c,	\ 30
  -1 c,	\ 31
  -1 c,	\ 32
  -1 c,	\ 33
  -1 c,	\ 34
  2 c,	\ 35
  -1 c,	\ 36
  -1 c,	\ 37
  -1 c,	\ 38
  -1 c,	\ 39
  -1 c,	\ 40
  -1 c,	\ 41
  -1 c,	\ 42
  2 c,	\ 43
  -1 c,	\ 44
  -1 c,	\ 45
  -1 c,	\ 46
  -1 c,	\ 47
  -1 c,	\ 48
  -1 c,	\ 49
  -1 c,	\ 50
  2 c,	\ 51
  -1 c,	\ 52
  -1 c,	\ 53
  -1 c,	\ 54
  -1 c,	\ 55
  2 c,	\ 56
  -1 c,	\ 57
  1 c,	\ 58
  -1 c,	\ 59
  2 c,	\ 60
  -1 c,	\ 61
  -1 c,	\ 62
  -1 c,	\ 63
  -1 c,	\ 64
  -1 c,	\ 65
  2 c,	\ 66
  2 c,	\ 67
  -1 c,	\ 68
  -1 c,	\ 69
  -1 c,	\ 70
  -1 c,	\ 71
  -1 c,	\ 72
  -1 c,	\ 73
  1 c,	\ 74
  -1 c,	\ 75
  -1 c,	\ 76
  -1 c,	\ 77
  -1 c,	\ 78
  -1 c,	\ 79
  -1 c,	\ 80
  2 c,	\ 81
  1 c,	\ 82
  1 c,	\ 83
  2 c,	\ 84
  -1 c,	\ 85
  -1 c,	\ 86
  -1 c,	\ 87
  -1 c,	\ 88
  -1 c,	\ 89
  2 c,	\ 90
  2 c,	\ 91
  -1 c,	\ 92
  -1 c,	\ 93
  -1 c,	\ 94
  -1 c,	\ 95
  -1 c,	\ 96
  -1 c,	\ 97
  2 c,	\ 98
  -1 c,	\ 99
  2 c,	\ 100
  -1 c,	\ 101
  -1 c,	\ 102
  -1 c,	\ 103
  -1 c,	\ 104
  1 c,	\ 105
  1 c,	\ 106
  -1 c,	\ 107
  1 c,	\ 108
  -1 c,	\ 109
  -1 c,	\ 110
  -1 c,	\ 111
  -1 c,	\ 112
  2 c,	\ 113
  2 c,	\ 114
  -1 c,	\ 115
  2 c,	\ 116
  -1 c,	\ 117
  -1 c,	\ 118
  -1 c,	\ 119
  2 c,	\ 120
  2 c,	\ 121
  -1 c,	\ 122
  -1 c,	\ 123
  -1 c,	\ 124
  -1 c,	\ 125
  -1 c,	\ 126
  -1 c,	\ 127
  -1 c,	\ 128
  -1 c,	\ 129
  2 c,	\ 130
  -1 c,	\ 131
  2 c,	\ 132
  -1 c,	\ 133
  -1 c,	\ 134
  -1 c,	\ 135
  -1 c,	\ 136
  -1 c,	\ 137
  2 c,	\ 138
  2 c,	\ 139
  2 c,	\ 140
  -1 c,	\ 141
  -1 c,	\ 142
  -1 c,	\ 143
  -1 c,	\ 144
  -1 c,	\ 145
  2 c,	\ 146
  2 c,	\ 147
  2 c,	\ 148
  -1 c,	\ 149
  -1 c,	\ 150
  -1 c,	\ 151
  -1 c,	\ 152
  -1 c,	\ 153
  2 c,	\ 154
  2 c,	\ 155
  2 c,	\ 156
  -1 c,	\ 157
  -1 c,	\ 158
  -1 c,	\ 159
  -1 c,	\ 160
  -1 c,	\ 161
  2 c,	\ 162
  2 c,	\ 163
  2 c,	\ 164
  -1 c,	\ 165
  -1 c,	\ 166
  -1 c,	\ 167
  -1 c,	\ 168
  -1 c,	\ 169
  2 c,	\ 170
  -1 c,	\ 171
  2 c,	\ 172
  -1 c,	\ 173
  -1 c,	\ 174
  -1 c,	\ 175
  -1 c,	\ 176
  -1 c,	\ 177
  -1 c,	\ 178
  -1 c,	\ 179
  -1 c,	\ 180
  -1 c,	\ 181
  0 c,	\ 182
  -1 c,	\ 183
  -1 c,	\ 184
  -1 c,	\ 185
  -1 c,	\ 186
  -1 c,	\ 187
  -1 c,	\ 188
  -1 c,	\ 189
  -1 c,	\ 190
  -1 c,	\ 191
  -1 c,	\ 192
  -1 c,	\ 193
  -1 c,	\ 194
  -1 c,	\ 195
  -1 c,	\ 196
  -1 c,	\ 197
  -1 c,	\ 198
  -1 c,	\ 199
  -1 c,	\ 200
: burm_arity@ ( i -- x )
  burm_arity c@ ;

200 constant burm_max_op
83 constant burm_max_state

: burm_string1 ." stmt: reg" ;
: burm_string2 ." addr: ADDI(reg, cons)" ;
: burm_string3 ." reg: VREGP" ;
: burm_string4 ." cons: LITS" ;
: burm_string5 ." con: LITI" ;
: burm_string6 ." reg: cons" ;
: burm_string7 ." reg: con" ;
: burm_string8 ." reg: FETCHC(addr)" ;
: burm_string9 ." reg: FETCHC(reg)" ;
: burm_string10 ." reg: FETCHI(addr)" ;
: burm_string11 ." reg: FETCHI(reg)" ;
: burm_string12 ." stmt: STOREC(reg, addr)" ;
: burm_string13 ." stmt: STOREC(reg, reg)" ;
: burm_string14 ." stmt: STOREI(reg, addr)" ;
: burm_string15 ." stmt: STOREI(reg, reg)" ;
: burm_string16 ." reg: ADDI(reg, reg)" ;
: burm_string17 ." reg: ADDI(reg, cons)" ;
: burm_string18 ." reg: SUBI(reg, reg)" ;
: burm_string19 ." lmul: MUL(reg, reg)" ;
: burm_string20 ." reg: MULIL(lmul)" ;
: burm_string21 ." reg: MULIH(lmul)" ;
: burm_string22 ." ldiv: DIV(reg, reg)" ;
: burm_string23 ." reg: DIVI(ldiv)" ;
: burm_string24 ." reg: MODI(ldiv)" ;
: burm_string25 ." reg: NEGI(reg)" ;
: burm_string26 ." reg: ABSI(reg)" ;
: burm_string27 ." reg: BANDU(reg, reg)" ;
: burm_string28 ." reg: BANDU(reg, cons)" ;
: burm_string29 ." reg: BORU(reg, reg)" ;
: burm_string30 ." reg: BORU(reg, cons)" ;
: burm_string31 ." reg: BXORU(reg, reg)" ;
: burm_string32 ." reg: BXORU(reg, cons)" ;
: burm_string33 ." reg: BINVU(reg)" ;
: burm_string34 ." reg: LSHU(reg, reg)" ;
: burm_string35 ." reg: LSHI(reg, reg)" ;
: burm_string36 ." reg: LSHU(reg, cons)" ;
: burm_string37 ." reg: LSHI(reg, cons)" ;
: burm_string38 ." reg: RSHU(reg, reg)" ;
: burm_string39 ." reg: RSHI(reg, reg)" ;
: burm_string40 ." reg: RSHU(reg, cons)" ;
: burm_string41 ." reg: RSHI(reg, cons)" ;
: burm_string42 ." reg: SLTI(reg, reg)" ;
: burm_string43 ." reg: SLTI(reg, cons)" ;
: burm_string44 ." reg: SLTU(reg, reg)" ;
: burm_string45 ." reg: SLTU(reg, cons)" ;
: burm_string46 ." stmt: BEQI(reg, reg)" ;
: burm_string47 ." stmt: BGEI(reg, reg)" ;
: burm_string48 ." stmt: BGEI(reg, cons)" ;
: burm_string49 ." stmt: BGEI(cons, reg)" ;
: burm_string50 ." stmt: BGEU(reg, reg)" ;
: burm_string51 ." stmt: BGEU(reg, cons)" ;
: burm_string52 ." stmt: BGEU(cons, reg)" ;
: burm_string53 ." stmt: BGTI(reg, reg)" ;
: burm_string54 ." stmt: BGTI(reg, cons)" ;
: burm_string55 ." stmt: BGTI(cons, reg)" ;
: burm_string56 ." stmt: BGTU(reg, reg)" ;
: burm_string57 ." stmt: BGTU(reg, cons)" ;
: burm_string58 ." stmt: BGTU(cons, reg)" ;
: burm_string59 ." stmt: BLEI(reg, reg)" ;
: burm_string60 ." stmt: BLEI(reg, cons)" ;
: burm_string61 ." stmt: BLEI(cons, reg)" ;
: burm_string62 ." stmt: BLEU(reg, reg)" ;
: burm_string63 ." stmt: BLEU(reg, cons)" ;
: burm_string64 ." stmt: BLEU(cons, reg)" ;
: burm_string65 ." stmt: BLTI(reg, reg)" ;
: burm_string66 ." stmt: BLTI(reg, cons)" ;
: burm_string67 ." stmt: BLTI(cons, reg)" ;
: burm_string68 ." stmt: BLTU(reg, reg)" ;
: burm_string69 ." stmt: BLTU(reg, cons)" ;
: burm_string70 ." stmt: BLTU(cons, reg)" ;
: burm_string71 ." stmt: BNEI(reg, reg)" ;
: burm_string72 ." stmt: BNEI(reg, cons)" ;
: burm_string73 ." stmt: BNEI(cons, reg)" ;
: burm_string74 ." reg: FETCHF(addr)" ;
: burm_string75 ." stmt: STOREF(reg, addr)" ;
: burm_string76 ." reg: ADDF(reg, reg)" ;
: burm_string77 ." reg: SUBF(reg, reg)" ;
: burm_string78 ." reg: MULF(reg, reg)" ;
: burm_string79 ." reg: DIVF(reg, reg)" ;
: burm_string80 ." reg: NEGF(reg)" ;
: burm_string81 ." reg: ABSF(reg)" ;
: burm_string82 ." stmt: BEQF(reg, reg)" ;
: burm_string83 ." stmt: BGEF(reg, reg)" ;
: burm_string84 ." stmt: BGTF(reg, reg)" ;
: burm_string85 ." stmt: BLEF(reg, reg)" ;
: burm_string86 ." stmt: BLTF(reg, reg)" ;
: burm_string87 ." stmt: BNEF(reg, reg)" ;
88 array_noallot [burm_string]
  ' burm_string1 ,
  ' burm_string2 ,
  ' burm_string3 ,
  ' burm_string4 ,
  ' burm_string5 ,
  ' burm_string6 ,
  ' burm_string7 ,
  ' burm_string8 ,
  ' burm_string9 ,
  ' burm_string10 ,
  ' burm_string11 ,
  ' burm_string12 ,
  ' burm_string13 ,
  ' burm_string14 ,
  ' burm_string15 ,
  ' burm_string16 ,
  ' burm_string17 ,
  ' burm_string18 ,
  ' burm_string19 ,
  ' burm_string20 ,
  ' burm_string21 ,
  ' burm_string22 ,
  ' burm_string23 ,
  ' burm_string24 ,
  ' burm_string25 ,
  ' burm_string26 ,
  ' burm_string27 ,
  ' burm_string28 ,
  ' burm_string29 ,
  ' burm_string30 ,
  ' burm_string31 ,
  ' burm_string32 ,
  ' burm_string33 ,
  ' burm_string34 ,
  ' burm_string35 ,
  ' burm_string36 ,
  ' burm_string37 ,
  ' burm_string38 ,
  ' burm_string39 ,
  ' burm_string40 ,
  ' burm_string41 ,
  ' burm_string42 ,
  ' burm_string43 ,
  ' burm_string44 ,
  ' burm_string45 ,
  ' burm_string46 ,
  ' burm_string47 ,
  ' burm_string48 ,
  ' burm_string49 ,
  ' burm_string50 ,
  ' burm_string51 ,
  ' burm_string52 ,
  ' burm_string53 ,
  ' burm_string54 ,
  ' burm_string55 ,
  ' burm_string56 ,
  ' burm_string57 ,
  ' burm_string58 ,
  ' burm_string59 ,
  ' burm_string60 ,
  ' burm_string61 ,
  ' burm_string62 ,
  ' burm_string63 ,
  ' burm_string64 ,
  ' burm_string65 ,
  ' burm_string66 ,
  ' burm_string67 ,
  ' burm_string68 ,
  ' burm_string69 ,
  ' burm_string70 ,
  ' burm_string71 ,
  ' burm_string72 ,
  ' burm_string73 ,
  ' burm_string74 ,
  ' burm_string75 ,
  ' burm_string76 ,
  ' burm_string77 ,
  ' burm_string78 ,
  ' burm_string79 ,
  ' burm_string80 ,
  ' burm_string81 ,
  ' burm_string82 ,
  ' burm_string83 ,
  ' burm_string84 ,
  ' burm_string85 ,
  ' burm_string86 ,
  ' burm_string87 ,
: burm_string ( rule -- )
  [burm_string] @ execute ;

87 constant burm_max_rule

88 4 2 marray_noallot burm_cost
       0 ,     0 ,     0 ,     0 ,	\ 0
       0 ,     0 ,     0 ,     0 ,	\ 1
       0 ,     0 ,     0 ,     0 ,	\ 2
       0 ,     0 ,     0 ,     0 ,	\ 3
       0 ,     0 ,     0 ,     0 ,	\ 4
       0 ,     0 ,     0 ,     0 ,	\ 5
       1 ,     0 ,     0 ,     0 ,	\ 6
       1 ,     0 ,     0 ,     0 ,	\ 7
       1 ,     0 ,     0 ,     0 ,	\ 8
       1 ,     0 ,     0 ,     0 ,	\ 9
       1 ,     0 ,     0 ,     0 ,	\ 10
       1 ,     0 ,     0 ,     0 ,	\ 11
       1 ,     0 ,     0 ,     0 ,	\ 12
       1 ,     0 ,     0 ,     0 ,	\ 13
       1 ,     0 ,     0 ,     0 ,	\ 14
       1 ,     0 ,     0 ,     0 ,	\ 15
       1 ,     0 ,     0 ,     0 ,	\ 16
       1 ,     0 ,     0 ,     0 ,	\ 17
       1 ,     0 ,     0 ,     0 ,	\ 18
       1 ,     0 ,     0 ,     0 ,	\ 19
       1 ,     0 ,     0 ,     0 ,	\ 20
       1 ,     0 ,     0 ,     0 ,	\ 21
       1 ,     0 ,     0 ,     0 ,	\ 22
       1 ,     0 ,     0 ,     0 ,	\ 23
       1 ,     0 ,     0 ,     0 ,	\ 24
       1 ,     0 ,     0 ,     0 ,	\ 25
       1 ,     0 ,     0 ,     0 ,	\ 26
       1 ,     0 ,     0 ,     0 ,	\ 27
       1 ,     0 ,     0 ,     0 ,	\ 28
       1 ,     0 ,     0 ,     0 ,	\ 29
       1 ,     0 ,     0 ,     0 ,	\ 30
       1 ,     0 ,     0 ,     0 ,	\ 31
       1 ,     0 ,     0 ,     0 ,	\ 32
       1 ,     0 ,     0 ,     0 ,	\ 33
       1 ,     0 ,     0 ,     0 ,	\ 34
       1 ,     0 ,     0 ,     0 ,	\ 35
       1 ,     0 ,     0 ,     0 ,	\ 36
       1 ,     0 ,     0 ,     0 ,	\ 37
       1 ,     0 ,     0 ,     0 ,	\ 38
       1 ,     0 ,     0 ,     0 ,	\ 39
       1 ,     0 ,     0 ,     0 ,	\ 40
       1 ,     0 ,     0 ,     0 ,	\ 41
       1 ,     0 ,     0 ,     0 ,	\ 42
       1 ,     0 ,     0 ,     0 ,	\ 43
       1 ,     0 ,     0 ,     0 ,	\ 44
       1 ,     0 ,     0 ,     0 ,	\ 45
       1 ,     0 ,     0 ,     0 ,	\ 46
       1 ,     0 ,     0 ,     0 ,	\ 47
       1 ,     0 ,     0 ,     0 ,	\ 48
       1 ,     0 ,     0 ,     0 ,	\ 49
       1 ,     0 ,     0 ,     0 ,	\ 50
       1 ,     0 ,     0 ,     0 ,	\ 51
       1 ,     0 ,     0 ,     0 ,	\ 52
       1 ,     0 ,     0 ,     0 ,	\ 53
       1 ,     0 ,     0 ,     0 ,	\ 54
       1 ,     0 ,     0 ,     0 ,	\ 55
       1 ,     0 ,     0 ,     0 ,	\ 56
       1 ,     0 ,     0 ,     0 ,	\ 57
       1 ,     0 ,     0 ,     0 ,	\ 58
       1 ,     0 ,     0 ,     0 ,	\ 59
       1 ,     0 ,     0 ,     0 ,	\ 60
       1 ,     0 ,     0 ,     0 ,	\ 61
       1 ,     0 ,     0 ,     0 ,	\ 62
       1 ,     0 ,     0 ,     0 ,	\ 63
       1 ,     0 ,     0 ,     0 ,	\ 64
       1 ,     0 ,     0 ,     0 ,	\ 65
       1 ,     0 ,     0 ,     0 ,	\ 66
       1 ,     0 ,     0 ,     0 ,	\ 67
       1 ,     0 ,     0 ,     0 ,	\ 68
       1 ,     0 ,     0 ,     0 ,	\ 69
       1 ,     0 ,     0 ,     0 ,	\ 70
       1 ,     0 ,     0 ,     0 ,	\ 71
       1 ,     0 ,     0 ,     0 ,	\ 72
       1 ,     0 ,     0 ,     0 ,	\ 73
       1 ,     0 ,     0 ,     0 ,	\ 74
       1 ,     0 ,     0 ,     0 ,	\ 75
       1 ,     0 ,     0 ,     0 ,	\ 76
       1 ,     0 ,     0 ,     0 ,	\ 77
       1 ,     0 ,     0 ,     0 ,	\ 78
       1 ,     0 ,     0 ,     0 ,	\ 79
       1 ,     0 ,     0 ,     0 ,	\ 80
       1 ,     0 ,     0 ,     0 ,	\ 81
       1 ,     0 ,     0 ,     0 ,	\ 82
       1 ,     0 ,     0 ,     0 ,	\ 83
       1 ,     0 ,     0 ,     0 ,	\ 84
       1 ,     0 ,     0 ,     0 ,	\ 85
       1 ,     0 ,     0 ,     0 ,	\ 86
       1 ,     0 ,     0 ,     0 ,	\ 87
: burm_cost@ ( i j -- x )
  burm_cost @ ;

: burm_ntname1 ." stmt" ;
: burm_ntname2 ." addr" ;
: burm_ntname3 ." reg" ;
: burm_ntname4 ." cons" ;
: burm_ntname5 ." con" ;
: burm_ntname6 ." lmul" ;
: burm_ntname7 ." ldiv" ;
88 array_noallot [burm_ntname]
  ' burm_ntname1 ,
  ' burm_ntname2 ,
  ' burm_ntname3 ,
  ' burm_ntname4 ,
  ' burm_ntname5 ,
  ' burm_ntname6 ,
  ' burm_ntname7 ,
: burm_ntname ( rule -- )
  [burm_ntname] @ execute ;

bl word burm_STATE_LABEL find nip 0<> [IF]
  : burm_INCLUDE_EXTRA ;
[ELSE]
  bl word STATE_LABEL find nip 0<> [IF]
    : burm_INCLUDE_EXTRA ;
    : burm_STATE_LABEL STATE_LABEL ;
    : burm_STATE_LABEL@ STATE_LABEL@ ;
    : burm_STATE_LABEL! STATE_LABEL! ;
    : burm_LEFT_CHILD LEFT_CHILD ;
    : burm_LEFT_CHILD@ LEFT_CHILD@ ;
    : burm_LEFT_CHILD! LEFT_CHILD! ;
    : burm_OP_LABEL OP_LABEL ;
    : burm_OP_LABEL@ OP_LABEL@ ;
    : burm_OP_LABEL! OP_LABEL! ;
    : burm_RIGHT_CHILD RIGHT_CHILD ;
    : burm_RIGHT_CHILD@ RIGHT_CHILD@ ;
    : burm_RIGHT_CHILD! RIGHT_CHILD! ;
  [ENDIF] \ STATE_LABEL
[ENDIF] \ burm_STATE_LABEL

bl word burm_INCLUDE_EXTRA find nip 0<> [IF]

: burm_label ( node-addr -- state )
  dup 0= burm_assert" NULL pointer passed to burm_label"
  dup burm_OP_LABEL@ burm_arity@ case
    0 of
      >r NIL NIL
      r@ burm_OP_LABEL@ burm_state
      dup r> burm_STATE_LABEL! endof
    1 of
      >r r@ burm_LEFT_CHILD@ recurse
      NIL
      r@ burm_OP_LABEL@ burm_state
      dup r> burm_STATE_LABEL! endof
    2 of
      >r r@ burm_LEFT_CHILD@ recurse
      r@ burm_RIGHT_CHILD@ recurse
      r@ burm_OP_LABEL@ burm_state
      dup r> burm_STATE_LABEL! endof
    >r true burm_assert" Bad op in burm_label" r> endcase ;

: burm_kids1
  >r
  rdrop ;
: burm_kids2
  >r
  r@
  rdrop ;
: burm_kids3
  >r
  r@ burm_LEFT_CHILD@
  rdrop ;
: burm_kids4
  >r
  r@ burm_RIGHT_CHILD@
  r@ burm_LEFT_CHILD@
  rdrop ;
88 array [burm_kids]
  ' burm_kids1 3 [burm_kids] !
  ' burm_kids1 4 [burm_kids] !
  ' burm_kids1 5 [burm_kids] !
  ' burm_kids2 1 [burm_kids] !
  ' burm_kids2 6 [burm_kids] !
  ' burm_kids2 7 [burm_kids] !
  ' burm_kids3 8 [burm_kids] !
  ' burm_kids3 9 [burm_kids] !
  ' burm_kids3 10 [burm_kids] !
  ' burm_kids3 11 [burm_kids] !
  ' burm_kids3 20 [burm_kids] !
  ' burm_kids3 21 [burm_kids] !
  ' burm_kids3 23 [burm_kids] !
  ' burm_kids3 24 [burm_kids] !
  ' burm_kids3 25 [burm_kids] !
  ' burm_kids3 26 [burm_kids] !
  ' burm_kids3 33 [burm_kids] !
  ' burm_kids3 74 [burm_kids] !
  ' burm_kids3 80 [burm_kids] !
  ' burm_kids3 81 [burm_kids] !
  ' burm_kids4 2 [burm_kids] !
  ' burm_kids4 12 [burm_kids] !
  ' burm_kids4 13 [burm_kids] !
  ' burm_kids4 14 [burm_kids] !
  ' burm_kids4 15 [burm_kids] !
  ' burm_kids4 16 [burm_kids] !
  ' burm_kids4 17 [burm_kids] !
  ' burm_kids4 18 [burm_kids] !
  ' burm_kids4 19 [burm_kids] !
  ' burm_kids4 22 [burm_kids] !
  ' burm_kids4 27 [burm_kids] !
  ' burm_kids4 28 [burm_kids] !
  ' burm_kids4 29 [burm_kids] !
  ' burm_kids4 30 [burm_kids] !
  ' burm_kids4 31 [burm_kids] !
  ' burm_kids4 32 [burm_kids] !
  ' burm_kids4 34 [burm_kids] !
  ' burm_kids4 35 [burm_kids] !
  ' burm_kids4 36 [burm_kids] !
  ' burm_kids4 37 [burm_kids] !
  ' burm_kids4 38 [burm_kids] !
  ' burm_kids4 39 [burm_kids] !
  ' burm_kids4 40 [burm_kids] !
  ' burm_kids4 41 [burm_kids] !
  ' burm_kids4 42 [burm_kids] !
  ' burm_kids4 43 [burm_kids] !
  ' burm_kids4 44 [burm_kids] !
  ' burm_kids4 45 [burm_kids] !
  ' burm_kids4 46 [burm_kids] !
  ' burm_kids4 47 [burm_kids] !
  ' burm_kids4 48 [burm_kids] !
  ' burm_kids4 49 [burm_kids] !
  ' burm_kids4 50 [burm_kids] !
  ' burm_kids4 51 [burm_kids] !
  ' burm_kids4 52 [burm_kids] !
  ' burm_kids4 53 [burm_kids] !
  ' burm_kids4 54 [burm_kids] !
  ' burm_kids4 55 [burm_kids] !
  ' burm_kids4 56 [burm_kids] !
  ' burm_kids4 57 [burm_kids] !
  ' burm_kids4 58 [burm_kids] !
  ' burm_kids4 59 [burm_kids] !
  ' burm_kids4 60 [burm_kids] !
  ' burm_kids4 61 [burm_kids] !
  ' burm_kids4 62 [burm_kids] !
  ' burm_kids4 63 [burm_kids] !
  ' burm_kids4 64 [burm_kids] !
  ' burm_kids4 65 [burm_kids] !
  ' burm_kids4 66 [burm_kids] !
  ' burm_kids4 67 [burm_kids] !
  ' burm_kids4 68 [burm_kids] !
  ' burm_kids4 69 [burm_kids] !
  ' burm_kids4 70 [burm_kids] !
  ' burm_kids4 71 [burm_kids] !
  ' burm_kids4 72 [burm_kids] !
  ' burm_kids4 73 [burm_kids] !
  ' burm_kids4 75 [burm_kids] !
  ' burm_kids4 76 [burm_kids] !
  ' burm_kids4 77 [burm_kids] !
  ' burm_kids4 78 [burm_kids] !
  ' burm_kids4 79 [burm_kids] !
  ' burm_kids4 82 [burm_kids] !
  ' burm_kids4 83 [burm_kids] !
  ' burm_kids4 84 [burm_kids] !
  ' burm_kids4 85 [burm_kids] !
  ' burm_kids4 86 [burm_kids] !
  ' burm_kids4 87 [burm_kids] !
: burm_kids ( node-addr rule -- node-addr ... node-addr )
  over 0= burm_assert" NULL pointer passed to burm_kids"
  [burm_kids] @ execute ;

: burm_child ( node-addr index -- node-addr )
  over 0= burm_assert" NULL pointer passed to burm_child"
  case
    0 of burm_LEFT_CHILD endof
    1 of burm_RIGHT_CHILD endof
    >r true burm_assert" Bad index in burm_child" r> endcase ;

: burm_op_label ( node-addr -- op )
  over 0= burm_assert" NULL pointer passed to burm_op_label"
  burm_OP_LABEL@ ;

: burm_state_label ( node-addr -- state )
  over 0= burm_assert" NULL pointer passed to burm_state_label"
  burm_STATE_LABEL@ ;

[ENDIF] \ burm_INCLUDE_EXTRA


:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname 	drop ;
:noname  ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_eq 0 r@ node_cost ! r> inst_ok drop ;
:noname 	>r ['] asm_sltui r> inst_ok ;
:noname 	>r ['] asm_sltu r> inst_ok ;
:noname 	>r ['] asm_slti r> inst_ok ;
:noname 	>r ['] asm_slt r> inst_ok ;
:noname 	>r ['] asm_rshi r> inst_ok ;
:noname 	>r ['] asm_rshui r> inst_ok ;
:noname 	>r ['] asm_rsh r> inst_ok ;
:noname 	>r ['] asm_rshu r> inst_ok ;
:noname 	>r ['] asm_lshi r> inst_ok ;
:noname 	>r ['] asm_lshi r> inst_ok ;
:noname 	>r ['] asm_lsh r> inst_ok ;
:noname 	>r ['] asm_lsh r> inst_ok ;
:noname 	>r NIL ['] asm_not r> inst_ok ;
:noname 	>r ['] asm_xori r> inst_ok ;
:noname 	>r ['] asm_xor r> inst_ok ;
:noname 	>r ['] asm_ori r> inst_ok ;
:noname 	>r ['] asm_or r> inst_ok ;
:noname 	>r ['] asm_andi r> inst_ok ;
:noname 	>r ['] asm_and r> inst_ok ;
:noname 	>r NIL ['] asm_abs r> inst_ok ;
:noname 	>r NIL ['] asm_neg r> inst_ok ;
:noname 	>r NIL ['] asm_divh r> inst_ok ;
:noname 	>r NIL ['] asm_divl r> inst_ok ;
:noname 	>r ['] asm_div r> inst_ok ;
:noname 	>r NIL ['] asm_mulh r> inst_ok ;
:noname 	>r NIL ['] asm_mull r> inst_ok ;
:noname 	>r ['] asm_mul r> inst_ok ;
:noname 	>r ['] asm_sub r> inst_ok ;
:noname 	>r ['] asm_addi r> inst_ok ;
:noname 	>r ['] asm_add r> inst_ok ;
:noname 	>r 0 r@ node_reg ! ['] asm_storeregi r> inst_ok drop ;
:noname 	>r 0 r@ node_reg ! ['] asm_storei r> inst_ok drop ;
:noname 	>r 0 r@ node_reg ! ['] asm_storeregc r> inst_ok drop ;
:noname 	>r 0 r@ node_reg ! ['] asm_storec r> inst_ok drop ;
:noname 	>r r@ inst_notdone NIL ['] asm_fetchregi r@ inst inst_s!_list @ slist_insert drop r> inst_ok ;
:noname 	>r r@ inst_notdone NIL ['] asm_fetchi r@ inst inst_s!_list @ slist_insert drop r> inst_ok ;
:noname 	>r r@ inst_notdone NIL ['] asm_fetchregc r@ inst inst_s!_list @ slist_insert drop r> inst_ok ;
:noname 	>r r@ inst_notdone NIL ['] asm_fetchc r@ inst inst_s!_list @ slist_insert drop r> inst_ok ;
:noname 	node_dup >r r@ inst_notdone NIL ['] asm_lit r> inst_ok ;
:noname 	node_dup >r r@ inst_notdone NIL ['] asm_lit r> inst_ok ;
:noname 	dup inst_done ;
:noname 	dup inst_done ;
:noname 	 ;
:noname 	>r dup node_val @ r@ node_val ! ['] asm_addr r> inst_ok ;
:noname 	2drop ;
88 array_noallot [burm_reduce]
  0 ,
  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,
  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,
  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,
  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,
  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,
  ,  ,  ,  ,  ,  ,  ,
: burm_reduce ( goal node-addr -- )
  \ ." printReduce1" hex.s cr
  tuck burm_STATE_LABEL@ swap burm_rule
  dup 0= if
    nip nip
    burm_assert" no cover" cr else
    2dup 2>r
    dup burm_nts@ >r				\ (R: goal node_addr nts-addr)
    depth >r burm_kids depth r> - 2 +
    r>
    swap case
      0 of
        endof
      1 of
        swap >r endof
      2 of
        rot >r swap >r endof endcase
    >r
    begin
      r> dup @ dup 0<> while
      r> rot cell+ >r
      recurse
      repeat
    2drop
    2r>
    [burm_reduce] @ execute
    endif
  \ ." printReduce2" hex.s cr
  ;

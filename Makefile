# Makefile	makefile for testoutputs
#
# Copyright (C) 1995-96 Martin Anton Ertl, Christian Pirker
#
# This file is part of RAFTS.
#
#	RAFTS is free software; you can redistribute it and/or
#	modify it under the terms of the GNU General Public License
#	as published by the Free Software Foundation; either version 2
#	of the License, or (at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

DIR		= examples

FORTH		= gforth
FORTH_FILE	= compiler.fs
FORTH_FILE.p	= $(DIR)/pperf.fs
TIME		= time
DIFF		= diff

COMPILER	= compiler.fs options.fs \
		  mips/r3000.asm.fs mips/r3000.asm.opc.fs regs.fs \
		  mips/r3000.disasm.fs mips/r3000.disasm.opc.fs \
		  node.fs dataflow.fs basic.fs func.fs \
		  primitives.fs control.fs primitives_ext.fs \
		  inst-selection.fs inst-scheduling.fs \
		  stdlib/stdlib.fs stdlib/marray.fs stdlib/array.fs \
		  stdlib/slist.fs stdlib/dlist.fs stdlib/queue.fs \
		  stdlib/stack.fs stdlib/btree.fs stdlib/string.fs

%.dmp:		%.fs $(COMPILER)
		$(FORTH) $(FORTH_FILE) $(FORTH_FILE) $< >$@ 2>&1
%.p1:		%.fs $(COMPILER)
		$(TIME) $(FORTH) $(FORTH_FILE) $(FORTH_FILE) $< >$@ 2>&1
%.p2:		%.fs $(COMPILER)
		$(TIME) $(FORTH) $(FORTH_FILE.p) $< >$@ 2>&1
%.diff:		%.p1 %.p2
		-$(DIFF) $^ >$@
BURG	= burg/burg
BFEF	= burg/bfef
BURGFLAGS = -d -= -I

%.gr:	%.burg $(BFEF).awk
	$(BFEF) $< >$@
%.fs:	%.gr
	$(BURG) -F $(BURGFLAGS) -o $@ $<

.SUFFIXES:	.dmp .p1 .p2 .diff

FILES		= $(wildcard $(DIR)/dmp*.fs) \
		  $(wildcard $(DIR)/s*.fs)
DMP		= $(FILES:.fs=.dmp)
FILES.p1	= $(FILES:.fs=.p1)
FILES.p2	= $(FILES:.fs=.p2)
DIFFS		= $(FILES:.fs=.diff)

all:		mips/grammar.fs install_mips
install_i486:
		ln -sf i486/header.fs
		ln -sf i486/grammar.fs
install_mips:
		ln -sf mips/header.fs
		ln -sf mips/grammar.fs

dmp:		$(DMP)
dmpclean:	
		$(RM) $(DMP)
diff:		$(FILES.p1) $(FILES.p2) $(DIFFS)
diffclean:
		$(RM) $(FILES.p1) $(FILES.p2) $(DIFFS)

clean:		dmpclean diffclean
distclean:	clean
		$(RM) *.dmp *.p1 *.p2 *.diff

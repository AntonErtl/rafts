# $Id: Makefile,v 1.1 1995/10/06 18:12:53 anton Exp $
#
# Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
# All Rights Reserved.
#
# $Log: Makefile,v $
# Revision 1.1  1995/10/06 18:12:53  anton
# Initial revision
#

FORTH		= gforth
FORTH_FILE	= compiler.fs
FORTH_FILE_T	= compilert.fs
FORTH_FILE.p	= pperf.fs
TIME		= time
DIFF		= diff

COMPILER	= compiler.fs options.fs \
		  mips/r3000.asm.fs mips/r3000.asm.opc.fs regs.fs \
		  mips/r3000.disasm.fs mips/r3000.disasm.opc.fs \
		  node.fs dataflow.fs basic.fs func.fs cacheflush.fs \
		  primitives.fs control.fs primitives_ext.fs \
		  inst-selection.fs inst-scheduling.fs \
		  stdlib/stdlib.fs stdlib/marray.fs stdlib/array.fs \
		  stdlib/slist.fs stdlib/dlist.fs stdlib/queue.fs \
		  stdlib/stack.fs stdlib/btree.fs stdlib/string.fs

%.dmp:		%.fs $(COMPILER)
		$(FORTH) $(FORTH_FILE) $(FORTH_FILE) $< >$@
%.p1:		%.fs $(COMPILER)
		$(TIME) $(FORTH) $(FORTH_FILE) $(FORTH_FILE) $< >$@ 2>&1
%.p2:		%.fs $(COMPILER)
		$(TIME) $(FORTH) $(FORTH_FILE.p) $< >$@ 2>&1
%.diff:		%.p1 %.p2
		-$(DIFF) $^ >$@
.SUFFIXES:	.dmp .p1 .p2 .diff

DIR		= examples
FILES		= $(DIR)/dmp0.fs $(DIR)/dmp1.fs \
		  $(DIR)/dmp2.fs $(DIR)/dmp3.fs \
  		  $(DIR)/dmp4.fs $(DIR)/dmp5.fs \
  		  $(DIR)/s.fs $(DIR)/ss.fs \
		  $(DIR)/sss.fs $(DIR)/ssss.fs
DMP		= $(FILES:.fs=.dmp)
FILES.p1	= $(FILES:.fs=.p1)
FILES.p2	= $(FILES:.fs=.p2)
DIFFS		= $(FILES:.fs=.diff)

all:		$(DMP)
alldiff:	$(FILES.p1) $(FILES.p2) $(DIFFS)
clean:
		$(RM) $(DMP) $(DIFFS) $(FILES.p1) $(FILES.p2)

DIR.p		= performance
PERFS		= $(DIR.p)/perf.fact1.fs $(DIR.p)/perf.fact2.fs \
		  $(DIR.p)/perf.fib1.fs $(DIR.p)/perf.fib2.fs \
		  $(DIR.p)/perf.prim1.fs
PERFS.p1	= $(PERFS:.fs=.p1)
PERFS.p2	= $(PERFS:.fs=.p2)
PERFS.diff	= $(PERFS:.fs=.diff)

perf:		$(PERFS.p1) $(PERFS.p2) $(PERFS.diff)
perfclean:
		$(RM) $(PERFS.p1) $(PERFS.p2) $(PERFS.diff)

distclean:	clean perfclean
		$(RM) *.dmp *.p1 *.p2 *.diff

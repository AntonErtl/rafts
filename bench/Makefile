TIME	= time

CC = gcc
CFLAGS = -I. -I../../../../public/gforth2c-0.1alpha/ -g

GFORTH	= gforth
GFLAGS	= -m 4M

.c.opt:
	$(CC) $(CFLAGS) -O2 -o $@ $<

.c.o:
	$(CC) $(CFLAGS) -O2 -c -o $@ $<

.c.noopt:
	$(CC) $(CFLAGS) -o $@ $<

.o.odmp:
	dis -S $< > $@

.opt.cdmp:
	-$(TIME) $< > $@ 2>&1

.fs.gdmp:
	$(TIME) $(GFORTH) $(GFLAGS) $< -e "main bye" > $@ 2>&1

.fs.fi-0:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-0 $< -e "main savesystem $@ bye" > /dev/null 2>&1
.fs.fi-1:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-1 $< -e "main savesystem $@ bye" > /dev/null 2>&1
.fs.fi-2:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-2 $< -e "main savesystem $@ bye" > /dev/null 2>&1
.fs.fi-3:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-3 $< -e "main savesystem $@ bye" > /dev/null 2>&1
.fs.fi-4:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-4 $< -e "main savesystem $@ bye" > /dev/null 2>&1
.fs.fi-5:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-5 $< -e "main savesystem $@ bye" > /dev/null 2>&1
.fs.fi-6:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-6 $< -e "main savesystem $@ bye" > /dev/null 2>&1
.fs.fi-7:
	$(GFORTH) $(GFLAGS) -i ../rafts.fi-7 $< -e "main savesystem $@ bye" > /dev/null 2>&1

.fi-0.rdmp-0:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-1.rdmp-1:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-2.rdmp-2:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-3.rdmp-3:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-4.rdmp-4:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-5.rdmp-5:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-6.rdmp-6:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-7.rdmp-7:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1

.SUFFIXES:	.fs .opt .noopt .odmp .cdmp .gdmp .fi-0 .fi-1 .fi-2 .fi-3 .fi-4 .fi-5 .fi-6 .fi-7 .rdmp-0 .rdmp-1 .rdmp-2 .rdmp-3 .rdmp-4 .rdmp-5 .rdmp-6 .rdmp-7

t0		= bubble-sort
t0_c		= $(t0).c
t0_fs		= $(t0).fs
t0_opt		= $(t0).opt
t0_noopt	= $(t0).noopt
t0_cdmp		= $(t0).cdmp
t0_odmp		= $(t0).odmp
t0_gdmp		= $(t0).gdmp
t0_fi		= $(t0).fi-0 $(t0).fi-1 $(t0).fi-2 $(t0).fi-3 $(t0).fi-4 $(t0).fi-5 $(t0).fi-6 $(t0).fi-7
t0_rdmp-N	= $(t0).rdmp-0 $(t0).rdmp-1 $(t0).rdmp-2 $(t0).rdmp-3 $(t0).rdmp-4 $(t0).rdmp-5 $(t0).rdmp-6 $(t0).rdmp-7

all::		bubble-sort
bubble-sort:	t0
		;
t0:		$(t0_cdmp) $(t0_gdmp) $(t0_fi) $(t0_rdmp-N)
		;
clean::
		$(RM) $(t0_cdmp)
		$(RM) $(t0_gdmp)
		$(RM) $(t0_fi)
		$(RM) $(t0_rdmp-N)

t1		= fib
t1_c		= $(t1).c
t1_fs		= $(t1).fs
t1_opt		= $(t1).opt
t1_noopt	= $(t1).noopt
t1_cdmp		= $(t1).cdmp
t1_odmp		= $(t1).odmp
t1_gdmp		= $(t1).gdmp
t1_fi		= $(t1).fi-0 $(t1).fi-1 $(t1).fi-2 $(t1).fi-3 $(t1).fi-4 $(t1).fi-5 $(t1).fi-6 $(t1).fi-7
t1_rdmp-N	= $(t1).rdmp-0 $(t1).rdmp-1 $(t1).rdmp-2 $(t1).rdmp-3 $(t1).rdmp-4 $(t1).rdmp-5 $(t1).rdmp-6 $(t1).rdmp-7

all::		fib
fib:		t1
		;
t1:		$(t1_cdmp) $(t1_gdmp) $(t1_fi) $(t1_rdmp-N)
		;
clean::
		$(RM) $(t1_cdmp)
		$(RM) $(t1_gdmp)
		$(RM) $(t1_fi)
		$(RM) $(t1_rdmp-N)

t2		= matrix-mult
t2_c		= $(t2).c
t2_fs		= $(t2).fs
t2_opt		= $(t2).opt
t2_noopt	= $(t2).noopt
t2_cdmp		= $(t2).cdmp
t2_odmp		= $(t2).odmp
t2_gdmp		= $(t2).gdmp
t2_fi		= $(t2).fi-0 $(t2).fi-1 $(t2).fi-2 $(t2).fi-3 $(t2).fi-4 $(t2).fi-5 $(t2).fi-6 $(t2).fi-7
t2_rdmp-N	= $(t2).rdmp-0 $(t2).rdmp-1 $(t2).rdmp-2 $(t2).rdmp-3 $(t2).rdmp-4 $(t2).rdmp-5 $(t2).rdmp-6 $(t2).rdmp-7

all::		matrix-mult
matrix-mult:	t2
		;
t2:		$(t2_cdmp) $(t2_gdmp) $(t2_fi) $(t2_rdmp-N)
		;
clean::
		$(RM) $(t2_cdmp)
		$(RM) $(t2_gdmp)
		$(RM) $(t2_fi)
		$(RM) $(t2_rdmp-N)

t3		= siev
t3_c		= $(t3).c
t3_fs		= $(t3).fs
t3_opt		= $(t3).opt
t3_noopt	= $(t3).noopt
t3_cdmp		= $(t3).cdmp
t3_odmp		= $(t3).odmp
t3_gdmp		= $(t3).gdmp
t3_fi		= $(t3).fi-0 $(t3).fi-1 $(t3).fi-2 $(t3).fi-3 $(t3).fi-4 $(t3).fi-5 $(t3).fi-6 $(t3).fi-7
t3_rdmp-N	= $(t3).rdmp-0 $(t3).rdmp-1 $(t3).rdmp-2 $(t3).rdmp-3 $(t3).rdmp-4 $(t3).rdmp-5 $(t3).rdmp-6 $(t3).rdmp-7

all::		siev
siev:		t3
		echo
t3:		$(t3_cdmp) $(t3_gdmp) $(t3_fi) $(t3_rdmp-N)
		echo
clean::
		$(RM) $(t3_cdmp)
		$(RM) $(t3_gdmp)
		$(RM) $(t3_fi)
		$(RM) $(t3_rdmp-N)


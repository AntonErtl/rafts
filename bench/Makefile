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

.fi-0.dmp0:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-1.dmp1:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-2.dmp2:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-3.dmp3:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1
.fi-4.dmp4:
	$(TIME) $(GFORTH) $(GFLAGS) -i $< -e "main bye" > $@ 2>&1

.SUFFIXES:	.fs .opt .noopt .odmp .cdmp .gdmp .fi-0 .fi-1 .fi-2 .fi-3 .fi-4 .dmp0 .dmp1 .dmp2 .dmp3 .dmp4

files	= $(wildcard *.c)
noopt	= $(files:.c=.noopt)
opt	= $(files:.c=.opt)
cdmp	= $(files:.c=.cdmp)
odmp	= $(files:.c=.odmp)

gfiles	= $(wildcard *.fs)
gdmp	= $(gfiles:.fs=.gdmp)
fi	= $(gfiles:.fs=.fi-0) $(gfiles:.fs=.fi-1) $(gfiles:.fs=.fi-2) $(gfiles:.fs=.fi-3) $(gfiles:.fs=.fi-4)
dmpN	= $(gfiles:.fs=.dmp0) $(gfiles:.fs=.dmp1) $(gfiles:.fs=.dmp2) $(gfiles:.fs=.dmp3) $(gfiles:.fs=.dmp4)

all:	cdmp gdmp fi dmpN

opt:	$(opt)
noopt:	$(noopt)
clean::
	$(RM) $(opt)
	$(RM) $(noopt)
odmp:	$(odmp)
clean::
	$(RM) $(odmp)

cdmp:	$(cdmp)
gdmp:	$(gdmp)
fi:	$(fi)
dmpN:	$(dmpN)
clean::
	$(RM) $(fi)
	$(RM) $(cdmp)
	$(RM) $(gdmp)
	$(RM) $(dmpN)

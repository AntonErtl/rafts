TIME	= time

CC = gcc
CFLAGS = -I../../../../public/gforth2c-0.1alpha/ -g

GFORTH	= gforth
GFLAGS	= -m 4M -i ../compiler.fi

.c.opt:
	$(CC) $(CFLAGS) -O2 -o $@ $<

.c.o:
	$(CC) $(CFLAGS) -O2 -c -o $@ $<

.c.noopt:
	$(CC) $(CFLAGS) -o $@ $<

.o.odmp:
	dis -S $< > $@

.opt.dmp:
	dis -S $< > $@

.fs.dmp2:
	$(TIME) $(GFORTH) $(GFLAGS)-2 $< -e "main bye" > $@ 2>&1

.fs.dmp4:
	$(TIME) $(GFORTH) $(GFLAGS)-4 $< -e "main bye" > $@ 2>&1

.SUFFIXES:	.fs .opt .noopt .odmp .dmp .dmp2 .dmp4

files = $(wildcard *.c)
obj = $(files:.c=.o)
noopt = $(files:.c=.noopt)
opt = $(files:.c=.opt)
dmp = $(files:.c=.dmp)
odmp = $(files:.c=.odmp)

gfiles = $(wildcard *.fs)
dmp2 = $(gfiles:.fs=.dmp2)
dmp4 = $(.files:.fs=.dmp4)

all:	$(opt) $(noopt)
obj:	$(obj)
dmp:	$(dmp)
odmp:	$(odmp)

bench:	$(dmp2) $(dmp4)

clean:
	$(RM) $(opt) $(noopt)
	$(RM) $(obj)
	$(RM) $(dmp) $(odmp)

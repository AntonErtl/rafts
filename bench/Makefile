CC = gcc
CFLAGS = -I../../../../public/gforth2c-0.1alpha/ -g

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

.SUFFIXES:	.opt .noopt .odmp .dmp

files = $(wildcard *.c)
obj = $(files:.c=.o)
noopt = $(files:.c=.noopt)
opt = $(files:.c=.opt)
dmp = $(files:.c=.dmp)
odmp = $(files:.c=.odmp)

all:	$(opt) $(noopt)
obj:	$(obj)
dmp:	$(dmp)
odmp:	$(odmp)

clean:
	$(RM) $(opt) $(noopt)
	$(RM) $(obj)
	$(RM) $(dmp) $(odmp)

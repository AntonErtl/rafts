CFLAGS = -I/usr/users/pirky/diplom/gforth2c-0.1alpha

.c.opt:
	$(CC) $(CFLAGS) -O2 -o $@ $<

.c.noopt:
	$(CC) $(CFLAGS) -o $@ $<

.SUFFIXES:	.opt .noopt

files = $(wildcard *.c)
noopt = $(files:.c=.noopt)
opt = $(files:.c=.opt)

all:	$(opt) $(noopt)

clean:
	$(RM) $(opt) $(noopt)

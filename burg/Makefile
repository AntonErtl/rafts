# $Id: Makefile,v 1.1 1996/08/14 18:46:22 anton Exp $

CC	= gcc
CFLAGS	= -O2 # -Wall # -Wshadow -Wcast-qual -Wcast-align -Wstrict-prototypes -Winline
#CFLAGS	= -O
#CFLAGS	= -O -DNOLEX
#CFLAGS	= -g -DDEBUG
#CFLAGS	= -g -DNOLEX -DDEBUG
LDFLAGS	= -s
YFLAGS	= -d

YACC	= bison -y
LEX	= flex -l

BU_SRCS = burs.c closure.c delta.c item.c list.c map.c nonterminal.c \
	operator.c pattern.c queue.c rule.c table.c trim.c zalloc.c

FE_SRCS = be.c fe.c lex.c burg.c plank.c string.c symtab.c gram.y

BU_OBJS = $(BU_SRCS:.c=.o)
FE1_OBJS = $(FE_SRCS:.c=.o)
FE_OBJS = $(FE1_OBJS:.y=.o)

BURG	= burg
BURGFLAGS = -d -I -=

%.c:	%.gr
	./$(BURG) -C $(BURGFLAGS) -o $@ $<
%.fs:	%.gr
	./$(BURG) -F $(BURGFLAGS) -o $@ $<
.SUFFIXES:	.fs .c

all: $(BURG)
clean:
	$(RM) *.o y.tab.h gram.c core
	$(RM) sample sample.c tsample.c
	$(RM) *.aux *.log
distclean:	clean
	$(RM) $(BURG)
	$(RM) *.dvi

$(BURG): $(BU_OBJS) $(FE_OBJS)

$(BU_OBJS):	b.h
$(FE_OBJS):	b.h fe.h

lex.o:		y.tab.h
y.tab.h:	gram.c
gram.c:		gram.y

doc.dvi: doc.tex
	latex doc; latex doc

	# ./$(BURG) -F -I     <xx.gr   >xx.fs && $(CC) $(CFLAGS) $(LDFLAGS) -o xx xx.fs && ./xx
	# ./$(BURG) -F -I      xx.gr   >txx.fs && cmp txx.fs xx.fs
	# ./$(BURG) -F -I     <xx.gr -o txx.fs && cmp txx.fs xx.fs
	# ./$(BURG) -F -I      xx.gr -o txx.fs && cmp txx.fs xx.fs
	# ./$(BURG) -F -I -O0 <xx.gr   >txx.fs && cmp txx.fs xx.fs
	# ./$(BURG) -F -I -=  <xx.gr   >txx.fs && cmp txx.fs xx.fs

test: all xx.gr
	./$(BURG) -F -d -= -I      xx.gr   >txx.fs && diff -u txx.fs xx.fs >txx.diff

sample:	sample.gr
	./$(BURG) -C -d -= -I     <sample.gr   >sample.c && $(CC) $(CFLAGS) $(LDFLAGS) -o sample sample.c

test1: all sample.gr
	./$(BURG) -C -d -= -I     <sample.gr   >sample.c && $(CC) $(CFLAGS) $(LDFLAGS) -o sample sample.c && ./sample
	./$(BURG) -C -d -= -I      sample.gr   >tsample.c && cmp tsample.c sample.c
	./$(BURG) -C -d -= -I     <sample.gr -o tsample.c && cmp tsample.c sample.c
	./$(BURG) -C -d -= -I      sample.gr -o tsample.c && cmp tsample.c sample.c
	./$(BURG) -C -d -= -I -O0 <sample.gr   >tsample.c && cmp tsample.c sample.c
	./$(BURG) -C -d -= -I -=  <sample.gr   >tsample.c && cmp tsample.c sample.c

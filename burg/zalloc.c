#ident "@(#)$Id: zalloc.c,v 1.1 1996/08/14 18:42:33 anton Exp $";

#include "b.h"

int DEFUN(fatal, (name, line), char *name AND int line)
{
    fprintf(stderr, "assertion failed: file %s, line %d\n", name, line);
    exit(1);
    return 0;
}

PTR DEFUN(zalloc, (size), unsigned int size)
{
    PTR t = (PTR ) malloc(size);
    if (!t) {
	fprintf(stderr, "Malloc failed---PROGRAM ABORTED\n");
	exit(1);
    }
    memset(t, 0, size);
    return t;
}

void DEFUN(zfree, (p), PTR p)
{
    free(p);
}

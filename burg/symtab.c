#ident "@(#)$Id: symtab.c,v 1.1 1996/08/14 18:42:32 anton Exp $";

#include "b.h"
#include "fe.h"

static List symtab;

Symbol DEFUN(newSymbol, (name), char *name)
{
    Symbol s;

    s = (Symbol) zalloc(sizeof(struct symbol));
    assert(s);
    s->name = name;
    return s;
}

Symbol DEFUN(enter, (name, new), char *name AND boolean *new)
{
    List l;
    Symbol s;

    *new = FALSE;
    for (l = symtab; l; l = l->next) {
	s = (Symbol) l->x;
	if (!strcmp(name, s->name)) {
	    return s;
	}
    }
    *new = TRUE;
    s = newSymbol(name);
    symtab = newList(s, symtab);
    return s;
}

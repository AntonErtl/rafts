#ident "@(#)$Id: string.c,v 1.1 1996/08/14 18:42:32 anton Exp $";

#include "b.h"
#include "fe.h"

StrTable DEFUN_VOID(newStrTable)
{
    return (StrTable) zalloc(sizeof(struct strTable));
}

static StrTableElement DEFUN_VOID(newStrTableElement)
{
    return (StrTableElement) zalloc(sizeof(struct strTableElement));
}

void DEFUN(dumpStrTable, (t), StrTable t)
{ 
    List e;
    IntList r;

    printf("Begin StrTable\n");
    for (e=t->elems; e; e=e->next) {
	StrTableElement el = (StrTableElement) e->x;
	printf("%s: ", el->str);
	for (r=el->erulenos; r; r=r->next) {
	    int i = r->x;
	    printf("(%d)", i);
	}
	printf("\n");
    }
    printf("End StrTable\n");
}

StrTableElement DEFUN(addString, (t, s, eruleno, new),
	StrTable t AND char *s AND int eruleno AND boolean *new)
{
    List l;
    StrTableElement ste;

    assert(t);
    for (l=t->elems; l; l=l->next) {
	StrTableElement e = (StrTableElement) l->x;
	assert(e);
	if (!strcmp(s, e->str)) {
	    e->erulenos = newIntList(eruleno, e->erulenos);
	    *new = FALSE;
	    return e;
	}
    }
    ste = newStrTableElement();
    ste->erulenos = newIntList(eruleno, NULL);
    ste->str = (char *) zalloc(strlen(s) + 1);
    strcpy(ste->str, s);
    t->elems = newList(ste, t->elems);
    *new = TRUE;
    return ste;
}

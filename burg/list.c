#ident "@(#)$list.c,v 4.2 1992/01/07 17:32:57 todd Exp $";

#include "b.h"

IntList DEFUN(newIntList, (x, next), int x AND IntList next)
{
    IntList l;

    l = (IntList) zalloc(sizeof(*l));
    assert(l);
    l->x = x;
    l->next = next;
    return l;
}

List DEFUN(newList, (x, next), PTR x AND List next)
{
    List l;

    l = (List) zalloc(sizeof(*l));
    assert(l);
    l->x = x;
    l->next = next;
    return l;
}

List DEFUN(appendList, (x, l), PTR x AND List l)
{
    List last;
    List p;

    last = NULL;
    for (p=l; p; p=p->next)
	last = p;
    if (last) {
	last->next = newList(x, NULL);
	return l;
    } else
	return newList(x, NULL);
}

void DEFUN(foreachList, (f, l), ListFn f AND List l)
{
    for (; l; l=l->next)
	(*f)(l->x);
}

void DEFUN(reveachList, (f, l), ListFn f AND List l)
{
    if (l) {
	reveachList(f, l->next);
	(*f)(l->x);
    }
}

int DEFUN(length, (l), List l)
{
    int c = 0;

    for(; l; l=l->next)
	c++;
    return c;
}

#ident "@(#)$Id: nonterminal.c,v 1.1 1996/08/14 18:42:29 anton Exp $";

#include "b.h"

NonTerminal	start;
NonTerminalNum	max_nonterminal = 1;
NonTerminalNum	last_user_nonterminal;
List		nonterminals;

NonTerminal DEFUN(newNonTerminal, (name), char *name)
{
    NonTerminal nt;

    nt = (NonTerminal) zalloc(sizeof(struct nonterminal));
    assert(nt);
    if (max_nonterminal == 1)
	start = nt;
    nt->name = name;
    nt->num = max_nonterminal++;
    nonterminals = newList(nt, nonterminals);
    return nt;
}

int DEFUN(nonTerminalName, (buf, i), char *buf AND int i)
{
    List l;

    for (l=nonterminals; l; l=l->next) {
	NonTerminal nt = (NonTerminal) l->x;
	if (nt->num == i) {
	    strcpy(buf, nt->name);
	    return 1;
	}
    }
    strcpy(buf, "(Unknown NonTerminal)");
    return 0;
}

void DEFUN(dumpNonTerminal, (n), NonTerminal n)
{
    printf("%s(%d)", n->name, n->num);
}

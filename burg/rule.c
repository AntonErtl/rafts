#ident "@(#)$Id: rule.c,v 1.1 1996/08/14 18:42:31 anton Exp $";

#include "b.h"

RuleNum max_rule;
int max_erule_num;

struct rule stub_rule;

List rules;

Rule DEFUN(newRule, (delta, erulenum, lhs, pat),
	DeltaPtr delta AND ERuleNum erulenum AND NonTerminal lhs AND Pattern pat)
{
    Rule p;

    p = (Rule) zalloc(sizeof(struct rule));
    assert(p);
    ASSIGNCOST(p->delta, delta);
    p->erulenum = erulenum;
    if (erulenum > max_erule_num) {
	max_erule_num = erulenum;
    }
    p->num = max_rule++;
    p->lhs = lhs;
    p->pat = pat;

    rules = newList(p, rules);
    return p;
}

void DEFUN(dumpRule, (p), Rule p)
{
    dumpNonTerminal(p->lhs);
    printf(" : ");
    dumpPattern(p->pat);
    printf(" ");
    dumpCost(p->delta);
    printf("\n");
}

void DEFUN(dumpRuleList, (l), List l)
{
    foreachList((ListFn)dumpRule, l);
}

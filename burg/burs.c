#ident "@(#)$Id: burs.c,v 1.1 1996/08/14 18:42:25 anton Exp $";

#include "b.h"

__BEGIN_DECLS

static void EXFUN(doLeaf, (Operator leaf));

__END_DECLS

static void DEFUN(doLeaf, (leaf), Operator leaf)
{
    int new;
    List pl;
    Item_Set ts;
    Item_Set tmp;

    assert(leaf->arity == 0);
    ts = newItem_Set(leaf->table->relevant);
    for (pl = rules; pl; pl = pl->next) {
	Rule p = (Rule) pl->x;
	if (p->pat->op == leaf) {	
	    if (!ts->virgin[p->lhs->num].rule || p->delta < ts->virgin[p->lhs->num].delta) {
		ts->virgin[p->lhs->num].rule = p;
		ASSIGNCOST(ts->virgin[p->lhs->num].delta, p->delta);
		ts->op = leaf;
	    }
	}
    }
    trim(ts);
    zero(ts);
    tmp = encode(globalMap, ts, &new);
    if (new) {
	closure(ts);
	leaf->table->transition[0] = ts;
	addQ(globalQ, ts);
    } else {
	leaf->table->transition[0] = tmp;
	freeItem_Set(ts);
    }
}

void DEFUN_VOID(build)
{
    int new;
    List ol;
    Item_Set ts;

    globalQ = newQ();
    globalMap = newMapping(GLOBAL_MAP_SIZE);
    ts = newItem_Set(0);
    encode(globalMap, ts, &new);
    ts->closed = ts->virgin;
    addQ(globalQ, ts);

    foreachList((ListFn) doLeaf, leaves);

    debug(debugTables, printf("---initial set of states ---\n"));
    debug(debugTables, dumpMapping(globalMap));
    debug(debugTables, foreachList((ListFn) dumpItem_Set, globalQ->head));
	
    for (ts = popQ(globalQ); ts; ts = popQ(globalQ)) {
	for (ol = operators; ol; ol = ol->next) {
	    Operator op = (Operator) ol->x;
	    addToTable(op->table, ts);
	}
    }
}
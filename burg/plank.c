#ident "@(#)$Id: plank.c,v 1.4 1997/06/17 19:21:08 pirky Exp $";

#include "b.h"
#include "fe.h"

#define ERROR_VAL 0

int safely = 1;

static struct stateMapTable smt;
int exceptionTolerance = 0;
static int plankSize = 32;

extern int maxOperator;

static Plank EXFUN(newPlank, (NOARGS));
static PlankMap EXFUN(newPlankMap, (int));
static StateMap EXFUN(newStateMap, (NOARGS));
static Exception EXFUN(newException, (int, int));
static void EXFUN(enterStateMap, (PlankMap, DeltaPtr, int, int *));
static List EXFUN(assemblePlanks, (NOARGS));
static void EXFUN(assignRules, (RuleAST));
static int EXFUN(stateCompare, (Item_Set *, Item_Set *));
static int EXFUN(ruleCompare, (RuleAST *, RuleAST *));
static void EXFUN(renumber, (NOARGS));
static short *EXFUN(newVector, (NOARGS));
static int EXFUN(Width, (int));
static PlankMap EXFUN(mapToPmap, (Dimension));
static void EXFUN(doDimPmaps, (Operator));
static void EXFUN(doNonTermPmaps, (NonTerminal));
static void EXFUN(makePmaps, (NOARGS));
static void EXFUN(outPlank, (Plank));
static void EXFUN(purgePlanks, (List));
static void EXFUN(inToEx, (NOARGS));
static void EXFUN(makePlankRuleMacros, (NOARGS));
static void EXFUN(makePlankRule, (NOARGS));
static void EXFUN(exceptionSwitch, (List, char *, char *, char *, int, char *));
static void EXFUN(doPlankLabel, (Operator));
static void EXFUN(doPlankLabelSafely, (Operator));
static void EXFUN(doPlankLabelMacrosSafely, (Operator));
static void EXFUN(makePlankState, (NOARGS));

static Plank DEFUN_VOID(newPlank)
{
    Plank p;
    char buf[50];
    static int num = 0;

    p = (Plank) zalloc(sizeof(struct plank));
    LANG(sprintf(buf, "%s_plank_%d", prefix, num++),
	sprintf(buf, "%s-plank-%d", prefix, num++));
    p->name = (char *) zalloc(strlen(buf)+1);
    strcpy(p->name, buf);
    return p;
}

static PlankMap DEFUN(newPlankMap, (offset), int offset)
{
    PlankMap im;

    im = (PlankMap) zalloc(sizeof(struct plankMap));
    im->offset = offset;
    return im;
}

static StateMap DEFUN_VOID(newStateMap)
{
    char buf[50];
    static int num = 0;
    StateMap sm;

    sm = (StateMap) zalloc(sizeof(struct stateMap));
    sprintf(buf, "f%d", num++);
    sm->fieldname = (char *) zalloc(strlen(buf)+1);
    strcpy(sm->fieldname, buf);
    return sm;
}

static Exception DEFUN(newException, (index, value), int index AND int value)
{
    Exception e;

    e = (Exception) zalloc(sizeof(struct except));
    e->index = index;
    e->value = value;
    return e;
}

static void DEFUN(enterStateMap, (im, v, width, new),
	PlankMap im AND short *v AND int width AND int *new)
{
    int i;
    StateMap sm;
    List l;
    int size;

    assert(im);
    assert(v);
    assert(width > 0);
    size = globalMap->count - (safely ? 0 : 1);

    for (l = smt.maps; l; l = l->next) {
	int ecount;

	sm = (StateMap) l->x;
	ecount = 0;
	for (i = 0; i < size; i++) {
	    if (v[i] != -1 && sm->value[i] != -1 && v[i] != sm->value[i]) {
		if (++ecount > exceptionTolerance)
		    goto again;
	    }
	}
	for (i = 0; i < size; i++) {
	    assert(v[i] >= 0);
	    assert(sm->value[i] >= 0);
	    if (v[i] == -1)
		continue;
	    if (sm->value[i] == -1)
		sm->value[i] = v[i];
	    else if (v[i] != sm->value[i])
		im->exceptions = newList(newException(i, v[i]), im->exceptions);
	}
	im->values = sm;
	if (width > sm->width)
	    sm->width = width;
	*new = 0;
	return;
    again: ;
    }
    sm = newStateMap();
    im->values = sm;
    sm->value = v;
    sm->width = width;
    *new = 1;
    smt.maps = newList(sm, smt.maps);
}

static List DEFUN_VOID(assemblePlanks)
{
    List planks = 0;
    Plank pl;
    List p;
    List s;

    for (s = smt.maps; s; s = s->next) {
	StateMap sm = (StateMap) s->x;
	for (p = planks; p; p = p->next) {
	    pl = (Plank) p->x;
	    if (sm->width <= plankSize - pl->width) {
		pl->width += sm->width;
		pl->fields = newList(sm, pl->fields);
		sm->plank = pl;
		goto next;
	    }
	}
	pl = newPlank();
	pl->width = sm->width;
	pl->fields = newList(sm, 0);
	sm->plank = pl;
	planks = appendList(pl, planks);
    next: ;
    }
    return planks;
}

static Item_Set *sortedStates;
static RuleAST *sortedRules;

static int count;

static void DEFUN(assignRules, (ast), RuleAST ast)
{
    sortedRules[count++] = ast;
}

static int DEFUN(stateCompare, (s, t), Item_Set *s AND Item_Set *t)
{
    return strcmp((*s)->op->name, (*t)->op->name);
}

static int DEFUN(ruleCompare, (s, t), RuleAST *s AND RuleAST *t)
{
    return strcmp((*s)->lhs, (*t)->lhs);
}

void DEFUN_VOID(dumpSortedStates)
{
    int i;
	
    printf("dump Sorted States: ");
    for (i = 0; i < globalMap->count-(safely?0:1); i++)
	printf("%d ", sortedStates[i]->num);
    printf("\n");
}

void DEFUN_VOID(dumpSortedRules)
{
    int i;
	
    printf("dump Sorted Rules: ");
    for (i = 0; i < max_ruleAST; i++)
	printf("%d ", sortedRules[i]->rule->erulenum);
    printf("\n");
}

static void DEFUN_VOID(renumber)
{
    int i;
    Operator previousOp;
    NonTerminal previousLHS;
    int base_counter;

    sortedStates = (Item_Set*) zalloc(globalMap->count * sizeof(Item_Set));
    for (i = 1; i < globalMap->count; i++)
	sortedStates[i-1] = globalMap->set[i];
    qsort(sortedStates, globalMap->count-1, sizeof(Item_Set), /* (comparison_fn_t) */ stateCompare);
    previousOp = 0;
    for (i = 0; i < globalMap->count-1; i++) {
	sortedStates[i]->newNum = i;
	sortedStates[i]->op->stateCount++;
	if (previousOp != sortedStates[i]->op) {
	    sortedStates[i]->op->baseNum = i;
	    previousOp = sortedStates[i]->op;
	}
    }

    sortedRules = (RuleAST*) zalloc(max_ruleAST * sizeof(RuleAST));
    count = 0;
    foreachList((ListFn) assignRules, ruleASTs);
    qsort(sortedRules, max_ruleAST, sizeof(RuleAST), /* (comparison_fn_t) */ ruleCompare);
    previousLHS = 0;
    base_counter = 0;
    for (i = 0; i < max_ruleAST; i++) {
	if (previousLHS != sortedRules[i]->rule->lhs) {
	    sortedRules[i]->rule->lhs->baseNum = base_counter;
	    previousLHS = sortedRules[i]->rule->lhs;
	    if (safely)
		base_counter++; /* make space for 0 */
	}
	sortedRules[i]->rule->newNum = base_counter;
	sortedRules[i]->rule->lhs->ruleCount++;
	sortedRules[i]->rule->lhs->sampleRule = sortedRules[i]->rule; /* kludge for diagnostics */
	base_counter++;
    }
}

static short *DEFUN_VOID(newVector)
{
    short *p;
    p = (short *) zalloc(globalMap->count* sizeof(short));
    return p;
}

static int DEFUN(Width, (v), int v)
{
    int c;

    for (c = 0; v; v >>= 1)
	c++;
    return c;
}

static PlankMap DEFUN(mapToPmap, (d), Dimension d)
{
    PlankMap im;
    short *v;
    int i;
    int new;

    if (d->map->count == (2-safely))
	return 0;
    assert(d->map->count > (2-safely));
    im = newPlankMap(0);
    v = newVector();
    if (safely) {
	for (i = 0; i < globalMap->count-1; i++) {
	    int index = d->map->set[d->index_map.class[sortedStates[i]->num]->num]->num;
	    assert(index >= 0);
	    v[i+1] = index;
	}
	v[0] = 0;
	enterStateMap(im, v, Width(d->map->count-(safely?0:1)), &new);
    } else {
	for (i = 0; i < globalMap->count-1; i++) {
	    int index = d->map->set[d->index_map.class[sortedStates[i]->num]->num]->num;
	    v[i] = index-1;
	}
	enterStateMap(im, v, Width(d->map->count-(2-safely)), &new);
    }
    if (!new)
	zfree(v);
    return im;
}

static void DEFUN(doDimPmaps, (op), Operator op)
{
    int i, j;
    Dimension d;
    short *v;
    PlankMap im;
    int new;

    if (!op->table->rules)
	return;
    switch (op->arity) {
    case 0:
	break;
    case 1:
	d = op->table->dimen[0];
	if (d->map->count > 2-safely) {
	    v = newVector();
	    im = newPlankMap(op->baseNum);
	    for (i = 0; i < globalMap->count-1; i++) {
		int index = d->map->set[d->index_map.class[sortedStates[i]->num]->num]->num;
		if (index) {
		    Item_Set *ts = transLval(op->table, index, 0);
		    v[i+safely] = (*ts)->newNum - op->baseNum + safely;
		    assert(v[i+safely] >= 0);
		} else {
		    if (!safely)
			v[i] = -1;
		}
	    }
	    enterStateMap(im, v, Width(d->map->count-(2-safely)), &new);
	    if (!new)
		zfree(v);
	    d->pmap = im;
	}
	break;
    case 2:
	if (op->table->dimen[0]->map->count == (2-safely) &&
	    op->table->dimen[1]->map->count == (2-safely)) {
	    op->table->dimen[0]->pmap = 0;
	    op->table->dimen[1]->pmap = 0;
	} else if (op->table->dimen[0]->map->count == (2-safely)) {
	    v = newVector();
	    im = newPlankMap(op->baseNum);
	    d = op->table->dimen[1];
	    for (i = 0; i < globalMap->count-1; i++) {
		int index = d->map->set[d->index_map.class[sortedStates[i]->num]->num]->num;
		if (index) {
		    Item_Set *ts = transLval(op->table, 1, index);
		    v[i+safely] = (*ts)->newNum - op->baseNum+safely;
		    assert(v[i+safely] >= 0);
		} else {
		    if (!safely)
			v[i] = -1;
		}
	    }
	    enterStateMap(im, v, Width(d->map->count-(2-safely)), &new);
	    if (!new)
		zfree(v);
	    d->pmap = im;
	} else if (op->table->dimen[1]->map->count == (2-safely)) {
	    v = newVector();
	    im = newPlankMap(op->baseNum);
	    d = op->table->dimen[0];
	    for (i = 0; i < globalMap->count-1; i++) {
		int index = d->map->set[d->index_map.class[sortedStates[i]->num]->num]->num;
		if (index) {
		    Item_Set *ts = transLval(op->table, index, 1);
		    v[i +safely] = (*ts)->newNum - op->baseNum +safely;
		    assert(v[i +safely] >= 0);
		} else {
		    if (!safely)
			v[i] = -1;
		}
	    }
	    enterStateMap(im, v, Width(d->map->count-(2-safely)), &new);
	    if (!new)
		zfree(v);
	    d->pmap = im;
	} else {
	    op->table->dimen[0]->pmap = mapToPmap(op->table->dimen[0]);
	    op->table->dimen[1]->pmap = mapToPmap(op->table->dimen[1]);
	    /* output table */
	    SOURCE(("static unsigned %s %s_%s_transition[%d][%d] = {", 
		op->stateCount <= 255 ? "char" : "short",
		prefix,
		op->name,
		op->table->dimen[0]->map->count-(1-safely),
		op->table->dimen[1]->map->count-(1-safely)),
		("%d %d %smatrix-noallot %s-%s-transition", 
		op->table->dimen[0]->map->count-(1-safely),
		op->table->dimen[1]->map->count-(1-safely),
		op->stateCount <= 255?"c":"",
		prefix,
		op->name));
	    for (i = (1-safely); i < op->table->dimen[0]->map->count; i++) {
		if (i > (1-safely))
		    SOURCE((","),
			(""));
		SOURCE(("\n{"),
		    ("\n"));
		for (j = (1-safely); j < op->table->dimen[1]->map->count; j++) {
		    Item_Set *ts = transLval(op->table, i, j);
		    short diff;
		    if (j > (1-safely)) {
			SOURCE((","),
			    (" %s,", op->stateCount <= 255?"c":""));
			if (j % 10 == (1-safely))
			    SOURCE(("\t/* row %d, cols %d-%d*/\n",
				i-(1-safely), j-(11-safely), j-(2-safely)),
				("\t\\ row %d, cols %d-%d\n",
				i-(1-safely), j-(11-safely), j-(2-safely)));
		    }
		    if ((*ts)->num > 0)
			LANG((diff = (*ts)->newNum-op->baseNum+safely),
			    (diff = (*ts)->newNum+safely));
		    else
			diff = safely ? 0 : -1;
		    SOURCE((" %5d", diff),
			(" %5d", diff));
		}
		SOURCE(("}\t/* row %d */", i-(1-safely)),
		    (" %s,\t\\ row %d", op->stateCount <= 255?"c":"", i-(1-safely)));
	    }
	    SOURCE(("\n};\n\n"),
		("\n: %s-%s-transition@ ( i j -- x )\n  %s-%s-transition %s@ ;\n\n",
		 prefix, op->name, prefix, op->name, op->stateCount <= 255?"c":""));
	}
	break;
    default:
	assert(FALSE);
    }
}

static NonTerminal *ntVector;

static void DEFUN(doNonTermPmaps, (n), NonTerminal n)
{
    short *v;
    PlankMap im;
    int new;
    int i;

    ntVector[n->num] = n;
    if (n->num >= last_user_nonterminal)
	return;
    if (n->ruleCount <= (safely?0:1))
	return;
    im = newPlankMap(n->baseNum);
    v = newVector();
    for (i = 0; i < globalMap->count-1; i++) {
	Rule r = globalMap->set[sortedStates[i]->num]->closed[n->num].rule;
	if (r) {
	    r->used = 1;
	    v[i+safely] = r->newNum - n->baseNum /*+safely*/;
	    assert(v[i+safely] >= 0);
	} else {
	    if (!safely)
		v[i] = -1;
	}
    }
    enterStateMap(im, v, Width(n->ruleCount+safely), &new);
    if (!new)
	zfree(v);
    n->pmap = im;
}

static void DEFUN_VOID(makePmaps)
{
    foreachList((ListFn) doDimPmaps, operators);
    ntVector = (NonTerminal*) zalloc((max_nonterminal) * sizeof(NonTerminal));
    foreachList((ListFn) doNonTermPmaps, nonterminals);
}

static void DEFUN(outPlank, (p), Plank p)
{
    List f;
    int i;

    SOURCE(("static struct {\n"),
	(""));
    for (f = p->fields, i=0; f; f = f->next, i++) {
	StateMap sm = (StateMap) f->x;
	SOURCE(("\tunsigned int %s:%d;\n", sm->fieldname, sm->width),
	    ("%d constant %s-%s\n", i, prefix, sm->fieldname));
    }
    SOURCE(("} %s[] = {\n", p->name),
	("%d %d matrix-noallot %s\n", globalMap->count-(safely?0:1), i, p->name));

    for (i = 0; i < globalMap->count-(safely?0:1); i++) {
	SOURCE(("\t{"),
	    (""));
	for (f = p->fields; f; f = f->next) {
	    StateMap sm = (StateMap) f->x;
	    SOURCE((" %4d ,", sm->value[i] == -1 ? ERROR_VAL : sm->value[i]),
		(" %4d ,", sm->value[i] == -1 ? ERROR_VAL : sm->value[i]));
	}
	SOURCE(("},\t/* row %d */\n", i),
	    ("\t\\ row %d\n", i));
    }
    SOURCE(("};\n\n"),
	(": %s@ ( i j -- x )\n  %s @ ;\n\n", p->name, p->name));
}

static void DEFUN(purgePlanks, (planks), List planks)
{
    List p;

    for (p = planks; p; p = p->next) {
	Plank x = (Plank) p->x;
	outPlank(x);
    }
}

static void DEFUN_VOID(inToEx)
{
    int i;
    int counter;

    SOURCE(("static short %s_eruleMap[] = {\n", prefix),
	("%d array-noallot %s-eruleMap\n", max_ruleAST+2, prefix));
    counter = 0;
    for (i = 0; i < max_ruleAST; i++) {
	if (counter > 0) {
	    SOURCE((","),
		(" ,"));
	    if (counter % 10 == 0)
		SOURCE(("\t/* %d-%d */\n", counter-10, counter-1),
		    ("\t\\ %d-%d\n", counter-10, counter-1));
	}
	if (counter < sortedRules[i]->rule->newNum) {
	    assert(counter == sortedRules[i]->rule->newNum-1);
	    SOURCE((" %5d", 0),
		(" %5d", 0));
	    counter++;
	    if (counter > 0) {
		SOURCE((","),
		    (" ,"));
		if (counter % 10 == 0)
		    SOURCE(("\t/* %d-%d */\n", counter-10, counter-1),
			("\t\\ %d-%d\n", counter-10, counter-1));
	    }
	}
	SOURCE((" %5d", sortedRules[i]->rule->erulenum),
	    (" %5d", sortedRules[i]->rule->erulenum));
	counter++;
    }
    SOURCE(("\n};\n\n"),
	(" ,\n"
	 ": %s-eruleMap@ ( i -- x )\n  %s-eruleMap @ ;\n\n", prefix, prefix));
}

static void DEFUN_VOID(makePlankRuleMacros)
{
    int i;

    for (i = 1; i < last_user_nonterminal; i++) {
	List es;
	PlankMap im = ntVector[i]->pmap;
	SOURCE(("#define %s_%s_rule(state)\t", prefix, ntVector[i]->name),
	    (": %s-%s-rule ( state -- rule )\n", prefix, ntVector[i]->name));
	if (im) {
	    SOURCE(("%s_eruleMap[", prefix),
		("  "));
	    for (es = im->exceptions; es; es = es->next) {
		Exception e = (Exception) es->x;
		SOURCE(("((state) == %d ? %d :", e->index, e->value),
		    ("((state) == %d ? %d :", e->index, e->value)); /* LATER pirky */
	    }
	    SOURCE(("%s[state].%s", im->values->plank->name, im->values->fieldname),
		("%s-%s %s@", prefix, im->values->fieldname, im->values->plank->name));
	    for (es = im->exceptions; es; es = es->next)
		SOURCE((")"),
		    (")")); /* LATER pirky */
	    SOURCE((" +%d]", im->offset),
		(" dup if %d + endif %s-eruleMap@", im->offset, prefix));
	} else {
	    /* nonterminal never appears on LHS. */
	    assert(ntVector[i] ==  start);
	    SOURCE(("0"),
		(""));
	}
	SOURCE(("\n"),
	    (" ;\n"));
    }
}

static void DEFUN_VOID(makePlankRule)
{
    int i;

    makePlankRuleMacros();

    SOURCE(("int DEFUN(%s_rule, (state, goalnt), int state AND int goalnt) {\n", prefix),
	(": %s-rule ( state goalnt -- rule )\n", prefix));
    SOURCE(("\t%s_assert(state >= 0 && state < %d, %s_PANIC(\"Bad state %%d passed to %s_rule\\n\", state));\n",
	prefix, globalMap->count-(safely?0:1), prefix, prefix),
	("  over dup 0 < swap %d >= and %s-assert\" Bad state passed to %s-rule\"\n",
	globalMap->count-(safely?0:1), prefix, prefix));
    SOURCE(("\tswitch (goalnt) {\n"),
	("  case\n"));

    for (i = 1; i < last_user_nonterminal; i++) {
	SOURCE(("\tcase %d:\n", i),
	    ("    %d of", i));
	SOURCE(("\t\treturn %s_%s_rule(state);\n", prefix, ntVector[i]->name),
	    (" %s-%s-rule endof\n", prefix, ntVector[i]->name));
    }
    SOURCE(("\tdefault: %s_PANIC(\"Unknown nonterminal %%d in %s_rule\\n\", state); abort(); return 0;\n",
    	prefix, prefix),
	("    >r true %s-assert\" Unknown nonterminal in %s-rule\" r>", prefix, prefix));
    SOURCE(("\t}\n"),
	(" endcase"));
    SOURCE(("}\n\n"),
	(" ;\n\n"));
}

static void DEFUN(exceptionSwitch, (es, sw, pre, post, offset, def),
	List es AND char *sw AND char *pre AND char *post AND int offset AND char *def)
{
    if (es) {
	source("\t\tswitch (%s) {\n", sw);
	for (; es; es = es->next) {
	    Exception e = (Exception) es->x;
	    source("\t\tcase %d: %s %d; %s\n", e->index, pre, e->value+offset, post);
	}
	if (def)
	    source("\t\tdefault: %s;\n", def);
	source("\t\t}\n");
    } else if (def)
	source("\t\t%s;\n", def);
}

static void DEFUN(doPlankLabel, (op), Operator op)
{
    PlankMap im0;
    PlankMap im1;
    char buf[100];

    SOURCE(("\tcase %d:\n", op->num),
	("    %d of\n", op->num));
    switch (op->arity) {
	case 0:
	    SOURCE(("\t\treturn %d;\n", op->table->transition[0]->newNum),
		("      2drop %d\n", op->table->transition[0]->newNum));
	    break;
	case 1:
	    im0 = op->table->dimen[0]->pmap;
	    if (im0) {
		exceptionSwitch(im0->exceptions, "l", "return ", "", im0->offset, 0);
		source("\t\treturn %s[l].%s+%d;\n", 
		    im0->values->plank->name, im0->values->fieldname, im0->offset);
	    } else {
		Item_Set *ts = transLval(op->table, 1, 0);
		if (*ts)
		    SOURCE(("\t\treturn %d;\n", (*ts)->newNum),
			("\t\treturn %d;\n", (*ts)->newNum));
		else
		    SOURCE(("\t\treturn %d;\n", ERROR_VAL),
			("\t\treturn %d;\n", ERROR_VAL));
	    }
	    break;
	case 2:
	    im0 = op->table->dimen[0]->pmap;
	    im1 = op->table->dimen[1]->pmap;
	    if (!im0 && !im1) {
		Item_Set *ts = transLval(op->table, 1, 1);
		if (*ts)
		    source("\t\treturn %d;\n", (*ts)->newNum);
		else
		    source("\t\treturn %d;\n", ERROR_VAL);
	    } else if (!im0) {
		    exceptionSwitch(im1->exceptions, "r", "return ", "", im1->offset, 0);
		    source("\t\treturn %s[r].%s+%d;\n", 
		    im1->values->plank->name, im1->values->fieldname, im1->offset);
	    } else if (!im1) {
		    exceptionSwitch(im0->exceptions, "l", "return ", "", im0->offset, 0);
		    source("\t\treturn %s[l].%s+%d;\n", 
		    im0->values->plank->name, im0->values->fieldname, im0->offset);
	    } else {
		    assert(im0->offset == 0);
		    assert(im1->offset == 0);
		    sprintf(buf, "l = %s[l].%s",
		    im0->values->plank->name, im0->values->fieldname);
		    exceptionSwitch(im0->exceptions, "l", "l =", "break;", 0, buf);
		    sprintf(buf, "r = %s[r].%s",
		    im1->values->plank->name, im1->values->fieldname);
		    exceptionSwitch(im1->exceptions, "r", "r =", "break;", 0, buf);

		    source("\t\treturn %s_%s_transition[l][r]+%d;\n",
			prefix, op->name, op->baseNum);
	    }
	    break;
	default:
	    assert(FALSE);
    }
}

static void DEFUN(doPlankLabelMacrosSafely, (op), Operator op)
{
    PlankMap im0;
    PlankMap im1;

    assert(safely);
    switch (op->arity) {
	case -1:
	    SOURCE(("#define %s_%s_state\t0\n", prefix, op->name),
		(": %s-%s-state ( left right -- state )\n  2drop 0 ;\n", prefix, op->name));
	    break;
	case 0:
	    SOURCE(("#define %s_%s_state\t%d\n", prefix, op->name, op->table->transition[0]->newNum+safely),
		(": %s-%s-state ( left right -- state )\n  2drop %d ;\n", prefix, op->name, op->table->transition[0]->newNum+safely));
	    break;
	case 1:
	    im0 = op->table->dimen[0]->pmap;
	    if (im0) {
		SOURCE(("#define %s_%s_state(l)", prefix, op->name),
		    (": %s-%s-state ( left right -- state )\n  drop", prefix, op->name));
		if (im0->exceptions) {
		    List es = im0->exceptions;
		    assert(FALSE);
		    SOURCE(("\t\tswitch (l) {\n"),
			("  case\n"));
		    for (; es; es = es->next) {
			Exception e = (Exception) es->x;
			SOURCE(("\t\tcase %d: return %d;\n",
			    e->index, e->value ? e->value+im0->offset : 0),
			    ("    %d of drop %d endof\n",
			    e->index, e->value ? e->value+im0->offset : 0));
		    }
		    SOURCE(("\t\t}\n"),
			("    endcase\n"));
		}
		SOURCE(("\t( (%s_TEMP = %s[l].%s) ? %s_TEMP+%d : 0 )\n",
		    prefix, im0->values->plank->name, im0->values->fieldname, prefix, im0->offset),
		    (" %s-%s %s@ dup if %d + endif ;\n",
			prefix, im0->values->fieldname, im0->values->plank->name, im0->offset));
	    } else {
		Item_Set *ts = transLval(op->table, 1, 0);
		if (*ts)
		    SOURCE(("#define %s_%s_state(l)\t%d\n", (*ts)->newNum+safely, prefix, op->name),
			(": %s-%s-state ( left right -- state )\n  2drop %d ; \n", prefix, op->name, (*ts)->newNum+safely));
		else
		    SOURCE(("#define %s_%s_state(l)\t0\n", prefix, op->name),
			(": %s-%s-state ( left right -- state )\n  2drop 0 ;\n", prefix, op->name));
	    }
	    break;
	case 2:
	    im0 = op->table->dimen[0]->pmap;
	    im1 = op->table->dimen[1]->pmap;
	    if (!im0 && !im1) {
		Item_Set *ts = transLval(op->table, 1, 1);
		assert(FALSE);
		if (*ts)
		    SOURCE(("#define %s_%s_state(l, r)\treturn %d;\n",
			prefix, op->name, (*ts)->newNum+safely),
			("%d %s-%s-state ( left right -- state )\n",
			(*ts)->newNum+safely, prefix, op->name));
		else
		    SOURCE(("#define %s_%s_state(l, r)\treturn %d;\n",
			prefix, op->name, 0),
			("%d %s-%s-state ( left right -- state )\n",
			0, prefix, op->name));
	    } else if (!im0) {
		assert(FALSE);
		if (im1->exceptions) {
		    List es = im1->exceptions;
		    source("\t\tswitch (r) {\n");
		    for (; es; es = es->next) {
			Exception e = (Exception) es->x;
			source("\t\tcase %d: return %d;\n",
			e->index, e->value ? e->value+im1->offset : 0);
		    }
		    source("\t\t}\n");
		}
		source("\t\tstate = %s[r].%s; offset = %d;\n", 
		im1->values->plank->name, im1->values->fieldname, im1->offset);
		source("\t\tbreak;\n");
	    } else if (!im1) {
		assert(FALSE);
		if (im0->exceptions) {
		    List es = im0->exceptions;
		    source("\t\tswitch (l) {\n");
		    for (; es; es = es->next) {
			Exception e = (Exception) es->x;
			source("\t\tcase %d: return %d;\n", e->index, e->value ? e->value+im0->offset : 0);
		    }
		    source("\t\t}\n");
		}
		source("\t\tstate = %s[l].%s; offset = %d;\n", 
		im0->values->plank->name, im0->values->fieldname, im0->offset);
		source("\t\tbreak;\n");
	    } else {
		assert(im0->offset == 0);
		assert(im1->offset == 0);
		/*
		sprintf(buf, "l = %s[l].%s",
		im0->values->plank->name, im0->values->fieldname);
		exceptionSwitch(im0->exceptions, "l", "l =", "break;", 0, buf);
		sprintf(buf, "r = %s[r].%s",
		im1->values->plank->name, im1->values->fieldname);
		exceptionSwitch(im1->exceptions, "r", "r =", "break;", 0, buf);

		source("\t\tstate = %s_%s_transition[l][r]; offset = %d;\n", 
		prefix, op->name, op->baseNum);
		source("\t\tbreak;\n");
		*/

		SOURCE(("#define %s_%s_state(l, r)", prefix, op->name),
		    (": %s-%s-state ( left right -- state )\n", prefix, op->name));

		SOURCE(("\t( (%s_TEMP = %s_%s_transition[%s[l].%s][%s[r].%s]) ? ",
		    prefix, prefix, op->name,
		    im0->values->plank->name, im0->values->fieldname,
		    im1->values->plank->name, im1->values->fieldname),
		    ("  swap %s-%s %s@ swap %s-%s %s@ %s-%s-transition@",
		    prefix, im0->values->fieldname, im0->values->plank->name,
		    prefix, im1->values->fieldname, im1->values->plank->name,
		    prefix, op->name));
		SOURCE(("%s_TEMP+%d : 0 )\n", prefix, op->baseNum),
		    (" ;\n"));
	    }
	    break;
	default:
	    assert(FALSE);
    }
}

static void DEFUN(doPlankLabelSafely, (op), Operator op)
{
    assert(safely);
    SOURCE(("\tcase %d:\n", op->num),
	(""));
    switch (op->arity) {
	case -1:
	    SOURCE(("\t\treturn 0;\n"),
		("  0 %d [%s-state] !\n", op->num, prefix));
	    break;
	case 0:
	    SOURCE(("\t\treturn %s_%s_state;\n", prefix, op->name),
		("  ' %s-%s-state %d [%s-state] !\n", prefix, op->name, op->num, prefix));
	    break;
	case 1:
	    SOURCE(("\t\treturn %s_%s_state(l);\n", prefix, op->name),
		("  ' %s-%s-state %d [%s-state] !\n", prefix, op->name, op->num, prefix));
	    break;
	case 2:
	    SOURCE(("\t\treturn %s_%s_state(l, r);\n", prefix, op->name),
		("  ' %s-%s-state %d [%s-state] !\n", prefix, op->name, op->num, prefix));
	    break;
	default:
	    assert(FALSE);
    }
}

static void DEFUN_VOID(makePlankState)
{
    SOURCE(("\n"),
	(""));
    foreachList((ListFn) doPlankLabelMacrosSafely, operators);
    SOURCE(("\n"),
	(""));

    SOURCE((""),
	("%d array [%s-state]\n", maxOperator+1, prefix));
    switch (max_arity) {
    case -1:
	WARNING("ERROR: no terminals in grammar.\n");
	exit(1);
    case 0:
	SOURCE(("int DEFUN(%s_state, (op), int op) {\n", prefix),
	    (""));
	break;
    case 1:
	SOURCE(("int DEFUN(%s_state, (op, l), int op AND int l) {\n", prefix),
	    (""));
	break;
    case 2:
	SOURCE(("int DEFUN(%s_state, (op, l, r), int op AND int l AND int r) {\n", prefix),
	    (""));
	break;
    default:
	assert(FALSE);
    }

    SOURCE(("\tregister int %s_TEMP;\n", prefix),
	(""));

    SOURCE(("\tswitch (op) {\n"),
	(""));	/* LATER pirky */
    /* opsOfArity(2); */
    if (max_arity >= 2) {
	SOURCE(("\t\t%s_assert(r >= 0 && r < %d, %s_PANIC(\"Bad state %%d passed to %s_state\\n\", r));\n",
	    prefix, globalMap->count, prefix, prefix),
	    (""));
	SOURCE(("\t\t/*FALLTHROUGH*/\n"),
	    (""));
    }
    /* opsOfArity(1); */
    if (max_arity > 1) {
	SOURCE(("\t\t%s_assert(l >= 0 && l < %d, %s_PANIC(\"Bad state %%d passed to %s_state\\n\", l));\n",
	    prefix, globalMap->count, prefix, prefix),
	    (""));
	SOURCE(("\t\t/*FALLTHROUGH*/\n"),
	    (""));
    }
    /* opsOfArity(0); */
    SOURCE(("\t\tbreak;\n"),
	(""));
    SOURCE(("\t}\n"),
	(""));

    if (safely) {
	SOURCE(("\tswitch (op) {\n"),
	    (""));
	foreachList((ListFn) doPlankLabelSafely, operators);
	SOURCE(("\tdefault: %s_PANIC(\"Unknown op %%d in %s_state\\n\", op); abort(); return 0;\n",
	    prefix, prefix),
	    (""));
	SOURCE(("\t}\n"),
	    (""));
    } else {
	SOURCE(("\tswitch (op) {\n"),
	    (""));
	foreachList((ListFn) doPlankLabel, operators);
	SOURCE(("\tdefault: %s_PANIC(\"Unknown op %%d in %s_state\\n\", op); abort(); return 0;\n",
	    prefix, prefix),
	    (""));
	SOURCE(("\t}\n"),
	    (""));
    }
    switch (max_arity) {
    case -1:
	WARNING("ERROR: no terminals in grammar.\n");
	exit(1);
    case 0:
	SOURCE((""),
	    (": %s-state ( left right op -- state )\n", prefix));
	break;
    case 1:
	SOURCE((""),
	    (": %s-state ( left right op -- state )\n", prefix));
	break;
    case 2:
	SOURCE((""),
	    (": %s-state ( left right op -- state )\n", prefix));
	break;
    default:
	assert(FALSE);
    }
    SOURCE(("}\n\n"),
	("  [%s-state] @ execute ;\n\n", prefix));
}

void DEFUN_VOID(makePlanks)
{
    List planks;

    renumber();
    makePmaps();
    planks = assemblePlanks();
    purgePlanks(planks);
    inToEx();
    makePlankRule();
    makePlankState();
}

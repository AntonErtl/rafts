#ident "@(#)$Id: be.c,v 1.1 1996/08/14 18:42:24 anton Exp $";

#include "b.h"
#include "fe.h"

#define ERROR_VAL 0

FILE *infile;
FILE *outfile;
char *prefix = "burm";

static void EXFUN(doKids, (RuleAST));
static void EXFUN(doLabel, (Operator));
static void EXFUN(doLayout, (RuleAST));
static void EXFUN(doMakeTable, (Operator));
static void EXFUN(doVector, (RuleAST));
static void EXFUN(layoutNts, (PatternAST));
static void EXFUN(makeIndex_Map, (Dimension));
static void EXFUN(makePvector, (NOARGS));
static void EXFUN(makeState, (NOARGS));
static void EXFUN(printPatternAST, (PatternAST));
static void EXFUN(setVectors, (PatternAST));
static int EXFUN(seminal, (int from, int to));

#include <stdarg.h>

void DEFUN(source, (s), CONST char *s DOTS)
{
    va_list p;

    va_start(p, s);
    vfprintf(outfile, s, p);
    va_end(p);
}

void DEFUN(WARNING, (s), CONST char *s DOTS)
{
    va_list p;

    fprintf(stderr, "warning: ");
    va_start(p, s);
    vfprintf(stderr, s, p);
    va_end(p);
}

void DEFUN(ERROR, (s), CONST char *s DOTS)
{
    va_list p;

    fprintf(stderr, "ERROR: ");
    va_start(p, s);
    vfprintf(stderr, s, p);
    va_end(p);
}

void DEFUN(VERBOSE, (s), CONST char *s DOTS)
{
    va_list p;

    va_start(p, s);
    vfprintf(stderr, s, p);
    va_end(p);
}

static void DEFUN(doLabel, (op), Operator op)
{
    source("\tcase %d:\n", op->num);
    switch (op->arity) {
	default:
	    assert(FALSE);
	    break;
	case 0:
	    source("\t\treturn %d;\n", op->table->transition[0]->num);
	    break;
	case 1:
	    if (op->table->rules) {
	        source("\t\treturn %s_%s_transition[l];\n", prefix, op->name);
	    } else {
	        source("\t\treturn %d;\n", ERROR_VAL);
	    }
	    break;
	case 2:
	    if (op->table->rules) {
	        source("\t\treturn %s_%s_transition[%s_%s_imap_1[l]][%s_%s_imap_2[r]];\n", prefix, op->name, prefix, op->name, prefix, op->name);
	    } else {
	        source("\t\treturn %d;\n", ERROR_VAL);
	    }
	    break;
    }
}

int DEFUN(opsOfArity, (arity), int arity)
{
    int c;
    List l;

    c = 0;
    for (l = operators; l; l = l->next) {
	Operator op = (Operator) l->x;
	if (op->arity == arity) {
	    if (c == 0)
		SOURCE((""),
		    ("    false"));
	    SOURCE(("\tcase %d:\n", op->num),
		("\n    over %d = or", op->num));
	    c++;
	}
    }
    return c;
}

void DEFUN_VOID(makeLabelinternals)
{
    SOURCE(("int DEFUN(%s_label, (n), %s_NODEPTR_TYPE n) {\n", prefix, prefix),
	(": %s_label ( node-addr -- state )\n", prefix));
    SOURCE(("\t%s_assert(n, %s_PANIC(\"NULL pointer passed to %s_label\\n\"));\n",
	prefix, prefix, prefix),
	("  dup 0= %s_assert\" NULL pointer passed to %s_label\"\n",
	prefix, prefix, prefix));
    SOURCE(("\tif (%s_STATE_LABEL(n)) {\n\t\treturn %s_STATE_LABEL(n);\n\t}\n", prefix, prefix),
	(""));
    SOURCE(("\tswitch (%s_arity[%s_OP_LABEL(n)]) {\n", prefix, prefix),
	("  dup %s_OP_LABEL@ %s_arity@ case\n", prefix, prefix));
    SOURCE(("\tcase 0:\n"),
	("    0 of\n"));
    SOURCE(("\t\treturn %s_STATE_LABEL(n) = %s_state(%s_OP_LABEL(n), 0, 0);\n",
	prefix, prefix, prefix),
	("      >r NIL NIL\n"
	 "      r@ %s_OP_LABEL@ %s_state\n"
	 "      dup r> %s_STATE_LABEL! endof\n", prefix, prefix, prefix));
    SOURCE(("\tcase 1:\n"),
	("    1 of\n"));
    SOURCE(("\t\treturn %s_STATE_LABEL(n) = %s_state(%s_OP_LABEL(n), %s_label(%s_LEFT_CHILD(n)), 0);\n",
	prefix, prefix, prefix, prefix, prefix),
	("      >r r@ %s_LEFT_CHILD@ recurse\n"
	 "      NIL\n"
	 "      r@ %s_OP_LABEL@ %s_state\n"
	 "      dup r> %s_STATE_LABEL! endof\n" , prefix, prefix, prefix, prefix));
    SOURCE(("\tcase 2:\n"),
	("    2 of\n"));
    SOURCE(("\t\treturn %s_STATE_LABEL(n) = %s_state(%s_OP_LABEL(n), %s_label(%s_LEFT_CHILD(n)), %s_label(%s_RIGHT_CHILD(n)));\n",
	prefix, prefix, prefix, prefix, prefix, prefix, prefix),
	("      >r r@ %s_LEFT_CHILD@ recurse\n"
	 "      r@ %s_RIGHT_CHILD@ recurse\n"
	 "      r@ %s_OP_LABEL@ %s_state\n"
	 "      dup r> %s_STATE_LABEL! endof\n" , prefix, prefix, prefix, prefix, prefix));
    SOURCE(("\tdefault: %s_PANIC(\"Bad op %%d in %s_label\\n\", %s_OP_LABEL(n)); abort(); return 0;\n", 
	prefix, prefix, prefix),
	("    >r true %s_assert\" Bad op in %s_label\" r>", prefix, prefix));
    SOURCE(("\t}\n"),
	(" endcase"));
    SOURCE(("}\n\n"),
	(" ;\n\n"));
}

void DEFUN_VOID(makeLabel)
{
    int flag = 0;

    SOURCE(("int DEFUN(%s_label, (n), %s_NODEPTR_TYPE n) {\n", prefix, prefix),
	(": %s_label ( node-addr -- state )\n", prefix));
    SOURCE(("\t%s_assert(n, %s_PANIC(\"NULL pointer passed to %s_label\\n\"));\n",
	prefix, prefix, prefix),
	("  dup 0= %s_assert\" NULL pointer passed to %s_label\"\n",
	prefix, prefix, prefix));
    SOURCE(("\tif (%s_STATE_LABEL(n)) {\n\t\treturn %s_STATE_LABEL(n);\n\t}\n", prefix, prefix),
	("  dup %s_STATE_LABEL@ dup if\n    nip else\n", prefix));
    SOURCE(("\tswitch (%s_OP_LABEL(n)) {\n", prefix),
	("    drop dup %s_OP_LABEL@\n", prefix));
    if (opsOfArity(0) > 0) {
	flag++;
	SOURCE(("\t\treturn %s_STATE_LABEL(n) = %s_state(%s_OP_LABEL(n), 0, 0);\n",
	    prefix, prefix, prefix),
	    (" if\n"
	     "      drop >r NIL NIL\n"
	     "      r@ %s_OP_LABEL@ %s_state\n"
	     "      dup r> %s_STATE_LABEL! else\n", prefix, prefix, prefix));
    }
    if (opsOfArity(1) > 0) {
	flag++;
	SOURCE(("\t\treturn %s_STATE_LABEL(n) = %s_state(%s_OP_LABEL(n), %s_label(%s_LEFT_CHILD(n)), 0);\n",
	    prefix, prefix, prefix, prefix, prefix),
	    (" if\n"
	     "      drop >r r@ %s_LEFT_CHILD@ recurse\n"
	     "      NIL\n"
	     "      r@ %s_OP_LABEL@ %s_state\n"
	     "      dup r> %s_STATE_LABEL! else\n" , prefix, prefix, prefix, prefix));
    }
    if (opsOfArity(2) > 0) {
	flag++;
	SOURCE(("\t\treturn %s_STATE_LABEL(n) = %s_state(%s_OP_LABEL(n), %s_label(%s_LEFT_CHILD(n)), %s_label(%s_RIGHT_CHILD(n)));\n",
	    prefix, prefix, prefix, prefix, prefix, prefix, prefix),
	    (" if\n"
	     "      drop >r r@ %s_LEFT_CHILD@ recurse\n"
	     "      r@ %s_RIGHT_CHILD@ recurse\n"
	     "      r@ %s_OP_LABEL@ %s_state\n"
	     "      dup r> %s_STATE_LABEL! else\n" , prefix, prefix, prefix, prefix, prefix));
    }
    SOURCE(("\tdefault: %s_PANIC(\"Bad op %%d in %s_label\\n\", %s_OP_LABEL(n)); abort(); return 0;\n", 
	prefix, prefix, prefix),
	("      drop true %s_assert\" Bad op in %s_label\"", prefix, prefix));
    SOURCE(("\t}\n"),
	(""));
    while(flag-- > 0)
	SOURCE((""),
	    (" endif"));
    SOURCE(("}\n\n"),
	(" endif ;\n\n"));
}

static void DEFUN_VOID(makeState)
{
    SOURCE(("int DEFUN(%s_state, (op, l, r), int op AND int l AND int r) {\n", prefix),
	(": %s_state ( left right op -- state )\n", prefix));
    SOURCE(("\t%s_assert(l >= 0 && l < %d, PANIC(\"Bad state %%d passed to %s_state\\n\", l));\n",
	prefix, globalMap->count, prefix),
	(""));
    SOURCE(("\t%s_assert(r >= 0 && r < %d, PANIC(\"Bad state %%d passed to %s_state\\n\", r));\n",
	prefix, globalMap->count, prefix),
	(""));
    SOURCE(("\tswitch (op) {\n"),
	("  case\n"));
    SOURCE(("\tdefault: %s_PANIC(\"Bad op %%d in %s_state\\n\", index); abort(); return 0;\n", 
	prefix, prefix, prefix),
	("    >r true %s_assert\" Bad op in %s_state\" r>", prefix, prefix));

    foreachList((ListFn) doLabel, operators);

    SOURCE(("\t}\n"),
	(" endcase"));
    SOURCE(("}\n\n"),
	("\n\n"));
}

static char cumBuf[4000];
static int vecIndex;
static char vecBuf[4000];

static void DEFUN(setVectors, (ast), PatternAST ast)
{
    char old[4000];

    switch (ast->sym->tag) {
	default:
	    assert(FALSE);
	    break;
	case NONTERMINAL:
	    LANG(sprintf(old, "\t\tkids[%d] = %s;\n", vecIndex, vecBuf),
		sprintf(old, "%s\n", vecBuf));
	    strcat(cumBuf, old);
	    vecIndex++;
	    return;
	case OPERATOR:
	    switch (ast->sym->u.op->arity) {
		default:
		    assert(FALSE);
		    break;
		case 0:
		    return;
		case 1:
		    strcpy(old, vecBuf);
		    LANG(sprintf(vecBuf, "%s_LEFT_CHILD(%s)", prefix, old),
			sprintf(vecBuf, "%s %s_LEFT_CHILD@", old, prefix));
		    setVectors((PatternAST) ast->children->x);
		    strcpy(vecBuf, old);
		    return;
		case 2:
		    strcpy(old, vecBuf);
		    LANG(sprintf(vecBuf, "%s_LEFT_CHILD(%s)", prefix, old),
			sprintf(vecBuf, "%s %s_RIGHT_CHILD@", old, prefix));
		    LANG(setVectors((PatternAST) ast->children->x),
			setVectors((PatternAST) ast->children->next->x));

		    LANG(sprintf(vecBuf, "%s_RIGHT_CHILD(%s)", prefix, old),
			sprintf(vecBuf, "%s %s_LEFT_CHILD@", old, prefix));
		    LANG(setVectors((PatternAST) ast->children->next->x),
			setVectors((PatternAST) ast->children->x));
		    strcpy(vecBuf, old);
		    return;
	    }
	    break;
    }
}

#define MAX_VECTOR	10

void DEFUN_VOID(makeRuleTable)
{
    int s, nt;

    source("static short %s_RuleNo[%d][%d] = {\n",
	prefix, globalMap->count, last_user_nonterminal-1);
    for (s = 0; s < globalMap->count; s++) {
	Item_Set ts = globalMap->set[s];
	if (s > 0)
	    source(",\n");
	source("/* state %d */\n", s);
	source("{");
	for (nt = 1; nt < last_user_nonterminal; nt++) {
	    if (nt > 1) {
		source(",");
		if (nt % 10 == 1)
		    source("\t/* state %d; Nonterminals %d-%d */\n", s, nt-10, nt-1);
	    }
	    if (ts->closed[nt].rule) {
		ts->closed[nt].rule->used = 1;
		source("%5d", ts->closed[nt].rule->erulenum);
	    } else
		source("%5d", ERROR_VAL);
	}
	source("}");
    }
    source("};\n");
}

static void DEFUN(makeIndex_Map, (d), Dimension d)
{
    int s;

    for (s = 0; s < globalMap->count; s++) {
	if (s > 0) {
	    source(",");
	    if (s % 10 == 0) {
		source("\t/* %d-%d */\n", s-10, s-1);
	    }
	}
	source("%5d", d->map->set[d->index_map.class[s]->num]->num);
    }
    source("};\n");
}

static void DEFUN(doMakeTable, (op), Operator op)
{
    int s;
    int i, j;
    Dimension d;

    switch (op->arity) {
	default:
	    assert(FALSE);
	    break;
	case 0:
	    return;
	case 1:
	    if (!op->table->rules)
		return;
	    d = op->table->dimen[0];
	    source("static short %s_%s_transition[%d] = {\n", prefix, op->name, globalMap->count);
	    for (s = 0; s < globalMap->count; s++) {
		if (s > 0) {
		    source(", ");
		    if (s % 10 == 0)
			source("\t/* %d-%d */\n", s-10, s-1);
		}
		source("%5d", op->table->transition[d->map->set[d->index_map.class[s]->num]->num]->num);
	    }
	    source("};\n");
	    break;
	case 2:
	    if (!op->table->rules)
		return;
	    source("static short %s_%s_imap_1[%d] = {\n", prefix, op->name, globalMap->count);
	    makeIndex_Map(op->table->dimen[0]);
	    source("static short %s_%s_imap_2[%d] = {\n", prefix, op->name, globalMap->count);
	    makeIndex_Map(op->table->dimen[1]);

	    source("static short %s_%s_transition[%d][%d] = {", prefix, op->name,
	    op->table->dimen[0]->map->count,
	    op->table->dimen[1]->map->count);
	    for (i = 0; i < op->table->dimen[0]->map->count; i++) {
		if (i > 0)
		    source(",");
		source("\n");
		source("{");
		for (j = 0; j < op->table->dimen[1]->map->count; j++) {
		    Item_Set *ts = transLval(op->table, i, j);
		    if (j > 0)
			source(",");
		    source("%5d", (*ts)->num);
		}
		source("}\t/* row %d */", i);
	    }
	    source("\n};\n");
	    break;
    }
}

void DEFUN_VOID(makeTables)
{
    foreachList((ListFn) doMakeTable, operators);
}

static RuleAST *pVector;

void DEFUN_VOID(makeLHSmap)
{
    int i;

    if (!pVector)
	makePvector();
    source("short %s_lhs[] = {\n", prefix);
    for (i = 0; i <= max_erule_num; i++) {
	if (pVector[i]) {
	    source("\t%s_%s_NT,\n", prefix, pVector[i]->lhs);
	} else {
	    source("\t0,\n");
	}
    }
    source("};\n\n");
}

static int DEFUN(seminal, (from, to), int from AND int to)
{
    return allpairs[from][to].rule ? allpairs[from][to].rule->erulenum : 0;

    /*
    int tmp, last;
    tmp = 0;
    for (;;) {
	last = tmp;
	tmp = allpairs[to][from].rule ? allpairs[to][from].rule->erulenum : 0;
	if (!tmp) {
	    break;
	}
	assert(pVector[tmp]);
	to = pVector[tmp]->rule->pat->children[0]->num;
    }
    return last;
    */
}

void DEFUN_VOID(makeClosureArray)
{
    int i, j;

    if (!pVector)
	makePvector();
    SOURCE(("short %s_closure[%d][%d] = {\n", prefix, last_user_nonterminal, last_user_nonterminal),
	(""));
    for (i = 0; i < last_user_nonterminal; i++) {
	SOURCE(("\t{"),
	    (""));
	for (j = 0; j < last_user_nonterminal; j++) {
	    if (j > 0 && j%10 == 0)
		SOURCE(("\n\t "),
		    (""));
	    SOURCE((" %4d,", seminal(i, j)),
		(""));
	}
	SOURCE(("},\n"),
	    (""));
    }
    SOURCE(("};\n"),
	(""));
}

void DEFUN_VOID(makeCostArray)
{
    int i;

    if (!pVector)
	makePvector();
    SOURCE(("short %s_cost[][%d] = {\n", prefix, DELTAWIDTH),
	("%d %d 2 marray_noallot %s_cost\n", max_erule_num+1, DELTAWIDTH, prefix, prefix));
    for (i = 0; i <= max_erule_num; i++) {
	if (i > 0)
	    SOURCE((",\t/* %d */\n", i - 1),
		(" ,\t\\ %d\n", i - 1));
	SOURCE(("\t{"),
	    ("  "));
#ifdef NOLEX
	if (pVector[i])
	    SOURCE((" %5d", pVector[i]->rule->delta),
		(" %5d", pVector[i]->rule->delta));
	else
	    SOURCE((" %5d", 0),
		(" %5d", 0));
#else
	{
	int j;
	for (j = 0; j < DELTAWIDTH; j++) {
	    if (j > 0)
		SOURCE((","),
		    (" ,"));
	    if (pVector[i])
		SOURCE((" %5d", pVector[i]->rule->delta[j]),
		    (" %5d", pVector[i]->rule->delta[j]));
	    else
		SOURCE((" %5d", 0),
		    (" %5d", 0));
	}
	}
#endif /* NOLEX */
	SOURCE(("}"),
	    (""));
    }
    SOURCE((" \t/* %d */\n};\n\n", max_erule_num),
	(" ,\t\\ %d\n"
	 ": %s_cost@ ( i j -- x )\n  %s_cost @ ;\n\n",
	 max_erule_num, prefix, prefix, prefix, prefix, prefix));
}

static void DEFUN(printPatternAST, (p), PatternAST p)
{
    List l;

    if (p) {
	source("%s", p->op);
	if (p->children) {
	    source("(");
	    for (l = p->children; l; l = l->next) {
		PatternAST pat = (PatternAST) l->x;
		if (l != p->children)
		    source(", ");
		printPatternAST(pat);
	    }
	    source(")");
	}
    }
}

static void DEFUN(layoutNts, (ast), PatternAST ast)
{
    char out[30];

    switch (ast->sym->tag) {
	default:
	    assert(FALSE);
	    break;
	case NONTERMINAL:
	    LANG(sprintf(out, "%d, ", ast->sym->u.nt->num),
		sprintf(out, " %d ,", ast->sym->u.nt->num));
	    strcat(cumBuf, out);
	    return;
	case OPERATOR:
	    switch (ast->sym->u.op->arity) {
		default:
		    assert(FALSE);
		    break;
		case 0:
		    return;
		case 1:
		    layoutNts((PatternAST) ast->children->x);
		    return;
		case 2:
		    layoutNts((PatternAST) ast->children->x);
		    layoutNts((PatternAST) ast->children->next->x);
		    return;
	    }
	    break;
    }
}

static void DEFUN(doVector, (ast), RuleAST ast)
{
    if (pVector[ast->rule->erulenum]) {
	ERROR("non-unique external rule number: (%d)\n", ast->rule->erulenum);
	exit(1);
    }
    pVector[ast->rule->erulenum] = ast;
}

static void DEFUN_VOID(makePvector)
{
    pVector = (RuleAST*) zalloc((max_erule_num+1) * sizeof(RuleAST));
    foreachList((ListFn) doVector, ruleASTs);
}

static void DEFUN(doLayout, (ast), RuleAST ast)
{
    LANG(sprintf(cumBuf, "{ "),
	sprintf(cumBuf, " "));
    layoutNts(ast->pat);
    LANG(strcat(cumBuf, "0 }"),
	strcat(cumBuf, " 0 ,"));
}

void DEFUN_VOID(makeNts)
{
    int i;
    int new;
    StrTable nts;

    nts = newStrTable();
    if (!pVector)
	makePvector();
    for (i = 0; i <= max_erule_num; i++) {
	if (pVector[i]) {
	    doLayout(pVector[i]);
	    pVector[i]->nts = addString(nts, cumBuf, i, &new);
	    if (new) {
		char ename[50];

		sprintf(ename, "%s_r%d_nts", prefix, i);
		pVector[i]->nts->ename = (char*) zalloc(strlen(ename)+1);
		strcpy(pVector[i]->nts->ename, ename);
		SOURCE(("static short %s[] = %s;\n", ename, cumBuf),
		    ("create %s\n%s\n", ename, cumBuf));
	    }
	}
    }

    SOURCE(("short *%s_nts[] = {\n", prefix),
	("%d array_noallot %s_nts\n", max_erule_num+1, prefix));
    for (i = 0; i <= max_erule_num; i++) {
	if (pVector[i])
	    SOURCE(("\t%s,\n", pVector[i]->nts->ename),
		("  %s ,\n", pVector[i]->nts->ename));
	else
	    SOURCE(("\t0,\n"),
		("  0 ,\n"));
    }
    SOURCE(("};\n\n"),
	(": %s_nts@ ( i -- x )\n  %s_nts @ ;\n\n",
	 prefix, prefix));
}

void DEFUN_VOID(makeStringArray)
{
    int i;

    if (!pVector)
	makePvector();
    SOURCE(("char *%s_string[] = {\n", prefix),
	(""));
    for (i = 0; i <= max_erule_num; i++) {
	if (pVector[i]) {
	    SOURCE(("\t\"%s: ", pVector[i]->rule->lhs->name),
		(": %s_string%d .\" %s: ", prefix, i, pVector[i]->rule->lhs->name));
	    printPatternAST(pVector[i]->pat);
	    SOURCE(("\",\n"),
		("\" ;\n"));
	} else
	    SOURCE(("\t0,\n"),
		(""));
    }
    SOURCE((""),
	("%d array_noallot [%s_string]\n", max_erule_num+1, prefix));
    for (i = 0; i <= max_erule_num; i++) {
	if (pVector[i]) {
	    SOURCE((""),
		("  ' %s_string%d ,\n", prefix, i));
	}
    }
    SOURCE(("};\n\n"),
	(": %s_string ( rule -- )\n  [%s_string] @ execute ;\n\n", prefix, prefix));

    SOURCE(("int %s_max_rule = %d;\n", prefix, max_erule_num),
	("%d constant %s_max_rule\n\n", max_erule_num, prefix));
}

void
DEFUN_VOID(makeRule)
{
    SOURCE(("int DEFUN(%s_rule, (state, goalnt), int state AND int goalnt) {\n", prefix),
	(": %s_rule ( state goalnt -- rule )\n", prefix));
    SOURCE(("\t%s_assert(state >= 0 && state < %d, %s_PANIC(\"Bad state %%d passed to %s_rule\\n\", state));\n",
	prefix, globalMap->count, prefix, prefix),
	("  over dup 0 < swap %d >= and %s_assert\" Bad state passed to %s_rule\"\n",
	globalMap->count, prefix, prefix));
    source("\t%s_assert(goalnt >= 1 && goalnt < %d, PANIC(\"Bad goalnt %%d passed to %s_rule\\n\", state));\n",
	prefix, max_nonterminal, prefix);
    source("\treturn %s_RuleNo[state][goalnt-1];\n", prefix);
    SOURCE(("}\n\n"),
	(" ;\n\n"));
}

static StrTable kids;

static void DEFUN(doKids, (ast), RuleAST ast)
{
    int new;

    vecIndex = 0;
    cumBuf[0] = '\0';
    LANG(strcpy(vecBuf, "p"),
	strcpy(vecBuf, "  r@"));
    setVectors(ast->pat);

    ast->kids = addString(kids, cumBuf, ast->rule->erulenum, &new);

}

void DEFUN_VOID(makeKids)
{
    List e;
    IntList r;
    int n;

    kids = newStrTable();
    SOURCE(("%s_NODEPTR_TYPE * DEFUN(%s_kids, (p, rulenumber, kids), %s_NODEPTR_TYPE p AND int rulenumber AND %s_NODEPTR_TYPE *kids) {\n",
	prefix, prefix, prefix, prefix),
	(""));
    SOURCE(("\t%s_assert(p, %s_PANIC(\"NULL pointer passed to %s_kids\\n\"));\n",
	prefix, prefix, prefix),
	(""));
    SOURCE(("\t%s_assert(kids, %s_PANIC(\"NULL kids pointer passed to %s_kids\\n\"));\n",
	prefix, prefix, prefix),
	(""));
    SOURCE(("\tswitch (rulenumber) {\n"),
	(""));

    foreachList((ListFn) doKids, ruleASTs);

    for (e = kids->elems, n=1; e; e = e->next, n++) {
	StrTableElement el = (StrTableElement) e->x;
	SOURCE((""),
	    (": %s_kids%d\n  >r\n", prefix, n));
	SOURCE(("%s", el->str),
	    ("%s", el->str));
	SOURCE((""),
	    ("  rdrop ;\n"));
    }

    SOURCE((""),
	("%d array [%s_kids]\n", max_erule_num+1, prefix));
    for (e = kids->elems, n=1; e; e = e->next, n++) {
	StrTableElement el = (StrTableElement) e->x;
	for (r = el->erulenos; r; r = r->next) {
	    int i = r->x;
	    SOURCE(("\tcase %d:\n", i),
		("  ' %s_kids%d %d [%s_kids] !\n", prefix, n, i, prefix));
	}
	SOURCE(("%s", el->str),
	    (""));
	SOURCE(("\t\tbreak;\n"),
	    (""));
    }

    SOURCE(("\tdefault: %s_PANIC(\"Unknown Rule %%d in %s_kids\\n\", rulenumber); abort();\n", 
	prefix, prefix),
	(""));
    SOURCE(("\t\t/* NOTREACHED */\n"),
	(""));
    SOURCE(("\t}\n"),
	(""));
    SOURCE((""),
	(": %s_kids ( node-addr rule -- node-addr ... node-addr )\n", prefix));
    SOURCE((""),
	("  over 0= %s_assert\" NULL pointer passed to %s_kids\"\n",
	prefix, prefix, prefix));

    SOURCE(("\tswitch (rulenumber) {\n"),
	(""));
    SOURCE(("\treturn kids;\n"),
	(""));
    SOURCE(("}\n\n"),
	("  [%s_kids] @ execute ;\n\n", prefix));
}

void DEFUN_VOID(makeOpLabel)
{
    SOURCE(("int DEFUN(%s_op_label, (p), %s_NODEPTR_TYPE p) {\n", prefix, prefix),
	(": %s_op_label ( node-addr -- op )\n", prefix));
    SOURCE(("\t%s_assert(p, %s_PANIC(\"NULL pointer passed to %s_op_label\\n\"));\n",
	prefix, prefix, prefix),
	("  over 0= %s_assert\" NULL pointer passed to %s_op_label\"\n",
	prefix, prefix, prefix));
    SOURCE(("\treturn %s_OP_LABEL(p);\n", prefix),
	("  %s_OP_LABEL@", prefix));
    SOURCE(("}\n\n"),
	(" ;\n\n"));
}

void DEFUN_VOID(makeStateLabel)
{
    SOURCE(("int DEFUN(%s_state_label, (p), %s_NODEPTR_TYPE p) {\n", prefix, prefix),
	(": %s_state_label ( node-addr -- state )\n", prefix));
    SOURCE(("\t%s_assert(p, %s_PANIC(\"NULL pointer passed to %s_state_label\\n\"));\n",
	prefix, prefix, prefix),
	("  over 0= %s_assert\" NULL pointer passed to %s_state_label\"\n",
	prefix, prefix, prefix));
    SOURCE(("\treturn %s_STATE_LABEL(p);\n", prefix),
	("  %s_STATE_LABEL@", prefix));
    SOURCE(("}\n\n"),
	(" ;\n\n"));
}

void DEFUN_VOID(makeChild)
{
    SOURCE(("%s_NODEPTR_TYPE DEFUN(%s_child, (p, index), %s_NODEPTR_TYPE p AND int index) {\n",
	prefix, prefix, prefix),
	(": %s_child ( node-addr index -- node-addr )\n", prefix));
    SOURCE(("\t%s_assert(p, %s_PANIC(\"NULL pointer passed to %s_child\\n\"));\n",
	prefix, prefix, prefix),
	("  over 0= %s_assert\" NULL pointer passed to %s_child\"\n",
	prefix, prefix, prefix));
    SOURCE(("\tswitch (index) {\n"),
	("  case\n"));
    SOURCE(("\tcase 0:\n"),
	("    0 of"));
    SOURCE(("\t\treturn %s_LEFT_CHILD(p);\n", prefix),
	(" %s_LEFT_CHILD endof\n", prefix));
    SOURCE(("\tcase 1:\n"),
	("    1 of"));
    SOURCE(("\t\treturn %s_RIGHT_CHILD(p);\n", prefix),
	(" %s_RIGHT_CHILD endof\n", prefix));
    SOURCE(("\tdefault: %s_PANIC(\"Bad index %%d in %s_child\\n\", index); abort(); return 0;\n", 
	prefix, prefix, prefix),
	("    >r true %s_assert\" Bad index in %s_child\" r>", prefix, prefix));
    SOURCE(("\t}\n"),
	(" endcase"));
    SOURCE(("}\n\n"),
	(" ;\n\n"));
}

static Operator *opVector;
int maxOperator;

void DEFUN_VOID(makeOperatorVector)
{
    List l;

    maxOperator = 0;
    for (l = operators; l; l = l->next) {
	Operator op = (Operator) l->x;
	if (op->num > maxOperator) {
	    maxOperator = op->num;
	}
    }
    opVector = (Operator*) zalloc((maxOperator+1) * sizeof(*opVector));
    for (l = operators; l; l = l->next) {
	Operator op = (Operator) l->x;
	if (opVector[op->num]) {
	    ERROR("Non-unique external symbol number (%d)\n", op->num);
	    exit(1);
	}
	opVector[op->num] = op;
    }
}

void DEFUN_VOID(makeOperators)
{
    int i;

    if (!opVector)
	makeOperatorVector();
    SOURCE(("char * %s_opname[] = {\n", prefix),
	(""));
    for (i = 0; i <= maxOperator; i++) {
	if (i > 0)
	    SOURCE((",\t/* %d */\n", i-1),
		(""));
	if (opVector[i])
	    SOURCE(("\t\"%s\"", opVector[i]->name),
		(": %s_opname%d .\" %s \" ;\n", prefix, i, opVector[i]->name));
	else
	    SOURCE(("\t0"),
		(""));
    }
    SOURCE(("\t0\n};\n\n"),
	("%d array [%s_opname]\n", maxOperator+1, prefix));
    for (i = 0; i <= maxOperator; i++) {
	if (opVector[i])
	    SOURCE((""),
		("  ' %s_opname%d %d [%s_opname] !\n", prefix, i, i, prefix));
    }
    SOURCE((" \t/* %d */\n};\n\n", maxOperator),
	(": %s_opname ( rule -- )\n  [%s_opname] @ execute ;\n\n", prefix, prefix));

    SOURCE(("char %s_arity[] = {\n", prefix),
	("%d carray_noallot %s_arity\n", maxOperator+1, prefix));
    for (i = 0; i <= maxOperator; i++) {
	if (i > 0)
	    SOURCE((",\t/* %d */\n", i-1),
		("\t\\ %d\n", i-1));
	SOURCE(("\t%d", opVector[i] ? opVector[i]->arity : -1),
	    ("  %d c,", opVector[i] ? opVector[i]->arity : -1));
    }
    SOURCE((" \t/* %d */\n};\n\n", maxOperator),
	("\t\\ %d\n: %s_arity@ ( i -- x )\n  %s_arity c@ ;\n\n", maxOperator, prefix, prefix));

    SOURCE(("int %s_max_op = %d;\n", prefix, maxOperator),
	("%d constant %s_max_op\n", maxOperator, prefix));
    SOURCE(("int %s_max_state = %d;\n\n", prefix, globalMap->count-1),
	("%d constant %s_max_state\n\n", globalMap->count-1, prefix));
}

void DEFUN_VOID(makeSimple)
{
    makeRuleTable();
    makeTables();
    makeRule();
    makeState();
}

void DEFUN_VOID(startOptional)
{
    SOURCE(("#ifdef %s_STATE_LABEL\n", prefix),
	("bl word %s_STATE_LABEL find nip 0<> [IF]\n", prefix));
    SOURCE(("#define %s_INCLUDE_EXTRA\n", prefix),
	("  : %s_INCLUDE_EXTRA ;\n", prefix));
    SOURCE(("#else\n"),
	("[ELSE]\n"));
    SOURCE(("#ifdef STATE_LABEL\n"),
	("  bl word STATE_LABEL find nip 0<> [IF]\n"));
    SOURCE(("#define %s_INCLUDE_EXTRA\n", prefix),
	("    : %s_INCLUDE_EXTRA ;\n", prefix));
    SOURCE(("#define %s_STATE_LABEL\tSTATE_LABEL\n", prefix),
	("    : %s_STATE_LABEL STATE_LABEL ;\n"
	 "    : %s_STATE_LABEL@ STATE_LABEL@ ;\n"
	 "    : %s_STATE_LABEL! STATE_LABEL! ;\n", prefix, prefix, prefix));
    SOURCE(("#define %s_NODEPTR_TYPE\tNODEPTR_TYPE\n", prefix),
	(""));
    SOURCE(("#define %s_LEFT_CHILD\tLEFT_CHILD\n", prefix),
	("    : %s_LEFT_CHILD LEFT_CHILD ;\n"
	 "    : %s_LEFT_CHILD@ LEFT_CHILD@ ;\n"
	 "    : %s_LEFT_CHILD! LEFT_CHILD! ;\n", prefix, prefix, prefix));
    SOURCE(("#define %s_OP_LABEL\tOP_LABEL\n", prefix),
	("    : %s_OP_LABEL OP_LABEL ;\n"
	 "    : %s_OP_LABEL@ OP_LABEL@ ;\n"
	 "    : %s_OP_LABEL! OP_LABEL! ;\n", prefix, prefix, prefix));
    SOURCE(("#define %s_RIGHT_CHILD\tRIGHT_CHILD\n", prefix),
	("    : %s_RIGHT_CHILD RIGHT_CHILD ;\n"
	 "    : %s_RIGHT_CHILD@ RIGHT_CHILD@ ;\n"
	 "    : %s_RIGHT_CHILD! RIGHT_CHILD! ;\n", prefix, prefix, prefix));
    SOURCE(("#endif /* STATE_LABEL */\n"),
	("  [ENDIF] \\ STATE_LABEL\n"));
    SOURCE(("#endif /* %s_STATE_LABEL */\n\n", prefix),
	("[ENDIF] \\ %s_STATE_LABEL\n\n", prefix));

    SOURCE(("#ifdef %s_INCLUDE_EXTRA\n\n", prefix),
	("bl word %s_INCLUDE_EXTRA find nip 0<> [IF]\n\n", prefix));
}

void DEFUN_VOID(makeTerminals)
{
    int i;

    for (i = 0; i <= maxOperator; i++) {
	if (opVector[i])
	    SOURCE(("#define %s %d\n", opVector[i]->name, i),
		("%d constant %s\n", i, opVector[i]->name));
    }
    SOURCE(("\n"),
	("\n"));
}

void DEFUN_VOID(makeNonterminals)
{
    List l;

    for (l = nonterminals; l; l = l->next) {
	NonTerminal nt = (NonTerminal) l->x;
	if (nt->num < last_user_nonterminal) {
	    SOURCE(("#define %s_%s_NT %d\n", prefix, nt->name, nt->num),
		("%d constant %s_%s_NT\n", nt->num, prefix, nt->name));
	}
    }
    SOURCE(("#define %s_NT %d\n\n", prefix, last_user_nonterminal-1),
	("%d constant %s_NT\n\n", last_user_nonterminal-1, prefix));
}

void DEFUN_VOID(makeNonterminalArray)
{
    int i;
    List l;
    NonTerminal *nta;

    nta = (NonTerminal *) zalloc(sizeof(*nta) * last_user_nonterminal);

    for (l = nonterminals; l; l = l->next) {
	NonTerminal nt = (NonTerminal) l->x;
	if (nt->num < last_user_nonterminal)
	    nta[nt->num] = nt;
    }

    SOURCE(("char *%s_ntname[] = {\n", prefix),
	(""));
    SOURCE(("\t\"Error: Nonterminals are > 0\",\n"),
	(""));
    for (i = 1; i < last_user_nonterminal; i++)
	SOURCE(("\t\"%s\",\n", nta[i]->name),
	    (": %s_ntname%d .\" %s\" ;\n", prefix, i, nta[i]->name));
    SOURCE(("\t0\n};\n\n"),
	("%d array_noallot [%s_ntname]\n", max_erule_num+1, prefix));
    for (i = 1; i < last_user_nonterminal; i++)
	SOURCE((""),
	    ("  ' %s_ntname%d ,\n", prefix, i));
    SOURCE(("};\n\n"),
	(": %s_ntname ( rule -- )\n  [%s_ntname] @ execute ;\n\n", prefix, prefix));

    zfree(nta);
}

void DEFUN_VOID(endOptional)
{
    SOURCE(("#endif /* %s_INCLUDE_EXTRA */\n\n", prefix),
	("[ENDIF] \\ %s_INCLUDE_EXTRA\n\n", prefix));
}

void DEFUN_VOID(startBurm)
{
    SOURCE(("\n#ifndef %s_PANIC\n", prefix),
	("\nbl word %s_PANIC\" find nip 0= [IF]\n", prefix));
    SOURCE(("#define %s_PANIC\tPANIC\n", prefix),
	(": %s_PANIC\" ( -- )\n  .\" PANIC\" ; immediate\n", prefix));
    SOURCE(("#endif /* %s_PANIC */\n", prefix),
	("[ENDIF]\n"));
    SOURCE(("#define %s_assert(x, y)\tif(!(x)) {y; abort();}\n\n", prefix),
	(": %s_assert\" postpone abort\" ; immediate restrict\n\n", prefix));
}

void DEFUN_VOID(reportDiagnostics)
{
    List l;

    for (l = operators; l; l = l->next) {
	Operator op = (Operator) l->x;
	if (!op->ref) {
	    WARNING("Unreferenced Operator: %s\n", op->name);
	}
    }
    for (l = rules; l; l = l->next) {
	Rule r = (Rule) l->x;
	if (!r->used && r->num < max_ruleAST) {
	    WARNING("Unused Rule: #%d\n", r->erulenum);
	}
    }
    if (!start->pmap) {
	WARNING("Start Nonterminal (%s) does not appear on LHS.\n", start->name);
    }

    VERBOSE("start symbol = \"%s\"\n", start->name);
    VERBOSE("# of states = %d\n", globalMap->count-1);
    VERBOSE("# of nonterminals = %d\n", max_nonterminal-1);
    VERBOSE("# of user nonterminals = %d\n", last_user_nonterminal-1);
    VERBOSE("# of rules = %d\n", max_rule);
    VERBOSE("# of user rules = %d\n", max_ruleAST);
}

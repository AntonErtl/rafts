#ident "@(#)$Id: fe.c,v 1.1 1996/08/14 18:42:27 anton Exp $";

#include "b.h"
#include "fe.h"

static int arity;

List	ruleASTs;

static void EXFUN(doBinding, (Binding b));
static void EXFUN(doDecl, (Arity a));
static NonTerminal EXFUN(lookup, (Pattern p));
static NonTerminal EXFUN(normalize, (PatternAST ast, NonTerminal nt, Pattern *patt));
static void EXFUN(doEnterNonTerm, (RuleAST ast));
static void EXFUN(doRule, (RuleAST ast));
static void EXFUN(doTable, (Operator op));

static void DEFUN(doBinding, (b), Binding b)
{
    int new;
    Symbol s;

    s = enter(b->name, &new);
    if (!new) {
	fprintf(stderr, "Non-unique name: %s\n", b->name);
	exit(1);
    }
    s->tag = OPERATOR;
    s->u.op = newOperator(b->name, b->opnum, arity);
    if (arity == 0)
	leaves = newList(s->u.op, leaves);
}

static void DEFUN(doDecl, (a), Arity a)
{
    if (!a)
	return;
    arity = a->arity;
    foreachList((ListFn) doBinding, a->bindings);
}

static List xpatterns;
static int tcount;

static NonTerminal DEFUN(lookup, (p), Pattern p)
{
    char buf[10];
    char *s;
    List l;
    NonTerminal n;
    DeltaCost dummy;

    for (l = xpatterns; l; l = l->next) {
	Pattern x = (Pattern) l->x;
	if (x->op == p->op 
		&& x->children[0] == p->children[0]
		&& x->children[1] == p->children[1])
	    return x->normalizer;
    }
    sprintf(buf, "n%%%d", tcount++);
    s = (char *) zalloc(strlen(buf)+1);
    strcpy(s, buf);
    n = newNonTerminal(s);
    p->normalizer = n;
    xpatterns = newList(p, xpatterns);
    ZEROCOST(dummy);
    (void) newRule(dummy, 0, n, p);
    return n;
}

static NonTerminal DEFUN(normalize, (ast, nt, patt),
	PatternAST ast AND NonTerminal nt AND Pattern *patt)
{
    Symbol s;
    int new;
    Pattern dummy;

    s = enter(ast->op, &new);
    ast->sym = s;
    if (new) { 
	fprintf(stderr, "Illegal use of %s --- undefined symbol\n", s->name);
	exit(1);
	return 0; /* shut up compilers */
    } else if (s->tag == NONTERMINAL) {
	if (ast->children) {
	    fprintf(stderr, "Illegal use of %s, a non-terminal, as a terminal\n", s->name);
	    exit(1);
	}
	*patt = newPattern(0);
	(*patt)->children[0] = s->u.nt;
	return s->u.nt;
    } else {
	s->u.op->ref = 1;
	*patt = newPattern(s->u.op);
	if (s->u.op->arity == -1) {
	    if (!ast->children) {
		s->u.op->arity = 0;
		leaves = newList(s->u.op, leaves);
	    } else if (!ast->children->next) {
		s->u.op->arity = 1;
	    } else if (!ast->children->next->next) {
		s->u.op->arity = 2;
	    } else {
		fprintf(stderr, "ERROR: Too many children (max = 2) for \"%s\"\n", s->name);
		exit(1);
	    }
	    if (s->u.op->arity > max_arity)
		max_arity = s->u.op->arity;
	}
	switch (s->u.op->arity) {
	default:
	    assert(FALSE);
	    break;
	case 0:
	    if (ast->children) {
		fprintf(stderr, "ERROR: Incorrect number of children for leaf operator, \"%s\"\n", s->name);
		exit(1);
	    }
	    break;
	case 1:
	    if (!ast->children || ast->children->next) {
		fprintf(stderr, "ERROR: Incorrect number of children for unary operator, \"%s\"\n", s->name);
		exit(1);
	    }
	    (*patt)->children[0] = normalize((PatternAST) ast->children->x, 0, &dummy);
	    break;
	case 2:
	    if (!ast->children || !ast->children->next) {
		fprintf(stderr, "ERROR: Incorrect number of children for binary operator, \"%s\"\n", s->name);
		exit(1);
	    }
	    (*patt)->children[0] = normalize((PatternAST) ast->children->x, 0, &dummy);
	    (*patt)->children[1] = normalize((PatternAST) ast->children->next->x, 0, &dummy);
	    break;
	}
	if (nt) {
	    (*patt)->normalizer = nt;
	    return nt;
	} else {
	    return lookup(*patt);
	}
    }
}

static void DEFUN(doEnterNonTerm, (ast), RuleAST ast)
{
    int new;
    Symbol s;
    DeltaCost delta;
    int i;
    IntList p;

    s = enter(ast->lhs, &new);
    if (new) {
	s->u.nt = newNonTerminal(s->name);
	s->tag = NONTERMINAL;
    } else {
	if (s->tag != NONTERMINAL) {
	    fprintf(stderr, "Illegal use of %s as a non-terminal\n", s->name);
	    exit(1);
	}
    }
    ZEROCOST(delta);
    for (p = ast->cost, i = 0; p; p = p->next, i++) {
	int x = p->x;
#ifndef NOLEX
	if (lexical) {
	    if (i < DELTAWIDTH)
		delta[i] = x;
	} else 
#endif /* NOLEX */
	{
	    if (i == principleCost)
		PRINCIPLECOST(delta) = x;
	}
    }
    ast->rule = newRule(delta, ast->erulenum, s->u.nt, 0);
}

static void DEFUN(doRule, (ast), RuleAST ast)
{
    Pattern pat;

    (void) normalize(ast->pat, ast->rule->lhs, &pat);
    ast->rule->pat = pat;
}

static void DEFUN(doTable, (op), Operator op)
{
    op->table = newTable(op);
}

void DEFUN(doSpec, (decls, rules), List decls AND List rules)
{
    foreachList((ListFn) doDecl, decls);
    debug(debugTables, foreachList((ListFn) dumpOperator_l, operators));

    ruleASTs = rules;
    reveachList((ListFn) doEnterNonTerm, rules);

    last_user_nonterminal = max_nonterminal;

    reveachList((ListFn) doRule, rules);
    debug(debugTables, foreachList((ListFn) dumpRule, rules));

    foreachList((ListFn) doTable, operators);
}

void DEFUN(doStart, (name), char *name)
{
    Symbol s;
    int new;

    if (start) {
	yyerror1("Redeclaration of start symbol to be ");
	fprintf(stderr, "\"%s\"\n", name);
	exit(1);
    }
    s = enter(name, &new);
    if (new) {
	s->u.nt = newNonTerminal(s->name);
	s->tag = NONTERMINAL;
    } else {
	if (s->tag != NONTERMINAL) {
	    fprintf(stderr, "Illegal use of %s as a non-terminal\n", s->name);
	    exit(1);
	}
    }
}

Arity DEFUN(newArity, (ar, b), int ar AND List b)
{
    Arity a = (Arity) zalloc(sizeof(struct arity));
    a->arity = ar;
    a->bindings = b;
    return a;
}

Binding DEFUN(newBinding, (name, opnum), char *name AND int opnum)
{
    Binding b = (Binding) zalloc(sizeof(struct binding));
    if (opnum == 0) {
	yyerror1("ERROR: Non-positive external symbol number, ");
	fprintf(stderr, "%d", opnum);
	exit(1);
    }
    b->name = name;
    b->opnum = opnum;
    return b;
}

PatternAST DEFUN(newPatternAST, (op, children), char *op AND List children)
{
    PatternAST p = (PatternAST) zalloc(sizeof(struct patternAST));
    p->op = op;
    p->children = children;
    return p;
}

int max_ruleAST;

RuleAST DEFUN(newRuleAST, (lhs, pat, erulenum, cost),
	char *lhs AND PatternAST pat AND int erulenum AND IntList cost)
{
    RuleAST p = (RuleAST) zalloc(sizeof(struct ruleAST));
    p->lhs = lhs;
    p->pat = pat;
    if (erulenum <= 0) {
	yyerror1("External Rulenumber ");
	fprintf(stderr, "(%d) <= 0\n", erulenum);
	exit(1);
    }
    p->erulenum = erulenum;
    p->cost = cost;
    max_ruleAST++;
    return p;
}

void DEFUN(dumpBinding, (b), Binding b)
{
    printf("%s=%d ", b->name, b->opnum);
}

void DEFUN(dumpArity, (a), Arity a)
{
    List l;

    printf("Arity(%d) ", a->arity);
    for (l = a->bindings; l; l = l->next) {
	Binding b = (Binding) l->x;
	dumpBinding(b);
    }
    printf("\n");
}

void DEFUN(dumpPatternAST, (p), PatternAST p)
{
    List l;

    printf("%s", p->op);
    if (p->children) {
	printf("(");
	for (l = p->children; l; l = l->next) {
	    PatternAST past = (PatternAST) l->x;
	    dumpPatternAST(past);
	    if (l->next)
		printf(", ");
	}
	printf(")");
    }
}

void DEFUN(dumpRuleAST, (p), RuleAST p)
{
    printf("%s : ", p->lhs);
    dumpPatternAST(p->pat);
    printf(" = %d (%ld)\n", p->erulenum, (long) p->cost);
}

void DEFUN(dumpDecls, (decls), List decls)
{
    List l;

    for (l = decls; l; l = l->next) {
	Arity a = (Arity) l->x;
	dumpArity(a);
    }
}

void DEFUN(dumpRules, (rules), List rules)
{
    List l;

    for (l = rules; l; l = l->next) {
	RuleAST p = (RuleAST) l->x;
	dumpRuleAST(p);
    }
}

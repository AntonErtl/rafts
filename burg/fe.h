/*
#ident "@(#)$Id: fe.h,v 1.1 1996/08/14 18:46:24 anton Exp $"
 */

#ifndef _FE_H
#define _FE_H

struct binding {
	char	*name;
	int	opnum;
};
typedef struct binding	*Binding;

struct arity {
	int	arity;
	List	bindings;
};
typedef struct arity	*Arity;

struct patternAST {
	struct symbol *sym;
	char	*op;
	List	children;
};
typedef struct patternAST	*PatternAST;

struct ruleAST {
	char			*lhs;
	PatternAST		pat;
	int			erulenum;
	IntList			cost;
	struct rule		*rule;
	struct strTableElement	*kids;
	struct strTableElement	*nts;
};
typedef struct ruleAST	*RuleAST;

typedef enum {
	UNKNOWN,
	OPERATOR,
	NONTERMINAL
} TagType;

struct symbol {
	char	*name;
	TagType	tag;
	union {
		NonTerminal	nt;
		Operator	op;
	} u;
};
typedef struct symbol	*Symbol;

struct strTableElement {
	char *str;
	IntList erulenos;
	char *ename;
};
typedef struct strTableElement	*StrTableElement;

struct strTable {
	List elems;
};
typedef struct strTable	*StrTable;

extern int	max_ruleAST;
extern List	ruleASTs;

extern FILE	*infile;
extern FILE	*outfile;
extern char	*prefix;
extern int 	safely;
extern int 	trimflag;

__BEGIN_DECLS

void EXFUN(source, (CONST char* DOTS));
void EXFUN(VERBOSE, (CONST char* DOTS));
void EXFUN(WARNING, (CONST char* DOTS));
void EXFUN(ERROR, (CONST char* DOTS));

#define LANG(a, b)      ((language == LANG_C) ? a : b)
#define SOURCE(a, b)    (LANG((source a), (source b)))

/* be.c */
extern int EXFUN(opsOfArity, (int arity));
extern void EXFUN(makeLabel, (NOARGS));
extern void EXFUN(makeLabelinternals, (NOARGS));
extern void EXFUN(makeRuleTable, (NOARGS));
extern void EXFUN(makeTables, (NOARGS));
extern void EXFUN(makeLHSmap, (NOARGS));
extern void EXFUN(makeClosureArray, (NOARGS));
extern void EXFUN(makeCostArray, (NOARGS));
extern void EXFUN(makeNts, (NOARGS));
extern void EXFUN(makeStringArray, (NOARGS));
extern void EXFUN(makeRule, (NOARGS));
extern void EXFUN(makeKids, (NOARGS));
extern void EXFUN(makeOpLabel, (NOARGS));
extern void EXFUN(makeStateLabel, (NOARGS));
extern void EXFUN(makeChild, (NOARGS));
extern void EXFUN(makeOperatorVector, (NOARGS));
extern void EXFUN(makeOperators, (NOARGS));
extern void EXFUN(makeDebug, (NOARGS));
extern void EXFUN(makeSimple, (NOARGS));
extern void EXFUN(startOptional, (NOARGS));
extern void EXFUN(makeTerminals, (NOARGS));
extern void EXFUN(makeNonterminals, (NOARGS));
extern void EXFUN(makeNonterminalArray, (NOARGS));
extern void EXFUN(endOptional, (NOARGS));
extern void EXFUN(startBurm, (NOARGS));
extern void EXFUN(reportDiagnostics, (NOARGS));

/* burg.c */
extern void EXFUN(main, (int argc, char **argv));

/* fe.c */
extern void EXFUN(doSpec, (List decls, List rules));
extern void EXFUN(doStart, (char *name));
extern Arity EXFUN(newArity, (int ar, List b));
extern Binding EXFUN(newBinding, (char *name, int opnum));
extern PatternAST EXFUN(newPatternAST, (char *op, List children));
extern RuleAST EXFUN(newRuleAST, (char *lhs, PatternAST pat, int erulenum, IntList cost));
extern void EXFUN(dumpBinding, (Binding b));
extern void EXFUN(dumpArity, (Arity a));
extern void EXFUN(dumpPatternAST, (PatternAST p));
extern void EXFUN(dumpRuleAST, (RuleAST p));
extern void EXFUN(dumpDecls, (List decls));
extern void EXFUN(dumpRules, (List rules));

/* gram.y */
extern int EXFUN(yyparse, (NOARGS));

/* lex.c */
extern void EXFUN(yypurge, (NOARGS));
extern void EXFUN(yyfinished, (NOARGS));
extern int EXFUN(yylex, (NOARGS));
extern void EXFUN(yyerror, (char *str));
extern void EXFUN(yyerror1, (char *str));

/* plank.c */
extern void EXFUN(dumpSortedStates, (NOARGS));
extern void EXFUN(dumpSortedRules, (NOARGS));
extern void EXFUN(makePlanks, (NOARGS));

/* string.c */
extern StrTable EXFUN(newStrTable, (NOARGS));
extern void EXFUN(dumpStrTable, (StrTable t));
extern StrTableElement EXFUN(addString, (StrTable t, char *s, int eruleno, int *new));

/* symtab.c */
extern Symbol EXFUN(newSymbol, (char *name));
extern Symbol EXFUN(enter, (char *name, int *new));

__END_DECLS

#endif

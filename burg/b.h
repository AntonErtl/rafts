/*
#ident "@(#)$Id: b.h,v 1.1 1996/08/14 18:46:23 anton Exp $"
 */

#ifndef _B_H
#define _B_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <ansidecl.h>
#include <malloc.h>
#include <string.h>
#include <math.h>

#ifndef HAVE_BOOLEAN
#define HAVE_BOOLEAN
typedef int boolean;
#endif
#ifndef FALSE			/* in case these macros already exist */
#define FALSE ((boolean) 0)	/* values of boolean */
#endif
#ifndef TRUE
#define TRUE ((boolean) 1)
#endif

#define MAX_ARITY	2

typedef int ItemSetNum;
typedef int OperatorNum;
typedef int NonTerminalNum;
typedef int RuleNum;
typedef int ArityNum;
typedef int ERuleNum;

extern NonTerminalNum	last_user_nonterminal;
extern NonTerminalNum	max_nonterminal;
extern RuleNum		max_rule;
extern ERuleNum		max_erule_num;
extern int		max_arity;

#ifndef NOLEX
#define DELTAWIDTH	4
typedef short DeltaCost[DELTAWIDTH];
typedef short *DeltaPtr;

#define	__BEGIN_DECLS
#define	__END_DECLS

__BEGIN_DECLS

extern void EXFUN(ASSIGNCOST, (DeltaPtr l, DeltaPtr r));
extern void EXFUN(ADDCOST, (DeltaPtr l, DeltaPtr r));
extern void EXFUN(MINUSCOST, (DeltaPtr l, DeltaPtr r));
extern void EXFUN(ZEROCOST, (DeltaPtr x));
extern int EXFUN(LESSCOST, (DeltaPtr l, DeltaPtr r));
extern int EXFUN(EQUALCOST, (DeltaPtr l, DeltaPtr r));

__END_DECLS

#define PRINCIPLECOST(x) (x[0])
#else
#define DELTAWIDTH 1
typedef int DeltaCost;
typedef int DeltaPtr;
#define ASSIGNCOST(l, r)	((l) = (r))
#define ADDCOST(l, r)		((l) += (r))
#define MINUSCOST(l, r)		((l) -= (r))
#define ZEROCOST(x)		((x) = 0)
#define LESSCOST(l, r)		((l) < (r))
#define EQUALCOST(l, r)		((l) == (r))
#define PRINCIPLECOST(x)	(x)
#endif /* NOLEX */
#define NODIVERGE(c,state,nt,base)		if (prevent_divergence > 0) CHECKDIVERGE(c,state,nt,base);

struct list {
	void		*x;
	struct list	*next;
};
typedef struct list	*List;

struct intlist {
	int		x;
	struct intlist	*next;
};
typedef struct intlist	*IntList;

struct operator {
	char		*name;
	unsigned int	ref:1;
	OperatorNum	num;
	ItemSetNum	baseNum;
	ItemSetNum	stateCount;
	ArityNum	arity;
	struct table	*table;
};
typedef struct operator	*Operator;

struct nonterminal {
	char		*name;
	NonTerminalNum	num;
	ItemSetNum	baseNum;
	ItemSetNum	ruleCount;
	struct plankMap *pmap;

	struct rule	*sampleRule; /* diagnostic---gives "a" rule that with this lhs */
};
typedef struct nonterminal	*NonTerminal;

struct pattern {
	NonTerminal	normalizer;
	Operator	op;		/* NULL if NonTerm -> NonTerm */
	NonTerminal	children[MAX_ARITY];
};
typedef struct pattern	*Pattern;

struct rule {
	DeltaCost	delta;
	ERuleNum	erulenum;
	RuleNum		num;
	RuleNum		newNum;
	NonTerminal	lhs;
	Pattern		pat;
	unsigned int	used:1;
};
typedef struct rule	*Rule;

struct item {
	DeltaCost	delta;
	Rule		rule;
};
typedef struct item	Item;

typedef short 	*Relevant;	/* relevant non-terminals */

typedef Item	*ItemArray;

struct item_set {	/* indexed by NonTerminal */
	ItemSetNum	num;
	ItemSetNum	newNum;
	Operator	op;
	struct item_set *kids[2];
	struct item_set *representative;
	Relevant	relevant;
	ItemArray	virgin;
	ItemArray	closed;
};
typedef struct item_set	*Item_Set;

#define DIM_MAP_SIZE	(1 << 8)
#define GLOBAL_MAP_SIZE	(1 << 15)

struct mapping {	/* should be a hash table for TS -> int */
	List		*hash;
	int		hash_size;
	int		max_size;
	ItemSetNum	count;
	Item_Set	*set;	/* map: int <-> Item_Set */
};
typedef struct mapping	*Mapping;

struct index_map {
	ItemSetNum	max_size;
	Item_Set	*class;
};
typedef struct index_map	Index_Map;

struct dimension {
	Relevant	relevant;
	Index_Map	index_map;
	Mapping		map;
	ItemSetNum	max_size;
	struct plankMap *pmap;
};
typedef struct dimension	*Dimension;

struct table {
	Operator	op;
	List		rules;
	Relevant	relevant;
	Dimension	dimen[MAX_ARITY];	/* 1 for each dimension */
	Item_Set	*transition;	/* maps local indices to global
						itemsets */
};
typedef struct table	*Table;

struct relation {
	Rule	rule;
	DeltaCost	chain;
	NonTerminalNum	nextchain;
	DeltaCost	sibling;
	int		sibFlag;
	int		sibComputed;
};
typedef struct relation	*Relation;

struct queue {
	List head;
	List tail;
};
typedef struct queue	*Queue;

struct plank {
	char *name;
	List fields;
	int width;
};
typedef struct plank	*Plank;

struct except {
	short index;
	short value;
};
typedef struct except	*Exception;

struct plankMap {
	List exceptions;
	int offset;
	struct stateMap *values;
};
typedef struct plankMap	*PlankMap;

struct stateMap {
	char *fieldname;
	Plank plank;
	int width;
	short *value;
};
typedef struct stateMap	*StateMap;

struct stateMapTable {
	List maps;
};

extern NonTerminal	start;
extern List		rules;
extern List		chainrules;
extern List		operators;
extern List		leaves;
extern List		nonterminals;
extern Queue		globalQ;
extern Mapping		globalMap;
extern int		exceptionTolerance;
extern int		prevent_divergence;
extern int		principleCost;
extern int		lexical;
extern struct rule	stub_rule;
extern Relation 	*allpairs;

extern int		debugTrim;

extern int 	language;
#define LANG_C		0
#define LANG_FORTH	1

#define TABLE_INCR	8
#define STATES_INCR	64

#ifdef DEBUG
#define debug(a,b)	if (a) b
#else
#define debug(a,b)
#endif
extern int debugTables;

#ifdef NDEBUG
#define assert(c) ((void) 0)
#else
#define assert(c) ((void) ((c) || fatal(__FILE__,__LINE__)))
#endif

__BEGIN_DECLS

/* burs.c */
extern void EXFUN(build, (NOARGS));

/* closure.c */
extern void EXFUN(findChainRules, (NOARGS));
extern void EXFUN(zero, (Item_Set t));
extern void EXFUN(closure, (Item_Set t));

/* delta.c */
extern void EXFUN(CHECKDIVERGE, (DeltaPtr c, Item_Set its, int nt, int base));

/* item.c */
extern ItemArray EXFUN(newItemArray, (NOARGS));
extern ItemArray EXFUN(itemArrayCopy, (ItemArray src));
extern Item_Set EXFUN(newItem_Set, (Relevant relevant));
extern void EXFUN(freeItem_Set, (Item_Set ts));
extern int EXFUN(equivSet, (Item_Set a, Item_Set b));
extern void EXFUN(printRepresentative, (FILE *f, Item_Set s));
extern void EXFUN(dumpItem, (Item *t));
extern void EXFUN(dumpItem_Set, (Item_Set ts));
extern void EXFUN(dumpCost, (DeltaCost dc));

/* list.c */
extern IntList EXFUN(newIntList, (int x, IntList next));
extern List EXFUN(newList, (PTR x, List next));
extern List EXFUN(appendList, (PTR x, List l));
extern int EXFUN(length, (List l));

typedef PTR EXFUN((*ListFn), (PTR));
extern void EXFUN(foreachList, (ListFn f, List l));
extern void EXFUN(reveachList, (ListFn f, List l));

/* map.c */
extern Mapping EXFUN(newMapping, (int size));
extern Item_Set EXFUN(decode, (Mapping m, ItemSetNum t));
extern Item_Set EXFUN(encode, (Mapping m, Item_Set ts, int *new));
extern void EXFUN(dumpMapping, (Mapping m));

/* nonterminal.c */
extern NonTerminal EXFUN(newNonTerminal, (char *name));
extern int EXFUN(nonTerminalName, (char *buf, int i));
extern void EXFUN(dumpNonTerminal, (NonTerminal n));

/* operator.c */
extern Operator EXFUN(newOperator, (char *name, OperatorNum num, ArityNum arity));
extern void EXFUN(dumpOperator_s, (Operator op));
extern void EXFUN(dumpOperator, (Operator op, int full));
extern void EXFUN(dumpOperator_l, (Operator op));

/* pattern.c */
extern Pattern EXFUN(newPattern, (Operator op));
extern void EXFUN(dumpPattern, (Pattern p));

/* queue.c */
extern Queue EXFUN(newQ, (NOARGS));
extern void EXFUN(addQ, (Queue q, Item_Set ts));
extern Item_Set EXFUN(popQ, (Queue q));
extern void EXFUN(dumpQ, (Queue q));

/* rule.c */
extern Rule EXFUN(newRule, (DeltaPtr delta, ERuleNum erulenum, NonTerminal lhs, Pattern pat));
extern void EXFUN(dumpRule, (Rule p));
extern void EXFUN(dumpRuleList, (List l));

/* table.c */
extern void EXFUN(addRelevant, (Relevant r, NonTerminalNum rt));
extern Table EXFUN(newTable, (Operator op));
extern void EXFUN(addToTable, (Table t, Item_Set ts));
extern Item_Set *EXFUN(transLval, (Table t, int row, int col));
extern void EXFUN(dumpRelevant, (Relevant r));
extern void EXFUN(dumpIndex_Map, (Index_Map *r));
extern void EXFUN(dumpDimension, (Dimension d));
extern void EXFUN(dumpTable, (Table t, int full));
extern void EXFUN(dumpTransition, (Table t));

/* trim.c */
extern void EXFUN(findAllPairs, (NOARGS));
extern void EXFUN(trim, (Item_Set t));
extern void EXFUN(dumpRelation, (Relation r));
extern void EXFUN(dumpAllPairs, (NOARGS));

/* zalloc.c */
extern int EXFUN(fatal, (char *name, int line));
extern PTR EXFUN(zalloc, (unsigned int size));
extern void EXFUN(zfree, (PTR p));

__END_DECLS

#endif

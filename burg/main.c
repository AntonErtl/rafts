#ident "@(#)$Id: main.c,v 1.1 1997/02/27 13:35:11 pirky Exp $";

#include "b.h"
#include "fe.h"
#ifdef	HAVE_GETOPT_H
#include <getopt.h>
#else
#include "getopt.h"
#endif

int debugTables = 0;
static int simpleTables = 0;
static int internals = 0;
static int diagnostics = 0;

static char *inFileName;
static char *outFileName;

static char version[] = "BURG, Version 1.0";

int language = LANG_C;

void DEFUN(main, (argc, argv), int argc AND char **argv)
{
    extern int optind;
    extern char *optarg;

    char c;

    while ((c = getopt(argc, argv, "CFIO:TVc:de:i:o:p:t=")) != EOF)
	switch (c) {
	    case 'V':
		fprintf(stderr, "%s\n", version);
		break;
	    case 'C':
		language = LANG_C;
		break;
	    case 'F':
		language = LANG_FORTH;
		break;
	    case 'p':
		prefix = optarg;
		break;
	    case 'i':
		inFileName = optarg;
	    case 'o':
		outFileName = optarg;
		break;
	    case 'I':
		internals = 1;
		break;
	    case 'T':
		simpleTables = 1;
		break;
	    case '=':
#ifdef NOLEX
		fprintf(stderr, "'%s' was not compiled to support lexicographic ordering\n", argv[0]);
#else
		lexical = 1;
#endif /* NOLEX */
		break;
	    case 'O':
		principleCost = atoi(optarg);
		break;
	    case 'c':
		prevent_divergence = atoi(optarg);
		break;
	    case 'e':
		exceptionTolerance = atoi(optarg);
		break;
	    case 'd':
		diagnostics = 1;
		break;
	    case 't':
		trimflag = 1;
		break;
	    default:
		fprintf(stderr, "Bad OPTION (%s)\n", argv[0]);
		exit(1);
	}

    if ((argc-optind) == 1)
	if (inFileName) {
	    fprintf(stderr, "Unexpected Filename (%s) after (%s)\n", argv[0], inFileName);
	    exit(1);
	}
	else
            inFileName = argv[optind];

    if (inFileName) {
	if ((infile = fopen(inFileName, "r")) == NULL) {
	    fprintf(stderr, "Failed opening (%s)", inFileName);
	    exit(1);
	}
    } else
	infile = stdin;

    if (outFileName) {
	if ((outfile = fopen(outFileName, "w")) == NULL) {
	    fprintf(stderr, "Failed opening (%s)", outFileName);
	    exit(1);
	}
    } else
	outfile = stdout;


    yyparse();

    if (!rules) {
	fprintf(stderr, "ERROR: No rules present\n");
	exit(1);
    }

    findChainRules();
    findAllPairs();
    build();

    debug(debugTables, foreachList((ListFn) dumpOperator_l, operators));
    debug(debugTables, printf("---final set of states ---\n"));
    debug(debugTables, dumpMapping(globalMap));

    startBurm();
    makeNts();
    makeOperatorVector();
    if (simpleTables)
	makeSimple();
    else
	makePlanks();

    makeTerminals();
    makeNonterminals();
    if (internals) {
	makeOperators();
	makeStringArray();
	makeCostArray();
	makeNonterminalArray();
	/*
	makeLHSmap();
	*/
    }
    makeClosureArray();

    startOptional();
    if (internals) {
	makeLabelinternals();
	makeKids();
	makeChild();
	makeOpLabel();
	makeStateLabel();
    } else {
	makeLabel();
	makeKids();
    }
    endOptional();

    if (diagnostics)
	reportDiagnostics();

    yypurge();
    exit(0);
}

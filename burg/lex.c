#ident "@(#)$Id: lex.c,v 1.1 1996/08/14 18:42:28 anton Exp $";

#include "b.h"
#include "fe.h"
#include "y.tab.h"

static char buf[BUFSIZ];

static int yyline = 1;

typedef int EXFUN((*ReadFn), (NOARGS));

static char *EXFUN(StrCopy, (char * s));
static int EXFUN(simple_get, (NOARGS));
static int EXFUN(simple_unget, (int ch));
static int EXFUN(code_get, (NOARGS));
static int EXFUN(simple_put, (int ch));
static void EXFUN(ReadCharString, (ReadFn rdfn, int which));
static void EXFUN(ReadOldComment, (ReadFn rdfn));
static void EXFUN(ReadCodeBlock, (NOARGS));

static char *DEFUN(StrCopy, (s), char *s)
{
    char *t = (char *)zalloc(strlen(s) + 1);
    strcpy(t, s);
    return t;
}

static int DEFUN_VOID(simple_get)
{
    int ch;
    if ((ch = fgetc(infile)) == '\n')
	yyline++;
    return ch;
}

static int DEFUN(simple_unget, (ch), int ch)
{
    ungetc(ch, infile);
    if (ch == '\n')
	yyline--;
    return ch;
}

static int DEFUN_VOID(code_get)
{
    int ch = simple_get();
    if (ch != EOF)
	simple_put(ch);
    return ch;
}

static int DEFUN(simple_put, (ch), int ch)
{
    fputc(ch, outfile);
    return ch;
}

void DEFUN_VOID(yypurge)
{
    int ch;

    while ((ch = simple_get()) != EOF) {
	if (ch == '\\')
	    ch = code_get();
	else {
	    simple_put(ch);
	    if (ch == '"' || ch == '\'')
		ReadCharString(code_get, ch);
	    else if (ch == '/') {
		ch = simple_get();
		if (ch == '*') {
		    simple_put(ch);
		    ReadOldComment(code_get);
		    continue;
		} else
		    simple_unget(ch);
	    }
	}
    }
}

static void DEFUN(ReadCharString, (rdfn, which), ReadFn rdfn AND int which)
{
    int ch;
    int backslash = 0;
    int firstline = yyline;

    while ((ch = rdfn()) != EOF) {
	if (ch == which && !backslash)
	    return;
	if (ch == '\\' && !backslash)
	    backslash = 1;
	else
	    backslash = 0;
    }
    yyerror1("Unexpected EOF in string on line ");
    fprintf(stderr, "%d\n", firstline);
    exit(1);
}

static void DEFUN(ReadOldComment, (rdfn), ReadFn rdfn)
{
    /* will not work for comments delimiter in string */
    int ch;
    int starred = 0;
    int firstline = yyline;

    while ((ch = rdfn()) != EOF) {
	if (ch == '*')
	    starred = 1;
	else if (ch == '/' && starred)
	    return;
	else
	    starred = 0;
    }
    yyerror1("Unexpected EOF in comment on line ");
    fprintf(stderr, "%d\n", firstline);
    exit(1);
}

static void DEFUN_VOID(ReadCodeBlock)
{
    int ch;
    int firstline = yyline;

    while ((ch = simple_get()) != EOF) {
	if (ch == '%') {
	    ch = simple_get();
	    if (ch != '}')
		yyerror("bad %%");
	    return;
	}
	if (ch == '\\')
	    ch = code_get();
	else {
	    simple_put(ch);
	    if (ch == '"' || ch == '\'')
		ReadCharString(code_get, ch);
	    else if (ch == '/') {
		ch = simple_get();
		if (ch == '*') {
		    simple_put(ch);
		    ReadOldComment(code_get);
		    continue;
		} else
		    simple_unget(ch);
	    }
	}
    }
    yyerror1("Unclosed block of code started on line ");
    fprintf(stderr, "%d\n", firstline);
    exit(1);
}

static int done = 0;

void DEFUN_VOID(yyfinished)
{
    done = 1;
}

int DEFUN_VOID(yylex)
{
    int ch;
    char *ptr = buf;

    if (done) return 0;
    while ((ch = simple_get()) != EOF) {
	switch (ch) {
	case ' ':
	case '\f':
	case '\t':
	    continue;
	case '\n':
	    continue;
	case '(':
	case ')':
	case ',':
	case ':':
	case ';':
	case '=':
	    return(ch);
	case '/':
	    ch = simple_get();
	    if (ch == '*') {
		ReadOldComment(simple_get);
		continue;
	    } else {
		simple_unget(ch);
		yyerror("illegal char /");
		continue;
	    }
	case '%':
	    ch = simple_get();
	    switch (ch) {
	    case '%':
		return (K_PPERCENT);
	    case '{':
		ReadCodeBlock();
		continue;
	    case 'l':
	    case 'u':
	    case 'b':
	    case 's':
	    case 't':
		do {
		    if (ptr >= &buf[BUFSIZ]) {
			yyerror("ID too long");
			return(ERROR);
		    } else
			*ptr++ = ch;
		    ch = simple_get();
		} while (isalpha(ch) || isdigit(ch) || ch == '_');
		simple_unget(ch);
		*ptr = '\0';
		if (!strcmp(buf, "term")) return K_TERM;
		if (!strcmp(buf, "start")) return K_START;
		yyerror("illegal character after %%");
		continue;
	    default:
		yyerror("illegal character after %%");
		continue;
	    }
	default:
	    if (isalpha(ch) ) {
		do {
		    if (ptr >= &buf[BUFSIZ]) {
			yyerror("ID too long");
			return(ERROR);
		    } else
			*ptr++ = ch;
		    ch = simple_get();
		} while (isalpha(ch) || isdigit(ch) || ch == '_');
		simple_unget(ch);
		*ptr = '\0';
		yylval.y_string = StrCopy(buf);
		return(ID);
	    } 
	    if (isdigit(ch)) {
		int val=0;
		do {
		    val *= 10;
		    val += (ch - '0');
		    ch = simple_get();
		} while (isdigit(ch));
		simple_unget(ch);
		yylval.y_int = val;
		return(INT);
	    }
	    yyerror1("illegal char ");
	    fprintf(stderr, "(\\%03o)\n", ch);
	    exit(1);
	}
    }
    return(0);
}

void DEFUN(yyerror1, (str), char *str)
{
    fprintf(stderr, "line %d: %s", yyline, str);
}

void DEFUN(yyerror, (str), char *str)
{
    yyerror1(str);
    fprintf(stderr, "\n");
    exit(1);
}

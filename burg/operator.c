#ident "@(#)$Id: operator.c,v 1.1 1996/08/14 18:42:30 anton Exp $";

#include "b.h"

int max_arity = -1;

List operators;
List leaves;

Operator DEFUN(newOperator, (name, num, arity), char *name AND OperatorNum num AND ArityNum arity)
{
    Operator op;

    assert(arity <= MAX_ARITY);
    op = (Operator) zalloc(sizeof(struct operator));
    assert(op);
    op->name = name;
    op->num = num;
    op->arity = arity;

    operators = newList(op, operators);
    return op;
}

void DEFUN(dumpOperator_s, (op), Operator op)
{
    printf("Op: %s(%d)=%d\n", op->name, op->arity, op->num);
}

void DEFUN(dumpOperator, (op, full), Operator op AND int full)
{
    dumpOperator_s(op);
    if (full) {
	dumpTable(op->table, 0);
    }
}

void DEFUN(dumpOperator_l, (op), Operator op)
{
    dumpOperator(op, 1);
}


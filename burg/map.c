#ident "@(#)$Id: map.c,v 1.1 1996/08/14 18:42:29 anton Exp $";

#include "b.h"

Mapping globalMap;

__BEGIN_DECLS

static void EXFUN(growMapping, (Mapping m));
static int EXFUN(hash, (Item_Set ts, int mod));

__END_DECLS

Mapping DEFUN(newMapping, (size), int size)
{
    Mapping m;

    m = (Mapping) zalloc(sizeof(struct mapping));
    assert(m);

    m->count = 0;
    m->hash = (List*) zalloc(size * sizeof(List));
    m->hash_size = size;
    m->max_size = STATES_INCR;
    m->set = (Item_Set*) zalloc(m->max_size * sizeof(Item_Set));
    assert(m->set);
    return m;
}

static void DEFUN(growMapping, (m), Mapping m)
{
    Item_Set *tmp;

    m->max_size += STATES_INCR;
    tmp = (Item_Set*) zalloc(m->max_size * sizeof(Item_Set));
    memcpy(tmp, m->set, m->count * sizeof(Item_Set));
    zfree(m->set);
    m->set = tmp;
}

static int DEFUN(hash, (ts, mod), Item_Set ts AND int mod)
{
    register Item *p = ts->virgin;
    register int v;
    register Relevant r = ts->relevant;
    register int nt;

    if (!ts->op)
	return 0;
    v = 0;
    for (; (nt = *r) != 0; r++)
	v ^= ((int)p[nt].rule) + (PRINCIPLECOST(p[nt].delta)<<4);
    v >>= 4;
    v &= (mod-1);
    return v;
}

Item_Set DEFUN(encode, (m, ts, new), Mapping m AND Item_Set ts AND int *new)
{
    int h;
    List l;

    assert(m);
    assert(ts);
    assert(m->count <= m->max_size);
    *new = 0;
    h = hash(ts, m->hash_size);
    for (l = m->hash[h]; l; l = l->next) {
	Item_Set s = (Item_Set) l->x;
	if (ts->op == s->op && equivSet(ts, s)) {
	    ts->num = s->num;
	    return s;
	}
    }
    if (m->count >= m->max_size)
	growMapping(m);
    assert(m->count < m->max_size);
    m->set[m->count] = ts;
    ts->num = m->count++;
    *new = 1;
    m->hash[h] = newList(ts, m->hash[h]);
    return ts;
}

Item_Set DEFUN(decode, (m, t), Mapping m AND ItemSetNum t)
{
    assert(m);
    assert(t);
    assert(m->count < m->max_size);
    assert(t < m->count);
    return m->set[t];
}

void DEFUN(dumpMapping, (m), Mapping m)
{
    int i;

    printf("BEGIN Mapping: Size=%d\n", m->count);
    for (i = 0; i < m->count; i++)
	dumpItem_Set(m->set[i]);
    printf("END Mapping\n");
}

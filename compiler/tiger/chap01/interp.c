#include "util.h"
#include "slp.h"
#include "prog1.h"
#include <assert.h>
#include <stdlib.h>

/* table op */
typedef struct table *Table_;
struct table {string id; int value; Table_ tail;};
Table_ Table(string id, int value, struct table *tail)
{
    Table_ t = malloc(sizeof(*t));
    t->id    = id;
    t->value = value;
    t->tail  = tail;
    return t;
}

#define NIL     (~0)
int lookup(Table_ t, string key)
{
    while (t)
    {
        if ( strcmp(t->id, key) == 0 )
            return t->value;

        t = t->tail;
    }

    return NIL;
}

Table_ update(Table_ t1, string key, int value)
{
    return Table(key, value, t1);
}



/* interpExp */
Table_ interpStm(A_stm s, Table_ t);

struct IntAndTable {int i; Table_ t;};
struct IntAndTable interpExp(A_exp e, Table_ t)
{
    int a, b;
    struct IntAndTable ret;
    switch (e->kind)
    {
    case A_idExp:
        ret.i = lookup(t, e->u.id);
        ret.t = t;
        return ret;

    case A_numExp:
        ret.i = e->u.num;
        ret.t = t;
        return ret;

    case A_opExp:
        ret = interpExp(e->u.op.left, t);
        a = ret.i;
        ret = interpExp(e->u.op.right, ret.t);
        b = ret.i;
        switch (e->u.op.oper)
        {
        case A_plus: ret.i = a + b; break;
        case A_minus: ret.i = a - b; break;
        case A_times: ret.i = a * b; break;
        case A_div: ret.i = a / b; break;
        }
        return ret;

    case A_eseqExp:
        t = interpStm(e->u.eseq.stm, t);
        ret = interpExp(e->u.eseq.exp, t);
        return ret;
    }
    assert(0 || "error");
}

Table_ interpPrint(A_expList l, Table_ t)
{
    struct IntAndTable ret;
    switch (l->kind)
    {
    case A_pairExpList:
        ret = interpExp(l->u.pair.head, t);
        printf("%d\n", ret.i);
        return interpPrint(l->u.pair.tail, t);

    case A_lastExpList:
        ret = interpExp(l->u.last, t);
        printf("%d\n", ret.i);
        return ret.t;
    }
    assert(0 || "print error");
}

Table_ interpStm(A_stm s, Table_ t)
{
    struct IntAndTable ret;
    switch (s->kind)
    {
    case A_compoundStm:
        t = interpStm(s->u.compound.stm1, t);
        return interpStm(s->u.compound.stm2, t);

    case A_assignStm:
        ret = interpExp(s->u.assign.exp, t);
        return update(ret.t, s->u.assign.id, ret.i);

    case A_printStm:
        return interpPrint(s->u.print.exps, t);
    }
    assert(0 || "stm error");
}

void interp(A_stm s)
{
    interpStm(s, NULL);
}

int main()
{
    interp(prog());
    return 0;
}


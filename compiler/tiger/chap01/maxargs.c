#include "util.h"
#include "slp.h"
#include "prog1.h"

int explistcnt(A_expList el)
{
    int cnt = 1;
    while (el->kind != A_lastExpList)
    {
        el = el->u.pair.tail;
        cnt++;
    }
    return cnt;
}

int maxargs(A_stm stm);
int expcnt(A_exp e)
{
    if ( e->kind == A_eseqExp )
        return maxargs(e->u.eseq.stm);

    return 0;
}

int explistcnt2(A_expList el)
{
    int a, b;

    if ( el->kind == A_lastExpList )
        return expcnt(el->u.last);

    a = expcnt(el->u.pair.head);
    b = explistcnt2(el->u.pair.tail);
    return a > b ? a : b;
}

int maxargs(A_stm stm)
{
    int a, b;
    switch (stm->kind)
    {
    case A_compoundStm:
        a = maxargs(stm->u.compound.stm1);
        b = maxargs(stm->u.compound.stm2);
        return a > b ? a : b;

    case A_assignStm:
        if ( stm->u.assign.exp->kind == A_eseqExp )
            return maxargs(stm->u.assign.exp->u.eseq.stm);
        break;

    case A_printStm:
        {
        int a, b;
        a = explistcnt(stm->u.print.exps);
        b = explistcnt2(stm->u.print.exps);
        return a > b ? a : b;
        }
    }

    return 0;
}

int main()
{
    printf("maxargs = %d\n", maxargs(prog()));
    return 0;
}


/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : inter_36.c
*      Purpose          : Interpolating the normalized correlation
*                       : with 1/3 or 1/6 resolution.
*
********************************************************************************
*/
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "inter_36.h"
const char inter_36_id[] = "@(#)$Id $" inter_36_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "cnst.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#define UP_SAMP_MAX  6

#include "inter_36.tab"

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  Interpol_3or6()
 *
 *  PURPOSE:  Interpolating the normalized correlation with 1/3 or 1/6
 *            resolution.
 *
 *************************************************************************/
Word16 Interpol_3or6 (  /* o : interpolated value                        */
    Word16 *x,          /* i : input vector                              */
    Word16 frac,        /* i : fraction  (-2..2 for 3*, -3..3 for 6*)    */
    Word16 flag3        /* i : if set, upsampling rate = 3 (6 otherwise) */
)
{
    Word16 i, k;
    Word16 *x1, *x2;
    const Word16 *c1, *c2;
    Word32 s;

    test();
    if (flag3 != 0)
    {
      frac = shl (frac, 1);   /* inter_3[k] = inter_6[2*k] -> k' = 2*k */
    }
    
    test (); 
    if (frac < 0)
    {
        frac = add (frac, UP_SAMP_MAX);
        x--;
    }
    
    x1 = &x[0];                         move16 (); 
    x2 = &x[1];                         move16 (); 
    c1 = &inter_6[frac];                move16 (); 
    c2 = &inter_6[sub (UP_SAMP_MAX, frac)]; move16 (); 

    s = 0;                              move32 (); 
    for (i = 0, k = 0; i < L_INTER_SRCH; i++, k += UP_SAMP_MAX)
    {
        s = L_mac (s, x1[-i], c1[k]);
        s = L_mac (s, x2[i], c2[k]);
    }

    return round (s);
}

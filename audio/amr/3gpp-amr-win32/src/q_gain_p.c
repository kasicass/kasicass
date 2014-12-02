/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : q_gain_p.c
*      Purpose          : Scalar quantization of the pitch gain
*
********************************************************************************
*/
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "q_gain_p.h"
const char q_gain_p_id[] = "@(#)$Id $" q_gain_p_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "basic_op.h"
#include "oper_32b.h"
#include "count.h"
#include "cnst.h"
 
/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#include "gains.tab"
 
/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
Word16 q_gain_pitch (   /* Return index of quantization                      */
    enum Mode mode,     /* i  : AMR mode                                     */
    Word16 gp_limit,    /* i  : pitch gain limit                             */
    Word16 *gain,       /* i/o: Pitch gain (unquant/quant),              Q14 */
    Word16 gain_cand[], /* o  : pitch gain candidates (3),   MR795 only, Q14 */ 
    Word16 gain_cind[]  /* o  : pitch gain cand. indices (3),MR795 only, Q0  */ 
)
{
    Word16 i, index, err, err_min;

    err_min = abs_s (sub (*gain, qua_gain_pitch[0]));
    index = 0;                                              move16 (); 

    for (i = 1; i < NB_QUA_PITCH; i++)
    {
        test ();
        if (sub (qua_gain_pitch[i], gp_limit) <= 0)
        {
            err = abs_s (sub (*gain, qua_gain_pitch[i]));
            
            test (); 
            if (sub (err, err_min) < 0)
            {
                err_min = err;                                  move16 (); 
                index = i;                                      move16 (); 
            }
        }
    }

    test ();
    if (sub (mode, MR795) == 0)
    {
        /* in MR795 mode, compute three gain_pit candidates around the index
         * found in the quantization loop: the index found and the two direct
         * neighbours, except for the extreme cases (i=0 or i=NB_QUA_PITCH-1),
         * where the direct neighbour and the neighbour to that is used.
         */
        Word16 ii;

        test ();
        if (index == 0)
        {
            ii = index;                                     move16 ();
        }
        else
        {
            test (); test ();
            if (   sub (index, NB_QUA_PITCH-1) == 0
                || sub (qua_gain_pitch[index+1], gp_limit) > 0)
            {
                ii = sub (index, 2);
            }
            else
            {
                ii = sub (index, 1);
            }
        }

        /* store candidate indices and values */
        for (i = 0; i < 3; i++)
        {
            gain_cind[i] = ii;                              move16 ();
            gain_cand[i] = qua_gain_pitch[ii];              move16 ();
            ii = add (ii, 1);
        }
        
        *gain = qua_gain_pitch[index];                      move16 (); 
    }
    else
    {
        /* in MR122 mode, just return the index and gain pitch found.
         * If bitexactness is required, mask away the two LSBs (because
         * in the original EFR, gain_pit was scaled Q12)
         */
       test ();
       if (sub(mode, MR122) == 0)
       {
          /* clear 2 LSBits */
          *gain = qua_gain_pitch[index] & 0xFFFC; logic16 (); move16 ();
       }
       else
       {
          *gain = qua_gain_pitch[index];                      move16 (); 
       }
    }
    return index;
}

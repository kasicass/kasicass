/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : d4_17pf.c
*      Purpose          : Algebraic codebook decoder
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "d4_17pf.h"
const char d4_17pf_c_id[] = "@(#)$Id $" d4_17pf_h;
 
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
#define NB_PULSE 4

#include "gray.tab"
 
/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  decod_ACELP()
 *
 *  PURPOSE:   Algebraic codebook decoder
 *
 *************************************************************************/

void decode_4i40_17bits(
    Word16 sign,   /* i : signs of 4 pulses.                       */
    Word16 index,  /* i : Positions of the 4 pulses.               */
    Word16 cod[]   /* o : algebraic (fixed) codebook excitation    */
)
{
    Word16 i, j;
    Word16 pos[NB_PULSE];

    /* Decode the positions */

    i = index & 7;                                       logic16 ();
    i = dgray[i];                                        move16 ();

    pos[0] = add(i, shl(i, 2));   /* pos0 =i*5 */        move16 ();

    index = shr(index, 3);
    i = index & 7;                                       logic16 ();
    i = dgray[i];                                        move16 ();

    i = add(i, shl(i, 2));        /* pos1 =i*5+1 */
    pos[1] = add(i, 1);                                  move16 ();

    index = shr(index, 3);
    i = index & 7;                                       logic16 ();
    i = dgray[i];                                        move16 ();

    i = add(i, shl(i, 2));        /* pos2 =i*5+1 */
    pos[2] = add(i, 2);                                  move16 ();

    index = shr(index, 3);
    j = index & 1;                                       logic16 ();
    index = shr(index, 1);
    i = index & 7;                                       logic16 ();
    i = dgray[i];                                        move16 ();

    i = add(i, shl(i, 2));        /* pos3 =i*5+3+j */
    i = add(i, 3);
    pos[3] = add(i, j);                                  move16 ();

    /* decode the signs  and build the codeword */

    for (i = 0; i < L_SUBFR; i++) {
        cod[i] = 0;                                      move16 ();
    }

    for (j = 0; j < NB_PULSE; j++) {
        i = sign & 1;                                    logic16 ();
        sign = shr(sign, 1);

        test ();
        if (i != 0) {
            cod[pos[j]] = 8191;                          move16 ();
        } else {
            cod[pos[j]] = -8192;                         move16 ();
        }
    }

    return;
}

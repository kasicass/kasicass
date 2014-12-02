/*
*****************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
*****************************************************************************
*
*      File             : d2_9pf.c
*      Purpose          : Algebraic codebook decoder
*
*****************************************************************************
*/

/*
*****************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
*****************************************************************************
*/
#include "d2_9pf.h"
const char d2_9pf_c_id[] = "@(#)$Id $" d2_9pf_h;
 
/*
*****************************************************************************
*                         INCLUDE FILES
*****************************************************************************
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
#define NB_PULSE 2           /* number of pulses */
 
#include "c2_9pf.tab"

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  decode_2i40_9bits (decod_ACELP())
 *
 *  PURPOSE:   Algebraic codebook decoder. For details about the encoding se
 *             c2_9pf.c
 *
 *************************************************************************/

void decode_2i40_9bits(
    Word16 subNr,  /* i : subframe number                          */
    Word16 sign,   /* i : signs of 2 pulses.                       */
    Word16 index,  /* i : Positions of the 2 pulses.               */
    Word16 cod[]   /* o : algebraic (fixed) codebook excitation    */
)
{
    Word16 i, j, k;
    Word16 pos[NB_PULSE];

    /* Decode the positions */
    /* table bit  is the MSB */ 
    j = shr((index & 64),6);                                 logic16 ();

    i = index & 7;                                       logic16 ();

    i = add(i, shl(i, 2));       /* pos0 =i*5+startPos[j*8+subNr*2] */
    k = startPos[add(shl(j, 3), shl(subNr, 1))];
    pos[0] = add(i, k);                                  move16 ();    

    index = shr(index, 3);
    i = index & 7;                                       logic16 ();

    i = add(i, shl(i, 2));       /* pos1 =i*5+startPos[j*8+subNr*2+1] */
    k = startPos[add(add(shl(j, 3), shl(subNr, 1)), 1)];
    pos[1] = add(i, k);                                  move16 ();

    /* decode the signs  and build the codeword */

    for (i = 0; i < L_SUBFR; i++) {
        cod[i] = 0;                                      move16 ();
    }

    for (j = 0; j < NB_PULSE; j++) {
        i = sign & 1;                                    logic16 ();
        sign = shr(sign, 1);

        test ();
        if (i != 0) {
            cod[pos[j]] = 8191;                          move16 (); /* +1.0 */
        } else {
            cod[pos[j]] = -8192;                         move16 (); /* -1.0 */
        }
    }

    return;
}

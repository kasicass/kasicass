/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : d2_11pf.c
*      Purpose          : Algebraic codebook decoder
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "d2_11pf.h"
const char d2_11pf_c_id[] = "@(#)$Id $" d2_11pf_h;
 
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
#define NB_PULSE 2           /* number of pulses */
 
/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  decode_2i40_11bits (decod_ACELP())
 *
 *  PURPOSE:   Algebraic codebook decoder
 *
 *************************************************************************/

void decode_2i40_11bits(
    Word16 sign,   /* i : signs of 2 pulses.                       */
    Word16 index,  /* i : Positions of the 2 pulses.               */
    Word16 cod[]   /* o : algebraic (fixed) codebook excitation    */
)
{
    Word16 i, j;
    Word16 pos[NB_PULSE];

    /* Decode the positions */

    j = index & 1;                                       logic16 ();
    index = shr(index, 1);
    i = index & 7;                                       logic16 ();

    i = add(i, shl(i, 2));        /* pos0 =i*5+1+j*2 */
    i = add(i, 1);
    j = shl(j, 1);  
    pos[0] = add(i, j);                                  move16 ();
       
    index = shr(index, 3);
    j = index & 3;                                       logic16 ();
    index = shr(index, 2);
    i = index & 7;                                       logic16 ();

    test();
	if (sub(j, 3) == 0)
    {       
       i = add(i, shl(i, 2));        /* pos1 =i*5+4 */
       pos[1] = add(i, 4);                               move16 ();
    }
    else
    {       
       i = add(i, shl(i, 2));        /* pos1 =i*5+j */
       pos[1] = add(i, j);                               move16 ();
    }
       
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
